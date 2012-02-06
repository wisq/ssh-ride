import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.Process
import System.Posix.Process
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath
import Data.List
import Data.List.Split (splitOn)
import System.IO
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (sha1, showDigest)

data Options = Options
  { optRawOpts :: [String]
  , optNonOpts :: [String]
  , optMaster  :: Bool
  , optTTY     :: Maybe Bool
  , optX11     :: Maybe Bool
  , optAbort   :: Bool
  , optLogin   :: Maybe String
  , optPort    :: Maybe Int
  } deriving Show

emptyOptions :: Options
emptyOptions = Options
  { optRawOpts = []
  , optNonOpts = []
  , optMaster  = False
  , optTTY     = Nothing
  , optX11     = Nothing
  , optLogin   = Nothing
  , optPort    = Nothing
  , optAbort   = False
  }

type OptMunger = (Options -> Options)
type OptArgMunger = String -> OptMunger
type MungeDescr = OptDescr OptMunger

allOpts :: [MungeDescr]
allOpts =
    [ shortOption 'M' False (setMaster True)
    , shortOption 't' True  (setTTY True)
    , shortOption 'T' True  (setTTY False)
    , shortOption 'X' True  (setX11 True)
    , shortOption 'x' True  (setX11 False)
    , shortOption 'V' False setAbort

    , shortOptArg 'S' False (dropArg setAbort)
    , shortOptArg 'l' True  setLogin
    , shortOptArg 'p' True  setPort

    -- Debian:
    , Option [] ["help"] (NoArg setAbort) ""
    ]
    ++ otherOptions "1246afgknqsvACKNPYy"
    ++ otherOptArgs "bceimoDFILORwWy"
  where
    otherOptions = map (\x -> shortOption x True (passOpt x))
    otherOptArgs = map (\x -> shortOptArg x True (passArg x))
    passOpt x     = addRawOpts [['-', x]]
    passArg x arg = addRawOpts [['-', x], arg]

setMaster :: Bool -> OptMunger
setMaster b opt = opt { optMaster = b }

setTTY :: Bool -> OptMunger
setTTY b opt = opt { optTTY = Just b }

setX11 :: Bool -> OptMunger
setX11 b opt = opt { optX11 = Just b }

setLogin :: OptArgMunger
setLogin arg opt = opt { optLogin = Just arg }

setPort :: OptArgMunger
setPort arg opt = opt { optPort = Just (read arg) }

setAbort :: OptMunger
setAbort opt = opt { optAbort = True }

addRawOpts :: [String] -> OptMunger
addRawOpts args opt = opt { optRawOpts = optRawOpts opt ++ args }

shortOption :: Char -> Bool -> OptMunger -> MungeDescr
shortOption c pass f = Option [c] [] (NoArg func) ""
  where
    func | pass = addRawOpts [['-', c]] . f
         | otherwise = f

shortOptArg :: Char -> Bool -> OptArgMunger -> MungeDescr
shortOptArg c pass f = Option [c] [] (ReqArg func "") ""
  where
    func arg | pass = addRawOpts [['-', c], arg] . f arg
             | otherwise = f arg

dropArg :: OptMunger -> OptArgMunger
dropArg f _ = f

main :: IO ()
main = do
    args <- getArgs
    let abort = executeRawSSH args

    let (mungers, nonArgs, errs) = getOpt RequireOrder allOpts args
    let opt = foldl (flip ($)) (startOptions nonArgs) mungers

    unless (null errs) (showErrors errs >> abort)
    when (optAbort opt) abort

    rideSSH (tunnelMode opt) opt
  where
    startOptions non = emptyOptions { optNonOpts = non }
    showErrors = mapM $ putStr . ("ssh-ride: " ++)

executeRawSSH :: [String] -> IO ()
executeRawSSH args = executeFile "/usr/bin/ssh" False args Nothing

executeSSH :: Options -> IO ()
executeSSH = executeRawSSH . optionArgs

optionArgs :: Options -> [String]
optionArgs opt = optRawOpts opt ++ optNonOpts opt

trySSH :: Options -> IO ExitCode
trySSH opt = do
  devNull <- openFile "/dev/null" WriteMode
  (_, _, _, ph) <- createProcess (proc "/usr/bin/ssh" (optionArgs opt)) { std_err = UseHandle devNull }
  waitForProcess ph

data TunnelMode = CreateTunnel | UseTunnel | IgnoreTunnel
  deriving Show

tunnelMode :: Options -> TunnelMode
tunnelMode opt
    | optMaster opt = CreateTunnel
    | interactive   = CreateTunnel
    | optX11 opt == Just True = CreateTunnel
    | optTTY opt == Just True = IgnoreTunnel
    | otherwise     = UseTunnel
  where interactive = length (optNonOpts opt) < 2

socketPrefix :: Options -> String
socketPrefix opt = hash fields
  where
    hash   = take 8 . showDigest . sha1 . pack
    fields = intercalate "-" [host, show port, login]
    host   = head (optNonOpts opt)
    port   = fromMaybe 22 (optPort opt)
    login  = fromMaybe "none" (optLogin opt)

socketSuffix :: FilePath
socketSuffix = "%h-%p-%r.ctl"

getTunnelFile :: Options -> IO FilePath
getTunnelFile opt = do
    time <- getCurrentTime
    let file = intercalate "__" [socketPrefix opt, (show . stamp) time, socketSuffix]
    path <- getTunnelDirectory
    return $ joinPath [path, file]
  where
    stamp :: UTCTime -> Integer
    stamp = round . utcTimeToPOSIXSeconds

getTunnelDirectory :: IO FilePath
getTunnelDirectory = do
  sshDir <- getAppUserDataDirectory "ssh"
  return $ joinPath [sshDir, "ride"]


rideSSH :: TunnelMode -> Options -> IO ()

rideSSH IgnoreTunnel opt =
  (executeSSH . addRawOpts ["-S", "none"]) opt

rideSSH CreateTunnel opt = do
  getTunnelDirectory >>= createDirectoryIfMissing True
  file <- getTunnelFile opt
  (executeSSH . addRawOpts ["-M", "-S", file]) opt

rideSSH UseTunnel opt = do
    tunDir <- getTunnelDirectory
    files  <- getDirectoryContents tunDir
    let matched = filter (\f -> hasPrefix f && hasSuffix f) files
    _ <- mapM (tryFile . (\f -> joinPath [tunDir, f])) matched
    rideSSH IgnoreTunnel opt
  where
    hasPrefix = ((socketPrefix opt ++ "__") `isPrefixOf`)
    hasSuffix = (".ctl" `isSuffixOf`)
    unfill = intercalate "__" . (++ [socketSuffix]) . take 2 . splitOn "__" . takeBaseName
    tryFile f = do
      let f' = joinPath $ map ($f) [takeDirectory, unfill]
      let opt' = addRawOpts ["-S", f'] opt
      code <- (trySSH . addRawOpts ["-O", "check"]) opt'
      when (code == ExitSuccess) (executeSSH opt')
