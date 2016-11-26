ssh-ride
========

ssh-ride is a wrapper for OpenSSH that automatically accelerates SSH connections, by having short sessions piggyback ("ride") on longer sessions.

The problem
-----------

OpenSSH offers a means for a new SSH session to ride on an existing SSH session, using the ControlPath (-S) and ControlMaster (-M) configuration options.  By setting your ControlPath to a common value and applying -M as desired, subsequent SSH sessions will reuse the existing SSH connection rather than creating a new one.

This is a great feature.  However, trying to use it in an automatic fashion creates some hassles.  If you don't enable ControlMaster by default, then you have to remember to use -M for master sessions.  If you do enable it, it doesn't distinguish between connection types -- you can end up with multiple long-running sessions all dying when a single master closes, and the master process might even be something short-lived like a file transfer.

The solution
------------

ssh-ride classifies SSH sessions according to their predicted length (long/short or unknown).

 * Long sessions create sockets for riding.
 * Short sessions ride on long sessions.
 * Intermediate / unknown sessions don't ride at all.

No other riding occurs, meaning you don't end up with (say) multiple long sessions all riding on (and dying along with) a single long session.

Installation
------------

ssh-ride is written in Haskell and must be compiled using the Glasgow Haskell Compiler (GHC), available at [http://www.haskell.org/ghc/].  GHC packages for most Linux distributions are available.  Mac OSX users should use the 7.x packages directly from the GHC website; using GHC from Macports is not recommended.

Once compiled (via "make"), you should install it somewhere in your $PATH.  You can either install it as "ssh" (and make sure it comes before /usr/bin/ssh in your $PATH) so you never have to think about it, or some other name (e.g. "sshr") if you prefer.

How it works
------------

Sessions are classified using these rules:

 * A session is rideable (creates a socket) ...
  * if -M is specified, or
  * if interactive (no command is specified)
 * A non-interactive session does *not* ride ...
  * if a terminal is requested (-t), or
  * if X11 forwarding is requested (-X)
 * Otherwise, the session will ride an existing rideable session.

Interactive sessions are rideable because they usually last a while, and you have direct control over when they close.

Sessions that run a remote command will generally ride if they can.  However, they do not ride if they request X11 forwarding or a terminal, since that usually indicates longer-running or more important commands that shouldn't be interrupted if a master session goes down.

When creating sockets for riding purposes, ssh-ride will create a unique socket for every long-running session.  When choosing a socket to ride, it will test all available sockets and use the first one that works.  This increases the likelihood of there being a rideable socket available.

Feedback regarding these rules is welcome (based on your own SSHing habits).

License
-------

ssh-ride is released under the standard 2-clause BSD license.  See COPYING for details.

To-do
-----

 * Serious code cleanup.  The options parsing needs to be split out, and I was a bit rushed when I wrote the final bits to tie it all together.
 * Tests, if possible.
 * Timeouts while testing SSH sockets, in case some of them are unresponsive and just stall the test.
 * Parallel testing of all SSH sockets, for the same reason.
