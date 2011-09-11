all: ssh-ride

ssh-ride: ssh-ride.hs
	ghc --make ssh-ride

clean:
	rm -f ssh-ride *.hi *.o
