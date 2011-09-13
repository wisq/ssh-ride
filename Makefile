all: ssh-ride

install: ssh-ride
	cp -f ssh-ride ~/bin/ssh-ride

ssh-ride: ssh-ride.hs
	ghc --make -Wall ssh-ride

clean:
	rm -f ssh-ride *.hi *.o
