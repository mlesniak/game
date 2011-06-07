OPTIONS=--make -O2 -threaded -Wall -fno-warn-unused-do-bind
SOURCES=Window.hs Main.hs Tools.hs

all: 
	ghc $(OPTIONS) $(SOURCES) -main-is Main -o main 

hlint:
	hlint *.hs

clean:
	rm -f *.o *.hi tags main
