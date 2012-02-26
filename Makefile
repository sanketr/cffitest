all: install 

install:
	gcc -O2 -c test.c -Wall 
	ghc -o T -O2 T.hs test.o -lpthread -threaded -rtsopts
	rm -rf *stub* *.hi

clean: 
	rm -rf *.o c t T *.hi *stub*
