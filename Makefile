all :
	ghc --make -Wall mastermind.hs

clean : 
	rm -f mastermind mastermind.hi mastermind.o
