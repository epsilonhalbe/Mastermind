all :
	ghc --make -Wall Mastermind.hs

clean : 
	rm -f Mastermind Mastermind.hi Mastermind.o
