# Makefile for FLP project - xgarip00
# Translation of the main file with the -Wall flag
# Resulting binary: flp-fun

all: flp-fun

flp-fun: flp-fun.hs
	ghc -Wall -o flp-fun flp-fun.hs

clean:
	rm -f *.hi *.o flp-fun
