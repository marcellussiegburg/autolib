# $Header$

MAIN = Loop

GHC = ghc --make -O
INC = -i../fa -i../util -i../
PACKS = -package data -package text
FLAGS = -fglasgow-exts \
	-fallow-overlapping-instances \
	-fallow-undecidable-instances

all : $(MAIN)

$(MAIN) : $(wildcard *.hs)
	$(GHC) $(FLAGS) $(INC) $(MAIN).hs -o $(MAIN) $(PACKS)

