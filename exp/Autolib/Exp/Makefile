# $Header$

MAIN = Loop

GHC = ghc --make -O2
INC =  -i../util -i../
FLAGS = -fglasgow-exts \
	-fallow-overlapping-instances \
	-fallow-undecidable-instances

all : $(MAIN)

$(MAIN) : $(wildcard *.hs)
	$(GHC) $(FLAGS) $(INC) $(MAIN).hs -o $(MAIN) $(PACKS)

