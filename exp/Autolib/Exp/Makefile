# $Header$

MAIN = Loop

GHC = ghc --make -O2

FLAGS = -fglasgow-exts \
	-fallow-overlapping-instances \
	-fallow-undecidable-instances

all : $(MAIN)

$(MAIN) : $(wildcard *.hs)
	$(GHC) $(FLAGS) $(MAIN).hs -o $(MAIN) $(PACKS)

