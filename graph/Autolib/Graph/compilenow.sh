#!/bin/bash

if test -z $1; then
  echo "usage $0 Loesung.hs";
  exit;
fi

ghc --make -fallow-overlapping-instances                       \
           -fallow-undecidable-instances                       \
           -fglasgow-exts                                      \
           -i../hugslib/data/ -i../hugslib/text/parsec/  \
	   $1

