#!/bin/bash

# in proper order:
for pack in \
    derive \
    todoc tex reader output data reporter \
    algorithm util relation \
    dot fa exp rewriting fta graph \
    genetic \
    lib 
do
    echo "entering $pack"
    pushd $pack
    # cabal clean
    # cabal build
    cabal install --global --root-cmd=sudo
    cabal sdist
    popd
    echo "leaving $pack"
done
