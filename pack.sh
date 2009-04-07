#!/bin/bash

version=1.0 # version info must be extracted from individual .cabal files

target=archive

rm -rf $target

for cab in */*.cabal
do
    base=$(dirname $cab)
    ( cd $base ; cabal configure && cabal sdist )
    name=$(basename $cab .cabal)
    xcab=$target/$name/$version/$(basename $cab)
    mkdir -p $(dirname $xcab)
    cp -v $cab $xcab
    ball=$target/packages/$name-$version/tarball
    mkdir -p $(dirname $ball)
    cp -v $base/dist/$name-$version.tar.gz $ball
done

( cd $target ; tar cvfz 00-index.tar.gz $(find . -name "*.cabal") )

for cab in */*.cabal
do
    name=$(basename $cab .cabal)
    rm -rfv $target/$name
done

