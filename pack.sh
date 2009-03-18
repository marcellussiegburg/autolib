#!/bin/bash

pre=autolib
version=1.0

for cab in */package.cabal
do
    base=$(dirname $cab)
    cp -v $cab archive/$pre-$base.cabal
    cp -v $base/dist/*.tar.gz archive/
done

tar cvfz 00-index.tar.gz archive/*.cabal
