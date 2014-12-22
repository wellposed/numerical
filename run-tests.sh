#!/bin/sh

set -e

cabal configure --enable-tests --enable-coverage
cabal build
cabal test

cabal configure --enable-tests --enable-coverage \
    --builddir=dist-O2 --ghc-options="-O2"
cabal build --builddir=dist-O2
cabal test --builddir=dist-O2

cabal configure --enable-tests --enable-coverage \
    --builddir=dist-O2-llvm --ghc-options="-O2 -fllvm"
cabal build --builddir=dist-O2-llvm
cabal test --builddir=dist-O2-llvm
