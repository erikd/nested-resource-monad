#!/bin/bash -eu

(cd nested-resource && \
  cabal-1.24 install --only-dependencies --enable-tests && \
  cabal-1.24 configure --enable-tests && \
  cabal-1.24 build && \
  dist/build/test/test && \
  cabal-1.24 haddock && \
  cabal-1.24 sdist)

(cd nested-resource-linux && \
  cabal-1.24 install --only-dependencies --enable-tests && \
  cabal-1.24 configure --enable-tests && \
  cabal-1.24 build && \
  cabal-1.24 haddock && \
  cabal-1.24 sdist)
