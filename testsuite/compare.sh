# Compares the test suite results for two points in history (typically branches)

#!/bin/bash

git checkout $1
pushd ..
git submodule update
cabal install
popd

runghc RunTests.hs *.hs | tee compare-res-$1

git checkout $2
pushd ..
git submodule update
cabal install
popd

runghc RunTests.hs *.hs | tee compare-res-$2

