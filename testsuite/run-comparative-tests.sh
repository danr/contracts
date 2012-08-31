#!/bin/bash

# try z3, z3 (smt), eprover, vampire, equinox and only on UNSAT problems
export PROVERS=zsevx
export ONLY=UNSAT
export TIMEOUT=1
export TIMING=true

FILES="*.hs"

SETTING[0]='-'
SETTING[1]='m'
SETTING[2]='-var-scrut-constr'
SETTING[3]='-case-lift-inner'
SETTING[4]='-no-skolemisation'
SETTING[5]='-no-pull-quants'

# store everything in res directory and subdirectories
mkdir res -p
cd res

for ARGS in ${SETTING[*]}
do
    export HCC_ARGS="-$ARGS"
    echo $HCC_ARGS
    echo $TIMING
    echo $PROVERS
    # we save all files in the directory files-$ARGS. All *.hs files
    # are copied there and the Contracts.hs file. If we had them in
    # the same directories, they would overwrite each other.
    DIR=files-$ARGS
    mkdir $DIR -p
    cp ../$FILES $DIR
    cp ../Contracts.hs $DIR
    (cd $DIR ; (runghc ../../RunTests.hs $FILES | tee ../res-$ARGS)) &
done
