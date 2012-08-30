#!/bin/bash

# only use z3, and only consider problems thought to be UNSAT
export PROVERS=z
export ONLY=UNSAT
export TIMEOUT=1
export TIMING=true

FILES="*.hs"

SETTING[0]='-'
SETTING[1]='-var-scrut-constr'
SETTING[2]='-case-lift-inner'
SETTING[3]='-no-skolemisation'
SETTING[4]='-no-pull-quants'

# store everything in results directory and subdirectories
mkdir results -p
cd results

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
