#!/bin/sh

BRANCH=$1

if [ $# -ne 1 ]; then
    echo "Usage: contracts-checkout <branch-name>"
else
    echo "Checking out contracts branch $BRANCH"
    git checkout $BRANCH
    git submodule update
    HC=`cd halo && git log --pretty=oneline -1 HEAD`
    BC=`cd halo && git log --pretty=oneline -1 $BRANCH`
    if [ "$HC" != "$BC" ]; then
    	echo "Aborting: halo submodule not in sync with $BRANCH"
	echo "HEAD   :" $HC
	echo "$BRANCH:" $BC
    else
    	cd halo && git checkout $BRANCH
    fi
fi