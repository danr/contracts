#!/bin/sh

BRANCH=$1

if [ $# -ne 1 ]; then
    echo "Usage: contracts-checkout <branch-name>"
else
    echo "Checking out contracts branch $BRANCH"
    git checkout $BRANCH || exit
    git submodule update || exit
    HC=`cd halo && git log --pretty=oneline -1 HEAD`
    BC=`cd halo && git log --pretty=oneline -1 $BRANCH`
    BC_EXIT_CODE=$?
    if [ "$HC" != "$BC" ]; then
    	echo "Aborting: halo submodule not in sync with $BRANCH"
        echo 'HEAD:    ' $HC
        echo '$BRANCH: ' $BC
        if [ $BC_EXIT_CODE -ne 0 ]; then
            echo "Branch $BRANCH does not seem to exist in halo."
            echo "Possible solution is to track it:"
            echo '    $ cd halo'
            echo "    \$ git checkout -t origin/$BRANCH"
        elif [ ! `cd halo && git diff --quiet $BRANCH origin/$BRANCH` ]; then
            echo "Branch $BRANCH does not seem to be up to date in halo."
            echo "Possible solution is to pull:"
            echo '    $ cd halo'
            echo "    \$ git checkout $BRANCH"
            echo '    $ git pull'
        fi
    else
    	cd halo && git checkout $BRANCH
    fi
fi
