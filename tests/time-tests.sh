#!/bin/bash
#
# If the contract name contains holds, thm, or unsat it should
# indeed hold, otherwise there should be a countermodel.
#
# Tries paradox, equinox and vampire (in that order)
# To remove a tool, uncomment line towards the end of this file
#

#!/bin/bash

TIMEOUT=60

# Kill whole testing process on Ctrl-C
trap 'exit 1' INT

# Remove all tptp files here
rm -v *.tptp

# Generate all contracts
hcc $@ -d -i -b

# Result is stored in .tmp
# Time info is stored in .tmptime
# Arguments
#    $1 : is a good grep
#    $2 : is a bad grep
#    $3 : says what the tool name is
function res_parser {
#    echo "Res parser $1 $2 from $3"
    if [[ `grep -i " $1" .tmp` ]]; then
        echo -ne "$3:`cat .tmptime`\t"
        return 0
    elif [[ `grep -i " $2" .tmp` ]]; then
        echo -ne "$3:FAIL\t"
        return 0
    else
        echo -ne "$3:----\t"
        return 1
    fi
}

# Arguments
#   $1 : Tool to run (equinox/paradox)
#   $2 : Timeout in seconds
#   $3 : Filename
#   $4 : good grep
#   $5 : bad grep
#   $6 : Short name of tool
function run_koentool {
#    echo "koentool $4 $5"
    ((TIME=%U time --quiet timeout $2 $1 --no-progress --tstp $3) 2> .tmptime | grep RESULT) > .tmp
    res_parser $4 $5 $6
}

# Arguments
#   $1 : Timeout in seconds
#   $2 : Filename
#   $3 : good grep
#   $4 : bad grep
function run_vampire {
    ((TIME=%U time --quiet timeout $1 vampire_lin32 -mode casc -t $1 < $2) 2> .tmptime | grep status) > .tmp
    res_parser $3 $4 v
}

# Arguments
#   $1 : Timeout in seconds
#   $2 : Filename
#   $3 : good grep
#   $4 : bad grep
function run_eprover {
    ((TIME=%U time --quiet eprover -tAuto -xAuto --tptp3-format $2 --cpu-limit=$1 -s) 2> .tmptime | grep status) > .tmp
    res_parser $3 $4 e
}

# Arguments
#   $1 : Timeout in seconds
#   $2 : Filename
#   $3 : good grep
#   $4 : bad grep
function run_z3 {
    sed 's/\$min/min/g' $2 > .z3.tmp
    ((TIME=%U time --quiet timeout $1 z3 -tptp -nw .z3.tmp) 2> .tmptime | grep status) > .tmp
    res_parser $3 $4 z
}

for FILE in `find -iname '*.tptp'`
do
    # holds=0 if it should hold, 1 otherwise
    holds=`echo $FILE | egrep '(unsat|thm|holds)'`
    # We skip things that are not "big" when timing
#    if [[ `echo $FILE | grep -v big` ]]; then
#        continue
#    fi
    if [[ $holds ]]; then
        # continue
        printf "UNS %-50s\t" `basename $FILE .tptp`
        good="unsat"
        bad="sat"
    else
        # continue
        printf "SAT %-50s\t" `basename $FILE .tptp`
        good="sat"
        bad="unsat"
    fi
    run_koentool paradox $TIMEOUT $FILE $good $bad p
    run_koentool equinox $TIMEOUT $FILE $good $bad x
    run_z3               $TIMEOUT $FILE $good $bad
    run_vampire          $TIMEOUT $FILE $good $bad
    run_eprover          $TIMEOUT $FILE $good $bad
    echo
done

