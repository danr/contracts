#!/bin/bash

echo "Should all be UNSAT"
(for i in `ls Good*unsat*tptp`
do echo $i
   (equinox $i | grep RESULT)
done | grep -B 1 Satisfiable)

echo "Should all be SAT"
(for i in `ls Good.sat*tptp`
do echo $i
   (equinox $i | grep RESULT)
done | grep -B 1 Unsatisfiable)

