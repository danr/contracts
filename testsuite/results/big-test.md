The `big-test-60s-*' files were run from testsuite in the the commit:

    commit c7bdacfba86089f1e79941736351e3b9fb822d88
    Author: Dan Rosén <danr@student.chalmers.se>
    Date:   Tue Jul 10 09:19:34 2012 +0100

        Add raw results from the big test suite run

This commit has the tag `big-test'.

They were run on cam-02-unx and the run time is about 8 hours. The
timeout for the theorem provers were 60 seconds.

The inliner versions were run with and without optimisation, from commit:

    commit 716440914f80ce3afdd7425a12a8c06f50bf8313
    Author: Dan Rosén <danr@student.chalmers.se>
    Date:   Wed Jul 18 10:24:37 2012 +0100

        Add inliner and use it when translating contracts

This commit has the tag `inliner'.

Results of running `runghc Compare.hs big-test-60s big-test-60s-inlining big-test-60s-inlining-no-optimise big-test-60s-min big-test-60s-min-inlining big-test-60s-min-inlining-no-optimise`

    Total   SAT     UNS successes   Filename
    102     29      73      big-test-60s
    106     29      77      big-test-60s-inlining
    92      26      66      big-test-60s-inlining-no-optimise
    126     48      78      big-test-60s-min
    127     48      79      big-test-60s-min-inlining
    127     48      79      big-test-60s-min-inlining-no-optimise


    Prover : paradox
    40      26      14      0.007   0.007   0.007   big-test-60s
    40      26      14      0.006   0.005   0.006   big-test-60s-inlining
    37      23      14      0.004   0.003   0.006   big-test-60s-inlining-no-optimise
    52      47      5       0.346   0.383   0.002   big-test-60s-min
    52      47      5       0.237   0.261   0.004   big-test-60s-min-inlining
    52      47      5       0.284   0.314   0.002   big-test-60s-min-inlining-no-optimise


    Prover : equinox
    74      26      48      1.659   0.008   2.554   big-test-60s
    76      26      50      3.375   0.006   5.126   big-test-60s-inlining
    68      23      45      3.319   0.003   5.014   big-test-60s-inlining-no-optimise
    110     34      76      2.321   1.887   2.515   big-test-60s-min
    111     34      77      3.488   2.038   4.129   big-test-60s-min-inlining
    112     34      78      3.016   1.988   3.464   big-test-60s-min-inlining-no-optimise


    Prover : z3
    96      26      70      0.007   0.002   0.009   big-test-60s
    101     26      75      0.166   0.002   0.224   big-test-60s-inlining
    87      23      64      0.006   0.000   0.008   big-test-60s-inlining-no-optimise
    101     25      76      0.044   0.028   0.049   big-test-60s-min
    101     25      76      0.037   0.024   0.041   big-test-60s-min-inlining
    101     25      76      0.037   0.024   0.042   big-test-60s-min-inlining-no-optimise


    Prover : vampire
    69      0       69      1.193   NaN     1.193   big-test-60s
    69      0       69      1.273   NaN     1.273   big-test-60s-inlining
    64      0       64      1.737   NaN     1.737   big-test-60s-inlining-no-optimise
    72      0       72      0.676   NaN     0.676   big-test-60s-min
    72      0       72      0.915   NaN     0.915   big-test-60s-min-inlining
    72      0       72      0.761   NaN     0.761   big-test-60s-min-inlining-no-optimise


    Prover : eprover
    87      29      58      0.406   0.004   0.608   big-test-60s
    89      29      60      0.098   0.003   0.144   big-test-60s-inlining
    82      26      56      0.079   0.004   0.114   big-test-60s-inlining-no-optimise
    94      28      66      0.077   0.008   0.107   big-test-60s-min
    96      29      67      0.045   0.011   0.059   big-test-60s-min-inlining
    95      28      67      0.623   0.005   0.881   big-test-60s-min-inlining-no-optimise
