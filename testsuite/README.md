Haskell contracts checker testsuite
===================================

Run testsuite
-------------

    runghc RunTests.hs *.hs

View all models
---------------

    MODELS=true runghc RunTests.hs *.hs       # or, alternatively:
    HCC_ARGS='--all-disjoint --ignore-types --print-raw-model' MODELS=true runghc RunTests.hs *.hs

Compare different theorem provers and settings
----------------------------------------------

    ./run-comparative-tests.sh
    runghc ResultSummary.hs res/res-*

Internally, this runs this command to get times in csv:

    TIMING=true runghc RunTests.hs *.hs

Options
-------

There are various options to `RunTests.hs` set as environment variables:

  * `PROFILE`: toggles the `+RTS -prof` flag to `hcc`,

  * `READABLE`: write readable tptp instead of dumping tptp with variables from `Unique`s,

  * `OPTIMISE`: use `--core-optimise` which runs the core2core passes,

  * `TYPED_METAS`: spell out typed metas in models,

  * `HCC_ARGS`: give arbitrary settings to `hcc`,

  * `ONLY`: set this to `UNSAT` or `SAT` to only test such statements,

  * `MIN`: set this to `false` to turn off min,

  * `TIMEOUT`: set the timeout to be something else than 1 second,

  * `PROVERS`: set the provers to use, each identified with a character:

    - `z`: z3
    - `s`: z3 using SMTLIB lineariser
    - `e`: eprover
    - `v`: vampire
    - `x`: equinox
    - `p`: paradox

    If this flag is not set, all provers will be run, with paradox
    last if assumed to be UNSAT, otherwise first.

Details
-------

The testsuite runner uses `sed` to change `$min` to `min` because z3's
tptp parser does not understand `$min`. So we get three files for each:

    SimpleExtensions.param_bad_unjuggle_id.tptp
    SimpleExtensions.param_bad_unjuggle_id.tptp.z3
    SimpleExtensions.param_bad_unjuggle_id.smt

Where the second is just this change from the first.

