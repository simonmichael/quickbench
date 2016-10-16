quickbench - quick & easy comparative benchmarking of command-line programs.
(c) Simon Michael 2008-2016

Can be used standalone as the quickbench executable, or
in haskell project benchmark suites as the QuickBench library.

This is based on the simplebench tool used in the hledger project.

Old:

Example:

$ stack install
$ quickbench --help
...
$ cat - >bench.tests
-f sample.ledger -s balance
-f ~/.ledger -s balance
$ quickbench -v hledger "ledger --no-cache" ledger
Using bench.tests
Running 2 tests 2 times in . with 3 executables at 2008-11-26 18:52:15.776357 UTC:
1: hledger -f sample.ledger -s balance	[0.02s]
2: hledger -f sample.ledger -s balance	[0.01s]
1: ledger --no-cache -f sample.ledger -s balance	[0.02s]
2: ledger --no-cache -f sample.ledger -s balance	[0.02s]
1: ledger -f sample.ledger -s balance	[0.02s]
2: ledger -f sample.ledger -s balance	[0.02s]
1: hledger -f ~/.ledger -s balance	[3.56s]
2: hledger -f ~/.ledger -s balance	[3.56s]
1: ledger --no-cache -f ~/.ledger -s balance	[0.10s]
2: ledger --no-cache -f ~/.ledger -s balance	[0.10s]
1: ledger -f ~/.ledger -s balance	[0.10s]
2: ledger -f ~/.ledger -s balance	[0.10s]

Summary (best iteration):

                            || hledger | ledger --no-cache | ledger
============================++=========+===================+=======
-f sample.ledger -s balance ||    0.01 |              0.02 |   0.02
    -f ~/.ledger -s balance ||    3.56 |              0.10 |   0.10
