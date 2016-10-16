quickbench - quick & easy comparative benchmarking of command-line programs.
(c) Simon Michael 2008-2016

This is a cleaned up successor to the simplebench benchmarking tool 
from the hledger  project. It can be used for general command-line 
benchmarking (quickbench) or as a  haskell library (QuickBench). 
It is not smart or complicated like "bench" or criterion; it is 
good for quick and dirty, exploratory, comparative measurements
that you can understand at a glance.

$ git clone https://github.com/simonmichael/quickbench.git
$ stack install ./quickbench
# ensure $PATH includes ~/.local/bin
$ quickbench -h
