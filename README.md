# quickbench 1.0

Quick & easy benchmarking of command-line programs.

  [About](#about)
| [Examples](#examples) 
| [Usage](#usage)
| [Related](#related-work)


## About

quickbench is an update and repackaging of a little benchmarking tool I've been 
using in the hledger project since 2008.
Use it like a more powerful "time" command for measuring the time taken by command-line programs,
or for creating repeatable benchmark scripts for your projects.

quickbench produces very simple output (elapsed seconds),
as quickly as possible (running commands just once by default),
and tabulates results from multiple executables.
I find it very useful for quick and dirty, exploratory, and comparative measurements
that you can understand at a glance.

## Examples

Install it easily on most platforms with [stack](https://haskell-lang.org/get-started) (or cabal). 
It's not yet on Hackage, so you'll need the source:

```
$ git clone https://github.com/simonmichael/quickbench
$ cd quickbench
$ stack install   # ensure $PATH includes ~/.local/bin
```

You can specify test commands as arguments:
```
$ quickbench 'sleep 1'
Running 1 tests 1 times at 2016-10-16 23:06:48.058578 UTC:

Best times:
+---------++------+
|         ||      |
+=========++======+
| sleep 1 || 1.01 |
+---------++------+
```

or put them in a file and use `-f FILE`. 
A file named `bench.sh` in the current directory will be used automatically:
```
$ echo 'echo 3 * 1000000' > bench.sh
$ quickbench
Running 1 tests 1 times at 2016-10-16 23:53:04.743899 UTC:

Best times:
+------------------++------+
|                  ||      |
+==================++======+
| echo 3 * 1000000 || 0.00 |
+------------------++------+
```

You can compare results with different executables:
```
$ quickbench -w echo,expr -p5
Running 1 tests 1 times with 2 executables at 2016-10-16 23:56:40.808703 UTC:

Best times:
+-------------++---------+---------+
|             ||    echo |    expr |
+=============++=========+=========+
| 3 * 1000000 || 0.00229 | 0.00195 |
+-------------++---------+---------+
```

and run tests repeatedly to reduce or observe jitter. 
quickbench assumes the quickest measurement is the truest one:
```
$ quickbench -w echo,expr -p5 -n100 -N2
Running 1 tests 100 times with 2 executables at 2016-10-16 23:57:34.387764 UTC:

Best times 1:
+-------------++---------+---------+
|             ||    echo |    expr |
+=============++=========+=========+
| 3 * 1000000 || 0.00112 | 0.00135 |
+-------------++---------+---------+

Best times 2:
+-------------++---------+---------+
|             ||    echo |    expr |
+=============++=========+=========+
| 3 * 1000000 || 0.00111 | 0.00136 |
+-------------++---------+---------+
```

You can turn a shell script into a benchmark suite by adding a shebang line:  
```
$ cat 410-run-time.sh
#!/usr/bin/env quickbench -v -p2 -n2 -w hledger-410-before,hledger-410-8bde75c -f
hledger -f 10000x1000x10.journal print
hledger -f 10000x1000x10.journal register
hledger -f 10000x1000x10.journal balance

$ ./410-run-time.sh
Running 3 tests 2 times with 2 executables at 2016-10-16 23:42:57.349721 UTC:
1: hledger-410-before -f 10000x1000x10.journal print
        [3.14s]
2: hledger-410-before -f 10000x1000x10.journal print
        [2.90s]
1: hledger-410-8bde75c -f 10000x1000x10.journal print
        [3.13s]
2: hledger-410-8bde75c -f 10000x1000x10.journal print
        [2.92s]
1: hledger-410-before -f 10000x1000x10.journal register
        [3.52s]
2: hledger-410-before -f 10000x1000x10.journal register
        [3.52s]
1: hledger-410-8bde75c -f 10000x1000x10.journal register
        [3.51s]
2: hledger-410-8bde75c -f 10000x1000x10.journal register
        [3.66s]
1: hledger-410-before -f 10000x1000x10.journal balance
        [3.38s]
2: hledger-410-before -f 10000x1000x10.journal balance
        [1.93s]
1: hledger-410-8bde75c -f 10000x1000x10.journal balance
        [1.93s]
2: hledger-410-8bde75c -f 10000x1000x10.journal balance
        [1.80s]

Best times:
+-----------------------------------++--------------------+---------------------+
|                                   || hledger-410-before | hledger-410-8bde75c |
+===================================++====================+=====================+
| -f 10000x1000x10.journal print    ||               2.90 |                2.92 |
| -f 10000x1000x10.journal register ||               3.52 |                3.51 |
| -f 10000x1000x10.journal balance  ||               1.93 |                1.80 |
+-----------------------------------++--------------------+---------------------+
```

## Usage

```
$ quickbench -h
quickbench 1.0
Run some test commands, possibly with different executables, once or more
and show their best execution times.
Commands are specified as one or more quote-enclosed arguments,
and/or one per line in CMDSFILE; or read from a default file [./bench.sh].
With -w, commands' first words are replaced with a new executable
(or multiple comma-separated executables, showing times for all).
Note: tests executable files only, not shell builtins; options must precede args.

Usage:
  quickbench [options] [<cmd>...]

Options:
  -f, --file CMDSFILE   file containing commands, one per line (- for stdin)
  -w, --with EXE[,...]  replace first word of commands with these executables
  -n, --iterations=N    run each test this many times [default: 1]
  -N, --cycles=N        run the whole suite this many times [default: 1]
  -p, --precision=N     show times with this many decimal places [default: 2]
  -v, --verbose         show commands being run
  -V, --more-verbose    show command output
      --debug           show debug output for this program
  -h, --help            show this help
```

## Related

[bench](https://github.com/Gabriel439/bench#readme) (Gabriel Gonzalez 2016) is another 
command line benchmarking tool written in Haskell.
Use that one if you need detailed statistical analysis and output, or HTML reports. 
Here is bench's output for the echo/expr example above: 
```
$ bench 'echo 3 * 1000000'; bench 'expr 3 \* 1000000'
benchmarking echo 3 * 1000000
time                 2.215 ms   (2.173 ms .. 2.250 ms)
                     0.995 R²   (0.989 R² .. 0.998 R²)
mean                 2.238 ms   (2.203 ms .. 2.296 ms)
std dev              147.6 μs   (92.84 μs .. 265.3 μs)
variance introduced by outliers: 48% (moderately inflated)

benchmarking expr 3 \* 1000000
time                 3.484 ms   (3.405 ms .. 3.556 ms)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 3.564 ms   (3.497 ms .. 3.684 ms)
std dev              280.3 μs   (178.0 μs .. 494.9 μs)
variance introduced by outliers: 52% (severely inflated)
```
