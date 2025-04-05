% quickbench(1) quickbench 1.1
% Simon Michael <simon@joyful.com>
% Apr 2025

# NAME

quickbench - quick & easy benchmarking of command-line programs

# SYNOPSIS

`quickbench [options] [<cmd>...]`

# DESCRIPTION

quickbench runs some test commands, possibly with different executables, 
once or more and shows their best execution times in tabular format.

It produces very simple output (elapsed seconds), 
as quickly as possible (running commands just once by default), 
and tabulates results from multiple executables. 
I find it useful for quick and dirty, exploratory, and comparative  
measurements that you (and others) can understand at a glance.

Commands are specified as one or more quote-enclosed arguments,
and/or one per line in CMDSFILE; or read from a default file [./bench.sh].

With -w, commands' first words are replaced with a new executable
(or multiple comma-separated executables, showing times for all).

# OPTIONS

`-f, --file CMDSFILE`
: file containing commands, one per line (- for stdin)

`-w, --with EXE[,...]`
: replace first word of commands with these executables

`-n, --iterations=N`
: run each test this many times [default: 1]

`-N, --cycles=N`
: run the whole suite this many times [default: 1]

`-p, --precision=N`
: show times with this many decimal places [default: 2]

`-v, --verbose`
: show commands being run

`-V, --more-verbose`
: show command output

`    --debug`
: show debug output for this program

`-h, --help`
: show this help

# EXAMPLES

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
```
$ echo 'echo 3 * 1000000' > bench.sh
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

# FILES

quickbench looks for tests in `./bench.sh` if no command arguments or
`--file` option are provided.

# LIMITATIONS

quickbench tests executable files on disk only, not shell builtins or aliases.

Options must precede arguments on the command line.

`-V` will become `-vv` when implementing that becomes easier.

# BUG REPORTS
 
https://github.com/simonmichael/quickbench/issues

# SEE ALSO

Home page: https://github.com/simonmichael/quickbench

bench(1): https://github.com/Gabriel439/bench

time(1)

# COPYRIGHT

Copyright (C) 2008-2025 Simon Michael.
Released under GNU GPL v3+
