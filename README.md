# quickbench

Quick & easy comparative benchmarking of command-line programs.

This is a reboot of the simplebench benchmarking tool from the hledger
project. You can use it as a fancier "time" command for benchmarking 
command-line programs, or as a haskell library (eg in package benchmark 
suites). 

quickbench is not smart or complicated like "bench" or criterion; it is 
good for quick and dirty, exploratory, comparative measurements
that you can run quickly and understand at a glance.
I find it very useful; patches welcome!

```bash
$ git clone https://github.com/simonmichael/quickbench.git
$ cd quickbench
$ stack install   # ensure $PATH includes ~/.local/bin

$ quickbench 'sleep 1'
Running 1 tests 1 times at 2016-10-16 23:06:48.058578 UTC:

Best times:
+---------++------+
|         ||      |
+=========++======+
| sleep 1 || 1.01 |
+---------++------+

$ quickbench -p4 -n100 -N2 -w echo,expr 'echo a' 'echo 3 * 1000000'
Running 2 tests 100 times with 2 executables at 2016-10-16 23:07:38.601136 UTC:

Best times 1:
+-------------++--------+--------+
|             ||   echo |   expr |
+=============++========+========+
| a           || 0.0011 | 0.0013 |
| 3 * 1000000 || 0.0011 | 0.0014 |
+-------------++--------+--------+

Best times 2:
+-------------++--------+--------+
|             ||   echo |   expr |
+=============++========+========+
| a           || 0.0011 | 0.0014 |
| 3 * 1000000 || 0.0011 | 0.0014 |
+-------------++--------+--------+

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