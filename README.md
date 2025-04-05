# quickbench

Quick & easy benchmarking of command-line programs.

  [About](#about)
| [Install](#install)
| [Usage](#usage)
| [Limitations](#limitations)
| [Todo](#todo)


## About

quickbench grew from a little benchmarking tool I used in the hledger project since 2008.
Think of it as a more powerful but still easy alternative to the unix `time` command,
for measuring the time taken by command-line programs,
or for creating repeatable benchmark scripts for your projects.
I find it very useful for quick, exploratory, and comparative measurements that can be understood at a glance.

Features:

- runs one or more commands, optionally substituting different executables
- shows results as quickly as possible, running each command just once by default
- shows very simple output: elapsed wall-clock seconds (with configurable precision)
- tabulates the results, with multiple executables shown side by side
- does not require knowledge of statistics
- is cross platform, GPLv3+ licensed, and actively used by its maintainer(s).

Timeline:

- 2008: I built a benchmarking tool early in the hledger project,
  possibly inspired by a [similar tool](https://hackage.haskell.org/package/darcs-benchmark-0.1) in Darcs
- 2016: released it as quickbench 1.0 on [hackage](https://hackage.haskell.org/package/quickbench)
- 9 years of quietly doing its job, with just a minor release in 2021 for latest deps
- 2025: development revived by some pull requests; updates, 1.1 in process

Related tools:

- [bench](https://github.com/Gabriel439/bench#readme) (2016) is another command line benchmarking tool written in Haskell.
  It provides detailed statistical output and nice HTML reports. Its default output is not easy to understand.
- [hyperfine](https://github.com/sharkdp/hyperfine) is the popular/powerful Rust alternative.

## Install

You can install quickbench from source on all major platforms.
You'll need Haskell build tools;
you can get these from your packaging system,
or [stack](https://docs.haskellstack.org/en/stable/),
or [ghcup](https://www.haskell.org/ghcup).
Then:
```
cabal install quickbench-1.1
```

or
```
stack install quickbench-1.1
```

or
```
git clone https://github.com/simonmichael/quickbench
cd quickbench
stack install
```

quickbench does not seem to be packaged *anywhere* yet - please help with that if you can !


## Usage

For help, run:
```
$ quickbench -h
...
Usage:
  quickbench [options] [CMD...]
...
```

You can specify one or more commands as arguments on the command line.
Here we time the "echo" and "sleep 1" commands.
"sleep 1" is multi-word command, so it must be wrapped in quotes:
```
$ quickbench echo 'sleep 1'
Running 2 tests 1 times at 2025-04-05 08:29:53 HST:

Best times:
+---------++------+
|         ||      |
+=========++======+
| echo    || 0.01 |
| sleep 1 || 1.01 |
+---------++------+
```

Note both of these commands run executable programs -
`echo` is both a shell builtin and an executable in the /bin directory,
and quickbench is running the latter:
```
$ which echo
echo is a shell builtin
echo is /bin/echo
$ which sleep
sleep is /bin/sleep
```

You can also run commands from a file.
Lines beginning with `#` are ignored:
```
$ cat - >script
# important benchmark
python3 -c "print(2 ** 10000)"
ruby    -e "puts 2 ** 10000"
ghc     -e "print $ 2 ** 10000"

$ quickbench -f script
Running 3 tests 1 times at 2025-04-05 11:17:13 HST:

Best times:
+---------------------------------++------+
|                                 ||      |
+=================================++======+
| python3 -c "print(2 ** 10000)"  || 0.05 |
| ruby    -e "puts 2 ** 10000"    || 0.07 |
| ghc     -e "print $ 2 ** 10000" || 0.41 |
+---------------------------------++------+
```

A file named `bench.sh` in the current directory will be used as the default source of commands:
```
$ mv script bench.sh
$ quickbench
Running 3 tests 1 times at 2025-04-05 11:19:43 HST:
...
```

The `-p DIGITS` option selects how many decimal places to display for times.

Some repetition options can help show/reduce jitter, giving more confidence in the results:
- `-n NUM` runs each test NUM times, keeping the fastest result
- `-N NUM` runs the whole suite NUM times, displaying each run.

And the `-v` flag shows the commands being run. Or `-V` will show them with their output.

```
$ quickbench -p3 -n5 -N2 -v
Running 3 tests 5 times at 2025-04-05 11:31:12 HST:
1: python3 -c print(2 ** 10000)
	[0.045s]
2: python3 -c print(2 ** 10000)
	[0.029s]
3: python3 -c print(2 ** 10000)
	[0.027s]
4: python3 -c print(2 ** 10000)
	[0.024s]
5: python3 -c print(2 ** 10000)
	[0.026s]
1: ruby -e puts 2 ** 10000
	[0.061s]
2: ruby -e puts 2 ** 10000
	[0.060s]
...
Best times 1:
+---------------------------------++-------+
|                                 ||       |
+=================================++=======+
| python3 -c "print(2 ** 10000)"  || 0.024 |
| ruby    -e "puts 2 ** 10000"    || 0.056 |
| ghc     -e "print $ 2 ** 10000" || 0.315 |
+---------------------------------++-------+

Best times 2:
+---------------------------------++-------+
|                                 ||       |
+=================================++=======+
| python3 -c "print(2 ** 10000)"  || 0.023 |
| ruby    -e "puts 2 ** 10000"    || 0.057 |
| ghc     -e "print $ 2 ** 10000" || 0.316 |
+---------------------------------++-------+
```

You can run each command with its first word replaced by a different executable:
```
$ quickbench -p3 -n5 -w ghc-9.6,ghc-9.8,ghc-9.10,ghc-9.12 'ghc -e "print $ 2 ** 1000000"'
Running 1 tests 5 times with 4 executables at 2025-04-05 11:39:24 HST:

Best times:
+---------------------------++---------+---------+----------+----------+
|                           || ghc-9.6 | ghc-9.8 | ghc-9.10 | ghc-9.12 |
+===========================++=========+=========+==========+==========+
| -e "print $ 2 ** 1000000" ||   0.319 |   0.318 |    0.309 |    0.335 |
+---------------------------++---------+---------+----------+----------+
```

You can convert a shell script to a benchmark suite by adding a quickbench shebang line,
like this (some systems will require `env -S`):
```
$ cat 410-times.sh
#!/usr/bin/env quickbench -n2 -w hledger-410-before,hledger-410-8bde75c -f
hledger -f 10k.journal print
hledger -f 10k.journal register
hledger -f 10k.journal balance

$ ./410-times.sh
Running 3 tests 2 times with 2 executables at 2016-10-16 23:42:57.349721 UTC:

Best times:
+-------------------------++--------------------+---------------------+
|                         || hledger-410-before | hledger-410-8bde75c |
+=========================++====================+=====================+
| -f 10k.journal print    ||               2.90 |                2.92 |
| -f 10k.journal register ||               3.52 |                3.51 |
| -f 10k.journal balance  ||               1.93 |                1.80 |
+-------------------------++--------------------+---------------------+
```


## Quirks

Some current limitations/quirks of quickbench:

- It doesn't allow quickbench options to be written after the commands.
- Some options, like `--debug`, may appear to work there, but they don't work completely and it still complains.
- If you forget the quotes, as in `quickbench echo -n a`, it gives an unclear "should appear before argument or is unknown" error.
- Commands which fail show an error message above the table, but not within it.


## Todo

- land PRs
- update docs
- doc deduplication/automation
- tests
- release 1.1
- packaging
