1.0.1 2021-10-06

- build cleanly with current GHC & libs

1.0 2016-10-21

- extract tools/simplebench from the hledger repo

- stop using error, make safe for reuse

- more robust command execution, signal handling, output, error reporting

- switch to docopt, options must precede args for now

- new UI: full commands are specified by arguments/file/stdin,
  alternate executables can be specified with -w, flags cleanup

- default file for test commands is ./bench.sh

- -N/--cycles repeats the whole test suite

- man page
