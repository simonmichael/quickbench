## 1.1 unreleased

- Improved: The EXE argument of -w/-with can now be multi-word (providing new first word and initial arguments) ([#7], Thomas Miedema)
- Improved: docs, command line help, debug output.
- Fixed: commands which have quoted arguments containing spaces are now handled correctly.
- Fixed a non-exhaustive patterns build warning.
- Added stack configs for ghc 9.6-9.12.
- quickbench now requires ghc 9.6+ (arbitrarily).

## 1.0.1 2021-10-06

- build cleanly with current GHC & libs

## 1.0 2016-10-21

- extract tools/simplebench from the hledger repo
- stop using error, make safe for reuse
- more robust command execution, signal handling, output, error reporting
- switch to docopt, options must precede args for now
- new UI: full commands are specified by arguments/file/stdin,
  alternate executables can be specified with -w, flags cleanup
- default file for test commands is ./bench.sh
- -N/--cycles repeats the whole test suite
- man page
