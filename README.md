# Morphism Proof Of Concept

This is a simple workspace for working out compiling morphisms (`map`, `fold`, `unfold` and related
like `filter`) to a limited stack machine.

The stack machine only supports a data stack and control stack, no temporary nor global storage.

Everything is written in [Janet](https://janet-lang.org/).

## Interpreter

`interp.janet` takes a source file with stack opcodes and verbosely runs it, printing out a full
trace of execution.  Good for debugging.

`test.ism` contains an example of every supported instruction and `run-test.sh` will interpret it
and confirm the output.

## Compiler

`compile.janet` takes a source file with S-exprs representing a simple program which supports
built-in morphisms.  For simplicity it is parsed with Janet's [`parse`](https://janet-lang.org/api/misc.html#parse) function and so the syntax
must resemble Janet.  It produces a list of instructions suitable for interpreting with
`interp.janet`.

The `.morph` files in the project are tests for each of the different supported morphisms and when
compiled produce `.morph.ism` files.
