
Code for Testing Noninterference
================================

### Description

- Haskell code associated to the "Testing Noninterference, Quickly" paper

### Prerequisites

- GHC 7.4.x-7.8.x (known to work with 7.4.1, 7.6.3, and 7.8.3)
- Haskell packages:
  - QuickCheck 2.7.x (known to work with 2.7.3)
  - CmdArgs >0.9.5 (available with 'cabal install cmdargs')
  - concurrent-extra (available with 'cabal install concurrent-extra')
- UNIX utilities like make, echo, rm, etc.

### Contents

README

Makefile
Makefile.common

basic/    -- Simple information-flow stack machine

        Machine.hs     : definition of the abstract machine
	Generation.hs  : random program generation
	Driver.hs      : experiment set up, top level properties, main
	Flags.hs       : various flags configuration of a machine
	Instr.hs       : ISA
	Labels.hs      : label systems, and observations
	Observable.hs  : observable classes
	ObservableInst.hs : observable classes and shrinking variations
        ...

extended/ -- Information-flow register machine with advanced features
        ...

common/ -- Common definitions, and helpers
	Aggregate.hs
	Machine.hs              : common definitions for generic "machines"
	Pretty.hs               : pretty printing
	Trace.hs                : execution traces
	Util.hs                 : misc. helpers
        ...

### Building

Run "make" in the top directory

### Using ghci

All you need to do is invoke ghci (e.g. C-c C-l in Emacs if using
haskell-mode default key bindings) and the .ghci files will take care
of the rest.

On Linux you might have permission problems (you might see "WARNING:
.ghci is writable by someone else, IGNORING!" when starting ghci), in
which case you need to make sure that this dir, its subdirs and the
.ghci files inside are not group readable. A command like
> chmod -R g-w .
should usually solve the problem.

### Old description (TODO: bring this up to date)

-= Running the test driver from command line =-

You may run the test driver from the command line with:
    ./Driver
This will run with the default configuration, shown in Flags.hs
However you may override one of the options manually as well -- for
instance:
   ./Driver --gen-strategy=GenNaive --tests=10000
or
   ./Driver --gen-strategy=GenByExec --tests=30000

All values (and names for named options) are specified in Flags.hs. Finally,
you may run:
   ./Driver --help
to see a list of available options.

-= Basic profiling of test generation =-

TMUDriver contains flags that can profile tests for some
basic statistics. Namely, the

  --prop-test=PropJustProfileLengths
and
  --prop-test=PropJustProfileWF

run profiling on tests with the current configuration. The former is
profiling execution lenghts, the latter is profiling the reasons for
termination. You may use --tests to determine the number of tests,
or --gen-strategy to determine under which strategy you want to profile, etc.

-= Bulk testing and profiling =-

Some new options have been introduced that help profile/test in the bulk.

Namely, you may want to give a string argument to --ifc-semantics, e.g.:
   --ifc-semantics="[IfcBugAddNoTaint,IfcBugStoreNoTaint]"
which will iterate over these two buggy behaviors. The --ifc-semantics
flag stands for a list of behaviours you would like to test. For convenience
there exists a wildcard value:
   --ifc-semantics="*"
which you may use to iterate over all bugs.

When testing you may want to suppress printing information from the
actual counterexample, which you can do with:
   --show-counterexamples=False

Here is an example usage:

./Driver --ifc-semantics="*"
            --gen-strategy=GenByExec
            --show-counterexamples=False

Iterates over all bugs using GenByExec, not showing
counterexamples. It will print out average results in some format
[DV:TODO]

### Coding syntax rules

The rationale for these rules is that they make refactoring easier.

In ghci, warnings will be raised:
- if a top-level declaration is missing a type signature
- if a tab is present
- if a do block uses a value of type m A, for A ≠ (), without binding
  it to a value
- if you have an incomplete set of pattern matches

For instance,
Prelude> let j (Just a) = a
<interactive>:2:5:
    Warning: Pattern match(es) are non-exhaustive
             In an equation for `j': Patterns not matched: Nothing

At compile-time, these warnings are errors: you're forced to stick to
these coding standards.
