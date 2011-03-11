## llc ##

A compiler for LLVM instructions written in an sexpr format.

## Installation ##

Install Scheme48 1.8, then run `make`.

## Usage ##

After installing, compile a program like this:

    ./llc example.scm

And run a compiled program like this:

    ./example

## Structure ##

Source files in this directory include:

+ `build.in`       -- a Scheme48 script that builds `llc.img`.
+ `interfaces.scm` -- package interfaces
+ `llc`            -- compile LLVM sexpr source to an executable
+ `llc.scm`        -- the compiler's interface to the outside world
+ `llvm.scm`       -- instruction emitter
+ `packages.scm`   -- package definitions
+ `run-time.c`     -- compiler run-time harness

