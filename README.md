# Status #
Hacking.

- `scheme-backend.scm`: a barely working interpreter that features
delimited continuations.

- `cps.scm`: a non-working source-to-source continuation passing
compiler that is not working.

# Separate Compilation of Modules #

All the top-level definitions in a module will be visible, and the
text of exported macros will be visible. A module require
recompilation of its users if it's interface changes; changing the
body of an exported macro counts as an interface change.

## A Predictable Top Level ##

The top level in scheme can get pretty weird. Modules should behave
like `letrec*` and any module should be able to host a repl (maybe).
As we figure out what "predictable" means, we'll implement it.

# Delimited Continuations #

A complete, efficient set of the delimited continuation primitives.
We're using a continuation passing compilation, with an inspectable
list of procedures representing the continuation. Prompts are a data
structure in the continuation, and are skipped over in a normal
return. `prompt`, `abort`, `capture`, and `compose` do not overlap in
function; i.e. `capture` captures the continuation to the prompt but
does not abort it, `shift` would require both a capture and an abort.

There are issues with `dynamic-wind`. We may not implement it. Dynamic
binding has some issues as well:
[delimited dynamic binding](http://okmij.org/ftp/Computation/dynamic-binding.html#DDBinding).
