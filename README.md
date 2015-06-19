## Pine - A simple toy ML-like language

This is the repository for Pine, a really simple ML-like language powered by Rust and LLVM. It aims to be simple
enough that a compiler can be written in a reasonably small amount of code, yet powerful enough to do high-level
programming tasks and, in particular, sophisticated abstractions.

Pine's type system is heavily inspired by the Hindley-Milner type system for the lambda calculus. Parameteric polymorhism
and type inference are major features of Pine's type system. In fact, one does not specify types for parameters to
functions in Pine - the types are inferred based on their usage within the function.

Pine compiles to native code and links against a minimal runtime. To accomplish this, the Pine compiler has a 
monomorphization pass where generic functions are instantiated with concrete types and turned into unique functions,
specialized to the instantiated types.

### Basic features
Pine is inspired heavily by ML and inherits much of its semantics. There is no difference between expressions
and statements in Pine - every construct is an expression and produces a value. Pine currently supports
* Conditionals via `if`
* Scoped local bindings via `let`
* Basic arithmetic
* Extern functions
* Bindings are recursive by default
* Aggressive tail call optimization

There are a LOT of things that will be added in the future. The goal with the initial release was to get a backend
capable of producing real code for a subset of the language, so that new language features could be implemented
through syntactic sugar.

### Future features, in order of priority
* User-defined types + limited pattern matching upon them
* `_` as an ignored identifier
* TESTS TESTS TESTS TESTS. Likely need to write my own test hardness
* Function literals, i.e. lambdas, a requirement for any self-respecting functional language
* `let () =` syntactic sugar for side-effect-only expressions, from OCaml
* `match` expression for generalized pattern matching
* exceptions?
* mutually recursive functions (to be solved either by forward declarations or the `and` construct from ML)
* #include-based mechanism for code inclusion + a standard library

### Things that aren't great and need to be fixed
* The lexer doesn't handle comments very well.
* The parser reports incorrect text positions for ASTs.
* The backend shells out to `opt` and `llc` to optimize and compile the LLVM bitcode it produces
instead of doing it through the LLVM C API. This is a performance killer, 98% of compilation is spent here.
* Document my code! This project was done for educational purposes so it would be nice to do something similar to
  CoffeeScript's literate compiler docs.
* Rust is awesome! Why'd I write the runtime in C?

### Building and Running
The makefile in the root directory can build everything that Pine needs to compile and run Pine programs.

A `make` in the root directory should suffice. You will need `libgc` and `llvm` installed on your machine, as Pine
uses `libgc` for runtime garbage collection and `llvm` as a compiler backend. `make install` will install the required
bits to `/usr/local/bin` (`pinec`, the compiler) and `/usr/local/lib` (`libpinert`, the runtime). 

`pinec` builds via Cargo. You can build `pinec` directly using Cargo from the `pinec` folder of the source tree.

Unfortunately the makefile is MacOS only right now, sorry! I'll fix that soon.

Once `pinec` and `libpinert` have been installed, there are two examples in `etc/examples` that build using makefiles.
They show off some sample Pine code doing some simple things. It's not much, but writing programs in a programming
language that you yourself invented is Pretty Cool(TM).

There's also an emacs mode!
