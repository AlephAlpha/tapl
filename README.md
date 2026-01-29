# TAPL

Implementations of the languages in Pierce's [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) in Rust. Based on [the official implementations in Ocaml](https://github.com/mspertus/TAPL).

Each language comes with a simple REPL with tab completion for keywords. But reading source codes from a file is not yet supported.

The REPL supports the following commands:

- `:eval`: Evaluate the expression.
- `:eval1`: Evaluate the expression for one step.
- `:type`: Find the type of the expression (for languages with types).
- `:kind`: Find the kind of the type (for languages with kinds).
- `:bind`: Bind a variable to an expression or a type.

If no command is given, it will either evaluate the input as an expression or bind it to a variable, depending on the input.
