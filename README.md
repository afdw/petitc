petitc
===

This is a compiler of a subset of the C language.

## Installation

To install dependencies, run `opam install --deps-only .`.

## Driver

The compiler executable supports stopping after different phases of compilation and outputting intermediate results, including:
* A list of tokens, for example [`test/lex_tokens.lex.txt`](test/lex_tokens.lex.txt).
* AST (Abstract syntax tree), for example [`test/parse_complex.parse.txt`](test/parse_complex.parse.txt).
* CFG (Control flow graph) with types, for example [`test/type_expressions.type.txt`](test/type_expressions.type.txt).
* CFG (Control flow graph) with types, in a graph format, for example [`test/type_instructions.type.dot`](test/type_instructions.type.dot); such files can be opened in programs like `xdot` and provide a clear visualization for an otherwise complex data structures.

## Tests

Simple integration tests are present, with exact comparison with expected output.

## Architecture

To simplify code, lexer and parser generators are used in place of manual lexing and parsing, as well as `ppx_deriving.show` and `ppx_deriving.map` to avoid writing simple boilerplate functions.

The compiler itself is not designed to be performant.

### Lexing

The `Lexer` module implements lexing using OCamllex.

### Parsing

The `Parser` module implements parsing using Menhir.

### Typechecking and CFG generation

The `Typed` module contains definitions of types required to represent explicitly typed program.

The types named `typ` is present in both module `Parser` and module `Type`. They are isomorphic in case of such a simple language, but duplicated as in general the syntactic and semantic types might not be the same.

The `Parser` module implements typechecking. Lexical environments have a kind of ID/name called `scope_path`. They hold the types of variables and functions in scope, as well as a list of used identifiers on the last level, which are not allowed to be reused at this same level by language rules (I believe this restriction can be lifted and that would result in a shadowing behavior).

The result of the typechecker is a list of non-nested functions, each of which is represented by a graph of "blocks". A block also has an ID/name, and consists of a list of expressions to execute and an action to do at the end, which can be one of:
* Unconditional jump.
* Conditional jump.
* Return from the current function.
* Unreachable: a marker that control should not reach (or can be assume no to reach) this place (needed because staticly proving such invariants might be difficult or impossible).

IDs/names are represented by so-called "paths", which are just list of identifiers. The idea is to generate a unique path for each object that needs it in a functional style, without having a global mutable counter.

The AST expressions are simplified a bit during typechecking, notably lvalues are replaced by explicit pointers are pointer arithmetic is implemented by multiplying or dividing by the value size in appropriate places, so that from now on pointers can be regarded just as integers.

Functions the typecheck and generate CFG for instructions accept, in addition to the AST types and lexical environments, so-called "continuations" are return the path to first (generated) block of the instruction. A continuation contains the return type of the current function and paths to blocks that need to be execute in case of normal fallthrough after the given instruction, after break or after continue.

As we need to keep track of all blocks, functions and variables (which can be arbitrarily nested) to be able to provide a full list of them in the end, "cumulations" are used, which can hold an arbitrary value and lists of aforementioned objects.

### Codegen

The `Codegen` module implements very crude code generation by directly generating x86-64 assembly, calculating expressions using the stack.

The ABI used is slightly different (arguments are always passed on the stack, additional pointers for the support of nested functions are pushed onto the stack), so there are adapters for `malloc`, `putchar` and `main`.
