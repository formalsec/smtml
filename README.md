# Smt.ml [![Build badge](https://github.com/formalsec/smtml/actions/workflows/build.yml/badge.svg)](https://github.com/formalsec/smtml/actions) [![Coverage Status](https://coveralls.io/repos/github/formalsec/smtml/badge.svg)](https://coveralls.io/github/formalsec/smtml) [![MIT](https://img.shields.io/github/license/formalsec/smtml)](LICENSE) ![Platform](https://img.shields.io/badge/platform-linux%20%7C%20macos-lightgrey)

Smt.ml is an SMT solver frontend for OCaml that simplifies integration
with various solvers through a consistent interface. Its parametric
encoding facilitates the easy addition of new solver backends, while
optimisations like formula simplification, result caching, and detailed
error feedback enhance performance and usability.

## Installation

### OPAM

Install [opam](https://opam.ocaml.org/doc/Install.html) and bootstrap the OCaml compiler:

```sh
opam init
opam switch create 5.1.1 5.1.1
```

Then install encoding:

```sh
opam install smtml
```

### Installing a Solver

Smt.ml uses optional dependencies (known as `depopts` in opam) to integrate
with different SMT solvers. By default, Smt.ml installs without a solver, but
you can enable support for a specific solver by installing it with opam.
For example, to install smtml with Z3:

```sh
opam install smtml z3
```

Alternatively, if you've already installed Smt.ml through opam, you can simply
install the solver of your choice and opam will recompile smtml for you.
For example, to install Z3 after installing smtml:

```sh
opam install z3
```

See the [Supported Solvers](#supported-solvers) section below for a complete
list of available solvers.

### Build from source

Clone the repo and install the dependencies:

```sh
git clone https://github.com/formalsec/smtml.git
cd smtml
opam install . --deps-only --with-test
```

Build and test:

```sh
dune build @install
dune runtest
```

Install `smtml` on your path by running:

```sh
dune install
```

### Code Coverage Reports

```sh
BISECT_FILE=`pwd`/bisect dune runtest --force --instrument-with bisect_ppx
bisect-ppx-report summary # Shell summary
bisect-ppx-report html    # Detailed Report in _coverage/index.html
```

## Supported Solvers

| Solver     | Status  | Opam Package |
|------------|:-------:|--------------|
| [Z3]       | ☑️ | [z3](https://opam.ocaml.org/packages/z3/) |
| [Colibri2] | ☑️ | [colibri2](https://opam.ocaml.org/packages/colibri2/) |
| [Bitwuzla] | ☑️ | [bitwuzla-cxx](https://opam.ocaml.org/packages/bitwuzla-cxx/) |
| [Alt-Ergo] | ☑️ | [alt-ergo](https://opam.ocaml.org/packages/alt-ergo/) |
| [cvc5]     | ☑️ | [cvc5](https://opam.ocaml.org/packages/cvc5/) |
| [Minisat]  | 📆 | [minisat](https://opam.ocaml.org/packages/minisat/) |

#### Legend

- ☑️ Solver is currently supported
- 🔄 Ongoing work to support solver
- 📆 Planned to support in the future

## About

### Project Name

The name `Smt.ml` is a portmanteau of the terms `SMT` and `OCaml`. The `.ml`
extension is a common file extension for OCaml source files. The library itself
is named `smtml` and can be imported into OCaml programs using the following
syntax:

```ocaml
open Smtml
```

### Changelog

See [CHANGES]

### Copyright

    MIT License

    Copyright (c) 2024 formalsec

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

[Z3]: https://github.com/Z3Prover/z3
[Colibri2]: https://git.frama-c.com/pub/colibrics
[Bitwuzla]: https://github.com/bitwuzla/ocaml-bitwuzla
[Alt-Ergo]: https://github.com/OCamlPro/alt-ergo
[CVC5]: https://github.com/cvc5/cvc5
[Minisat]: https://github.com/c-cube/ocaml-minisat

[CHANGES]: /CHANGES.md
