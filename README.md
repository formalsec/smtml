# Smt.ml [![Build badge](https://github.com/formalsec/smtml/actions/workflows/build.yml/badge.svg)](https://github.com/formalsec/smtml/actions) [![Coverage Status](https://coveralls.io/repos/github/formalsec/smtml/badge.svg)](https://coveralls.io/github/formalsec/smtml) [![GPL-3.0](https://img.shields.io/github/license/formalsec/smtml)](LICENSE) ![Platform](https://img.shields.io/badge/platform-linux%20%7C%20macos-lightgrey)

Smt.ml is a Multi Back-end Front-end for SMT Solvers in OCaml. The primary
objective of Smt.ml is to facilitate the effortless transition between
different SMT solvers during program analysis, as certain SMT solvers may prove
more efficient at handling specific logics and formulas. Presently, Smt.ml
offers support for [Z3], [Colibri2], and [Bitwuzla], and ongoing efforts are directed
towards incorporating support for [cvc5] and [Alt-Ergo].

## Installation

### OPAM

- Install [opam](https://opam.ocaml.org/doc/Install.html).
- Bootstrap the OCaml compiler:

```sh
opam init
opam switch create 5.1.0 5.1.0
```

- And, then install encoding:

```sh
opam install smtml
```

### Build from source

- Install the library dependencies:

```sh
git clone https://github.com/formalsec/smtml.git
cd smtml
opam install . --deps-only
```

- Build and test:

```sh
dune build
dune runtest
```

- Install `smtml` on your path by running:

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

| Solver | Status |
|--------|--------|
| [Z3] | Yes |
| [Colibri2] | Yes |
| [Bitwuzla] | Yes |
| [Alt-Ergo] | Yes |
| [cvc5] | Ongoing |
| [Minisat] | Planned |

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

    Smt.ml Copyright (C) 2023-2024 formalsec

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

[Z3]: https://github.com/Z3Prover/z3
[Colibri2]: https://git.frama-c.com/pub/colibrics
[Bitwuzla]: https://github.com/bitwuzla/ocaml-bitwuzla
[Alt-Ergo]: https://github.com/OCamlPro/alt-ergo
[CVC5]: https://github.com/cvc5/cvc5
[Minisat]: https://github.com/c-cube/ocaml-minisat

[CHANGES]: /CHANGES.md
