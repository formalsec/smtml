# Encoding [![Build badge](https://github.com/wasp-platform/encoding/actions/workflows/build.yml/badge.svg)](https://github.com/wasp-platform/encoding/actions) [![GPL-3.0](https://img.shields.io/github/license/wasp-platform/encoding)](LICENSE) ![Platform](https://img.shields.io/badge/platform-linux%20%7C%20macos-lightgrey)

Encoding is an OCaml SMT abstraction layer for constraint solvers. The primary
objective of Encoding is to facilitate the effortless transition between
different SMT solvers during program analysis, as certain SMT solvers may prove
more efficient at handling specific logics and formulas. Presently, Encoding
offers support for [Z3] and [Colibri2], and ongoing efforts are directed
towards incorporating support for [Bitwuzla], [Alt-Ergo], and [CVC5].

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
opam install encoding
```

### Build from source

- Install the library dependencies:

```sh
git clone https://github.com/wasp-platform/encoding.git
cd encoding
opam install . --deps-only
```

- Build and test:

```sh
dune build
dune runtest
```

- Install `encoding` on your path by running:

```sh
dune install
```

## Supported Solvers

| Solver | Status |
|--------|--------|
| [Z3] | Yes |
| [Colibri2] | Yes |
| [Bitwuzla] | Ongoing |
| [Alt-Ergo] | Planned |
| [CVC5] | Planned |
| [Minisat] | Planned |

## About

### Project Name

While this project is currently named *Encoding*, there are plans to rename it
to *Smtml* in the future.

### Changelog

See [CHANGES]

[Z3]: https://github.com/Z3Prover/z3
[Colibri2]: https://git.frama-c.com/pub/colibrics
[Bitwuzla]: https://github.com/bitwuzla/ocaml-bitwuzla
[Alt-Ergo]: https://github.com/OCamlPro/alt-ergo
[CVC5]: https://github.com/cvc5/cvc5
[Minisat]: https://github.com/c-cube/ocaml-minisat

[CHANGES]: /CHANGES.md
