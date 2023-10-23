# OCaml Constraint Abstraction Layer

[![Build badge](https://github.com/wasp-platform/encoding/actions/workflows/build.yml/badge.svg)](https://github.com/wasp-platform/encoding/actions)
[![GPL-3.0](https://img.shields.io/github/license/wasp-platform/encoding)](LICENSE)
![Platform](https://img.shields.io/badge/platform-linux%20%7C%20macos-lightgrey)

The OCaml Constraint Abstraction Layer (OCAL) serves as an abstracted constraint-solving
wrapper, currently utilising Z3 as its backend solver. However, future plans for OCAL
include support for other solvers in its backend, such as Yices and CVC5.

## Build from source

- Install [opam](https://opam.ocaml.org/doc/Install.html).
- Bootstrap the OCaml compiler:

```sh
opam init
opam switch create 5.1.0 5.1.0
```

- Then, install the library dependencies:

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

## Use encoding in your project

* To incorporate encoding into your project, you can pin to your project:

```sh
opam pin encoding git+https://github.com/wasp-platform/encoding
```
