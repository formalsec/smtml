# Encoding

[![GPL-3.0](https://img.shields.io/github/license/wasp-platform/encoding)](LICENSE)
![Platform](https://img.shields.io/badge/platform-linux%20%7C%20macos-lightgrey)
[![Commit](https://shields.io/category/activity)](https://github.com/wasp-platform/encoding/commit/master~0)

Encoding serves as an abstracted constraint-solving wrapper, currently 
utilising Z3 as its backend solver. However, future plans for Encoding 
include support for other solvers in its backend, such as CVC5.

## Build from source

- Install [opam](https://opam.ocaml.org/doc/Install.html).
- Bootstrap the OCaml compiler:

```sh
opam init
opam switch create 4.14.0 4.14.0
```

- Then install the library dependencies:

```sh
git clone https://github.com/wasp-platform/encoding.git
cd encoding
opam install . --deps-only
```

- Build and test

```sh
make
make test
```

- Install `encoding` on your path by running

```sh
make install
```
