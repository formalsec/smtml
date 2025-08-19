# Smt.ml [![Build badge](https://github.com/formalsec/smtml/actions/workflows/build.yml/badge.svg)](https://github.com/formalsec/smtml/actions) [![Coverage Status](https://coveralls.io/repos/github/formalsec/smtml/badge.svg)](https://coveralls.io/github/formalsec/smtml) [![MIT](https://img.shields.io/github/license/formalsec/smtml)](LICENSE) ![Platform](https://img.shields.io/badge/platform-linux%20%7C%20macos-lightgrey)

Smt.ml is an SMT solver frontend for OCaml that simplifies integration
with various solvers through a consistent interface. Its parametric
encoding facilitates the easy addition of new solver backends, while
optimisations like formula simplification, result caching, and detailed
error feedback enhance performance and usability.

## Installation

### OPAM

Install [opam](https://opam.ocaml.org/doc/Install.html) and bootstrap the OCaml compiler:

<!-- $MDX skip -->
```sh
$ opam init
$ opam switch create 5.3.0 5.3.0
```

Then install encoding:

<!-- $MDX skip -->
```sh
$ opam install smtml
```

### Installing a Solver

Smt.ml uses optional dependencies (known as `depopts` in opam) to integrate
with different SMT solvers. By default, Smt.ml installs without a solver, but
you can enable support for a specific solver by installing it with opam.
For example, to install smtml with Z3:

<!-- $MDX skip -->
```sh
$ opam install smtml z3
```

Alternatively, if you've already installed Smt.ml through opam, you can simply
install the solver of your choice and opam will recompile smtml for you.
For example, to install Z3 after installing smtml:

<!-- $MDX skip -->
```sh
$ opam install z3
```

See the [Supported Solvers](#supported-solvers) section below for a complete
list of available solvers.

### Build from source

Clone the repo and install the dependencies:

<!-- $MDX skip -->
```sh
$ git clone https://github.com/formalsec/smtml.git
$ cd smtml
$ opam install . --deps-only --with-test
```

Build and test:

<!-- $MDX skip -->
```sh
$ dune build @install
$ dune runtest
```

Install `smtml` on your path by running:

<!-- $MDX skip -->
```sh
$ dune install
```

### Code Coverage Reports

<!-- $MDX skip -->
```sh
$ BISECT_FILE=`pwd`/bisect dune runtest --force --instrument-with bisect_ppx
$ bisect-ppx-report summary # Shell summary
$ bisect-ppx-report html    # Detailed Report in _coverage/index.html
```

## Quick Start

```ocaml
# #require "smtml";;
# open Smtml;;
# #install_printer Expr.pp;;
# #install_printer Value.pp;;
# #install_printer Symbol.pp;;
# #install_printer Statistics.pp;;
# let pp_model = Model.pp ~no_values:false;;
val pp_model : Model.t Fmt.t = <fun>
# #install_printer pp_model;;

# module Z3 = Solver.Batch (Z3_mappings);;
module Z3 :
  sig
    type t = Smtml.Solver.Batch(Smtml.Z3_mappings).t
    type solver = Smtml.Solver.Batch(Smtml.Z3_mappings).solver
    val solver_time : float ref
    val solver_count : int ref
    val pp_statistics : t Fmt.t
    val create : ?params:Smtml.Params.t -> ?logic:Smtml.Logic.t -> unit -> t
    val interrupt : t -> unit
    val clone : t -> t
    val push : t -> unit
    val pop : t -> int -> unit
    val reset : t -> unit
    val add : t -> Expr.t list -> unit
    val add_set : t -> Expr.Set.t -> unit
    val get_assertions : t -> Expr.t list
    val get_statistics : t -> Statistics.t
    val check : t -> Expr.t list -> [ `Sat | `Unknown | `Unsat ]
    val check_set : t -> Expr.Set.t -> [ `Sat | `Unknown | `Unsat ]
    val get_value : t -> Expr.t -> Expr.t
    val model : ?symbols:Symbol.t list -> t -> Model.t option
    val get_sat_model :
      ?symbols:Symbol.t list ->
      t -> Expr.Set.t -> [ `Model of Model.t | `Unknown | `Unsat ]
  end
# let solver = Z3.create ();;
val solver : Z3.t = <abstr>

# let cond =
    let a = Expr.symbol (Symbol.make Ty.Ty_bool "a") in
    let b = Expr.symbol (Symbol.make Ty.Ty_bool "b") in
    Expr.(binop Ty_bool And a (unop Ty_bool Not b));;
val cond : Expr.t = (bool.and a (bool.not b))

# match Z3.check solver [ cond ] with
    | `Sat -> "Satisfiable"
    | `Unsat -> "Unsatisfiable"
    | `Unknown -> "Unknown";;
- : string = "Satisfiable"
```

## Features & Usage

### Multi-Solver Support

```ocaml
# module Z3 = Solver.Batch (Z3_mappings);;
...
# module Bzla = Solver.Batch (Bitwuzla_mappings);;
...
```

### Bitvector Arithmetic

```ocaml
# let cond =
    let x = Expr.Bitv.I32.sym "x" in
    let y = Expr.Bitv.I32.v 0xdeadbeefl in
    let sum = Expr.(binop (Ty_bitv 32) Add x y) in
    Expr.(relop Ty_bool Eq sum (Expr.Bitv.I32.v 0xffffffffl));;
val cond : Expr.t = (bool.eq (i32.add x -559038737) -1)

# let model =
    let () = Z3.add solver [ cond ] in
    let _ = Z3.check solver [] in
    Z3.model solver
val model : Model.t option = Some (model
                                (x i32 559038736))
```

### Model Inspection

```ocaml
# match model with
    | Some model -> Model.get_bindings model
    | None -> []
- : (Symbol.t * Value.t) list = [(x, 559038736)]
```

### Solver Statistics

<!-- $MDX non-deterministic -->
```ocaml
# let stats = Z3.get_statistics solver;;
val stats : Statistics.t =
  ((added eqs 4)
   (arith-make-feasible 2)
   (arith-max-columns 4)
   (bv bit2core 32)
   (del clause 2)
   (final checks 2)
   (max memory 17.15)
   (memory 17.15)
   (mk bool var 38)
   (mk clause 3)
   (num allocs 11363)
   (num checks 2)
   (propagations 3)
   (rlimit count 262))
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
is named `smtml` and can be imported into OCaml projects by:

```dune
(library
  (name client_library)
  (libraries smtml))
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
