# AGENTS.md

## Build, Test, Lint

```
dune build @install            # build the library
dune runtest                   # run all tests
dune build @fmt                # check formatting (CI lint)
dune build @fmt --auto-promote # auto-fix formatting
dune build @check              # type-check only (no codegen)
```

Formatting: `ocamlformat` v0.29.0, config in `.ocamlformat`.
Tests: `OUnit2` in `test/{unit,integration,regression,cli}/`.

## Architecture

`smtml` is an SMT solver frontend for OCaml. It provides a typed expression
API and dispatches to multiple solver backends.

Key modules:
- `typed.ml` / `typed.mli` — the public user-facing API, with phantom types
  (`'a expr = Expr.t`) for type-safe expression construction
- `mappings_intf.ml` — interface that solver backends must implement
- `mappings.ml` — generic encoding layer: translates typed expressions into
  solver-specific terms using the backend's mapping functions. Contains
  fallback logic when a solver doesn't implement a feature natively
- `*_mappings.default.ml` — solver backends (z3, cvc5, bitwuzla, altergo,
  colibri2), each a functor `Fresh_* ()` implementing `Mappings_intf.M`
- `*_mappings.nop.ml` — stub backends used when a solver is not installed
  (via dune `select`)
- `solver_intf.ml`, `solver.ml` — solver abstraction layer (create, add
  constraints, check, model extraction)

## Conventions

- SPDX + copyright header on new `.ml`/`.mli` files:
  ```ocaml
  (* SPDX-License-Identifier: MIT *)
  (* Copyright (C) 2023-2026 formalsec *)
  (* Written by the Smtml programmers *)
  ```
- Module doc comments use `(** ... *)` at the top of the module.

## Principles

1. **Prefer the type system over runtime checks.** Use `Result` or
   `Option` rather than raising exceptions for recoverable errors. If a
   function can fail in multiple ways, return `('a, [> ...]) result`
   with an open polymorphic variant error type (see `Value.of_string`,
   `Parse.from_file`). Use `Option` when there's no error variant to
   communicate (e.g. `to_ieee_bv : (term -> term) option` for optional
   solver features).

2. **Use exceptions only for unrecoverable states.** `assert false` is
   reserved for branches proven unreachable by the type system.
   `Fmt.failwith` is for genuinely unrecoverable situations
   (unsupported operator/theory combination, "not implemented" stubs).

3. **Follow the dual-API convention when both modes are needed.** A bare
   function returns a `result`, while an `_exn`-suffixed variant raises
   on failure (e.g. `trunc_f32_s` / `trunc_f32_s_exn`).

4. **Use the existing monadic combinators.** The codebase defines
   `Option.Syntax`, `Result.Syntax`, and `let+` bind operators in
   several modules. Use these instead of manual `match` nesting.

5. **New solver features follow a fixed cascade:**
   1. Add the operation signature to `mappings_intf.ml`
   2. Implement the fallback/encoding in `mappings.ml`
   3. Implement in each solver's `*_mappings.default.ml`
   4. If a solver doesn't support it, return `None` for optional
      features, or use `assert false` for required ones.

6. **Be aware of phantom type limitations.** `'a expr = private Expr.t`
   means OCaml can subvert the type distinction. Runtime correctness
   depends on constructing the right SMT operations. The `BitvN.Make`
   functor is NOT annotated with `: S`, so inferred return types are
   loose — be careful with bitwidth mismatches.
