## Unreleased

### Added
### Fixed
### Changed

## v0.2.1

### Fixed

- Add conflicts on solvers versions outside depopt range (@krtab)

## v0.2.0

### Added

- Added `List` and `App` values (@joaomhmpereira)
- Added `Naryop` to `Expr` (@joaomhmpereira)
- Added `val Expr.ptr : int32 -> t -> t` constructor (@filipeom)
- Added missing concrete simplification to `Eval` (@joaomhmpereira)

### Changed

- Renamed `Seq_` operators to `String_` (@joaomhmpereira)
- `String.concat` is now a nary operator (@joaomhmpereira)
- Made `Eval.TypeError` more explicit on which operator triggered the error. (@joaomhmpereira)
- Made `Expr.Ptr` a record (@filipeom)

### Fixed

- Missing bitwuzla operators `to_fp` and `of_ieee_bv`.

## v0.1.3

### Changed

- Changed `Num.( = )` to `Num.equal` to be more consistent with other modules

### Fixed

- Fixed comparison of boolean values

## v0.1.2

### Added

- Adds `Solver_dispacher.{is_available|available_solvers|solver}` to check
availability of installed solvers
- Model generation for Bitwuzla

### Changed

- Exposes `Optimizer.Make` to allow for parametric optimizers
- Makes Z3 optional

### Fixed

- Parametric mappings should only create sorts once

## v0.1.1

- Improves interoperability with multiple solvers
- Bug fixes for `colibri2` and `z3`
- Adds preliminary support for cvc5

## v0.1.0

- Renames project to `Smt.ml` and library to `smtml`
- Minor fixes and typos
- Adds preliminary support for the Bitwuzla solver
- Completes concrete simplifications
- `Solver.check` now returns a `Sat | Unsat | Unknown` instead of a `bool` value
- Adds owi's simplifications and smart op constructors
- Moves theory annotation (`Ty.t`) only to necessary variants

## v0.0.4

- Adds Arthur's clz and ctz implementations for i64s
- Completes missing `eval_numeric` operations
- Adds more tests to increase code coverage
- Adds `extend_ixx` to lexer
- Adds colibri2 mappings
- Fixes hash-consing in 72eeb6f
- Rename `declare-fun` to `let-const`
- Rotate_left and rotate_right operators
- Print floats in OCaml syntax (Closes #49)

## v0.0.3

- Improve bitv creation interface
- Add naive hash-consing of expressions
- Add `Ceil` and `Floor` FPA operators
- Start migrating inline tests to standard tests
- No simplifier on batch solver

## v0.0.2

- Support for bv8
- Refactor optimizer interface
- Fixes batch solver in `e061344`
- Adds default simplifier in z3 leading to great performance gains
- Adds logic configuration option to `mk_solver`
- Fixes pp function in `11476fb`
- Adds `ematching` and `timeout` parameters
- Improves documentation
- Relax ocaml compiler constraint to `>= 4.14.0`

## v0.0.1

Initial release
