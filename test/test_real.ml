open Encoding
open Ty
open Expr
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()

let%test "test-to_string-eq" =
  let x = mk_symbol Symbol.("x" @: Ty_real) in
  let y = mk_symbol Symbol.("y" @: Ty_real) in
  Batch.check solver
    [ Relop (Eq, Cvtop (ToString, x) @: Ty_real, Cvtop (ToString, y) @: Ty_real)
      @: Ty_str
    ]
