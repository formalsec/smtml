open Encoding
open Ty
open Expr
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()
let symb_x = Symbol.("x" @: Ty_bool)
let x = Expr.mk_symbol symb_x

let%test "test_not" =
  let pc =
    [ Unop (Not, Relop (Eq, x, Val (Bool true) @: Ty_bool) @: Ty_bool)
      @: Ty_bool
    ]
  in
  assert (Batch.check solver pc);
  let m = Batch.model solver in
  Some (Value.Bool false) = Model.evaluate (Option.get m) symb_x
