open Encoding
open Ty
open Expr
module Batch = Solver.Batch (Z3_mappings)

let solver = Batch.create ()
let abc = Val (Str "abc") @: Ty_str
let symb_x = Symbol.("x" @: Ty_str)
let x = Expr.mk_symbol symb_x
let zero = Val (Int 0) @: Ty_int
let two = Val (Int 2) @: Ty_int

let () =
  let pc =
    [ Relop (Eq, x, abc) @: Ty_str
    ; Relop
        (Eq, Unop (Len, Triop (Substr, x, zero, two) @: Ty_str) @: Ty_str, two)
      @: Ty_int
    ]
  in
  assert (Batch.check solver pc);
  let m = Batch.model solver in
  assert (Some (Value.Str "abc") = Model.evaluate (Option.get m) symb_x)

let () =
  let assertion =
    [ Relop
        ( Eq
        , Cvtop (String_to_code, Binop (Nth, abc, zero) @: Ty_str) @: Ty_str
        , Val (Int 97) @: Ty_int )
      @: Ty_str
    ]
  in
  assert (Batch.check solver assertion)

let () =
  let ord =
    Cvtop (String_to_code, Binop (Nth, abc, zero) @: Ty_str) @: Ty_str
  in
  let assertion =
    [ Relop
        (Eq, Cvtop (String_from_code, ord) @: Ty_str, Val (Str "a") @: Ty_str)
      @: Ty_str
    ]
  in
  assert (Batch.check solver assertion)
