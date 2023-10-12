open Encoding
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()
let abc = Strings.mk_val "abc"
let symb_x = Symbol.mk_symbol `StrType "x"
let x = Expression.mk_symbol symb_x
let zero = Integer.mk_val 0
let two = Integer.mk_val 2

(* Satisfiability *)
let%test "test_concrete_len" =
  Batch.check solver [ Integer.mk_ge (Strings.mk_len x) (Strings.mk_len abc) ]

let%test "test_constrained_len" =
  not
    (Batch.check solver
       [ Integer.mk_eq (Strings.mk_len x) (Integer.mk_val 4)
       ; Integer.mk_eq (Strings.mk_len x) (Strings.mk_len abc)
       ] )

let%test "test_concrete_substr" =
  let pc =
    [ Strings.mk_eq
        (Strings.mk_substr abc ~pos:zero ~len:two)
        (Strings.mk_val "ab")
    ]
  in
  Batch.check solver pc

let%test "test_symb_substr" =
  let pc =
    [ Strings.mk_eq x abc
    ; Integer.mk_eq
        (Strings.mk_len (Strings.mk_substr x ~pos:zero ~len:two))
        (Integer.mk_val 2)
    ]
  in
  assert (Batch.check solver pc);
  let m = Batch.model solver in
  Some (Value.Str "abc") = Model.evaluate (Option.get m) symb_x
