open Encoding

let solver = Batch.create ()
let encode e = try ignore (Z3_mappings.encode_expr e) with exn -> raise exn
let str = Strings.mk_val "abc"
let str_symb = Expression.mk_symbol `StrType "x"
let int_zero = Integer.mk_val 0
let int_two = Integer.mk_val 2

(* Encoding *)
let%test_unit _ = encode str
let%test_unit _ = encode str_symb

(* Satisfiability *)
let%test "test_concrete_len" =
  Batch.check_sat solver
    [ Integer.mk_ge (Strings.mk_len str_symb) (Strings.mk_len str) ]

let%test "test_constrained_len" =
  not
    (Batch.check_sat solver
       [
         Integer.mk_eq (Strings.mk_len str_symb) (Integer.mk_val 4);
         Integer.mk_eq (Strings.mk_len str_symb) (Strings.mk_len str);
       ])

let%test "test_concrete_substr" =
  let pc =
    [
      Strings.mk_eq
        (Strings.mk_substr str ~pos:int_zero ~len:int_two)
        (Strings.mk_val "ab");
    ]
  in
  Batch.check_sat solver pc

let%test "test_symb_substr" =
  let pc =
    [
      Strings.mk_eq str_symb str;
      Integer.mk_eq
        (Strings.mk_len (Strings.mk_substr str_symb ~pos:int_zero ~len:int_two))
        (Integer.mk_val 2);
    ]
  in
  Some (Expression.Str "abc") = Batch.eval solver str_symb pc
