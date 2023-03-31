open Encoding
open Types
open Expression

let solver = Batch.create ()
let encode e = try ignore (Common.encode_expr e) with exn -> raise exn
let str = Val (Str "abc")
let str_symb = Symbolic (`StrType, "x")

(* Encoding *)
let%test_unit _ = encode str
let%test_unit _ = encode str_symb

(* Satisfiability *)
let%test "test_concrete_len" =
  Batch.check_sat solver
    [ Relop (Int I.Ge, Unop (Str S.Len, str_symb), Unop (Str S.Len, str)) ]

let%test "test_constrained_len" =
  not
    (Batch.check_sat solver
       [
         Relop (Int I.Eq, Unop (Str S.Len, str_symb), Val (Int 4));
         Relop (Int I.Eq, Unop (Str S.Len, str_symb), Unop (Str S.Len, str));
       ])

let%test "test_concrete_substr" =
  let pc =
    [
      Relop
        ( Str S.Eq,
          Triop (Str S.SubStr, str, Val (Int 0), Val (Int 2)),
          Val (Str "ab") );
    ]
  in
  Batch.check_sat solver pc

let%test "test_symb_substr" =
  let pc =
    [
      Relop (Str S.Eq, str_symb, str);
      Relop
        ( Int I.Eq,
          Unop
            (Str S.Len, Triop (Str S.SubStr, str_symb, Val (Int 0), Val (Int 2))),
          Val (Int 2) );
    ]
  in
  Some (Str "abc") = Batch.eval solver str_symb pc
