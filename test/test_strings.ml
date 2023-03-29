open Base
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
let%test _ =
  Batch.check solver
    [ Relop (Str S.Eq, Unop (Str S.Len, str_symb), Unop (Str S.Len, str)) ]

let%test _ =
  not
    (Batch.check solver
       [
         Relop (Int I.Eq, Unop (Str S.Len, str_symb), Val (Int 4));
         Relop (Str S.Eq, Unop (Str S.Len, str_symb), Unop (Str S.Len, str));
       ])
