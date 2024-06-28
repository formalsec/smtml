open Smtml
open Ty
open Expr
open Value

let ( = ) = Expr.equal

let int i = value (Int i)

let str str = value (Str str)

(* bool *)
let () =
  let ty = Ty_bool in
  assert (Expr.equal (triop ty Ite (value True) (int 1) (int 0)) (int 1));
  assert (Expr.equal (triop ty Ite (value False) (int 1) (int 0)) (int 0))

(* str *)
let () =
  let ty = Ty_str in
  assert (triop ty String_extract (str "abcd") (int 1) (int 2) = str "bc");
  assert (triop ty String_index (str "abcd") (str "bc") (int 0) = int 1);
  assert (
    triop ty String_replace (str "abcd") (str "bc") (str "ef") = str "aefd" )

(* list *)
let () =
  let ty = Ty_list in
  let list l = value (List l) in
  assert (
    Expr.equal
      (triop ty List_set (list [ Int 0; Int 1; Int 2 ]) (int 1) (int 3))
      (list [ Int 0; Int 3; Int 2 ]) )
