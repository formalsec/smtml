open Smtml
open Ty
open Expr
open Value

(* ---------- Binop ---------- *)

(* list *)
let () = 
  let i i = value (Int i) in
  let empty_list = make (List []) in
  let mk_list l = make (List l) in 
  let bool_symb x = mk_symbol (Symbol.make Ty_bool x) in 
  assert (
    binop Ty_list List_append_last 
      empty_list
      (Bool.ite (bool_symb "x") (i 2) (i 1))
    = mk_list [Bool.ite (bool_symb "x") (i 2) (i 1)])

(* and *)

let () = 
  let x = mk_symbol (Symbol.make Ty_int "x") in 
  let y = mk_symbol (Symbol.make Ty_int "y") in 
  let ne = relop Ty_bool Ne in
  let eq = relop Ty_bool Eq in
  let lt = relop Ty_bool Lt in
  let gt = relop Ty_bool Gt in
  let le = relop Ty_bool Le in
  let ge = relop Ty_bool Ge in
  let (&&&) e1 e2 = Bool.and_ e1 e2 in 
  assert((ne x y) &&& (lt x y) = lt x y);
  assert((ne x y) &&& (gt x y) = gt x y);
  assert((ne x y) &&& (le x y) = lt x y);
  assert((ne x y) &&& (ge x y) = gt x y);

  assert((eq x y) &&& (lt x y) = le x y);
  assert((eq x y) &&& (gt x y) = ge x y);
  assert((eq x y) &&& (le x y) = le x y);
  assert((eq x y) &&& (ge x y) = ge x y)

(* ---------- Triop ---------- *)

(* ite *)
let () =
  let bool_symb x = mk_symbol (Symbol.make Ty_bool x) in 
  let ite = Bool.ite in 
  let and_ = Bool.and_ in
  let not = Bool.not in
  assert (
    ite (bool_symb "x") (value True) (value False) 
    = (bool_symb "x"));
  assert (
    ite (bool_symb "x") (value False) (value True) 
    = not (bool_symb "x"));
  assert (
    ite (bool_symb "x") (value False) (bool_symb "y") 
    = and_ (not (bool_symb "x")) (bool_symb "y"));
  assert (
    ite (bool_symb "x") (bool_symb "y") (value False)
    = and_ (bool_symb "x") (bool_symb "y"));


