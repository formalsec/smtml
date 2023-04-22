open Encoding

let opt = Optimizer.create ()
let int_symb = Expression.mk_symbol `IntType "x"

(* Satisfiability *)
let%test "opt_min" =
  let pc =
    [
      Integer.mk_ge int_symb (Integer.mk_val 0);
      Integer.mk_lt int_symb (Integer.mk_val 5);
    ]
  in
  Some (Expression.Int 0) = Optimizer.minimize opt int_symb pc

let%test "opt_max" =
  let pc =
    [
      Integer.mk_ge int_symb (Integer.mk_val 0);
      Integer.mk_lt int_symb (Integer.mk_val 5);
    ]
  in
  Some (Expression.Int 4) = Optimizer.maximize opt int_symb pc
