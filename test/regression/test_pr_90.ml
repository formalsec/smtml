open Encoding
open Ty
open Value

let () =
  let one = Int 1 in
  let zero = Int 0 in
  let minus_one = Int (-1) in
  assert (Eval_numeric.eval_unop Ty_int Neg one = minus_one);
  assert (Eval_numeric.eval_binop Ty_int Sub zero one = minus_one);
  assert (Eval_numeric.eval_relop Ty_int Lt zero one)
