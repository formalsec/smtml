open Smtml
open Ty
open Expr
module Optimizer = Optimizer.Z3

let v i = value (Int i)

let ( >= ) i1 i2 = relop Ty_int Ge i1 i2

let ( < ) i1 i2 = relop Ty_int Lt i1 i2

let () =
  let opt = Optimizer.create () in
  let x = mk_symbol Symbol.("x" @: Ty_int) in
  Optimizer.add opt [ x >= v 0; x < v 5 ];
  Optimizer.push opt;
  assert (Some (Value.Int 0) = Optimizer.minimize opt x);
  Optimizer.pop opt;
  assert (Some (Value.Int 4) = Optimizer.maximize opt x)
