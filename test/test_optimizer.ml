open Encoding
open Ty
open Expr
module Optimizer = Optimizer.Z3

let x = mk_symbol Symbol.("x" @: Ty_int)

(* Satisfiability *)
let%test "opt_min" =
  let opt = Optimizer.create () in
  Optimizer.add opt
    [ Relop (Ge, x, Val (Int 0) @: Ty_int) @: Ty_int
    ; Relop (Lt, x, Val (Int 5) @: Ty_int) @: Ty_int
    ];
  Some (Value.Int 0) = Optimizer.minimize opt x

let%test "opt_max" =
  let opt = Optimizer.create () in
  Optimizer.add opt
    [ Relop (Ge, x, Val (Int 0) @: Ty_int) @: Ty_int
    ; Relop (Lt, x, Val (Int 5) @: Ty_int) @: Ty_int
    ];
  Some (Value.Int 4) = Optimizer.maximize opt x
