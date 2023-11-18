open Encoding
open Ty
open Expr

let opt = Optimizer.create ()
let x = mk_symbol Symbol.("x" @: Ty_int)

(* Satisfiability *)
let%test "opt_min" =
  let pc =
    [ Relop (Ge, x, Val (Int 0) @: Ty_int) @: Ty_int
    ; Relop (Lt, x, Val (Int 5) @: Ty_int) @: Ty_int
    ]
  in
  Some (Value.Int 0) = Optimizer.minimize opt x pc

let%test "opt_max" =
  let pc =
    [ Relop (Ge, x, Val (Int 0) @: Ty_int) @: Ty_int
    ; Relop (Lt, x, Val (Int 5) @: Ty_int) @: Ty_int
    ]
  in
  Some (Value.Int 4) = Optimizer.maximize opt x pc
