open Expression
open Types

let mk_val (f : float) : expr = Val (Real f)
let mk_neg (e : expr) : expr = Unop (Real R.Neg, e)
let mk_add (e1 : expr) (e2 : expr) : expr = Binop (Real R.Add, e1, e2)
let mk_sub (e1 : expr) (e2 : expr) : expr = Binop (Real R.Sub, e1, e2)
let mk_mul (e1 : expr) (e2 : expr) : expr = Binop (Real R.Mul, e1, e2)
let mk_div (e1 : expr) (e2 : expr) : expr = Binop (Real R.Div, e1, e2)
let mk_eq (e1 : expr) (e2 : expr) : expr = Relop (Real R.Eq, e1, e2)
let mk_ne (e1 : expr) (e2 : expr) : expr = Relop (Real R.Ne, e1, e2)
let mk_lt (e1 : expr) (e2 : expr) : expr = Relop (Real R.Lt, e1, e2)
let mk_le (e1 : expr) (e2 : expr) : expr = Relop (Real R.Le, e1, e2)
let mk_gt (e1 : expr) (e2 : expr) : expr = Relop (Real R.Gt, e1, e2)
let mk_ge (e1 : expr) (e2 : expr) : expr = Relop (Real R.Ge, e1, e2)
let mk_to_string (e : expr) : expr = Cvtop (Real R.ToString, e)
let mk_of_string (e : expr) : expr = Cvtop (Real R.OfString, e)
