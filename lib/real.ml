open Expression
open Types

let mk_val (f : float) : Expression.t = Val (Real f)

let mk_neg (e : Expression.t) : Expression.t = Unop (Real R.Neg, e)
let mk_abs (e : Expression.t) : Expression.t = Unop (Real R.Abs, e)
let mk_sqrt (e : Expression.t) : Expression.t = Unop (Real R.Sqrt, e)
let mk_nearest (e : Expression.t) : Expression.t = Unop (Real R.Nearest, e)
let mk_is_nan (e : Expression.t) : Expression.t = Unop (Real R.IsNan, e)

let mk_add (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Real R.Add, e1, e2)

let mk_sub (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Real R.Sub, e1, e2)

let mk_mul (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Real R.Mul, e1, e2)

let mk_div (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Real R.Div, e1, e2)

let mk_min (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Real R.Min, e1, e2)

let mk_max (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Real R.Max, e1, e2)

let mk_rem (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Real R.Rem, e1, e2)

let mk_eq (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Real R.Eq, e1, e2)

let mk_ne (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Real R.Ne, e1, e2)

let mk_lt (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Real R.Lt, e1, e2)

let mk_le (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Real R.Le, e1, e2)

let mk_gt (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Real R.Gt, e1, e2)

let mk_ge (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Real R.Ge, e1, e2)
