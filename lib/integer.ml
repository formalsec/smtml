open Expression
open Types

let mk_val (i : int) : Expression.t = Val (Int i)
let mk_neg (e : Expression.t) : Expression.t = Unop (Int I.Neg, e)

let mk_add (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.Add, e1, e2)

let mk_sub (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.Sub, e1, e2)

let mk_mul (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.Mul, e1, e2)

let mk_div (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.Div, e1, e2)

let mk_rem (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.Rem, e1, e2)

let mk_shl (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.Shl, e1, e2)

let mk_shr_a (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.ShrA, e1, e2)

let mk_shr_l (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.ShrL, e1, e2)

let mk_and (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.And, e1, e2)

let mk_or (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.Or, e1, e2)

let mk_xor (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Int I.Xor, e1, e2)

let mk_eq (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Int I.Eq, e1, e2)

let mk_ne (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Int I.Ne, e1, e2)

let mk_lt (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Int I.Lt, e1, e2)

let mk_le (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Int I.Le, e1, e2)

let mk_gt (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Int I.Gt, e1, e2)

let mk_ge (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Int I.Ge, e1, e2)

let mk_to_string (e : Expression.t) : Expression.t = Cvtop (Int I.ToString, e)
let mk_of_string (e : Expression.t) : Expression.t = Cvtop (Int I.OfString, e)
