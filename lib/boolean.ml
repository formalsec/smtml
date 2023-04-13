open Expression
open Types

let mk_val (b : bool) : Expression.t = Val (Bool b)
let mk_not (e : Expression.t) : Expression.t = Unop (Bool B.Not, e)

let mk_and (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Bool B.And, e1, e2)

let mk_or (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Bool B.Or, e1, e2)

let mk_xor (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Binop (Bool B.Xor, e1, e2)

let mk_eq (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Bool B.Eq, e1, e2)

let mk_ne (e1 : Expression.t) (e2 : Expression.t) : Expression.t =
  Relop (Bool B.Ne, e1, e2)
