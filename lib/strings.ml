open Expression
open Types

let mk_val (s : String.t) : Expression.t = Val (Str s)
let mk_len (s : Expression.t) : Expression.t = Unop (Str S.Len, s)

let mk_nth (s : Expression.t) (i : Expression.t) : Expression.t =
  Binop (Str S.Nth, s, i)

let mk_concat (s1 : Expression.t) (s2 : Expression.t) : Expression.t =
  Binop (Str S.Concat, s1, s2)

let mk_eq (s1 : Expression.t) (s2 : Expression.t) : Expression.t =
  Relop (Str S.Eq, s1, s2)

let mk_ne (s1 : Expression.t) (s2 : Expression.t) : Expression.t =
  Relop (Str S.Ne, s1, s2)

let mk_substr (s : Expression.t) ~(pos : Expression.t) ~(len : Expression.t) :
    Expression.t =
  Triop (Str S.SubStr, s, pos, len)
