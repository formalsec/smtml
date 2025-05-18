open Smtml
open Expr

let pp_sat fmt = function
  | `Sat -> Fmt.string fmt "sat"
  | `Unsat -> Fmt.string fmt "unsat"
  | `Unknown -> Fmt.string fmt "unknown"

let assert_sat ?f result =
  let fail_msg =
    Fmt.str "%a: expected 'sat' but got '%a'" (Fmt.option Fmt.string) f pp_sat
      result
  in
  let b = match result with `Sat -> true | `Unsat | `Unknown -> false in
  OUnit2.assert_bool fail_msg b

let check a b =
  let pp_diff fmt (a, b) =
    Fmt.pf fmt "Got '%a' but expected '%a'" Expr.pp a Expr.pp b
  in
  OUnit2.assert_equal ~pp_diff ~cmp:Expr.equal a b

module Infix = struct
  let true_ = Expr.Bool.true_

  let false_ = Expr.Bool.false_

  let int x = value (Int x)

  let real x = value (Real x)

  let string x = value (Str x)

  let int8 x = value (Bitv (Bitvector.of_int8 x))

  let int32 x = value (Bitv (Bitvector.of_int32 x))

  let int64 x = value (Bitv (Bitvector.of_int64 x))

  let float32 x = value (Num (F32 (Int32.bits_of_float x)))

  let float64 x = value (Num (F64 (Int64.bits_of_float x)))

  let list x = value (List x)

  let app x = value (App (x, []))

  let symbol : type a. string -> a Ty.ty -> Expr.t =
   fun name ty -> symbol (Symbol.make (Ty ty) name)

  let ( = ) i1 i2 = relop Ty_bool Eq i1 i2

  let ( <> ) i1 i2 = relop Ty_bool Ne i1 i2

  let ( && ) b1 b2 = binop Ty_bool And b1 b2

  let ( || ) b1 b2 = binop Ty_bool Or b1 b2

  let ( => ) b1 b2 = binop Ty_bool Implies b1 b2

  module Int = struct
    let ( ~- ) i = unop Ty_int Neg i

    let ( + ) i1 i2 = binop Ty_int Add i1 i2

    let ( - ) i1 i2 = binop Ty_int Sub i1 i2

    let ( * ) i1 i2 = binop Ty_int Mul i1 i2

    let ( / ) i1 i2 = binop Ty_int Div i1 i2

    let ( % ) i1 i2 = binop Ty_int Rem i1 i2

    let ( ** ) i1 i2 = binop Ty_int Pow i1 i2

    let ( < ) i1 i2 = relop Ty_int Lt i1 i2

    let ( > ) i1 i2 = relop Ty_int Gt i1 i2

    let ( <= ) i1 i2 = relop Ty_int Le i1 i2

    let ( >= ) i1 i2 = relop Ty_int Ge i1 i2

    let to_real i = cvtop Ty_real Reinterpret_int i
  end
end
