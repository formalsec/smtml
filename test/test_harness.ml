open Encoding
open Expr

let assert_sat result =
  assert (
    match result with
    | `Sat -> true
    | `Unsat -> false
    | `Unknown -> failwith "Solver returned unknown" )

module Infix = struct
  let int i = value (Int i)

  let symbol name ty = symbol (Symbol.make ty name)

  let ( = ) i1 i2 = relop Ty_bool Eq i1 i2

  let ( <> ) i1 i2 = relop Ty_bool Ne i1 i2

  let ( && ) b1 b2 = binop Ty_bool And b1 b2

  let ( || ) b1 b2 = binop Ty_bool Or b1 b2

  let ( => ) b1 b2 =
    let left = unop Ty_bool Not b1 in
    binop Ty_bool Or left b2

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
