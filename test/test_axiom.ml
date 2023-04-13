open Base
open Encoding
open Types
open Expression

let solver = Batch.create ()
let _ = Batch.set_default_axioms solver.solver

let encode f = try ignore (Common.encode_formula f) with exn -> raise exn
let encode_int e = try ignore (Common.encode_expr e) with exn -> raise exn
let int_zero = Val (Int Int.zero)
let str_symb = Symbolic (`StrType, "x")

let axiom =
  Formula.Axiom
    ( Formula.Forall,
      [ ("x", `StrType) ],
      Formula.Relop
        (Relop
           ( Str S.Eq,
             Expression.Cvtop
               ( Int I.ToStr,
                 Expression.Cvtop (Str S.ToInt, Symbolic (`StrType, "x")) ),
             Val (Str "test_str") )),
      [ [ Expression.Cvtop (Str S.ToInt, Symbolic (`StrType, "x")) ] ] )

let%test_unit _ = encode axiom

let%test _ =
  Batch.check_sat solver
    [
      Relop
        ( Int I.Eq,
          Val (Int 0),
          Expression.Cvtop (Str S.ToInt, Symbolic (`StrType, "x")) );
      Relop
        ( Int I.Eq,
          Val (Int 0),
          Expression.Cvtop (Str S.ToInt, Symbolic (`StrType, "y")) );
      Unop (Bool B.Not, Relop (Str S.Eq, Symbolic (`StrType, "x"), Symbolic (`StrType, "y")));
    ]
