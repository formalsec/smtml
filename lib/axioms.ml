open Types

let axioms =
  [
    Formula.Quantifier
      ( Formula.Forall,
        [ ("x", `StrType) ],
        Formula.Relop
          (Relop
             ( Str S.Eq,
               Expression.Cvtop
                 ( Int I.ToStr,
                   Expression.Cvtop (Str S.ToInt, Symbolic (`StrType, "x")) ),
               Symbolic (`StrType, "x") )),
        [ [ Expression.Cvtop (Str S.ToInt, Symbolic (`StrType, "x")) ] ] );
  ]
