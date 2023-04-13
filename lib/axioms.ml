let axioms =
  [
    Expression.Quantifier
      ( Expression.Forall,
        [ ("x", `StrType) ],
          Strings.mk_eq
             (Integer.mk_to_string
                (Integer.mk_of_string (Expression.mk_symbolic `StrType "x")))
             (Expression.mk_symbolic `StrType "x"),
        [ [ Integer.mk_of_string (Expression.mk_symbolic `StrType "x") ] ] );
  ]
