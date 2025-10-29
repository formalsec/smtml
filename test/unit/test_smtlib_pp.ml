open Smtml

let () =
  let x = Expr.symbol @@ Symbol.make Ty_int "x" in
  let y = Expr.symbol @@ Symbol.make Ty_int "y" in
  let script =
    Ast.Script.of_exprs
      [ Expr.relop Ty_int Eq
          (Expr.binop Ty_int Add x y)
          (Expr.value (Value.Int 30))
      ; Expr.relop Ty_int Gt x
          (Expr.binop Ty_int Mul (Expr.value (Value.Int 2)) y)
      ; Expr.relop Ty_int Gt y (Expr.value (Value.Int 10))
      ]
  in
  Fmt.pr "@[<v>%a@]@." Ast.Script.pp script
