open Smtml

let () =
  let open Num in
  let nan0 = F32 (Int32.bits_of_float Float.nan) in
  let nan1 = F32 (Int32.bits_of_float Float.nan) in
  assert (Num.equal nan0 nan1)

let () =
  let open Expr.Fpa in
  let nan0 = F32.v Float.nan in
  let nan1 = F32.v Float.nan in
  assert (Expr.equal nan0 nan1);
  let eq = Expr.relop (Ty_fp 32) Eq nan0 nan1 in
  assert (Expr.equal (Expr.simplify eq) (Expr.value False))
