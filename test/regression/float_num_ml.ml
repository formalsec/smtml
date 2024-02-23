open Encoding

let () =
  let open Num in
  assert (
    let nan0 = F32 (Int32.bits_of_float Float.nan) in
    let nan1 = F32 (Int32.bits_of_float Float.nan) in
    not Num.(nan0 = nan1) )

let () =
  let open Expr.Fpa in
  assert (
    let nan0 = F32.v Float.nan in
    let nan1 = F32.v Float.nan in
    not (Expr.equal nan0 nan1) )
