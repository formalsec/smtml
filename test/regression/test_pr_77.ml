open Smtml

let () =
  let open Num in
  let nan0 = F32 (Int32.bits_of_float Float.nan) in
  let nan1 = F32 (Int32.bits_of_float Float.nan) in
  (* Library functions ensure total order of floats. *)
  assert (Num.equal nan0 nan1)

let () =
  let open Expr.Fpa in
  let nan0 = F32.v Float.nan in
  let nan1 = F32.v Float.nan in
  (* Library functions ensure total order of floats. *)
  assert (Expr.equal nan0 nan1);
  let assert_ = Expr.relop (Ty_fp 32) Eq nan0 nan1 in
  (* Evaluation functions do not. *)
  assert (Expr.equal assert_ (Expr.value False))

let () =
  let open Expr in
  let nan0 = value (Real Float.nan) in
  let nan1 = value (Real Float.nan) in
  (* Library functions ensure total order of floats. *)
  assert (Expr.equal nan0 nan1);
  let assert_ = Expr.relop Ty_real Eq nan0 nan1 in
  (* Evaluation functions do not. *)
  assert (Expr.equal assert_ (Expr.value False))
