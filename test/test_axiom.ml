open Encoding

let solver = Batch.create ()
let _ = Batch.set_default_axioms solver.solver
let encode f = try ignore (Z3_mappings.encode_expr f) with exn -> raise exn

let%test_unit _ = encode (List.hd Axioms.axioms)

let%test _ =
  let x = Expression.mk_symbol `StrType "x"
  and y = Expression.mk_symbol `StrType "y" in
  Batch.check_sat solver
    [
      Integer.mk_eq (Integer.mk_val 0) (Integer.mk_of_string x);
      Integer.mk_eq (Integer.mk_val 0) (Integer.mk_of_string y);
      Strings.mk_ne x y;
    ]
