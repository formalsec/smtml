open Encoding
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()

let () = Batch.add solver Axioms.axioms

let%test _ =
  let x = Expression.mk_symbol_s `StrType "x"
  and y = Expression.mk_symbol_s `StrType "y" in
  not
    (Batch.check solver
       [ Strings.mk_ne x y
       ; Integer.mk_eq (Integer.mk_val 0) (Integer.mk_of_string x)
       ; Integer.mk_eq (Integer.mk_val 0) (Integer.mk_of_string y)
       ] )
