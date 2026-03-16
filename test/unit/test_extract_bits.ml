open OUnit2
open Smtml
open Smtml.Typed

(* The skeleton key to convert Typed expressions to Raw expressions *)
let unwrap = Unsafe.unwrap

let get_val expr =
  match Expr.view (unwrap expr) with
  | Val (Bitv bv) -> Z.to_int (Bitvector.view bv)
  | _ -> assert_failure "Expression did not simplify to a constant bitvector"

let test_basic_bit_extraction _ =
  let v_F = Bitv32.of_int32 0xFl in

  let ext1 = Bitv32.extract v_F ~high:0 ~low:0 in
  assert_equal 1 (get_val ext1) ~msg:"Extract bit 0";

  let ext2 = Bitv32.extract v_F ~high:3 ~low:0 in 
  assert_equal 0xF (get_val ext2) ~msg:"Extract bits 3 to 0"

let test_non_byte_aligned _ =
  let v_AA = Bitv32.of_int32 0xAAl in
  
  let ext = Bitv32.extract v_AA ~high:5 ~low:2 in
  assert_equal 0xA (get_val ext) ~msg:"Extract bits 5 to 2"

let test_width_verification _ =
  let e = Bitv32.of_int32 0xFFFFFFFFl in

  let ext_8 = Bitv32.extract e ~high:7 ~low:0 in
  assert_equal (Ty.Ty_bitv 8) (Expr.ty (unwrap ext_8)) ~msg:"Width should be 8";

  let ext_16 = Bitv32.extract e ~high:15 ~low:0 in
  assert_equal (Ty.Ty_bitv 16) (Expr.ty (unwrap ext_16)) ~msg:"Width should be 16"

let test_boundary_conditions _ =
  let e = Bitv32.of_int32 0xFFFFFFFFl in

  let ext_31 = Bitv32.extract e ~high:31 ~low:0 in
  assert_equal (Ty.Ty_bitv 32) (Expr.ty (unwrap ext_31)) ~msg:"Width should be 32";

  let ext_0 = Bitv32.extract e ~high:0 ~low:0 in
  assert_equal (Ty.Ty_bitv 1) (Expr.ty (unwrap ext_0)) ~msg:"Width should be 1"

let test_typed_api_consistency _ =
  let e = Bitv32.of_int32 0xAABBCCDDl in
  let bytes = Bitv32.to_bytes e in
  assert_equal 4 (List.length bytes) ~msg:"should produce 4 chunks";
  assert_equal (Ty.Ty_bitv 8) (Expr.ty (unwrap (List.hd bytes))) ~msg:"each chunk should be 8 bits"

let test_suite =
  "Typed_Bit_Extraction"
  >:::[ "test_basic_bit_extraction" >:: test_basic_bit_extraction
       ; "test_non_byte_aligned" >:: test_non_byte_aligned
       ; "test_width_verification" >:: test_width_verification 
       ; "test_boundary_conditions" >:: test_boundary_conditions 
       ; "test_typed_api_consistency" >:: test_typed_api_consistency
       ] 

let () = run_test_tt_main test_suite