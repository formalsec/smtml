(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
open Smtml.Typed

let unwrap = Unsafe.unwrap

let get_val expr =
  match Expr.view (unwrap expr) with
  | Val (Bitv bv) -> Z.to_int (Bitvector.view bv)
  | _ -> Alcotest.fail "Expression did not simplify to a constant bitvector"

let test_basic_bit_extraction () =
  let v_F = Bitv32.of_int32 0xFl in

  let ext1 = Bitv32.extract v_F ~high:0 ~low:0 in
  Alcotest.(check int) "Extract bit 0" 1 (get_val ext1);

  let ext2 = Bitv32.extract v_F ~high:3 ~low:0 in
  Alcotest.(check int) "Extract bits 3 to 0" 0xF (get_val ext2)

let test_non_byte_aligned () =
  let v_AA = Bitv32.of_int32 0xAAl in

  let ext = Bitv32.extract v_AA ~high:5 ~low:2 in
  Alcotest.(check int) "Extract bits 5 to 2" 0xA (get_val ext)

let test_width_verification () =
  let e = Bitv32.of_int32 0xFFFFFFFFl in

  let ext_8 = Bitv32.extract e ~high:7 ~low:0 in
  Alcotest.(check (testable Ty.pp Ty.equal) "Width should be 8")
    (Ty.Ty_bitv 8)
    (Expr.ty (unwrap ext_8));

  let ext_16 = Bitv32.extract e ~high:15 ~low:0 in
  Alcotest.(check (testable Ty.pp Ty.equal) "Width should be 16")
    (Ty.Ty_bitv 16)
    (Expr.ty (unwrap ext_16))

let test_boundary_conditions () =
  let e = Bitv32.of_int32 0xFFFFFFFFl in

  let ext_31 = Bitv32.extract e ~high:31 ~low:0 in
  Alcotest.(check (testable Ty.pp Ty.equal) "Width should be 32")
    (Ty.Ty_bitv 32)
    (Expr.ty (unwrap ext_31));

  let ext_0 = Bitv32.extract e ~high:0 ~low:0 in
  Alcotest.(check (testable Ty.pp Ty.equal) "Width should be 1")
    (Ty.Ty_bitv 1)
    (Expr.ty (unwrap ext_0))

let test_typed_api_consistency () =
  let e = Bitv32.of_int32 0xAABBCCDDl in
  let bytes = Bitv32.to_bytes e in
  Alcotest.(check int) "should produce 4 chunks" 4 (List.length bytes);
  Alcotest.(check (testable Ty.pp Ty.equal) "each chunk should be 8 bits")
    (Ty.Ty_bitv 8)
    (Expr.ty (unwrap (List.hd bytes)))

let () =
  Alcotest.run "Typed_Bit_Extraction"
    [ ( "Typed_Bit_Extraction"
      , [ Alcotest.test_case "test_basic_bit_extraction" `Quick
            test_basic_bit_extraction
        ; Alcotest.test_case "test_non_byte_aligned" `Quick
            test_non_byte_aligned
        ; Alcotest.test_case "test_width_verification" `Quick
            test_width_verification
        ; Alcotest.test_case "test_boundary_conditions" `Quick
            test_boundary_conditions
        ; Alcotest.test_case "test_typed_api_consistency" `Quick
            test_typed_api_consistency
        ] )
    ]
