open Smtml.Bitvector

let z n = Z.of_int n (* Helper to create Z.t values *)

let test_make () =
  let bv = make (z 5) 8 in
  assert (view bv = z 5);
  assert (numbits bv = 8)

let test_neg () =
  let bv = make (z 5) 8 in
  assert (equal (neg bv) (make (z (-5)) 8))

let test_add () =
  let bv1 = make (z 3) 8 in
  let bv2 = make (z 5) 8 in
  assert (view (add bv1 bv2) = z 8)

let test_sub () =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 3) 8 in
  assert (view (sub bv1 bv2) = z 7)

let test_mul () =
  let bv1 = make (z 4) 8 in
  let bv2 = make (z 3) 8 in
  assert (view (mul bv1 bv2) = z 12)

let test_div () =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 2) 8 in
  assert (view (div bv1 bv2) = z 5)

let test_div_u () =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 3) 8 in
  assert (view (div_u bv1 bv2) = z (10 / 3))

let test_logical_ops () =
  let bv1 = make (z 0b1100) 4 in
  let bv2 = make (z 0b1010) 4 in
  assert (view (logand bv1 bv2) = z 0b1000);
  assert (view (logor bv1 bv2) = z 0b1110);
  assert (view (logxor bv1 bv2) = z 0b0110)

let test_shifts () =
  let bv = make (z 0b0011) 4 in
  assert (view (shl bv (make (z 1) 4)) = z 0b0110);
  assert (view (lshr bv (make (z 1) 4)) = z 0b0001);
  assert (view (ashr bv (make (z 1) 4)) = z 0b0001)

let test_comparisons () =
  let bv1 = make (z 3) 4 in
  let bv2 = make (z 5) 4 in
  assert (lt bv1 bv2);
  assert (le bv1 bv2);
  assert (gt bv2 bv1);
  assert (ge bv2 bv1);
  assert (lt_u bv1 bv2);
  assert (gt_u bv2 bv1)

let test_rotate () =
  let bv = make (z 0b1101) 4 in
  let one = make (z 1) 4 in
  assert (view (rotate_left bv one) = z 0b1011);
  assert (view (rotate_right bv one) = z 0b1110)

let test_extensions () =
  let bv = make (z 0b1010) 4 in
  assert (numbits (zero_extend 4 bv) = 8);
  assert (numbits (sign_extend 4 bv) = 8)

let () =
  test_make ();
  test_neg ();
  test_add ();
  test_sub ();
  test_mul ();
  test_div ();
  test_div_u ();
  test_logical_ops ();
  test_shifts ();
  test_comparisons ();
  test_rotate ();
  test_extensions ()
