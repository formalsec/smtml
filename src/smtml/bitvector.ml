type t =
  { value : Z.t
  ; width : int
  }

let mask width = Z.pred (Z.shift_left Z.one width)

let make v m =
  let masked_value = Z.logand v (mask m) in
  { value = masked_value; width = m }

let of_int8 v =
  (* TODO: add a check on v to make sure it is not too big ? *)
  make (Z.of_int v) 8

let of_int32 v = make (Z.of_int32 v) 32

let of_int64 v = make (Z.of_int64 v) 64

let to_int32 v =
  if v.width <= 32 then Z.to_int32 v.value
  else
    Fmt.failwith "call to Smtml.Bitvector.to_int32 on a bitvector of size %d"
      v.width

let to_int64 v =
  if v.width <= 64 then Z.to_int64 v.value
  else
    Fmt.failwith "call to Smtml.Bitvector.to_int32 on a bitvector of size %d"
      v.width

let view { value; _ } = value

let numbits { width; _ } = width

let equal a b = Z.equal a.value b.value && a.width = b.width

let eqz v = Z.equal Z.zero v.value

let eq_one v = Z.equal Z.one v.value

let compare a b = Z.compare a.value b.value

let msb bv = Z.testbit bv.value (bv.width - 1)

let to_signed bv =
  let msb = msb bv in
  if msb then Z.sub bv.value (Z.shift_left Z.one bv.width) else bv.value

(** Bitvector pretty printer. By default it prints signed bitvectors. *)
let pp fmt bv =
  let value = to_signed bv in
  Z.pp_print fmt value

(* Unop *)
let neg bv = make (Z.neg bv.value) bv.width

let lognot a = make (Z.lognot a.value) a.width

let clz bv =
  let rec count_zeros i =
    if i >= bv.width || Z.testbit bv.value (bv.width - 1 - i) then i
    else count_zeros (i + 1)
  in
  make (Z.of_int @@ count_zeros 0) bv.width

let ctz bv =
  let rec count_zeros i =
    if i >= bv.width || Z.testbit bv.value i then i else count_zeros (i + 1)
  in
  make (Z.of_int @@ count_zeros 0) bv.width

let popcnt bv = make (Z.of_int @@ Z.popcount bv.value) bv.width

(* Binop *)
let add a b =
  assert (a.width = b.width);
  make (Z.add a.value b.value) a.width

let sub a b =
  assert (a.width = b.width);
  make (Z.sub a.value b.value) a.width

let mul a b =
  assert (a.width = b.width);
  make (Z.mul a.value b.value) a.width

let div a b =
  assert (a.width = b.width);
  if Z.equal b.value Z.zero then raise Division_by_zero;
  make (Z.div (to_signed a) (to_signed b)) a.width

let div_u a b =
  assert (a.width = b.width);
  if Z.equal b.value Z.zero then raise Division_by_zero;
  make (Z.div a.value b.value) a.width

let logand a b =
  assert (a.width = b.width);
  make (Z.logand a.value b.value) a.width

let logor a b =
  assert (a.width = b.width);
  make (Z.logor a.value b.value) a.width

let logxor a b =
  assert (a.width = b.width);
  make (Z.logxor a.value b.value) a.width

let shl a n =
  let n = Z.to_int n.value in
  make (Z.shift_left a.value n) a.width

let ashr a n =
  let n = Z.to_int n.value in
  let signed_value = to_signed a in
  make (Z.shift_right signed_value n) a.width

let lshr a n =
  let n = Z.to_int n.value in
  make (Z.shift_right_trunc a.value n) a.width

let rem a b =
  assert (a.width = b.width);
  if Z.equal b.value Z.zero then raise Division_by_zero;
  make (Z.rem (to_signed a) (to_signed b)) a.width

let rem_u a b =
  assert (a.width = b.width);
  if Z.equal b.value Z.zero then raise Division_by_zero;
  make (Z.rem a.value b.value) a.width

let rotate_left bv n =
  let n = Z.to_int n.value mod bv.width in
  let left_part = Z.shift_left bv.value n in
  let right_part = Z.shift_right bv.value (bv.width - n) in
  let rotated = Z.logor left_part right_part in
  make rotated bv.width

let rotate_right bv n =
  let n = Z.to_int n.value mod bv.width in
  let right_part = Z.shift_right bv.value n in
  let left_part = Z.shift_left bv.value (bv.width - n) in
  let rotated = Z.logor left_part right_part in
  make rotated bv.width

(* Relop *)
let lt_u a b = Z.lt a.value b.value

let gt_u a b = Z.gt a.value b.value

let le_u a b = Z.leq a.value b.value

let ge_u a b = Z.geq a.value b.value

let lt a b = Z.lt (to_signed a) (to_signed b)

let gt a b = Z.gt (to_signed a) (to_signed b)

let le a b = Z.leq (to_signed a) (to_signed b)

let ge a b = Z.geq (to_signed a) (to_signed b)

(* Extract and concat *)
let concat a b =
  let new_width = a.width + b.width in
  let shifted = Z.shift_left a.value b.width in
  let combined = Z.logor shifted b.value in
  make combined new_width

let extract bv ~high ~low =
  assert (high <= bv.width && low >= 0 && low < high);
  let width = high - low + 1 in
  let shifted = Z.shift_right bv.value low in
  let extracted = Z.logand shifted (mask width) in
  make extracted width

(* Cvtop *)
let zero_extend width bv =
  let new_width = bv.width + width in
  make bv.value new_width

let sign_extend width bv =
  let new_width = bv.width + width in
  let msb = msb bv in
  let sign_mask =
    if msb then
      let shift_amount = bv.width in
      Z.shift_left (mask width) shift_amount
    else Z.zero
  in
  let extended = Z.logor bv.value sign_mask in
  make extended new_width

let to_string bv = Fmt.str "%a" pp bv

let to_json bv = `String (to_string bv)
