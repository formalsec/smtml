import Mathlib

lemma simplification_unop_000001 {_ty : Type}  (x : Bool) :
  ¬¬x ↔ x :=
by simp

lemma simplification_unop_000002 {_ty : Type}  (x : _ty) :
  ¬¬x = x :=
by simp

lemma simplification_unop_000003 {ty : Type} [DecidableEq ty] (e2 e1 : ty) :
  ¬((e1) ≠ (e2)) ↔ ((e1) = (e2)) :=
by simp

lemma simplification_unop_000004 {ty : Type} [DecidableEq ty] (e2 e1 : ty) :
  ¬((e1) = (e2)) ↔ ((e1) ≠ (e2)) :=
by simp

lemma simplification_unop_000005 {ty : Type} [LinearOrder ty] (e2 e1 : ty) :
  ¬((e1) < (e2)) ↔ ((e2) ≤ (e1)) :=
by simp

lemma simplification_unop_000006 {ty : Type} [LinearOrder ty] (e2 e1 : ty) :
  ¬((e1) ≤ (e2)) ↔ ((e2) < (e1)) :=
by simp

lemma simplification_unop_000007 {ty : Type} [LinearOrder ty] (e2 e1 : ty) :
  ¬((e1) > (e2)) ↔ ((e1) ≤ (e2)) :=
by simp

lemma simplification_unop_000008 {ty : Type} [LinearOrder ty] (e2 e1 : ty) :
  ¬((e1) ≥ (e2)) ↔ ((e1) < (e2)) :=
by simp

lemma simplification_unop_000010 {_ty : Type}  (l : List _ty) :
  List.reverse (List.reverse (l)) = l :=
by simp

lemma simplification_binop_000012 {_ty : Type}  (hte : _ty) :
  (True) ∧ (hte) = hte :=
by simp

lemma simplification_binop_000014  {w : Nat} (hte2 bv : BitVec w) :
  (bv = 0) →
  (bv) + (hte2) = hte2 :=
by intro h; simp [h]

lemma simplification_binop_000015  {w : Nat} (hte2 bv : BitVec w) :
  (bv = 0) →
  (bv) ||| (hte2) = hte2 :=
by intro h; simp [h]

lemma simplification_binop_000016  {w : Nat} (hte1 bv : BitVec w) :
  (bv = 0) →
  (hte1) + (bv) = hte1 :=
by intro h; simp [h]

lemma simplification_binop_000017  {w : Nat} (hte1 bv : BitVec w) :
  (bv = 0) →
  (hte1) ||| (bv) = hte1 :=
by intro h; simp [h]

lemma simplification_binop_000018  {w : Nat} (_hte bv : BitVec w) :
  (bv = 0) →
  (bv) &&& (_hte) = 0 :=
by intro h; simp [h]

lemma simplification_binop_000019  {w : Nat} (_hte bv : BitVec w) :
  (bv = 0) →
  (bv) * (_hte) = 0 :=
by intro h; simp [h]

lemma simplification_binop_000020  {w : Nat} (_hte bv : BitVec w) :
  (bv = 0) →
  (_hte) &&& (bv) = 0 :=
by intro h; simp [h]

lemma simplification_binop_000021  {w : Nat} (_hte bv : BitVec w) :
  (bv = 0) →
  (_hte) * (bv) = 0 :=
by intro h; simp [h]

lemma simplification_binop_000022  {w : Nat} (hte2 bv : BitVec w) :
  (bv = 1) →
  (bv) * (hte2) = hte2 :=
by intro h; simp [h]

lemma simplification_binop_000023  {w : Nat} (hte1 bv : BitVec w) :
  (bv = 1) →
  (hte1) * (bv) = hte1 :=
by intro h; simp [h]

lemma simplification_binop_000024 {ty : Type} [Ring ty] (v1 x v2 : ty) :
  ((x) + (v1)) + (v2) = (x) + ((v1) + (v2)) :=
by apply add_assoc

lemma simplification_binop_000025 {ty : Type} [Ring ty] (v1 x v2 : ty) :
  ((x) - (v1)) - (v2) = (x) - ((v1) + (v2)) :=
by abel

lemma simplification_binop_000026 {ty : Type} [Ring ty] (v1 x v2 : ty) :
  ((x) * (v1)) * (v2) = (x) * ((v1) * (v2)) :=
by apply mul_assoc

lemma simplification_binop_000027 {ty : Type} [Ring ty] (v1 x v2 : ty) :
  (v1) + ((x) + (v2)) = ((v1) + (v2)) + (x) :=
by abel

lemma simplification_binop_000028 {ty : Type} [CommRing ty] (v1 x v2 : ty) :
  (v1) * ((x) * (v2)) = ((v1) * (v2)) * (x) :=
by ring

lemma simplification_triop_000034 {_ty : Type}  (_e2 e1 : _ty) :
  (if True then e1 else _e2) = e1 :=
by simp

lemma simplification_triop_000035 {_ty : Type}  (e2 _e1 : _ty) :
  (if False then _e1 else e2) = e2 :=
by simp

lemma simplification_triop_0000369 {ty : Type}  (c3 c2 c1 : Prop) (e4 e3 e2 e1 : ty)
  [Decidable c1] [Decidable c2] [Decidable c3] :
  (if c1 then (if c2 then e1 else e2) else (if c3 then e3 else e4)) = (if (c1) ∧ (c2) then e1 else (if c1 then e2 else (if c3 then e3 else e4))) :=
by grind

lemma simplification_naryop_000001  (l2 l1 : List String)  :
  (String.join ([(String.join l1), (String.join l2)])) = (String.join ((l1 ++ l2))) :=
by simp [String.join_eq]

lemma simplification_naryop_000002  (htes : List String) (hte : String)  :
  (String.join ([(String.join htes), hte])) = (String.join ((htes ++ [hte]))) :=
by simp [String.join_eq]

lemma simplification_naryop_000003  (htes : List String) (hte : String)  :
  (String.join ([hte, (String.join htes)])) = (String.join ((hte :: htes))) :=
by simp [String.join_eq]
