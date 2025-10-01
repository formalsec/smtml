import Mathlib

-- Unary operators
-- ¬(¬x) = x
lemma simplification_unop_000001 (x : Bool) :
  ¬(¬x) = x :=
by simp

-- ¬(e1 ≠ e2) → e1 = e2
lemma simplification_unop_000002 {α : Type} [DecidableEq α] (e1 e2 : α) :
  ¬(e1 ≠ e2) → e1 = e2 :=
by simp

-- ¬(e1 = e2) → e1 ≠ e2
lemma simplification_unop_000003 {α : Type} [DecidableEq α] (e1 e2 : α) :
  ¬(e1 = e2) → e1 ≠ e2 :=
by simp

-- ¬(e1 < e2) → e2 ≤ e1
lemma simplification_unop_000004 {α : Type} [LinearOrder α] (e1 e2 : α) :
  ¬(e1 < e2) → e2 ≤ e1 :=
by simp

-- ¬(e1 ≤ e2) → e2 < e1
lemma simplification_unop_000005 {α : Type} [LinearOrder α] (e1 e2 : α) :
  ¬(e1 ≤ e2) → e2 < e1 :=
by simp

-- ¬(e1 > e2) → e1 ≤ e2
lemma simplification_unop_000006 {α : Type} [LinearOrder α] (e1 e2 : α) :
  ¬(e1 > e2) → e1 ≤ e2 :=
by simp

-- ¬(e1 ≥ e1) → e1 < e2
lemma simplification_unop_000007 {α : Type} [LinearOrder α] (e1 e2 : α) :
  ¬(e1 ≥ e2) → e1 < e2 :=
by simp

-- length([x1, ..., xn]) = n
lemma simplification_unop_000008 {α : Type} (x : α) (n : Nat) :
  List.length (List.replicate n x) = n :=
by simp


-- Binary operators

-- True ∧ x = x
lemma simplification_binop_000001 (x : Bool) :
  True ∧ x = x :=
by simp

-- (0 + x) = x
lemma simplification_binop_000002 (x : Int) :
  (0 + x) = x :=
by simp

-- (x + c1) + c2 = x + (c1 + c2)
lemma simplification_binop_000003 (x : Int) (c1 c2 : Int) :
  (x + c1) + c2 = x + (c1 + c2) :=
by exact Int.add_assoc x c1 c2

-- (x - c1) - c2 = x - (c1 + c2)
lemma simplification_binop_000004 (x : Int) (c1 c2 : Int) :
  (x - c1) - c2 = x - (c1 + c2) :=
by exact Int.sub_sub x c1 c2

-- (x ⋅ c1) ⋅ c2 = x ⋅ (c1 ⋅ c2)
lemma simplification_binop_000005 (x : Int) (c1 c2 : Int) :
  (x * c1) * c2 = x * (c1 * c2) :=
by grind

-- c1 + (x + c2) → (c1 + c2) + x
lemma simplification_binop_000006 (x : Int) (c1 c2 : Int) :
  c1 + (x + c2) = (c1 + c2) + x :=
by ring_nf

-- c1 ⋅ (x ⋅ c2) = (c1 ⋅ c2) ⋅ x
lemma simplification_binop_000007 (x : Int) (c1 c2 : Int) :
  c1 * (x * c2) = (c1 * c2) * x :=
by grind

lemma simplification_binop_000008 (x : Real) (c1 c2 : Real) :
  (x + c1) + c2 = x + (c1 + c2) :=
by ring


-- Ternary Operators

-- ite(True, x, y) = x
lemma simplification_triop_000001 {α : Sort u} (x y : α) :
  ite True x y = x :=
by apply ite_true

-- ite(False, x, y) = y
lemma simplification_triop_000002 {α : Sort u} (x y : α) :
  ite False x y = y :=
by apply ite_false

/-
    ite(cond1, ite(cond2, r1, r2), ite(cond3, r3, r4))
      = ite(cond1 ∧ cond2, r1, ite(cond1, r2, ite(cond3, r3, r4)))
-/
lemma simplification_triop_000003 {α : Sort u} {cond1 cond2 cond3 : Prop}
  [Decidable cond1] [Decidable cond2] [Decidable cond3]
  (r1 r2 r3 r4 : α) :
  ite cond1 (ite cond2 r1 r2) (ite cond3 r3 r4)
    = ite (cond1 ∧ cond2) r1 (ite cond1 r2 (ite cond3 r3 r4)) :=
by grind

-- Nary Operators
lemma simplification_naryop_000001 (l1 l2 : List String) :
  String.join [String.join l1, String.join l2] = String.join (l1 ++ l2) :=
by simp [String.join_eq]

lemma simplification_naryop_000002 (s : String) (l : List String) :
  String.join [String.join l, s] = String.join (l ++ [s]) :=
by simp [String.join_eq]

lemma simplification_naryop_000003 (s : String) (l : List String) :
  String.join [s, String.join l] = String.join ([s] ++ l) :=
by simp [String.join_eq]
