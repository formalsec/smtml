(set-logic QF_FP)

(declare-fun pos_inf () (_ FloatingPoint 8 24))
(declare-fun neg_inf () (_ FloatingPoint 8 24))
(declare-fun pos_zero () (_ FloatingPoint 8 24))
(declare-fun neg_zero () (_ FloatingPoint 8 24))
(declare-fun nan_val () (_ FloatingPoint 8 24))

(assert (= pos_inf (_ +oo 8 24)))
(assert (= neg_inf (_ -oo 8 24)))
(assert (= pos_zero (_ +zero 8 24)))
(assert (= neg_zero (_ -zero 8 24)))
(assert (= nan_val (_ NaN 8 24)))

(check-sat)
(get-model)
