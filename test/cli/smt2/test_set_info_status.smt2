(set-info :status unsat) ; on purpose to test expected status tracking
(declare-fun a () Int)
(assert (= a 0))
(check-sat)
