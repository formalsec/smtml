(declare-const x Int)
(declare-const y Int)
(declare-const z Int)
(assert (> x 0))
(assert (<= x 10))
(assert (= (- (+ x y) y) x))
(assert (= (/ (* x y) y) x))
(assert (< (mod y x) 10))
(assert (= x (/ (* x x) x)))
(check-sat)
(assert (< (- z) 0))
(check-sat)
