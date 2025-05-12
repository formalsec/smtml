(assert (forall ((x Int) (y Int) (z Int)) (=> (and (>= x 3) (>= y 5) (= z (+ x y))) (>= z 8))))
(check-sat)
