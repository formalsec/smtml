Test smtml formatting:
  $ dune exec smtml -- fmt test_let.smtml
  (let-const a int)
  (assert
    (let ((x (int.add a 5)))
    (let ((y (int.mul 2 x)))
    (bool.and (int.gt y 10) (int.lt y 20)))))
  (check-sat)
  (get-model)
  $ dune exec smtml -- run test_let.smtml
  sat
  (model
    (a 1))
