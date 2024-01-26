Test smtml formatting:
  $ dune exec smtml -- fmt test_fmt.smtml
  (let-const a int)
  (let-const b int)
  (assert
    (let ((x (int.add a 5)))
    (let ((y (int.mul 2 x)))
    (bool.and (int.gt y 10) (int.lt y 20)))))
  (check-sat)
  (get-model)
