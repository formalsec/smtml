(declare-const x (_ BitVec 32))
(assert (= ((_ rotate_left 8) x) #x00001000))
(check-sat)
(get-model)
