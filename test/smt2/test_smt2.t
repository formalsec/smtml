Test parsing a nonexistent file:
  $ smtml run idontexist.smt2
  smtml: [WARNING] idontexist.smt2: No such file or directory

Test parsing an empty file:
  $ smtml run test_empty.smt2

Test Bool parsing:
  $ smtml run test_core_true.smt2
  sat
  $ smtml run test_core_const.smt2
  sat
  $ smtml run test_core_binary.smt2
  sat
  $ smtml run test_core_all.smt2
  sat
  sat
  (model
    (w false)
    (x true)
    (y true)
    (z true))

Test Int parsing:
  $ smtml run test_lia.smt2
  sat
  sat

Test Real parsing:
  $ smtml run test_lra.smt2
  sat
  (model
    (x 2.)
    (y 4.))

Test String parsing:
  $ smtml run test_string_all.smt2
  sat
  (model
    (x "abcd")
    (y "a"))

Test BitVector parsing:
  $ smtml run test_bitv_sort.smt2
  $ smtml run test_bitv_const.smt2
  sat
  $ smtml run test_bitv_funs.smt2
  sat

Test FloatingPoint parsing:
  $ smtml run test_fp.smt2
  sat
