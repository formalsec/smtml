Test parsing a nonexistent file:
  $ smtml run idontexist.smt2
  smtml: FILES… arguments: no 'idontexist.smt2' file or directory
  Usage: smtml run [OPTION]… [FILES]…
  Try 'smtml run --help' or 'smtml --help' for more information.
  [124]

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
    (w bool false)
    (x bool true)
    (y bool true)
    (z bool true))

Test Int parsing:
  $ smtml run test_lia.smt2
  sat
  sat

Test Real parsing:
  $ smtml run test_lra.smt2
  sat
  (model
    (x real 2.)
    (y real 4.))

Test String parsing:
  $ smtml run test_string_all.smt2
  sat
  (model
    (x str "abcd")
    (y str "a"))

Test BitVector parsing:
  $ smtml run test_bitv_sort.smt2
  $ smtml run test_bitv_const.smt2
  sat
  $ smtml run test_bitv_funs.smt2
  sat

Test FloatingPoint parsing:
  $ smtml run test_fp.smt2
  sat
  $ smtml run test_fp_literals.smt2
  sat
  (model
    (nan_val f32 nan)
    (neg_inf f32 neg_infinity)
    (neg_zero f32 -0.)
    (pos_inf f32 infinity)
    (pos_zero f32 0.))

Tests smt2 with the --from-file argument:
  $ cat <<EOF > test.list
  > test_empty.smt2
  > test_core_all.smt2
  > test_lia.smt2
  > test_lra.smt2
  > test_fp.smt2
  > test_string_all.smt2
  > EOF
  $ smtml run --from-file test.list
  sat
  sat
  (model
    (w bool false)
    (x bool true)
    (y bool true)
    (z bool true))
  sat
  sat
  sat
  (model
    (x real -2.)
    (y real 4.))
  sat
  sat
  (model
    (x str "abcd")
    (y str "a"))

Test Forall and Exists parsing:
  $ smtml run test_forall.smt2
  sat
  $ smtml run test_exists.smt2
  sat

Test implication:
  $ smtml run test_implication.smt2
  sat

Test uninterpreted:
  $ smtml run test_uninterpreted.smt2
  sat
  (model
    (x f32 0.))
