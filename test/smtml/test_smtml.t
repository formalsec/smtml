Test boolean parsing:
  $ smtml run test_core.smtml
  sat
  sat
  (model
    (w false)
    (x true)
    (y true)
    (z true))

 Test int parsing:
  $ smtml run test_lia.smtml
  sat
  sat

 Test real parsing:
  $ smtml run test_lra.smtml
  sat
  (model
    (x 2.)
    (y 4.))

 Test fp parsing:
  $ smtml run test_qf_fp.smtml
  unsat
  sat
  sat
  (model
    (v (f32 -0.))
    (w (f32 -0.))
    (x (f32 0.504000008106))
    (z (f32 1.)))
  sat
  (model
    (v (f32 -2.))
    (w (f32 -3.))
    (x (f32 -2.70000004768))
    (z (f32 -2.)))
  sat
  sat
  (model
    (r (f64 infinity))
    (s (f64 -3.38460706455e+125))
    (y (f64 314159265359.)))

 Test parsing string:
  $ smtml run test_qf_s.smtml
  sat
  (model
    (x "abcd"))

