Test fp parsing:
  $ dune exec -- smtml test_qf_fp.smtml
  unsat
  sat
  sat
  (model
    (v f32 (f32 -0.))
    (x f32 (f32 0.504000008106))
    (w f32 (f32 -0.))
    (z f32 (f32 1.)))
  sat
  (model
    (v f32 (f32 -2.))
    (x f32 (f32 -2.70000004768))
    (w f32 (f32 -3.))
    (z f32 (f32 -2.)))
  sat
  (model
    (s f64 (f64 -3.38460706455e+125))
    (x f32 (f32 -2.70000004768))
    (y f64 (f64 314159265359.))
    (r f64 (f64 infinity))
    (u f64 (f64 nan)))
