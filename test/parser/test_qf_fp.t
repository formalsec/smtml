Test fp parsing:
  $ dune exec -- smtml test_qf_fp.smtml
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
  (model
    (r (f64 infinity))
    (s (f64 -3.38460706455e+125))
    (u (f64 nan))
    (x (f32 -2.70000004768))
    (y (f64 314159265359.)))
