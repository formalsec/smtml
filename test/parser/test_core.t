Test boolean parsing:
  $ dune exec -- smtml test_core.smtml
  sat
  sat
  (model
    (w bool false)
    (z bool true)
    (x bool true)
    (y bool true))
