Test fp parsing:
  $ smtml run test_qf_fp.smtml
  unsat
  sat
  sat
  (model
    (v (fp 0x80000000))
    (w (fp 0x80000000))
    (x (fp 0x3f010625))
    (z (fp 0x3f800000)))
  sat
  (model
    (v (fp 0xc0000000))
    (w (fp 0xc0400000))
    (x (fp 0xc02ccccd))
    (z (fp 0xc0000000)))
  sat
  sat
  (model
    (r (fp 0x7ff0000000000000))
    (s (fp 0xda00000028000800))
    (y (fp 0x425249567d93c000)))
