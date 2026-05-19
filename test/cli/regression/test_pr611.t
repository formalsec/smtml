  $ smtml run --solver smtzilla --debug test_pr611_z3.smt2 2>&1 | sed -n '/Selected solver/,$p'
  smtml: [INFO] Selected solver z3
  sat

  $ smtml run --solver smtzilla --debug test_pr611_bitwuzla.smt2 2>&1 | sed -n '/Selected solver/,$p'
  smtml: [INFO] Selected solver bitwuzla
  sat
