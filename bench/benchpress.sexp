(prover
  (name smtml-z3)
  (cmd "smtml run --mode incremental --solver z3 $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(prover
  (name smtml-bitwuzla)
  (cmd "smtml run --mode incremental --solver bitwuzla $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(prover
  (name smtml-colibri2)
  (cmd "smtml run --mode incremental --solver colibri2 $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(prover
  (name smtml-alt-ergo)
  (cmd "smtml run --mode incremental --solver alt-ergo $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(dir
  (path $cur_dir)
  (pattern ".*.smt2")
  (expect (run smtlib-read-status)))

(task
  (name local-test)
  (synopsis "Run smtml on directories provided on the command line")
  (action
    (run_provers
      (provers (smtml-z3 z3))
      (timeout 30)
      (dirs ()))))

(task
  (name collections-c)
  (synopsis "Run smtml on collections-c benchmarks")
  (action
    (run_provers
      (provers (z3 smtml-z3 smtml-bitwuzla smtml-colibri2))
      (timeout 30)
      (dirs ($cur_dir/data/collections-c)))))
