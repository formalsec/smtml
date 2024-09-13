(prover
  (name smtml)
  (cmd "smtml run $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(dir
  (path $cur_dir)
  (pattern ".*.smt2")
  (expect (run z3)))

(task
  (name local-test)
  (synopsis "Run smtml on directories provided on the command line")
  (action
    (run_provers
      (provers (smtml z3))
      (timeout 30)
      (dirs ()))))

(task
  (name collections-c)
  (synopsis "Run smtml on collections-c benchmarks")
  (action
    (run_provers
      (provers (smtml z3))
      (timeout 30)
      (dirs ($cur_dir/data/collections-c)))))
