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
  (name smtml-cvc5)
  (cmd "smtml run --mode incremental --solver cvc5 $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(prover
  (name smtml-alt-ergo)
  (cmd "smtml run --mode incremental --solver alt-ergo $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(prover
  (name z3)
  (cmd "z3 $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(prover
  (name bitwuzla)
  (cmd "bitwuzla $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(prover
  (name colibri2)
  (cmd "colibri2 $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(prover
  (name cvc5)
  (cmd "cvc5 $file")
  (sat "^sat")
  (unsat "unsat")
  (unknown "unknown"))

(dir
   (path "./")
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
