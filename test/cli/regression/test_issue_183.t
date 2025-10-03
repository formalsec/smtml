Test solver not installed error message:
  $ smtml run --solver colibri2 - <<EOF
  > (let-const x int)
  > (assert (= x 1))
  > (check-sat)
  > EOF
  error: The Colibri2 solver is not installed.
  
  To install it, run the following command: opam install colibri2
  
  Alternatively, you can use a different solver.
  See supported solvers here: https://github.com/formalsec/smtml#supported-solvers
  
  Note: Installing the solver with your system package manager is not enough, you must install it through opam.
  [1]
