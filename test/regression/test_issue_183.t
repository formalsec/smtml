Test solver not installed error message:
  $ smtml run --solver colibri2 <<EOF
  > (let-const x int)
  > (assert (= x 1))
  > (check-sat)
  > EOF
  The Colibri2 solver is not installed. You must install it through opam with the command `opam install colibri2`. You could also try to use another solver (have a look at the supported solvers here: https://github.com/formalsec/smtml?tab=readme-ov-file#supported-solvers). Note that installing the solver with your system package manager is not enough, you must install it through opam.
  [1]
