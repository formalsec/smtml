#!/bin/sh

mkdir eq1

opam sw z3-bitwuzla
eval $(opam env)

mkdir eq1/z3-bitwuzla
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_BV -p z3 -p smtml-z3 -p bitwuzla -p smtml-bitwuzla > eq1/z3-bitwuzla/qf_bv.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_FP -p z3 -p smtml-z3 -p bitwuzla -p smtml-bitwuzla > eq1/z3-bitwuzla/qf_fp.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_LIA -p z3 -p smtml-z3 > eq1/z3-bitwuzla/qf_lia.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_SLIA -p z3 -p smtml-z3 > eq1/z3-bitwuzla/qf_slia.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_S -p z3 -p smtml-z3 > eq1/z3-bitwuzla/qf_s.out

opam sw cvc5
eval $(opam env)

mkdir eq1/cvc5
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_BV -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_bv.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_FP -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_fp.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_LIA -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_lia.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_SLIA -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_slia.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_S -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_s.out

opam sw colibri2
eval $(opam env)

mkdir eq1/colibri2
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_BV -p colibri2 -p smtml-colibri2 > eq1/colibri2/qf_bv.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_FP -p colibri2 -p smtml-colibri2 > eq1/colibri2/qf_fp.out
benchpress run -c benchpress.sexp --task eq1 ./smt-comp/smtlib/non-incremental/QF_LIA -p colibri2 -p smtml-colibri2 > eq1/colibri2/qf_lia.out
