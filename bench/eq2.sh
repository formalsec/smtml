#!/bin/bash

## Z3 and Bitwuzla ##
opam sw z3-bitwuzla
eval $(opam env)
#### QF_FP ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_singe --output-filename QF_FP_bitwuzla --prover smtml-bitwuzla
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_z3_solver --prover z3
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_bitwuzla_solver --prover bitwuzla
#### QF_LIA ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_z3_solver --prover z3
#### QF_BV ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_bitwuzla --prover smtml-bitwuzla
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_z3_solver --prover z3
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_bitwuzla --prover smtml-bitwuzla
#### QF_S ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_S --output-dir csvs_single --output-filename QF_S_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_S --output-dir csvs_single --output-filename QF_S_z3_solver --prover z3
#### QF_SLIA ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_SLIA --output-dir csvs_single --output-filename QF_SLIA_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_SLIA --output-dir csvs_single --output-filename QF_SLIA_z3_solver --prover z3

## cvc5 ##
opam sw cvc5
eval $(opam env)
#### QF_FP ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_cvc5_solver --prover cvc5
#### QF_LIA ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_cvc5_solver --prover cvc5
#### QF_BV ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_cvc5_solver --prover cvc5
#### QF_S ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_S --output-dir csvs_single --output-filename QF_S_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_S --output-dir csvs_single --output-filename QF_S_cvc5_solver --prover cvc5
#### QF_SLIA ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_SLIA --output-dir csvs_single --output-filename QF_SLIA_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_SLIA --output-dir csvs_single --output-filename QF_SLIA_cvc5_solver --prover cvc5

## Colibri2 ##
opam sw colibri2
eval $(opam env)
#### QF_FP ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_colibri2 --prover smtml-colibri2
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_colibri2_solver --prover colibri2
#### QF_LIA ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_colibri2 --prover smtml-colibri2
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_colibri2_solver --prover colibri2
#### QF_BV ####
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_colibri2 --prover smtml-colibri2
python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_colibri2_solver --prover colibri2

## Concat CSVs ##
#### QF_FP ####
python3 plots.py --output csvs_single/QF_FP_all.csv --concat --files csvs_single/QF_FP_z3.csv csvs_single/QF_FP_bitwuzla.csv csvs_single/QF_FP_cvc5.csv csvs_single/QF_FP_colibri2.csv csvs_single/QF_FP_z3_solver.csv csvs_single/QF_FP_bitwuzla_solver.csv csvs_single/QF_FP_cvc5_solver.csv csvs_single/QF_FP_colibri2_solver.csv
#### QF_LIA ####
python3 plots.py --output csvs_single/QF_LIA_all.csv --concat --files csvs_single/QF_LIA_z3.csv csvs_single/QF_LIA_cvc5.csv csvs_single/QF_LIA_colibri2.csv csvs_single/QF_LIA_z3_solver.csv csvs_single/QF_LIA_cvc5_solver.csv csvs_single/QF_LIA_colibri2_solver.csv
#### QF_BV ####
python3 plots.py --output csvs_single/QF_BV_all.csv --concat --files csvs_single/QF_BV_z3.csv csvs_single/QF_BV_bitwuzla.csv csvs_single/QF_BV_cvc5.csv csvs_single/QF_BV_colibri2.csv csvs_single/QF_BV_z3_solver.csv csvs_single/QF_BV_bitwuzla_solver.csv csvs_single/QF_BV_cvc5_solver.csv csvs_single/QF_BV_colibri2_solver.csv
#### QF_S ####
python3 plots.py --output csvs_single/QF_S_all.csv --concat --files csvs_single/QF_S_z3.csv csvs_single/QF_S_cvc5.csv csvs_single/QF_S_z3_solver.csv csvs_single/QF_S_cvc5_solver.csv
#### QF_SLIA ####
python3 plots.py --output csvs_single/QF_SLIA_all.csv --concat --files csvs_single/QF_SLIA_z3.csv csvs_single/QF_SLIA_cvc5.csv csvs_single/QF_SLIA_z3_solver.csv csvs_single/QF_SLIA_cvc5_solver.csv

## Generate plots ##
#### QF_FP ####
python3 plots.py --single --QF-FP --output plot_QF_FP --files csvs_single/QF_FP_all.csv
#### QF_LIA ####
python3 plots.py --single --QF-LIA --output plot_QF_LIA --files csvs_single/QF_LIA_all.csv
#### QF_BV ####
python3 plots.py --single --QF-BV --output plot_QF_BV --files csvs_single/QF_BV_all.csv
#### QF_S ####
python3 plots.py --single --QF-S --output plot_QF_S --files csvs_single/QF_S_all.csv
#### QF_SLIA ####
python3 plots.py --single --QF-SLIA --output plot_QF_SLIA --files csvs_single/QF_SLIA_all.csv