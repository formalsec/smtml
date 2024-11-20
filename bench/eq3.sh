#!/bin/bash

opam sw z3-bitwuzla
eval $(opam env)

#### QF_FP ####
## Z3 ##
if [ -e "csvs_single/QF_FP_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F QF_FP_paths.list --output-dir csvs_multi --output-filename QF_FP_z3 --prover smtml-z3

## Bitwuzla ##
if [ -e "csvs_single/QF_FP_bitwuzla_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_bitwuzla_solver --prover bitwuzla
fi

python3 run_benchmarks.py --multi -F QF_FP_paths.list --output-dir csvs_multi --output-filename QF_FP_bitwuzla_solver --prover bitwuzla

#### QF_LIA ####
## Z3 ##
if [ -e "csvs_single/QF_LIA_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F QF_LIA_paths.list --output-dir csvs_multi --output-filename QF_LIA_z3 --prover smtml-z3

#### QF_BV ####
## Z3 ##
if [ -e "csvs_single/QF_BV_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F QF_BV_paths.list --output-dir csvs_multi --output-filename QF_BV_z3 --prover smtml-z3

## Bitwuzla ##
if [ -e "csvs_single/QF_BV_bitwuzla_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_bitwuzla_solver --prover bitwuzla
fi

python3 run_benchmarks.py --multi -F QF_BV_paths.list --output-dir csvs_multi --output-filename QF_BV_bitwuzla_solver --prover bitwuzla

#### QF_S ####
## Z3 ##
if [ -e "csvs_single/QF_S_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_S --output-dir csvs_single --output-filename QF_S_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F QF_S_paths.list --output-dir csvs_multi --output-filename QF_S_z3 --prover smtml-z3

#### QF_SLIA ####
## Z3 ##
if [ -e "csvs_single/QF_SLIA_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir smt-comp/smtlib/non-incremental/QF_SLIA --output-dir csvs_single --output-filename QF_SLIA_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F QF_SLIA_paths.list --output-dir csvs_multi --output-filename QF_SLIA_z3 --prover smtml-z3


## Generate plots ##

## QF_FP ##
python3 plots.py --multi --QF-FP --output plot_QF_FP_multi --files csvs_multi/QF_FP_z3.csv csvs_multi/QF_FP_bitwuzla.csv csvs_single/QF_FP_z3_solver.csv csvs_single/QF_FP_bitwuzla_solver.csv

## QF_LIA ##
python3 plots.py --multi --QF-LIA --output plot_QF_LIA_multi --files csvs_multi/QF_LIA_z3.csv csvs_single/QF_LIA_z3_solver.csv

## QF_BV ##
python3 plots.py --multi --QF-BV --output plot_QF_BV_multi --files csvs_multi/QF_BV_z3.csv csvs_multi/QF_BV_bitwuzla.csv csvs_single/QF_BV_z3_solver.csv csvs_single/QF_BV_bitwuzla_solver.csv

## QF_S ##
python3 plots.py --multi --QF-S --output plot_QF_S_multi --files csvs_multi/QF_S_z3.csv csvs_single/QF_S_z3_solver.csv

## QF_SLIA ##
python3 plots.py --multi --QF-SLIA --output plot_QF_SLIA_multi --files csvs_multi/QF_SLIA_z3.csv csvs_single/QF_SLIA_z3_solver.csv