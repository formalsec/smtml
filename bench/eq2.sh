#!/bin/bash

## Z3 and Bitwuzla ##
opam sw z3-bitwuzla
eval $(opam env)
#### QF_FP ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_z3 --prover smtml-z3
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_singe --output-filename QF_FP_bitwuzla --prover smtml-bitwuzla
#### QF_LIA ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_z3 --prover smtml-z3
#### QF_BV ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_z3 --prover smtml-z3
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_bitwuzla --prover smtml-bitwuzla
#### QF_S ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_S --output-dir csvs_single --output-filename QF_S_z3 --prover smtml-z3
#### QF_SLIA ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_SLIA --output-dir csvs_single --output-filename QF_SLIA_z3 --prover smtml-z3

## cvc5 ##
opam sw cvc5
eval $(opam env)
#### QF_FP ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_cvc5 --prover smtml-cvc5
#### QF_LIA ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_cvc5 --prover smtml-cvc5
#### QF_BV ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_cvc5 --prover smtml-cvc5
#### QF_S ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_S --output-dir csvs_single --output-filename QF_S_cvc5 --prover smtml-cvc5
#### QF_SLIA ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_SLIA --output-dir csvs_single --output-filename QF_SLIA_cvc5 --prover smtml-cvc5

## Colibri2 ##
opam sw colibri2
eval $(opam env)
#### QF_FP ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_FP --output-dir csvs_single --output-filename QF_FP_colibri2 --prover smtml-colibri2
#### QF_LIA ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_LIA --output-dir csvs_single --output-filename QF_LIA_colibri2 --prover smtml-colibri2
#### QF_BV ####
python3 single_query.py --dir smt-comp/smtlib/non-incremental/QF_BV --output-dir csvs_single --output-filename QF_BV_colibri2 --prover smtml-colibri2

## Concat CSVs ##
#### QF_FP ####
python3 plots.py --output csvs_single/QF_FP_all.csv --concat csvs_single/QF_FP_z3.csv csvs_single/QF_FP_bitwuzla.csv csvs_single/QF_FP_cvc5.csv csvs_single/QF_FP_colibri2.csv
#### QF_LIA ####
python3 plots.py --output csvs_single/QF_LIA_all.csv --concat csvs_single/QF_LIA_z3.csv csvs_single/QF_LIA_cvc5.csv csvs_single/QF_LIA_colibri2.csv
#### QF_BV ####
python3 plots.py --output csvs_single/QF_BV_all.csv --concat csvs_single/QF_BV_z3.csv csvs_single/QF_BV_bitwuzla.csv csvs_single/QF_BV_cvc5.csv csvs_single/QF_BV_colibri2.csv
#### QF_S ####
python3 plots.py --output csvs_single/QF_S_all.csv --concat csvs_single/QF_S_z3.csv csvs_single/QF_S_cvc5.csv
#### QF_SLIA ####
python3 plots.py --output csvs_single/QF_SLIA_all.csv --concat csvs_single/QF_SLIA_z3.csv csvs_single/QF_SLIA_cvc5.csv

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