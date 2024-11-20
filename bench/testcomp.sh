#!/bin/bash

opam sw z3-bitwuzla
eval $(opam env)

#### array-examples ####
python3 run_benchmarks.py --single --dir smt-testcomp23/array-examples --output-dir csvs_single --output-filename array_examples_z3_solver --prover z3
python3 run_benchmarks.py --single --dir smt-testcomp23/array-examples --output-dir csvs_single --output-filename array_examples_z3 --prover smtml-z3
python3 run_benchmarks.py --multi -F array_examples_paths.list --output-dir csvs_multi --output-filename array_examples_z3 --prover smtml-z3

#### array-industry-pattern ####
python3 run_benchmarks.py --single --dir smt-testcomp23/array-industry-pattern --output-dir csvs_single --output-filename array_industry_pattern_z3_solver --prover z3
python3 run_benchmarks.py --single --dir smt-testcomp23/array-industry-pattern --output-dir csvs_single --output-filename array_industry_pattern_z3 --prover smtml-z3
python3 run_benchmarks.py --multi -F array_industry_pattern_paths.list --output-dir csvs_multi --output-filename array_industry_pattern_z3 --prover smtml-z3

#### eca-rers2018 ####
python3 run_benchmarks.py --single --dir smt-testcomp23/eca-rers2018 --output-dir csvs_single --output-filename eca_rers2018_z3_solver --prover z3
python3 run_benchmarks.py --single --dir smt-testcomp23/eca-rers2018 --output-dir csvs_single --output-filename eca_rers2018_z3 --prover smtml-z3
python3 run_benchmarks.py --multi -F eca_rers2018_paths.list --output-dir csvs_multi --output-filename eca_rers2018_z3 --prover smtml-z3

## Plots ##
python3 plots.py --testcomp --array-examples --output plot_array_examples_multi --files csvs_multi/array_examples_z3.csv csvs_single/array_examples_z3_solver.csv csvs_single/array_examples_z3.csv
python3 plots.py --testcomp --array-industry-pattern --output plot_array_industry_pattern_multi --files csvs_multi/array_industry_pattern_z3.csv csvs_single/array_industry_pattern_z3_solver.csv csvs_single/array_industry_pattern_z3.csv
python3 plots.py --testcomp --eca-rers2018 --output plot_eca_rers2018_multi --files csvs_multi/eca_rers2018_z3.csv csvs_single/eca_rers2018_z3_solver.csv csvs_single/eca_rers2018_z3.csv