#!/bin/bash

TEMPLATE=./tmp.XXXXXXXXXX

show_help() {
  echo "Usage: $0 [-n VALUE] [--help]"
  echo
  echo "Options:"
  echo "  -n VALUE   Number of benchmarks to run."
  echo "  --help     Show this help message and exit."
  exit 0
}

sample_benchmarks() {
  src=$1
  dst=$2
  n=$3

  echo "Sampling '$n' benchmarks from '$src' to '$dst'"
  files=$(find $src -type f -name "*.smt2" | shuf -n $n)
  for file in $files; do
    cp $file $dst
  done

  return 0
}

QF_BV_DIR=./smt-comp/smtlib/non-incremental/QF_BV
QF_FP_DIR=./smt-comp/smtlib/non-incremental/QF_FP
QF_LIA_DIR=./smt-comp/smtlib/non-incremental/QF_LIA
QF_SLIA_DIR=./smt-comp/smtlib/non-incremental/QF_SLIA
QF_S_DIR=./smt-comp/smtlib/non-incremental/QF_S

n_value=""
dry_value=false

while [[ $# -gt 0 ]]; do
  case $1 in
    -n)
      shift
      if [[ -z "$1" || "$1" == -* ]]; then
        echo "Error: Missing value for -n option."
        exit 1
      fi
      n_value=$1
      shift
      ;;
    --dry)
      dry_value=true
      shift
      ;;
    --help)
      show_help
      ;;
    *)
      echo "Error: Unknown argument: $1"
      show_help
      ;;
  esac
done

if [[ -n "$n_value" ]]; then
  echo "Sampling $n_value benchmarks ..."
  qf_bv=$(mktemp -d "$TEMPLATE")
  sample_benchmarks "$QF_BV_DIR" "$qf_bv" "$n_value"
  QF_BV_DIR="$qf_bv"

  qf_fp=$(mktemp -d "$TEMPLATE")
  sample_benchmarks "$QF_FP_DIR" "$qf_fp" "$n_value"
  QF_FP_DIR="$qf_fp"

  qf_lia=$(mktemp -d "$TEMPLATE")
  sample_benchmarks "$QF_LIA_DIR" "$qf_lia" "$n_value"
  QF_LIA_DIR="$qf_lia"

  qf_slia=$(mktemp -d "$TEMPLATE")
  sample_benchmarks "$QF_SLIA_DIR" "$qf_slia" "$n_value"
  QF_SLIA_DIR="$qf_slia"

  qf_s=$(mktemp -d "$TEMPLATE")
  sample_benchmarks "$QF_S_DIR" "$qf_s" "$n_value"
  QF_S_DIR="$qf_s"
fi

$dry_value && exit 0

## Z3 and Bitwuzla ##
opam sw z3-bitwuzla
eval $(opam env)
#### QF_FP ####
echo "Running Z3 and bitwuzla ..."
python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_bitwuzla --prover smtml-bitwuzla
python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_z3_solver --prover z3
python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_bitwuzla_solver --prover bitwuzla
#### QF_LIA ####
python3 run_benchmarks.py --single --dir $QF_LIA_DIR --output-dir csvs_single --output-filename QF_LIA_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir $QF_LIA_DIR --output-dir csvs_single --output-filename QF_LIA_z3_solver --prover z3
#### QF_BV ####
python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_bitwuzla --prover smtml-bitwuzla
python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_z3_solver --prover z3
python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_bitwuzla --prover smtml-bitwuzla
#### QF_S ####
python3 run_benchmarks.py --single --dir $QF_S_DIR --output-dir csvs_single --output-filename QF_S_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir $QF_S_DIR --output-dir csvs_single --output-filename QF_S_z3_solver --prover z3
#### QF_SLIA ####
python3 run_benchmarks.py --single --dir $QF_SLIA_DIR --output-dir csvs_single --output-filename QF_SLIA_z3 --prover smtml-z3
python3 run_benchmarks.py --single --dir $QF_SLIA_DIR --output-dir csvs_single --output-filename QF_SLIA_z3_solver --prover z3

## cvc5 ##
opam sw cvc5
eval $(opam env)
echo "Running cvc5 ..."
#### QF_FP ####
python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_cvc5_solver --prover cvc5
#### QF_LIA ####
python3 run_benchmarks.py --single --dir $QF_LIA_DIR --output-dir csvs_single --output-filename QF_LIA_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir $QF_LIA_DIR --output-dir csvs_single --output-filename QF_LIA_cvc5_solver --prover cvc5
#### QF_BV ####
python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_cvc5_solver --prover cvc5
#### QF_S ####
python3 run_benchmarks.py --single --dir $QF_S_DIR --output-dir csvs_single --output-filename QF_S_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir $QF_S_DIR --output-dir csvs_single --output-filename QF_S_cvc5_solver --prover cvc5
#### QF_SLIA ####
python3 run_benchmarks.py --single --dir $QF_SLIA_DIR --output-dir csvs_single --output-filename QF_SLIA_cvc5 --prover smtml-cvc5
python3 run_benchmarks.py --single --dir $QF_SLIA_DIR --output-dir csvs_single --output-filename QF_SLIA_cvc5_solver --prover cvc5

## Colibri2 ##
opam sw colibri2
eval $(opam env)
echo "Running Colibri2 ..."
#### QF_FP ####
python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_colibri2 --prover smtml-colibri2
python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_colibri2_solver --prover colibri2
#### QF_LIA ####
python3 run_benchmarks.py --single --dir $QF_LIA_DIR --output-dir csvs_single --output-filename QF_LIA_colibri2 --prover smtml-colibri2
python3 run_benchmarks.py --single --dir $QF_LIA_DIR --output-dir csvs_single --output-filename QF_LIA_colibri2_solver --prover colibri2
#### QF_BV ####
python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_colibri2 --prover smtml-colibri2
python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_colibri2_solver --prover colibri2

## Concat CSVs ##
echo "Concatenating csvs ..."
#### QF_FP ####
python3 plots.py --output csvs_single/QF_FP_all.csv \
  --concat \
  --files \
  csvs_single/QF_FP_z3.csv \
  csvs_single/QF_FP_bitwuzla.csv \
  csvs_single/QF_FP_cvc5.csv \
  csvs_single/QF_FP_colibri2.csv \
  csvs_single/QF_FP_z3_solver.csv \
  csvs_single/QF_FP_bitwuzla_solver.csv \
  csvs_single/QF_FP_cvc5_solver.csv \
  csvs_single/QF_FP_colibri2_solver.csv
#### QF_LIA ####
python3 plots.py --output csvs_single/QF_LIA_all.csv \
  --concat \
  --files \
  csvs_single/QF_LIA_z3.csv \
  csvs_single/QF_LIA_cvc5.csv \
  csvs_single/QF_LIA_colibri2.csv \
  csvs_single/QF_LIA_z3_solver.csv \
  csvs_single/QF_LIA_cvc5_solver.csv \
  csvs_single/QF_LIA_colibri2_solver.csv
#### QF_BV ####
python3 plots.py --output csvs_single/QF_BV_all.csv \
  --concat \
  --files \
  csvs_single/QF_BV_z3.csv \
  csvs_single/QF_BV_bitwuzla.csv \
  csvs_single/QF_BV_cvc5.csv \
  csvs_single/QF_BV_colibri2.csv \
  csvs_single/QF_BV_z3_solver.csv \
  csvs_single/QF_BV_bitwuzla_solver.csv \
  csvs_single/QF_BV_cvc5_solver.csv \
  csvs_single/QF_BV_colibri2_solver.csv
#### QF_S ####
python3 plots.py --output csvs_single/QF_S_all.csv \
  --concat \
  --files \
  csvs_single/QF_S_z3.csv \
  csvs_single/QF_S_cvc5.csv \
  csvs_single/QF_S_z3_solver.csv \
  csvs_single/QF_S_cvc5_solver.csv
#### QF_SLIA ####
python3 plots.py --output csvs_single/QF_SLIA_all.csv \
  --concat \
  --files csvs_single/QF_SLIA_z3.csv \
  csvs_single/QF_SLIA_cvc5.csv \
  csvs_single/QF_SLIA_z3_solver.csv \
  csvs_single/QF_SLIA_cvc5_solver.csv

## Generate plots ##
echo "Generating plots ..."
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
