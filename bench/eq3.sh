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

sample_dir_benchmarks() {
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

sample_list_benchmarks() {
  src=$1
  output=$2
  n=$3

  echo "Sampling '$n' benchmarks listed in '$src' to '$output'"

  if [[ ! -f $src ]]; then
    echo "Error: Source file '$src' does not exist."
    return 1
  fi

  sampled=$(shuf -n $n "$src")

  echo "$sampled" > "$output"

  return 0
}

QF_BV_DIR=./smt-comp/smtlib/non-incremental/QF_BV
QF_BV_LIST=./QF_BV_paths.list

QF_FP_DIR=./smt-comp/smtlib/non-incremental/QF_FP
QF_FP_LIST=./QF_FP_paths.list

QF_LIA_DIR=./smt-comp/smtlib/non-incremental/QF_LIA
QF_LIA_LIST=./QF_LIA_paths.list

QF_SLIA_DIR=./smt-comp/smtlib/non-incremental/QF_SLIA
QF_SLIA_LIST=./QF_SLIA_paths.list

QF_S_DIR=./smt-comp/smtlib/non-incremental/QF_S
QF_S_LIST=./QF_S_paths.list

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
  qf_bv_list=$(mktemp "$TEMPLATE")
  sample_dir_benchmarks "$QF_BV_DIR" "$qf_bv" "$n_value"
  sample_list_benchmarks "$QF_BV_LIST" "$qf_bv_list" "$n_value"
  QF_BV_DIR="$qf_bv"
  QF_BV_LIST="$qf_bv_list"

  qf_fp=$(mktemp -d "$TEMPLATE")
  qf_fp_list=$(mktemp "$TEMPLATE")
  sample_dir_benchmarks "$QF_FP_DIR" "$qf_fp" "$n_value"
  sample_list_benchmarks "$QF_FP_LIST" "$qf_fp_list" "$n_value"
  QF_FP_DIR="$qf_fp"
  QF_FP_LIST="$qf_fp_list"

  qf_lia=$(mktemp -d "$TEMPLATE")
  qf_lia_list=$(mktemp "$TEMPLATE")
  sample_dir_benchmarks "$QF_LIA_DIR" "$qf_lia" "$n_value"
  sample_list_benchmarks "$QF_LIA_LIST" "$qf_lia_list" "$n_value"
  QF_LIA_DIR="$qf_lia"
  QF_LIA_LIST="$qf_lia_list"

  qf_slia=$(mktemp -d "$TEMPLATE")
  qf_slia_list=$(mktemp "$TEMPLATE")
  sample_dir_benchmarks "$QF_SLIA_DIR" "$qf_slia" "$n_value"
  sample_list_benchmarks "$QF_SLIA_LIST" "$qf_slia_list" "$n_value"
  QF_SLIA_DIR="$qf_slia"
  QF_SLIA_LIST="$qf_slia_list"

  qf_s=$(mktemp -d "$TEMPLATE")
  qf_s_list=$(mktemp "$TEMPLATE")
  sample_dir_benchmarks "$QF_S_DIR" "$qf_s" "$n_value"
  sample_list_benchmarks "$QF_S_LIST" "$qf_s_list" "$n_value"
  QF_S_DIR="$qf_s"
  QF_S_LIST="$qf_s_list"
fi

$dry_value && exit 0

opam sw z3-bitwuzla
eval $(opam env)

#### QF_FP ####
## Z3 ##
if [ -e "csvs_single/QF_FP_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F $QF_FP_LIST --output-dir csvs_multi --output-filename QF_FP_z3 --prover smtml-z3

## Bitwuzla ##
if [ -e "csvs_single/QF_FP_bitwuzla_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir $QF_FP_DIR --output-dir csvs_single --output-filename QF_FP_bitwuzla_solver --prover bitwuzla
fi

python3 run_benchmarks.py --multi -F $QF_FP_LIST --output-dir csvs_multi --output-filename QF_FP_bitwuzla_solver --prover bitwuzla

#### QF_LIA ####
## Z3 ##
if [ -e "csvs_single/QF_LIA_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir $QF_LIA_DIR --output-dir csvs_single --output-filename QF_LIA_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F $QF_LIA_LIST --output-dir csvs_multi --output-filename QF_LIA_z3 --prover smtml-z3

#### QF_BV ####
## Z3 ##
if [ -e "csvs_single/QF_BV_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F $QF_BV_LIST --output-dir csvs_multi --output-filename QF_BV_z3 --prover smtml-z3

## Bitwuzla ##
if [ -e "csvs_single/QF_BV_bitwuzla_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir $QF_BV_DIR --output-dir csvs_single --output-filename QF_BV_bitwuzla_solver --prover bitwuzla
fi

python3 run_benchmarks.py --multi -F $QF_BV_LIST --output-dir csvs_multi --output-filename QF_BV_bitwuzla_solver --prover bitwuzla

#### QF_S ####
## Z3 ##
if [ -e "csvs_single/QF_S_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir $QF_S_DIR --output-dir csvs_single --output-filename QF_S_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F $QF_S_LIST --output-dir csvs_multi --output-filename QF_S_z3 --prover smtml-z3

#### QF_SLIA ####
## Z3 ##
if [ -e "csvs_single/QF_SLIA_z3_solver.csv" ]; then
  : # do nothing
else
  python3 run_benchmarks.py --single --dir $QF_SLIA_DIR --output-dir csvs_single --output-filename QF_SLIA_z3_solver --prover z3
fi

python3 run_benchmarks.py --multi -F $QF_SLIA_LIST --output-dir csvs_multi --output-filename QF_SLIA_z3 --prover smtml-z3


## Generate plots ##

## QF_FP ##
python3 plots.py --multi --QF-FP --output plot_QF_FP_multi \
  --files \
  csvs_multi/QF_FP_z3.csv \
  csvs_multi/QF_FP_bitwuzla.csv \
  csvs_single/QF_FP_z3_solver.csv \
  csvs_single/QF_FP_bitwuzla_solver.csv

## QF_LIA ##
python3 plots.py --multi --QF-LIA --output plot_QF_LIA_multi \
  --files \
  csvs_multi/QF_LIA_z3.csv \
  csvs_single/QF_LIA_z3_solver.csv

## QF_BV ##
python3 plots.py --multi --QF-BV --output plot_QF_BV_multi \
  --files \
  csvs_multi/QF_BV_z3.csv \
  csvs_multi/QF_BV_bitwuzla.csv \
  csvs_single/QF_BV_z3_solver.csv \
  csvs_single/QF_BV_bitwuzla_solver.csv

## QF_S ##
python3 plots.py --multi --QF-S --output plot_QF_S_multi \
  --files \
  csvs_multi/QF_S_z3.csv \
  csvs_single/QF_S_z3_solver.csv

## QF_SLIA ##
python3 plots.py --multi --QF-SLIA --output plot_QF_SLIA_multi \
  --files \
  csvs_multi/QF_SLIA_z3.csv \
  csvs_single/QF_SLIA_z3_solver.csv
