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

ARRAY_EXAMPLES_DIR=./smt-testcomp23/array-examples
ARRAY_EXAMPLES_LIST=./array_examples_paths.list

ARRAY_INDUSTRY_DIR=./smt-testcomp23/array-industry-pattern
ARRAY_INDUSTRY_LIST=./array_industry_pattern_paths.list

ECA_DIR=./smt-testcomp23/eca-rers2018
ECA_LIST=./eca_rers2018_paths.list

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
  array_examples=$(mktemp -d "$TEMPLATE")
  array_examples_list=$(mktemp "$TEMPLATE")
  sample_dir_benchmarks "$ARRAY_EXAMPLES_DIR" "$array_examples" "$n_value"
  sample_list_benchmarks "$ARRAY_EXAMPLES_LIST" "$array_examples_list" "$n_value"
  ARRAY_EXAMPLES_DIR="$array_examples"
  ARRAY_EXAMPLES_LIST="$array_examples_list"

  array_industry=$(mktemp -d "$TEMPLATE")
  array_industry_list=$(mktemp "$TEMPLATE")
  sample_dir_benchmarks "$ARRAY_INDUSTRY_DIR" "$array_industry" "$n_value"
  sample_list_benchmarks "$ARRAY_INDUSTRY_LIST" "$array_industry_list" "$n_value"
  ARRAY_INDUSTRY_DIR="$array_industry"
  ARRAY_INDUSTRY_LIST="$array_industry_list"

  eca_dir=$(mktemp -d "$TEMPLATE")
  eca_list=$(mktemp "$TEMPLATE")
  sample_dir_benchmarks "$ECA_DIR" "$eca_dir" "$n_value"
  sample_list_benchmarks "$ECA_LIST" "$eca_list" "$n_value"
  ECA_DIR="$eca_dir"
  ECA_LIST="$eca_list"
fi

$dry_value && exit 0

opam sw z3-bitwuzla
eval $(opam env)

echo "Running Z3 and bitwuzla ..."
#### array-examples ####
python3 run_benchmarks.py --single --dir $ARRAY_EXAMPLES_DIR \
  --output-dir csvs_single \
  --output-filename array_examples_z3_solver \
  --prover z3
python3 run_benchmarks.py --single --dir $ARRAY_EXAMPLES_DIR \
  --output-dir csvs_single \
  --output-filename array_examples_z3 \
  --prover smtml-z3
python3 run_benchmarks.py --multi -F $ARRAY_EXAMPLES_LIST \
  --output-dir csvs_multi \
  --output-filename array_examples_z3 \
  --prover smtml-z3

#### array-industry-pattern ####
python3 run_benchmarks.py --single --dir $ARRAY_INDUSTRY_DIR \
  --output-dir csvs_single \
  --output-filename array_industry_pattern_z3_solver \
  --prover z3
python3 run_benchmarks.py --single --dir $ARRAY_INDUSTRY_DIR \
  --output-dir csvs_single \
  --output-filename array_industry_pattern_z3 \
  --prover smtml-z3
python3 run_benchmarks.py --multi -F $ARRAY_INDUSTRY_LIST \
  --output-dir csvs_multi \
  --output-filename array_industry_pattern_z3 \
  --prover smtml-z3

#### eca-rers2018 ####
python3 run_benchmarks.py --single --dir $ECA_DIR \
  --output-dir csvs_single \
  --output-filename eca_rers2018_z3_solver \
  --prover z3
python3 run_benchmarks.py --single --dir $ECA_DIR \
  --output-dir csvs_single \
  --output-filename eca_rers2018_z3 \
  --prover smtml-z3
python3 run_benchmarks.py --multi -F $ECA_LIST \
  --output-dir csvs_multi \
  --output-filename eca_rers2018_z3 \
  --prover smtml-z3

## Plots ##
echo "Generating plots ..."
python3 plots.py --testcomp --array-examples \
  --output plot_array_examples_multi \
  --files \
  csvs_multi/array_examples_z3.csv \
  csvs_single/array_examples_z3_solver.csv \
  csvs_single/array_examples_z3.csv

python3 plots.py --testcomp --array-industry-pattern \
  --output plot_array_industry_pattern_multi \
  --files \
  csvs_multi/array_industry_pattern_z3.csv \
  csvs_single/array_industry_pattern_z3_solver.csv \
  csvs_single/array_industry_pattern_z3.csv

python3 plots.py --testcomp --eca-rers2018 \
  --output plot_eca_rers2018_multi \
  --files \
  csvs_multi/eca_rers2018_z3.csv \
  csvs_single/eca_rers2018_z3_solver.csv \
  csvs_single/eca_rers2018_z3.csv
