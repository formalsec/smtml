#!/bin/sh

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

mkdir eq1

opam sw z3-bitwuzla
eval $(opam env)

mkdir eq1/z3-bitwuzla
echo "Running Z3 and bitwuzla ..."
benchpress run -c benchpress.sexp --task eq1 $QF_BV_DIR -p z3 -p smtml-z3 -p bitwuzla -p smtml-bitwuzla > eq1/z3-bitwuzla/qf_bv.out
benchpress run -c benchpress.sexp --task eq1 $QF_FP_DIR -p z3 -p smtml-z3 -p bitwuzla -p smtml-bitwuzla > eq1/z3-bitwuzla/qf_fp.out
benchpress run -c benchpress.sexp --task eq1 $QF_LIA_DIR -p z3 -p smtml-z3 > eq1/z3-bitwuzla/qf_lia.out
benchpress run -c benchpress.sexp --task eq1 $QF_SLIA_DIR -p z3 -p smtml-z3 > eq1/z3-bitwuzla/qf_slia.out
benchpress run -c benchpress.sexp --task eq1 $QF_S_DIR -p z3 -p smtml-z3 > eq1/z3-bitwuzla/qf_s.out

opam sw cvc5
eval $(opam env)

mkdir eq1/cvc5
echo "Running cvc5 ..."
benchpress run -c benchpress.sexp --task eq1 $QF_BV_DIR -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_bv.out
benchpress run -c benchpress.sexp --task eq1 $QF_FP_DIR -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_fp.out
benchpress run -c benchpress.sexp --task eq1 $QF_LIA_DIR -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_lia.out
benchpress run -c benchpress.sexp --task eq1 $QF_SLIA_DIR -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_slia.out
benchpress run -c benchpress.sexp --task eq1 $QF_S_DIR -p cvc5 -p smtml-cvc5 > eq1/cvc5/qf_s.out

opam sw colibri2
eval $(opam env)

mkdir eq1/colibri2
echo "Running Colibri2 ..."
benchpress run -c benchpress.sexp --task eq1 $QF_BV_DIR -p colibri2 -p smtml-colibri2 > eq1/colibri2/qf_bv.out
benchpress run -c benchpress.sexp --task eq1 $QF_FP_DIR -p colibri2 -p smtml-colibri2 > eq1/colibri2/qf_fp.out
benchpress run -c benchpress.sexp --task eq1 $QF_LIA_DIR -p colibri2 -p smtml-colibri2 > eq1/colibri2/qf_lia.out
