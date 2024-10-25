# Smt.ml: A Multi-Backend Frontend for SMT Solvers in OCaml (Artifact)

This artifact contains the implementation of Smt.ml, an SMT solver frontend
for OCaml, along with scripts to reproduce the experimental results
discussed in the associated tool paper.
Smt.ml was thoroughly evaluated: (1) it was applied to a subset of the SMT-COMP
benchmark dataset, demonstrating consistency with results obtained directly
from the supported solvers, and (2) it shows comparable performance to direct
solver usage, with notable improvements for batched solver interactions.

## Getting the Artifact

You can download the artifact through [Zenodo](https://zenodo.org/records/13959671/files/smtml_0.3.tar.gz?download=1).

## Tested platforms

The artifact was developed, tested, and packaged using Ubuntu 24.04 LTS.
The recommended VM hardware requirements are:

- 32GiB of RAM
- 100GiB of disk space
- CPU >= Intel(R) Xeon(R) CPU E5-2620 v4 @ 2.10 GHz
- Cores >= 8

## License

Smt.ml is distributed under the MIT License, allowing for open-source use,
modification, and distribution.

## MD5 Hash of the Artifact

8edce799971395168bdf728c9a136399

## Building and Running the Docker Image

Load `smtml`'s docker image:

```sh
docker load < smtml_0.3.tar.gz
```

Run the Docker image and check the help page of smtml:

```sh
$ docker run -it --rm smtml:0.3
smtml@1ed0a190caec:~/smtml$ eval $(opam env) && smtml --help
```

## Early Light Review

To verify that the artifact can properly run we must ensure that
Smt.ml is properly installed, as well as all associated solvers.

#### Testing an Individual File

To verify the proper installation of solvers first create the following file:

```sh
$ cat <<EOF > test.smt2
(set-logic ALL)
(declare-const x (_ BitVec 32))
(assert (= x (_ bv0 32)))
(check-sat)
EOF
```

Then run the commands:

```sh
$ opam sw z3-bitwuzla && eval $(opam env) \
    && smtml run --solver z3 test.smt2 \
    && smtml run --solver bitwuzla test.smt2 \
    && z3 test.smt2 \
    && bitwuzla test.smt2
$ opam sw colibri2 && eval $(opam env) \
    && smtml run --solver colibri2 test.smt2 \
    && colibri2 test.smt2
$ opam sw cvc5 && eval $(opam env) \
    && smtml run --solver cvc5 test.smt2 \
    && cvc5 test.smt2
```

The expected output should be:

```sh
# Run eval $(opam env) to update the current shell environment
sat
sat
sat
sat
# Run eval $(opam env) to update the current shell environment
sat
sat
# Run eval $(opam env) to update the current shell environment
sat
sat
```

#### Testing with Benchpress

To verify the proper installation of the benchmarking tools run the
following commands one by one:

```sh
$ cd bench
$ git submodule update --init collections-c
$ opam sw z3-bitwuzla && eval $(opam env) \
    && benchpress run -c benchpress.sexp --task early-light-review -p z3 -p smtml-z3 -p bitwuzla -p smtml-bitwuzla
$ opam sw colibri2 && eval $(opam env) \
    && benchpress run -c benchpress.sexp --task early-light-review -p colibri2 -p smtml-colibri2
$ opam sw cvc5 && eval $(opam env) \
    && benchpress run -c benchpress.sexp --task early-light-review -p cvc5 -p smtml-cvc5
```

After starting each command, you should see `benchpress` output
benchmark results to the standard output. Each command should take
around 30 seconds to execute, and at the end, you should see a
summary similar to the one shown below:

```sh
STAT:
provers      │z3  │smtml-z3│smtml-bitwuzla│bitwuzla
─────────────┼────┼────────┼──────────────┼────────
sat          │138 │138     │138           │138
─────────────┼────┼────────┼──────────────┼────────
unsat        │118 │118     │118           │118
─────────────┼────┼────────┼──────────────┼────────
sat+unsat    │256 │256     │256           │256
─────────────┼────┼────────┼──────────────┼────────
errors       │0   │0       │0             │0
─────────────┼────┼────────┼──────────────┼────────
valid_proof  │0   │0       │0             │0
─────────────┼────┼────────┼──────────────┼────────
invalid_proof│0   │0       │0             │0
─────────────┼────┼────────┼──────────────┼────────
unknown      │0   │0       │0             │0
─────────────┼────┼────────┼──────────────┼────────
timeout      │0   │0       │0             │0
─────────────┼────┼────────┼──────────────┼────────
memory       │0   │0       │0             │0
─────────────┼────┼────────┼──────────────┼────────
total        │256 │256     │256           │256
─────────────┼────┼────────┼──────────────┼────────
total_time   │4.6s│5.6s    │4.3s          │3.2s
ANALYSIS:
provers      │z3 │smtml-z3│smtml-bitwuzla│bitwuzla
─────────────┼───┼────────┼──────────────┼────────
improved     │256│256     │256           │256
─────────────┼───┼────────┼──────────────┼────────
ok           │0  │0       │0             │0
─────────────┼───┼────────┼──────────────┼────────
disappoint   │0  │0       │0             │0
─────────────┼───┼────────┼──────────────┼────────
bad          │0  │0       │0             │0
─────────────┼───┼────────┼──────────────┼────────
valid proof  │0  │0       │0             │0
─────────────┼───┼────────┼──────────────┼────────
invalid proof│0  │0       │0             │0
─────────────┼───┼────────┼──────────────┼────────
errors       │0  │0       │0             │0
─────────────┼───┼────────┼──────────────┼────────
total        │256│256     │256           │256
```

Importantly, between a `<solver_name>` column and the respective
`smtml-<solver_name>`, the number of benchmarks in `sat` and `unsat`
should be equal, signaling the correct classification of each
benchmark's satisfiability status by `smtml-<solver_name>`.
This parity indicates that `smtml-<solver_name>` accurately mirrors
the original solver’s ability to determine satisfiability, thereby
validating the consistency of `smtml-<solver_name>` in reflecting
the solver’s intended outputs.

## Running the Evaluation Benchmarks

Once running the Docker image initialise the benchmarking submodules

```sh
$ git submodule init
```

### EQ1

To run and validate the claims of EQ1, perform the following steps:

1. Clone the benchmark submodule

```sh
$ cd bench
$ git submodule update --remote -- smt-comp/
```

2. Run the `eq1.sh` script. Note that, this script can take up multiple
hours to complete.

```sh
$ ./eq1.sh
```

The script will generate the results in directory `eq1`, which
has the following structure:

```
eq1
├── colibri2
│   ├── qf_bv.out
│   ├── qf_fp.out
│   └── qf_lia.out
├── cvc5
│   ├── qf_bv.out
│   ├── qf_fp.out
│   ├── qf_lia.out
│   ├── qf_slia.out
│   └── qf_s.out
└── z3-bitwuzla
    ├── qf_bv.out
    ├── qf_fp.out
    ├── qf_lia.out
    ├── qf_slia.out
    └── qf_s.out
```

Similarly to [[#Early Light Review]], you can validate the results by
examining each `*.out` file and ensuring that the number of benchmarks
in `sat` and `unsat` in the `<solver_name>` column matches that in
the corresponding `smtml-<solver_name>` column.

### EQ2

To run and generate the plots for EQ2, perform the following steps:

1. Clone the benchmark submodule (if you already did this for EQ1, skip this step)

```sh
$ cd bench
$ git submodule update --remote -- smt-comp/
```

2. Run the `eq2.sh` script. Note that, this script can take up multiple
hours to complete.

```sh
$ ./eq2.sh
```

The script will generate the following files:

```
.
├── csvs_single
│   ├── QF_FP_all.csv
│   ├── QF_LIA_all.csv
│   ├── QF_BV_all.csv
│   ├── QF_SLIA_all.csv
│   └── QF_S_all.csv
├── plot_QF_FP.pdf
├── plot_QF_LIA.pdf
├── plot_QF_BV.pdf
├── plot_QF_SLIA.pdf
└── plot_QF_S.pdf
```

The files `plot_*.pdf` correspond to the individual plots of Figure 5 from
Section 5.3 of the paper.

### EQ3

To run and generate the plots for EQ3, perform the following steps:

1. Clone the benchmark submodule (if you already did this for EQ1 or EQ2, skip this step)

```sh
$ cd bench
$ git submodule update --remote -- smt-comp/
```

2. Run the `eq3.sh` script. Note that, this script can take up multiple
hours to complete.

```sh
$ ./eq3.sh
```

The script will generate the following files:

```
.
├── csvs_multi
│   ├── QF_FP_z3.csv
│   ├── QF_LIA_z3.csv
│   ├── QF_BV_z3.csv
│   ├── QF_SLIA_z3.csv
│   ├── QF_S_z3.csv
│   ├── QF_FP_bitwuzla.csv
│   └── QF_BV_bitwuzla.csv
├── plot_QF_FP_multi.pdf
├── plot_QF_LIA_multi.pdf
├── plot_QF_BV_multi.pdf
├── plot_QF_SLIA_multi.pdf
└── plot_QF_S_multi.pdf
```

The files `plot_*.pdf` correspond to the individual plots of Figure 6 from
Section 5.4 of the paper.

### Case Study

To run and generate the plots for the symbolic execution case study, perform the following steps:

1. Clone the benchmark submodule

```sh
$ cd bench
$ git submodule update --remote -- smt-testcomp23/
```

2. Run the `testcomp.sh` script. Note that, this script can take up multiple
hours to complete.

```sh
$ ./testcomp.sh
```

The script will generate the following files:

```
.
├── csvs_single
│   ├── array_examples_z3_solver.csv
│   ├── array_examples_z3.csv
│   ├── array_industry_pattern_z3_solver.csv
│   ├── array_industry_pattern_z3.csv
│   ├── eca_rers2018_z3_solver.csv
│   └── eca_rers2018_z3.csv
├── csvs_multi
│   ├── array_examples_z3.csv
│   ├── array_industry_pattern_z3.csv
│   └── eca_rers2018_z3.csv
├── plot_array_examples_multi.pdf
├── plot_array_industry_pattern_multi.pdf
└── plot_eca_rers2018_multi.pdf
```

The files `plot_*.pdf` correspond to the individual plots of Figure 7 from
Section 6 of the paper.
