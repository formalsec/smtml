#!/bin/bash

# Path to the directory containing the main.exe file
PROGRAM_DIR="../_build/default/bin"

# Path to the queries
QUERY_DIR="../../queries/collections-c_incremental/array"

# Mode parameter
MODE="d"

# Execute the OCaml program with time measurement and pass the query path and mode as arguments
(time dune exec -- "${PROGRAM_DIR}/main.exe" "${QUERY_DIR}" -mode "${MODE}") 2>&1 | tee output.log
