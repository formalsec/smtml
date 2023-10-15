#!/bin/bash

# Path to the directory containing the main.exe file
PROGRAM_DIR="../_build/default/bin"

# Path to the main queries directory
MAIN_QUERY_DIR="../../queries/collections-c_batch"

# Mode parameter
MODE="0"

# Results directory
RESULTS_DIR="results"
mkdir -p "$RESULTS_DIR"

# Loop over each subdirectory under the main queries directory
for QUERY_SUBDIR in "${MAIN_QUERY_DIR}"/*; do
    if [ -d "$QUERY_SUBDIR" ]; then
        # Extract the name of the subdirectory
        QUERY_SUBDIR_NAME=$(basename "$QUERY_SUBDIR")
        
        # Output file name in the format "NAMEOFTHEFOLDER_MODEOFEXECUTION.out"
        OUTPUT_FILE="${RESULTS_DIR}/${QUERY_SUBDIR_NAME}_${MODE}.out"
        
        # Execute the OCaml program with time measurement and pass the query path and mode as arguments
        # Redirecting both stdout and stderr to the output file, suppressing console output
        (time dune exec -- "${PROGRAM_DIR}/main.exe" "${QUERY_SUBDIR}" -mode "${MODE}") > "${OUTPUT_FILE}" 2>&1
    fi
done
