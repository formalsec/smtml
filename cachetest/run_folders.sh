#!/bin/bash

# Path to the directory containing the main.exe file
PROGRAM_DIR="../_build/default/bin"

# Path to the main queries directory
MAIN_QUERY_DIR="../../queries/testcomp2023"

# Mode parameter
MODE="0"

# Results directory
RESULTS_DIR="results"
mkdir -p "$RESULTS_DIR"

# Define an array of subdirectory names to skip
SKIP_DIRS=("002" "014" "025" "037" "077" "081" "093" "094" "123" "128" "174" "145" "192" "217" "225" "248" "278" "283" "285" "288" "298" "299" "302" "317" "333" "338" "350" "368" "382" "396" "400")

# Loop over each subdirectory under the main queries directory
for QUERY_SUBDIR in "${MAIN_QUERY_DIR}"/*; do
    if [ -d "$QUERY_SUBDIR" ]; then
        # Extract the name of the subdirectory
        QUERY_SUBDIR_NAME=$(basename "$QUERY_SUBDIR")

        # Check if the subdirectory name is in the skip list
        if [[ " ${SKIP_DIRS[@]} " =~ " ${QUERY_SUBDIR_NAME%%_*} " ]]; then
            echo "Skipping folder: $QUERY_SUBDIR_NAME"
            continue
        fi

	# Output file name in the format "NAMEOFTHEFOLDER_MODEOFEXECUTION.out"
        OUTPUT_FILE="${RESULTS_DIR}/${QUERY_SUBDIR_NAME}_${MODE}.out"

        # Print the name of the folder being processed
        echo "Processing folder: $QUERY_SUBDIR_NAME"

        # Execute the OCaml program with time measurement and pass the query path and mode as arguments
        # Redirecting both stdout and stderr to the output file, suppressing console output
        (time dune exec -- "${PROGRAM_DIR}/main.exe" "${QUERY_SUBDIR}" -mode "${MODE}") > "${OUTPUT_FILE}" 2>&1
    fi
done
