#!/bin/bash

# Find unique prefixes
prefixes=$(ls results/*.out | cut -d_ -f1 | uniq)

# Loop through each prefix and diff the files
for prefix in $prefixes
do
    echo "Diffing files for prefix $prefix"
    diff "${prefix}_0.out" "${prefix}_n.out"
done

