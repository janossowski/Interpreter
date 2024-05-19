#!/bin/bash

# Define directories
dirs=("good" "bad")

# Iterate over directories
for dir in "${dirs[@]}"; do
  # Iterate over .smol files in the directory
  for smol_file in "$dir"/*.smol; do
    # Get the base name of the file (without directory and extension)
    base_name=$(basename "$smol_file" .smol)

    # Define output and error files
    out_file="$dir/$base_name.out"
    err_file="$dir/$base_name.err"

    # Run the interpreter and redirect output and error
    ./interpreter "$smol_file" >"$out_file" 2>"$err_file"

    echo "Generated $out_file and $err_file for $smol_file"
  done
done
