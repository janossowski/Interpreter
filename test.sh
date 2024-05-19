#!/bin/bash

# Define directories
dirs=("good" "bad")

# Iterate over directories
for dir in "${dirs[@]}"; do
  # Iterate over .smol files in the directory
  for smol_file in "$dir"/*.smol; do
    # Get the base name of the file (without directory and extension)
    base_name=$(basename "$smol_file" .smol)

    # Define expected output and error files
    expected_out="$dir/$base_name.out"
    expected_err="$dir/$base_name.err"

    # Define actual output and error files
    actual_out="$dir/$base_name.actual_out"
    actual_err="$dir/$base_name.actual_err"

    # Run the interpreter and redirect output and error
    ./interpreter "$smol_file" >"$actual_out" 2>"$actual_err"

    # Compare actual output with expected output
    if diff -q "$expected_out" "$actual_out" > /dev/null; then
      echo "Output for $smol_file matches expected output."
    else
      echo "Output for $smol_file does NOT match expected output."
      echo "Differences in output:"
      diff "$expected_out" "$actual_out"
    fi

    # Compare actual error with expected error
    if diff -q "$expected_err" "$actual_err" > /dev/null; then
      echo "Error for $smol_file matches expected error."
    else
      echo "Error for $smol_file does NOT match expected error."
      echo "Differences in error:"
      diff "$expected_err" "$actual_err"
    fi

    # Clean up actual output and error files
    rm "$actual_out" "$actual_err"
  done
done
