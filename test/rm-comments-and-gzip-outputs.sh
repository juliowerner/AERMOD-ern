#!/bin/bash
# This script removes comment lines (starting with '*') from all plot files

EXT=("*.PST" "*.PLT" "*.FIL" "*.DAT")
for name in ${EXT[@]}; do
    echo "Processing files with extension: $name"
    find outputs -type f -iname "$name" | while read -r file; do
        echo "Processing file: $file"
        # Create a temporary file to store the cleaned content
        temp_file=$(mktemp)

        # Remove comment lines and write to the temporary file
        grep -v '^\*' "$file" > "$temp_file"

        # Replace the original file with the cleaned content
        mv "$temp_file" "$file"
        gzip -9 "$file"
    done
done

