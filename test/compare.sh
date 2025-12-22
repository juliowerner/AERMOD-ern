#!/bin/bash
# This script compares all the files in the first folder on the another
#
# Usage: diff.sh folder1 folder2
#
# Example: diff.sh /path/to/folder1 /path/to/folder2
FOLDER1=$1
FOLDER2=$2
if [ -z $FOLDER1 ]; then
    FOLDER1="outputs"
fi
if [ -z $FOLDER2 ]; then
    FOLDER2="outputs-snapshot/24142"
fi
echo "Comparing files in $FOLDER1 and $FOLDER2"
n=0
for file1 in "$FOLDER1"/*; do
    filename=$(basename "$file1")
    file2="$FOLDER2/$filename"
    if [ -f $file2 ]; then
        zdiff "$file1"  "$file2"> /dev/null
        if [ $? -ne 0 ]; then
            echo "Files $filename are different"
            n=$((n+1))
        fi
    else
        echo "File $filename is missing in $FOLDER2"
    fi
done

if [ $n -eq 0 ]; then
    echo "All files are identical"
else
    echo "$n files are different"
fi
