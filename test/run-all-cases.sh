#!/bin/bash
BIN_PATH="../../bin"
cd cases
if [ -n $1 ]; then
    BIN="aermod"
else
    BIN=$1
fi

inp_files=$(ls *.inp)
for file in $inp_files; do
    echo "Running AERMOD for input file: $file"
    $BIN_PATH/$BIN $file
    if [ $? -ne 0 ]; then
        echo "AERMOD failed for input file: $file"
        exit
    fi
done
rm -f *.TMP
rm -f *.out
rm -f *.OUT
rm -f ../outputs/*.err
rm -f ../outputs/*.OUT
rm -f ../outputs/*.out
rm -f ../outputs/*.DBG*
rm -f ../outputs/*.SUM
rm -f ../outputs/*.sum


