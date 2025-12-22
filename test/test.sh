#!/bin/bash
rm -rf outputs
mkdir -p outputs
./run-all-cases.sh
./rm-comments-and-gzip-outputs.sh
./compare.sh outputs outputs-snapshot/24142
