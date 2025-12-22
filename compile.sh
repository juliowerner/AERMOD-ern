#!/bin/sh

SOURCE_DIR=$(pwd)/src
BIN_DIR=$(pwd)/bin
COMPILE_FLAGS="-fbounds-check -Wuninitialized -O2 -static"
LINK_FLAGS="-O2"

# Compile all Fortran source files in the source directory
for src in "$SOURCE_DIR"/*.f90; do
    gfortran -c $COMPILE_FLAGS "$src"
done

# Ensure the binary output directory exists
mkdir -p "$BIN_DIR"

# Link all object files into the final executable
gfortran -o "$BIN_DIR/aermod" $LINK_FLAGS ./*.o
rm *.o
rm *.mod
