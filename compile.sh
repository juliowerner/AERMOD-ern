#!/bin/sh

SOURCE_DIR=$(pwd)/src
BIN_DIR=$(pwd)/bin
COMPILE_FLAGS="-fbounds-check -Wuninitialized -O2 -static"
LINK_FLAGS="-O2"

gfortran -c $COMPILE_FLAGS $SOURCE_DIR/modules.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/grsm.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/aermod.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/setup.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/coset.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/soset.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/reset.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/meset.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/ouset.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/inpsum.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/metext.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/iblval.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/siggrid.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/tempgrid.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/windgrid.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/calc1.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/calc2.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/prise.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/arise.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/prime.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/sigmas.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/pitarea.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/uninam.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/output.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/evset.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/evcalc.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/evoutput.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/rline.f
gfortran -c $COMPILE_FLAGS $SOURCE_DIR/bline.f

gfortran -o $BIN_DIR/aermod $LINK_FLAGS modules.o grsm.o aermod.o setup.o coset.o soset.o reset.o meset.o ouset.o inpsum.o metext.o iblval.o siggrid.o tempgrid.o windgrid.o calc1.o calc2.o prise.o arise.o prime.o sigmas.o pitarea.o uninam.o output.o evset.o evcalc.o evoutput.o rline.o bline.o

rm *.o
rm *.mod
