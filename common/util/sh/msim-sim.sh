#!/bin/bash
# Runs modelsim to compile and simulate provided sources and toplevel.
# Call e.g. ./msim-sim.sh $TOP_LEVEL -s $SRC_LIST -g $GEN_LIST

ROOT=../../..
DOFILE=$ROOT/common/util/do/sim.do
TOP_LEVEL=$1
READ_MODE=
SRC_LIST=
GEN_LIST=
OUT_DIR=_out_$TOP_LEVEL

echo
echo "#### $TOP_LEVEL ####"

rm work -rf
vlib work

for i in $*
do
    if [ "$i" == "-s" ]; then
        READ_MODE="SRC"
    elif [ "$i" == "-g" ]; then
        READ_MODE="GEN"
    elif [ "$READ_MODE" == "SRC" ]; then
        SRC_LIST+="$ROOT/$i "
    elif [ "$READ_MODE" == "GEN" ]; then
        GEN_LIST+="-g$i "
    fi
done

#compile source files
vcom -work work $SRC_LIST
if test $? -ne 0
then
    exit 1
fi

#simulate design
vsim $TOP_LEVEL -c -do $DOFILE -lib work $GEN_LIST

#catch simulation return
RET=$?

#create output dir
mkdir $OUT_DIR -p
#copy work into
cp work $OUT_DIR/work -r
#copy waves and transcript into
cp transcript $OUT_DIR -r
cp *.wlf $OUT_DIR -r

#translate wlf into vcd
wlf2vcd -o $OUT_DIR/wave.vcd $OUT_DIR/*.wlf

#exit with simulation return
exit $RET
