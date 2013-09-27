#!/bin/bash
# Runs modelsim to compile and simulate provided sources and toplevel.
# Call e.g. ./msim-sim.sh $TOP_LEVEL -s $SRC_LIST -g $GEN_LIST

ROOT=../../..
DOFILE=$ROOT/common/util/do/sim.do
TOP_LEVEL=$1
VHDL_STD="-93"
READ_MODE=
OPTIMIZATION=
NO_RUN=
SRC_LIST=
GEN_LIST=
OUT_DIR=_out_$TOP_LEVEL

echo
echo "#### $TOP_LEVEL ####"

rm work -rf
vlib work

for i in $*
do
    if [ "$i" == "--no-run" ]; then
        READ_MODE=
        NO_RUN=1
    elif [ "$i" == "-87" ]; then
        VHDL_STD="$i"
        READ_MODE=
    elif [ "$i" == "-93" ]; then
        VHDL_STD="$i"
        READ_MODE=
    elif [ "$i" == "-2002" ]; then
        VHDL_STD="$i"
        READ_MODE=
    elif [ "$i" == "-2008" ]; then
        VHDL_STD="$i"
        READ_MODE=
    elif [ "$i" == "-novopt" ]; then
        OPTIMIZATION="$i"
        READ_MODE=
    elif [ "$i" == "-s" ]; then
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
vcom $VHDL_STD -work work $SRC_LIST
if test $? -ne 0
then
    exit 1
fi

#exit if --no-run
if [ -n "$NO_RUN" ]; then
    exit 0
fi

#simulate design
vsim $OPTIMIZATION $TOP_LEVEL -c -do $DOFILE -lib work $GEN_LIST

#catch simulation return
RET=$?

#create output dir
mkdir $OUT_DIR -p
#copy work into
cp work $OUT_DIR/work -r
#copy waves and transcript into
cp transcript $OUT_DIR -r
cp *.wlf $OUT_DIR -r
#set mode of copied files
chmod u+rw $OUT_DIR/* -R

echo
if [ $RET -ne 0 ]; then
    echo "ERROR"
else
    echo "PASS"
fi

#exit with simulation return
exit $RET
