#!/bin/bash
#
PAR=$*

ROOT=../../..

INIT_FILE="UNUSED"
STIM_FILE="${ROOT}/common/lib/tb/tbDpRamSplx_stim.txt"

SRC_LIST="\
common/lib/src/global.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/util/src/busMasterPkg.vhd \
common/util/src/busMasterBhv.vhd \
common/lib/src/cntRtl.vhd \
common/lib/src/dpRamSplx-e.vhd \
altera/lib/src/dpRamSplx-rtl-a.vhd \
common/lib/tb/tbDpRamSplx-bhv-tb.vhd \
"

GEN_LIST="\
gWordWidthA=32 \
gNumberOfWordsA=1024 \
gWordWidthB=32 \
gNumberOfWordsB=1024 \
gInitFile=${INIT_FILE} \
gStimFile=${STIM_FILE} \
"

TOP_LEVEL=tbDpRamSplx

chmod +x $ROOT/common/util/sh/msim-sim.sh
./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL $PAR -s $SRC_LIST -g $GEN_LIST

RET=$?

if test $RET -ne 0
then
    exit 1
fi
