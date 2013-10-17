#!/bin/bash
#
PAR=$*

ROOT=../../..

INIT_FILE="UNUSED"
STIM_FILE="${ROOT}/common/lib/tb/tbDpRam_stim.txt"

SRC_LIST="\
common/lib/src/global.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/util/src/busMasterPkg.vhd \
common/util/src/busMasterBhv.vhd \
common/lib/src/cntRtl.vhd \
common/lib/src/dpRam-e.vhd \
altera/lib/src/dpRam-rtl-a.vhd \
common/lib/tb/tbDpRam-bhv-tb.vhd \
"

GEN_LIST="\
gWordWidth=32 \
gNumberOfWords=1024 \
gInitFile=${INIT_FILE} \
gStimFile=${STIM_FILE} \
"

TOP_LEVEL=tbDpRam

chmod +x $ROOT/common/util/sh/msim-sim.sh
./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL $PAR -s $SRC_LIST -g $GEN_LIST

RET=$?

if test $RET -ne 0
then
    exit 1
fi
