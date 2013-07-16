#!/bin/bash
#
ROOT=../../..
PAR=$*
VHDL_STD="-2008"
OPTIMIZATION="-novopt"
PAR+=" "$OPTIMIZATION" "$VHDL_STD
SRC_LIST="common/lib/src/global.vhd \
common/util/src/resetGenBhv.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/enableGenBhv.vhd \
common/util/src/busMasterPkg.vhd \
common/util/src/busMasterBhv.vhd \
common/lib/src/registerFileRtl.vhd \
common/lib/tb/tbRegisterFileBhv.vhd"

TOP_LEVEL=tbRegisterFile

chmod +x $ROOT/common/util/sh/msim-sim.sh
./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL $PAR -s $SRC_LIST

if test $? -ne 0
then
    exit 1
fi

exit 0
