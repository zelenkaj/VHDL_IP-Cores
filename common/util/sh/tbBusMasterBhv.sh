#!/bin/bash
#
ROOT=../../..

PAR=$*
VHDL_STD="-2008"
PAR+=$VHDL_STD
SRC_LIST="common/lib/src/global.vhd \
common/util/src/busMasterPkg.vhd \
common/util/src/busMasterBhv.vhd \
common/util/tb/tbBusMasterBhv.vhd"

TOP_LEVEL=tbBusMasterBhv

chmod +x $ROOT/common/util/sh/msim-sim.sh
./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL $PAR -s $SRC_LIST

if test $? -ne 0
then
    exit 1
fi

exit 0
