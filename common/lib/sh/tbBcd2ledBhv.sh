#!/bin/bash
#
PAR=$@

ROOT=../../..

SRC_LIST="common/lib/src/global.vhd \
common/lib/src/bcd2ledRtl.vhd \
common/lib/tb/tbBcd2ledBhv.vhd"

TOP_LEVEL=tbBcd2led

chmod +x $ROOT/common/util/sh/msim-sim.sh
./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL $PAR -s $SRC_LIST

RET=$?

if test $RET -ne 0
then
    exit 1
fi

exit 0
