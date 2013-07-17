#!/bin/bash
#
ROOT=../../..

SRC_LIST="common/lib/src/global.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/tripleBuffer/src/tripleBufferPkg.vhd \
common/tripleBuffer/src/tripleLogicRtl.vhd \
common/tripleBuffer/tb/tbTripleLogicBhv.vhd"

TOP_LEVEL=tbTripleLogic

chmod +x $ROOT/common/util/sh/msim-sim.sh
./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL -s $SRC_LIST

if test $? -ne 0
then
    exit 1
fi

exit 0
