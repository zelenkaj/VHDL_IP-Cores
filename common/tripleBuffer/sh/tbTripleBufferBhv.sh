#!/bin/bash
#
PAR=$*

ROOT=../../..

SRC_LIST="common/lib/src/global.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/lib/src/dpRam-e.vhd \
common/util/src/dpRam-bhv-a.vhd \
common/lib/src/addrDecodeRtl.vhd \
common/lib/src/binaryEncoderRtl.vhd \
common/lib/src/edgedetectorRtl.vhd \
common/tripleBuffer/src/tripleBufferPkg.vhd \
common/tripleBuffer/src/tripleBridgeRtl.vhd \
common/tripleBuffer/src/tripleLogicRtl.vhd \
common/tripleBuffer/src/tripleBufferRtl.vhd \
common/tripleBuffer/tb/tbTripleBufferBhv.vhd"

TOP_LEVEL=tbTripleBuffer

chmod +x $ROOT/common/util/sh/msim-sim.sh
./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL -s $SRC_LIST $PAR

if test $? -ne 0
then
    exit 1
fi

exit 0
