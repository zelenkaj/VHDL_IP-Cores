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

GEN_LIST=\
"gInputBuffers=10 \
gInputBase=x\"E8C4A08C807C58341C1004\" \
gTriBufOffset=x\"1C81A418018015C1381381241101101040F80F80F40F00F00CC0A80A808406006004803003002401801800C000\" \
gDprSize=684 \
gPortAconfig=\"1111100000\" \
gPortAstream=1"

TOP_LEVEL=tbTripleBuffer

chmod +x $ROOT/common/util/sh/msim-sim.sh
./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL $PAR -s $SRC_LIST $PAR -g $GEN_LIST

if test $? -ne 0
then
    exit 1
fi

exit 0
