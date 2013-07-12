#!/bin/bash
#
ROOT=../../..

SRC_LIST="common/lib/src/global.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/lib/src/nShiftRegRtl.vhd \
common/lib/src/cntRtl.vhd \
common/lib/src/synchronizerRtl.vhd \
common/lib/src/edgedetectorRtl.vhd \
common/spi/src/spiSlave-e.vhd \
common/spi/src/spiSlave-rtl_aclk-a.vhd \
common/spi/src/spiSlave-rtl_sclk-a.vhd \
common/spi/tb/tbSpiSlaveBhv.vhd"

GEN_LIST=( \
"gRegisterSize=8  gPolarity=0 gPhase=0 gShiftDir=1 gArchSel=0" \
"gRegisterSize=8  gPolarity=0 gPhase=1 gShiftDir=1 gArchSel=0" \
"gRegisterSize=8  gPolarity=1 gPhase=0 gShiftDir=1 gArchSel=0" \
"gRegisterSize=8  gPolarity=1 gPhase=1 gShiftDir=1 gArchSel=0" \
"gRegisterSize=8  gPolarity=0 gPhase=0 gShiftDir=0 gArchSel=0" \
"gRegisterSize=8  gPolarity=0 gPhase=1 gShiftDir=0 gArchSel=0" \
"gRegisterSize=8  gPolarity=1 gPhase=0 gShiftDir=0 gArchSel=0" \
"gRegisterSize=8  gPolarity=1 gPhase=1 gShiftDir=0 gArchSel=0" \
"gRegisterSize=16 gPolarity=0 gPhase=0 gShiftDir=1 gArchSel=0" \
"gRegisterSize=16 gPolarity=0 gPhase=1 gShiftDir=1 gArchSel=0" \
"gRegisterSize=16 gPolarity=1 gPhase=0 gShiftDir=1 gArchSel=0" \
"gRegisterSize=16 gPolarity=1 gPhase=1 gShiftDir=1 gArchSel=0" \
"gRegisterSize=16 gPolarity=0 gPhase=0 gShiftDir=0 gArchSel=0" \
"gRegisterSize=16 gPolarity=0 gPhase=1 gShiftDir=0 gArchSel=0" \
"gRegisterSize=16 gPolarity=1 gPhase=0 gShiftDir=0 gArchSel=0" \
"gRegisterSize=16 gPolarity=1 gPhase=1 gShiftDir=0 gArchSel=0" \
"gRegisterSize=32 gPolarity=0 gPhase=0 gShiftDir=1 gArchSel=0" \
"gRegisterSize=32 gPolarity=0 gPhase=1 gShiftDir=1 gArchSel=0" \
"gRegisterSize=32 gPolarity=1 gPhase=0 gShiftDir=1 gArchSel=0" \
"gRegisterSize=32 gPolarity=1 gPhase=1 gShiftDir=1 gArchSel=0" \
"gRegisterSize=32 gPolarity=0 gPhase=0 gShiftDir=0 gArchSel=0" \
"gRegisterSize=32 gPolarity=0 gPhase=1 gShiftDir=0 gArchSel=0" \
"gRegisterSize=32 gPolarity=1 gPhase=0 gShiftDir=0 gArchSel=0" \
"gRegisterSize=32 gPolarity=1 gPhase=1 gShiftDir=0 gArchSel=0" \
"gRegisterSize=8  gPolarity=0 gPhase=0 gShiftDir=1 gArchSel=1" \
"gRegisterSize=8  gPolarity=0 gPhase=1 gShiftDir=1 gArchSel=1" \
"gRegisterSize=8  gPolarity=1 gPhase=0 gShiftDir=1 gArchSel=1" \
"gRegisterSize=8  gPolarity=1 gPhase=1 gShiftDir=1 gArchSel=1" \
"gRegisterSize=8  gPolarity=0 gPhase=0 gShiftDir=0 gArchSel=1" \
"gRegisterSize=8  gPolarity=0 gPhase=1 gShiftDir=0 gArchSel=1" \
"gRegisterSize=8  gPolarity=1 gPhase=0 gShiftDir=0 gArchSel=1" \
"gRegisterSize=8  gPolarity=1 gPhase=1 gShiftDir=0 gArchSel=1" \
"gRegisterSize=16 gPolarity=0 gPhase=0 gShiftDir=1 gArchSel=1" \
"gRegisterSize=16 gPolarity=0 gPhase=1 gShiftDir=1 gArchSel=1" \
"gRegisterSize=16 gPolarity=1 gPhase=0 gShiftDir=1 gArchSel=1" \
"gRegisterSize=16 gPolarity=1 gPhase=1 gShiftDir=1 gArchSel=1" \
"gRegisterSize=16 gPolarity=0 gPhase=0 gShiftDir=0 gArchSel=1" \
"gRegisterSize=16 gPolarity=0 gPhase=1 gShiftDir=0 gArchSel=1" \
"gRegisterSize=16 gPolarity=1 gPhase=0 gShiftDir=0 gArchSel=1" \
"gRegisterSize=16 gPolarity=1 gPhase=1 gShiftDir=0 gArchSel=1" \
"gRegisterSize=32 gPolarity=0 gPhase=0 gShiftDir=1 gArchSel=1" \
"gRegisterSize=32 gPolarity=0 gPhase=1 gShiftDir=1 gArchSel=1" \
"gRegisterSize=32 gPolarity=1 gPhase=0 gShiftDir=1 gArchSel=1" \
"gRegisterSize=32 gPolarity=1 gPhase=1 gShiftDir=1 gArchSel=1" \
"gRegisterSize=32 gPolarity=0 gPhase=0 gShiftDir=0 gArchSel=1" \
"gRegisterSize=32 gPolarity=0 gPhase=1 gShiftDir=0 gArchSel=1" \
"gRegisterSize=32 gPolarity=1 gPhase=0 gShiftDir=0 gArchSel=1" \
"gRegisterSize=32 gPolarity=1 gPhase=1 gShiftDir=0 gArchSel=1" \
)

TOP_LEVEL=tbSpiSlave

CNT=0
for i in "${GEN_LIST[@]}"
do
    chmod +x $ROOT/common/util/sh/msim-sim.sh
    ./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL -s $SRC_LIST -g $i

    RET=$?

    #add cnt value to output dir
    mv _out_$TOP_LEVEL _out_${TOP_LEVEL}_$CNT

    if test $RET -ne 0
    then
        exit 1
    fi
    CNT=$(( CNT + 1 ))
done

exit 0
