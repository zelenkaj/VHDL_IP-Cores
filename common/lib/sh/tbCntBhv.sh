#!/bin/bash
#
ROOT=../../..

SRC_LIST="common/lib/src/global.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/lib/src/cntRtl.vhd \
common/lib/tb/tbCntBhv.vhd"

GEN_LIST=( \
"gCntWidth=8 gTcntVal=123" \
"gCntWidth=8 gTcntVal=255" \
"gCntWidth=16 gTcntVal=1234" \
"gCntWidth=32 gTcntVal=1954" \
"gCntWidth=64 gTcntVal=21546" \
)

TOP_LEVEL=tbCnt

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
