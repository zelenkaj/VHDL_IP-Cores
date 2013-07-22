#!/bin/bash
#
ROOT=../../..

SRC_LIST="common/lib/src/global.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/lib/src/cntRtl.vhd \
common/lib/src/edgedetectorRtl.vhd \
common/spi/src/protStreamRtl.vhd \
common/spi/tb/tbProtStreamBhv.vhd"

GEN_LIST=( \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=8  gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=16 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=16 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=32 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=8  gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=16 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=16 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=32 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=8  gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=8  gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=16 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=16 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=32 gStreamSkipLoads=0 gStreamSkipValids=0 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=8  gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=16 gStreamSkipLoads=1 gStreamSkipValids=2 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=16 gStreamSkipLoads=1 gStreamSkipValids=2 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=32 gStreamSkipLoads=0 gStreamSkipValids=1 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=8  gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=16 gStreamSkipLoads=1 gStreamSkipValids=2 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=16 gStreamSkipLoads=1 gStreamSkipValids=2 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=32 gStreamSkipLoads=0 gStreamSkipValids=1 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=40  gRdBufBase=16#80# gRdBufSize=100" \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=8  gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=8  gStreamSkipLoads=3 gStreamSkipValids=4 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=16 gStreamSkipLoads=1 gStreamSkipValids=2 gBusDataWidth=16 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=16 gStreamSkipLoads=1 gStreamSkipValids=2 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
"gStreamDataWidth=32 gStreamSkipLoads=0 gStreamSkipValids=1 gBusDataWidth=32 gBusAddrWidth=8 gWrBufBase=16#00# gWrBufSize=100 gRdBufBase=16#80# gRdBufSize=40 " \
)

TOP_LEVEL=tbProtStream

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
