#!/bin/bash
#
ROOT=../../..
PAR=$*
VHDL_STD="-2008"
OPTIMIZATION=
PAR+=" "$OPTIMIZATION" "$VHDL_STD

SRC_LIST="\
common/lib/src/global.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/util/src/busMasterPkg.vhd \
common/util/src/busMasterBhv.vhd \
common/util/src/spRamBhv.vhd \
common/lib/src/dpRam-e.vhd \
common/util/src/dpRam-bhv-a.vhd \
common/lib/src/addrDecodeRtl.vhd \
common/lib/src/binaryEncoderRtl.vhd \
common/lib/src/cntRtl.vhd \
common/lib/src/edgedetectorRtl.vhd \
common/lib/src/lutFileRtl.vhd \
common/lib/src/synchronizerRtl.vhd \
common/lib/src/registerFileRtl.vhd \
common/hostinterface/src/hostInterfacePkg.vhd \
common/hostinterface/src/dynamicBridgeRtl.vhd \
common/hostinterface/src/irqGenRtl.vhd \
common/hostinterface/src/statusControlRegRtl.vhd \
common/hostinterface/src/hostInterfaceRtl.vhd \
common/hostinterface/tb/tbHostInterfaceBhv.vhd \
"

GEN_LIST=( \
"\
gUseMemBlock=0 \
gPcpStim=${ROOT}/common/hostinterface/tb/tbHostInterface_pcp_stim.txt \
gHostStim=${ROOT}/common/hostinterface/tb/tbHostInterface_ap_stim.txt \
" \
"\
gUseMemBlock=1 \
gPcpStim=${ROOT}/common/hostinterface/tb/tbHostInterface_pcp_stim.txt \
gHostStim=${ROOT}/common/hostinterface/tb/tbHostInterface_ap_stim.txt \
" \
)

TOP_LEVEL=tbHostInterface

CNT=0
for i in "${GEN_LIST[@]}"
do
    chmod +x $ROOT/common/util/sh/msim-sim.sh
    ./$ROOT/common/util/sh/msim-sim.sh $TOP_LEVEL $PAR -s $SRC_LIST -g $i

    RET=$?

    #add cnt value to output dir
    mv _out_$TOP_LEVEL _out_${TOP_LEVEL}_$CNT

    if test $RET -ne 0
    then
        exit 1
    fi
    CNT=$(( CNT + 1 ))
done
