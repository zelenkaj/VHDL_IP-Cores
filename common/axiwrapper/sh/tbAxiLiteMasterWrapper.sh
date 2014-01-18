#!/bin/bash
#
ROOT=../../..
PAR=$*
VHDL_STD="-2008"
OPTIMIZATION=
PAR+=" "$OPTIMIZATION" "$VHDL_STD

SRC_LIST="\
common/lib/src/global.vhd \
common/util/src/spRamBhv.vhd \
common/util/src/clkGenBhv.vhd \
common/util/src/resetGenBhv.vhd \
common/util/src/busMasterPkg.vhd \
common/util/src/busMasterBhv.vhd \
common/axiwrapper/src/axiLiteMasterWrapper-rtl-ea.vhd \
common/axiwrapper/src/axiLiteSlaveWrapper-rtl-ea.vhd \
common/axiwrapper/tb/tbAxiLiteMasterWrapper-bhv-ea.vhd \
"

GEN_LIST=( \
"\
gMasterStim=${ROOT}/common/axiwrapper/tb/tbMasterBhv_TB_stim.txt \
" \
)

TOP_LEVEL=tbAxiLiteMasterWrapper

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
