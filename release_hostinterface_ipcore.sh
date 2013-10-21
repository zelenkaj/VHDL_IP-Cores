#!/bin/bash
# ./release_tripleBuffer_ipcore.sh RELEASE_DIR
# e.g.: $ ./release_hostinterface_ipcore release

DIR_RELEASE=$1

if [ -z "${DIR_RELEASE}" ];
then
    DIR_RELEASE=release
fi

# create dir structure
echo "create dir structure..."
mkdir -p ${DIR_RELEASE}/altera/components
mkdir -p ${DIR_RELEASE}/altera/components/img
mkdir -p ${DIR_RELEASE}/altera/components/sdc
mkdir -p ${DIR_RELEASE}/altera/components/tcl
mkdir -p ${DIR_RELEASE}/altera/hostinterface/src
mkdir -p ${DIR_RELEASE}/altera/lib/src
mkdir -p ${DIR_RELEASE}/common/hostinterface/src
mkdir -p ${DIR_RELEASE}/common/lib/src

# copy docs
#echo "copy docs..."
#

# copy Altera host interface
echo "copy altera powerlink ipcore..."
cp altera/components/hostinterface_hw.tcl                   ${DIR_RELEASE}/altera/components
cp altera/components/hostinterface_sw.tcl                   ${DIR_RELEASE}/altera/components
cp altera/components/img/br.png                             ${DIR_RELEASE}/altera/components/img
cp altera/components/sdc/hostinterface-async.sdc            ${DIR_RELEASE}/altera/components/sdc
cp altera/components/tcl/hostinterface.tcl                  ${DIR_RELEASE}/altera/components/tcl
cp altera/lib/src/dpRam-rtl-a.vhd                           ${DIR_RELEASE}/altera/lib/src
cp common/lib/src/dpRam-e.vhd                               ${DIR_RELEASE}/common/lib/src
cp common/lib/src/addrDecodeRtl.vhd                         ${DIR_RELEASE}/common/lib/src
cp common/lib/src/binaryEncoderRtl.vhd                      ${DIR_RELEASE}/common/lib/src
cp common/lib/src/cntRtl.vhd                                ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd                       ${DIR_RELEASE}/common/lib/src
cp common/lib/src/lutFileRtl.vhd                            ${DIR_RELEASE}/common/lib/src
cp common/lib/src/registerFileRtl.vhd                       ${DIR_RELEASE}/common/lib/src
cp common/lib/src/synchronizerRtl.vhd                       ${DIR_RELEASE}/common/lib/src
cp altera/hostinterface/src/alteraHostInterfaceRtl.vhd      ${DIR_RELEASE}/altera/hostinterface/src
cp common/hostinterface/revision.txt                        ${DIR_RELEASE}/common/hostinterface
cp common/hostinterface/src/hostInterfacePkg.vhd            ${DIR_RELEASE}/common/hostinterface/src
cp common/hostinterface/src/hostInterfaceRtl.vhd            ${DIR_RELEASE}/common/hostinterface/src
cp common/hostinterface/src/irqGenRtl.vhd                   ${DIR_RELEASE}/common/hostinterface/src
cp common/hostinterface/src/dynamicBridgeRtl.vhd            ${DIR_RELEASE}/common/hostinterface/src
cp common/hostinterface/src/statusControlRegRtl.vhd         ${DIR_RELEASE}/common/hostinterface/src
cp common/hostinterface/src/parallelInterfaceRtl.vhd        ${DIR_RELEASE}/common/hostinterface/src
cp common/lib/src/global.vhd                                ${DIR_RELEASE}/common/lib/src
