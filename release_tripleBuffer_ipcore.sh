#!/bin/bash
# ./release_tripleBuffer_ipcore.sh RELEASE_DIR
# e.g.: $ ./release_tripleBuffer_ipcore release

DIR_RELEASE=$1

if [ -z "${DIR_RELEASE}" ];
then
    DIR_RELEASE=release
fi

# create dir structure
echo "create dir structure..."
mkdir -p ${DIR_RELEASE}/altera/components
mkdir -p ${DIR_RELEASE}/altera/components/tcl
mkdir -p ${DIR_RELEASE}/altera/tripleBuffer/src
mkdir -p ${DIR_RELEASE}/altera/lib/src
mkdir -p ${DIR_RELEASE}/common/tripleBuffer/src
mkdir -p ${DIR_RELEASE}/common/tripleBuffer/tcl
mkdir -p ${DIR_RELEASE}/common/lib/src
mkdir -p ${DIR_RELEASE}/common/util/tcl

# copy Altera
echo "copy altera ipcore..."
cp altera/components/tripleBuffer_hw.tcl                ${DIR_RELEASE}/altera/components
cp altera/components/tripleBuffer_sw.tcl                ${DIR_RELEASE}/altera/components
cp altera/components/tcl/tripleBuffer.tcl               ${DIR_RELEASE}/altera/components/tcl
cp altera/tripleBuffer/src/alteraTripleBufferRtl.vhd    ${DIR_RELEASE}/altera/tripleBuffer/src
cp altera/lib/src/dpRam-rtl-a.vhd                       ${DIR_RELEASE}/altera/lib/src
cp common/lib/src/global.vhd                            ${DIR_RELEASE}/common/lib/src
cp common/lib/src/dpRam-e.vhd                           ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd                   ${DIR_RELEASE}/common/lib/src
cp common/lib/src/addrDecodeRtl.vhd                     ${DIR_RELEASE}/common/lib/src
cp common/lib/src/binaryEncoderRtl.vhd                  ${DIR_RELEASE}/common/lib/src
cp common/tripleBuffer/tcl/calcTriBuf.tcl               ${DIR_RELEASE}/common/tripleBuffer/tcl
cp common/tripleBuffer/src/tripleBufferPkg.vhd          ${DIR_RELEASE}/common/tripleBuffer/src
cp common/tripleBuffer/src/tripleBridgeRtl.vhd          ${DIR_RELEASE}/common/tripleBuffer/src
cp common/tripleBuffer/src/tripleLogicRtl.vhd           ${DIR_RELEASE}/common/tripleBuffer/src
cp common/tripleBuffer/src/tripleBufferRtl.vhd          ${DIR_RELEASE}/common/tripleBuffer/src
cp common/util/tcl/writeFile.tcl                        ${DIR_RELEASE}/common/util/tcl
