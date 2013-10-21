#!/bin/bash
# ./release_spiBridge_ipcore.sh RELEASE_DIR
# e.g.: $ ./release_spiBridge_ipcore release

DIR_RELEASE=$1

if [ -z "${DIR_RELEASE}" ];
then
    DIR_RELEASE=release
fi

# create dir structure
echo "create dir structure..."
mkdir -p ${DIR_RELEASE}/altera/components
mkdir -p ${DIR_RELEASE}/altera/components/sdc
mkdir -p ${DIR_RELEASE}/altera/spi/src
mkdir -p ${DIR_RELEASE}/common/spi/src
mkdir -p ${DIR_RELEASE}/common/lib/src

# copy Altera
echo "copy altera ipcore..."
cp altera/components/spiBridge_hw.tcl       ${DIR_RELEASE}/altera/components
cp altera/components/sdc/spiBridge-aclk.sdc ${DIR_RELEASE}/altera/components/sdc
cp altera/spi/src/alteraSpiBridgeRtl.vhd    ${DIR_RELEASE}/altera/spi/src
cp common/lib/src/global.vhd                ${DIR_RELEASE}/common/lib/src
cp common/lib/src/synchronizerRtl.vhd       ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd       ${DIR_RELEASE}/common/lib/src
cp common/spi/src/spiSlave-e.vhd            ${DIR_RELEASE}/common/spi/src
cp common/spi/src/spiSlave-rtl_aclk-a.vhd   ${DIR_RELEASE}/common/spi/src
cp common/spi/src/protStreamRtl.vhd         ${DIR_RELEASE}/common/spi/src
cp common/spi/src/spiBridgeRtl.vhd          ${DIR_RELEASE}/common/spi/src
