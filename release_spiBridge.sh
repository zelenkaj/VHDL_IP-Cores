#!/bin/bash
#

# clean release dir
if [ -d release ]
then
    echo "clean release dir..."
    rm -r release
fi

# create dir structure
echo "create dir structure..."
mkdir -p release/altera/components
mkdir -p release/altera/spi/src
mkdir -p release/common/spi/src
mkdir -p release/common/lib/src

# copy Altera
echo "copy altera ipcore..."
cp altera/components/spiBridge_hw.tcl       release/altera/components
cp altera/spi/src/alteraSpiBridgeRtl.vhd    release/altera/spi/src
cp common/lib/src/global.vhd                release/common/lib/src
cp common/lib/src/synchronizerRtl.vhd       release/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd       release/common/lib/src
cp common/spi/src/spiSlave-e.vhd            release/common/spi/src
cp common/spi/src/spiSlave-rtl_aclk-a.vhd   release/common/spi/src
cp common/spi/src/protStreamRtl.vhd         release/common/spi/src
cp common/spi/src/spiBridgeRtl.vhd          release/common/spi/src
