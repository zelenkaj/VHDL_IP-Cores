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
mkdir -p release/altera/components/tcl
mkdir -p release/altera/tripleBuffer/src
mkdir -p release/altera/lib/src
mkdir -p release/common/tripleBuffer/src
mkdir -p release/common/tripleBuffer/tcl
mkdir -p release/common/lib/src
mkdir -p release/common/util/tcl

# copy Altera
echo "copy altera ipcore..."
cp altera/components/tripleBuffer_hw.tcl                release/altera/components
cp altera/components/tripleBuffer_sw.tcl                release/altera/components
cp altera/components/tcl/tripleBuffer.tcl               release/altera/components/tcl
cp altera/tripleBuffer/src/alteraTripleBufferRtl.vhd    release/altera/tripleBuffer/src
cp altera/lib/src/dpRam-rtl-a.vhd                       release/altera/lib/src
cp common/lib/src/global.vhd                            release/common/lib/src
cp common/lib/src/dpRam-e.vhd                           release/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd                   release/common/lib/src
cp common/lib/src/addrDecodeRtl.vhd                     release/common/lib/src
cp common/lib/src/binaryEncoderRtl.vhd                  release/common/lib/src
cp common/tripleBuffer/tcl/calcTriBuf.tcl               release/common/tripleBuffer/tcl
cp common/tripleBuffer/src/tripleBufferPkg.vhd          release/common/tripleBuffer/src
cp common/tripleBuffer/src/tripleBridgeRtl.vhd          release/common/tripleBuffer/src
cp common/tripleBuffer/src/tripleLogicRtl.vhd           release/common/tripleBuffer/src
cp common/tripleBuffer/src/tripleBufferRtl.vhd          release/common/tripleBuffer/src
cp common/util/tcl/writeFile.tcl                        release/common/util/tcl
