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
mkdir -p release/altera/components/img
mkdir -p release/altera/components/sdc
mkdir -p release/altera/components/tcl
mkdir -p release/altera/hostinterface/src
mkdir -p release/altera/lib/src
mkdir -p release/common/hostinterface/src
mkdir -p release/common/lib/src

# copy docs
#echo "copy docs..."
#

# copy Altera host interface
echo "copy altera powerlink ipcore..."
cp altera/components/hostinterface_hw.tcl         release/altera/components
cp altera/components/hostinterface_sw.tcl         release/altera/components
cp altera/components/img/br.png                   release/altera/components/img
cp altera/components/sdc/hostinterface-async.sdc  release/altera/components/sdc
cp altera/components/tcl/hostinterface.tcl        release/altera/components/tcl
cp altera/lib/src/dpRam-rtl-a.vhd                 release/altera/lib/src
cp common/lib/src/dpRam-e.vhd                     release/common/lib/src
cp common/lib/src/addrDecodeRtl.vhd               release/common/lib/src
cp common/lib/src/binaryEncoderRtl.vhd            release/common/lib/src
cp common/lib/src/cntRtl.vhd                      release/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd             release/common/lib/src
cp common/lib/src/lutFileRtl.vhd                  release/common/lib/src
cp common/lib/src/registerFileRtl.vhd             release/common/lib/src
cp common/lib/src/synchronizerRtl.vhd             release/common/lib/src
cp altera/hostinterface/src/alteraHostInterfaceRtl.vhd    release/altera/hostinterface/src
cp common/hostinterface/revision.txt                      release/common/hostinterface
cp common/hostinterface/src/hostInterfacePkg.vhd          release/common/hostinterface/src
cp common/hostinterface/src/hostInterfaceRtl.vhd          release/common/hostinterface/src
cp common/hostinterface/src/irqGenRtl.vhd                 release/common/hostinterface/src
cp common/hostinterface/src/dynamicBridgeRtl.vhd          release/common/hostinterface/src
cp common/hostinterface/src/statusControlRegRtl.vhd       release/common/hostinterface/src
cp common/hostinterface/src/parallelInterfaceRtl.vhd      release/common/hostinterface/src
cp common/lib/src/global.vhd                      release/common/lib/src
