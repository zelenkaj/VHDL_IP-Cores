#!/bin/bash
#
DIR_AXI_HOSTINTERFACE="axi_hostinterface_v1_00_a"
# clean release dir
if [ -d release ]
then
    echo "clean release dir..."
    rm -r release
fi

# create dir structure
echo "create dir structure..."
mkdir -p release/common/hostinterface/src
mkdir -p release/common/lib/src
mkdir -p release/xilinx
mkdir -p release/xilinx/lib
mkdir -p release/xilinx/lib/src
mkdir -p release/common/axiwrapper
mkdir -p release/common/axiwrapper/src
mkdir -p release/xilinx/hostinterface/
mkdir -p release/xilinx/hostinterface/src/
mkdir -p release/xilinx/library
mkdir -p release/xilinx/library/pcores
mkdir -p release/xilinx/library/pcores/axi_hostinterface_v1_00_a
mkdir -p release/xilinx/library/pcores/axi_hostinterface_v1_00_a/data
mkdir -p release/xilinx/library/pcores/axi_hostinterface_v1_00_a/hdl
mkdir -p release/xilinx/library/pcores/axi_hostinterface_v1_00_a/hdl/vhdl



# copy docs
#echo "copy docs..."
#

# copy Xilinx host interface
echo "copy Xilinx Host Interface ipcore..."
cp common/lib/src/dpRam-e.vhd                     release/common/lib/src
cp common/lib/src/addrDecodeRtl.vhd               release/common/lib/src
cp common/lib/src/binaryEncoderRtl.vhd            release/common/lib/src
cp common/lib/src/cntRtl.vhd                      release/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd             release/common/lib/src
cp common/lib/src/lutFileRtl.vhd                  release/common/lib/src
cp common/lib/src/registerFileRtl.vhd             release/common/lib/src
cp common/lib/src/synchronizerRtl.vhd             release/common/lib/src
cp common/hostinterface/revision.txt                      release/common/hostinterface
cp common/hostinterface/src/hostInterfacePkg.vhd          release/common/hostinterface/src
cp common/hostinterface/src/hostInterfaceRtl.vhd          release/common/hostinterface/src
cp common/hostinterface/src/irqGenRtl.vhd                 release/common/hostinterface/src
cp common/hostinterface/src/dynamicBridgeRtl.vhd          release/common/hostinterface/src
cp common/hostinterface/src/statusControlRegRtl.vhd       release/common/hostinterface/src
cp common/hostinterface/src/parallelInterfaceRtl.vhd      release/common/hostinterface/src
cp common/lib/src/global.vhd                      		  release/common/lib/src
cp xilinx/lib/src/dpRam-rtl-a.vhd						  release/xilinx/lib/src
cp common/axiwrapper/src/axiLiteSlaveWrapper-rtl-ea.vhd   release/common/axiwrapper/src
cp common/axiwrapper/src/axiLiteMasterWrapper-rtl-ea.vhd	  release/common/axiwrapper/src
cp xilinx/hostinterface/src/axi_hostinterface-rtl-ea.vhd      release/xilinx/hostinterface/src/  
cp xilinx/library/pcores/axi_hostinterface_vX_YY_Z/data/*.*		release/xilinx/library/pcores/$DIR_AXI_HOSTINTERFACE/data
cp xilinx/library/pcores/axi_hostinterface_vX_YY_Z/hdl/vhdl/*.*	release/xilinx/library/pcores/$DIR_AXI_HOSTINTERFACE/hdl/vhdl
