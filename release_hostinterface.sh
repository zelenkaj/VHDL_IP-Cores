#!/bin/bash
# ./release_hostinterface.sh RELEASE_DIR IP_VERSION
# e.g.: $ ./release_hostinterface.sh release v1_00_a

DIR_RELEASE=$1
IP_VERSION=$2
DIR_DOC=doc/hostinterface

if [ -z "${DIR_RELEASE}" ];
then
    DIR_RELEASE=release
fi

if [ -z "${IP_VERSION}" ];
then
    IP_VERSION=v1_00_a
fi

# create dir structure
echo "create release dir..."
mkdir -p ${DIR_RELEASE}

# generate docs
echo "generate docs..."
pushd $DIR_DOC
./create-this-doc --skip-doxygen
popd

# copy docs
echo "copy docs..."
cp --parents ${DIR_DOC}/images/hostif-common_vs_dynamic_bridge.png  ${DIR_RELEASE}
cp --parents ${DIR_DOC}/images/hostif-example_sys_int.png           ${DIR_RELEASE}
cp --parents ${DIR_DOC}/images/hostif-dyn_bridge.png                ${DIR_RELEASE}
cp --parents ${DIR_DOC}/images/hostif-memmap.png                    ${DIR_RELEASE}
cp --parents ${DIR_DOC}/md/hostif.md                                ${DIR_RELEASE}
cp --parents ${DIR_DOC}/md/hostif_sc.md                             ${DIR_RELEASE}
cp --parents ${DIR_DOC}/wavedrom/hostif_mplx_rd-wr.svg              ${DIR_RELEASE}
cp --parents ${DIR_DOC}/wavedrom/hostif_rd-wr.svg                   ${DIR_RELEASE}
cp --parents ${DIR_DOC}/wavedrom/hostif_mplx_rd.svg                 ${DIR_RELEASE}
cp --parents ${DIR_DOC}/wavedrom/hostif_rd.svg                      ${DIR_RELEASE}
cp --parents ${DIR_DOC}/wavedrom/hostif_mplx_wr.svg                 ${DIR_RELEASE}
cp --parents ${DIR_DOC}/wavedrom/hostif_wr.svg                      ${DIR_RELEASE}
cp --parents ${DIR_DOC}/Doxyfile                                    ${DIR_RELEASE}
cp --parents ${DIR_DOC}/mainpage.txt                                ${DIR_RELEASE}

# copy Altera host interface
echo "copy altera ipcore..."
cp --parents altera/components/hostinterface_hw.tcl                 ${DIR_RELEASE}
cp --parents altera/components/hostinterface_sw.tcl                 ${DIR_RELEASE}
cp --parents altera/components/img/br.png                           ${DIR_RELEASE}
cp --parents altera/components/sdc/hostinterface-async.sdc          ${DIR_RELEASE}
cp --parents altera/components/tcl/hostinterface.tcl                ${DIR_RELEASE}
cp --parents altera/memory/src/dpRam-rtl-a.vhd                      ${DIR_RELEASE}
cp --parents common/memory/src/dpRam-e.vhd                          ${DIR_RELEASE}
cp --parents altera/hostinterface/src/alteraHostInterfaceRtl.vhd    ${DIR_RELEASE}
cp --parents common/hostinterface/src/hostInterfacePkg.vhd          ${DIR_RELEASE}
cp --parents common/hostinterface/src/hostInterfaceRtl.vhd          ${DIR_RELEASE}
cp --parents common/hostinterface/src/irqGenRtl.vhd                 ${DIR_RELEASE}
cp --parents common/hostinterface/src/dynamicBridgeRtl.vhd          ${DIR_RELEASE}
cp --parents common/hostinterface/src/statusControlRegRtl.vhd       ${DIR_RELEASE}
cp --parents common/hostinterface/src/parallelInterfaceRtl.vhd      ${DIR_RELEASE}

# copy Xilinx host interface
echo "copy xilinx ipcore..."
mkdir -p ${DIR_RELEASE}/xilinx/components/pcores/axi_hostinterface_${IP_VERSION}/data

cp xilinx/components/pcores/axi_hostinterface_vX_YY_Z/data/axi_hostinterface_v2_1_0.mdd  ${DIR_RELEASE}/xilinx/components/pcores/axi_hostinterface_${IP_VERSION}/data
cp xilinx/components/pcores/axi_hostinterface_vX_YY_Z/data/axi_hostinterface_v2_1_0.mpd  ${DIR_RELEASE}/xilinx/components/pcores/axi_hostinterface_${IP_VERSION}/data
cp xilinx/components/pcores/axi_hostinterface_vX_YY_Z/data/axi_hostinterface_v2_1_0.pao  ${DIR_RELEASE}/xilinx/components/pcores/axi_hostinterface_${IP_VERSION}/data
cp xilinx/components/pcores/axi_hostinterface_vX_YY_Z/data/axi_hostinterface_v2_1_0.tcl  ${DIR_RELEASE}/xilinx/components/pcores/axi_hostinterface_${IP_VERSION}/data
cp xilinx/components/pcores/axi_hostinterface_vX_YY_Z/data/axi_hostinterface_v2_1_0.mui  ${DIR_RELEASE}/xilinx/components/pcores/axi_hostinterface_${IP_VERSION}/data

cp --parents xilinx/memory/src/dpRam-rtl-a.vhd                      ${DIR_RELEASE}
cp --parents common/memory/src/dpRam-e.vhd                          ${DIR_RELEASE}
cp --parents xilinx/hostinterface/src/axi_hostinterface-rtl-ea.vhd  ${DIR_RELEASE}
cp --parents common/hostinterface/src/hostInterfacePkg.vhd          ${DIR_RELEASE}
cp --parents common/hostinterface/src/hostInterfaceRtl.vhd          ${DIR_RELEASE}
cp --parents common/hostinterface/src/irqGenRtl.vhd                 ${DIR_RELEASE}
cp --parents common/hostinterface/src/dynamicBridgeRtl.vhd          ${DIR_RELEASE}
cp --parents common/hostinterface/src/statusControlRegRtl.vhd       ${DIR_RELEASE}
cp --parents common/hostinterface/src/parallelInterfaceRtl.vhd      ${DIR_RELEASE}

cp --parents common/axiwrapper/src/axiLiteSlaveWrapper-rtl-ea.vhd   ${DIR_RELEASE}
cp --parents common/axiwrapper/src/axiLiteMasterWrapper-rtl-ea.vhd  ${DIR_RELEASE}


# create revision.txt
REV_FILE=${DIR_RELEASE}/${DIR_DOC}/revision.md
echo "Revision {#revision}" > $REV_FILE
echo "========" >> $REV_FILE
echo "" >> $REV_FILE
git log --format="- %s" -- */hostinterface/* >> $REV_FILE
