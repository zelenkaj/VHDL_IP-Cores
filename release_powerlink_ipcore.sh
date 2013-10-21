#!/bin/bash
# ./release_powerlink_ipcore.sh RELEASE_DIR IP_VERSION
# e.g.: $ ./release_powerlink_ipcore release v0_30_a

DIR_RELEASE=$1
IP_VERSION=$2

if [ -z "${DIR_RELEASE}" ];
then
    DIR_RELEASE=release
fi

if [ -z "${IP_VERSION}" ];
then
    IP_VERSION=v0_30_a
fi

DIR_AXI_POWERLINK="axi_powerlink_${IP_VERSION}"
DIR_PLB_POWERLINK="plb_powerlink_${IP_VERSION}"

# create dir structure
echo "create dir structure..."
mkdir -p ${DIR_RELEASE}/altera/components
mkdir -p ${DIR_RELEASE}/altera/components/img
mkdir -p ${DIR_RELEASE}/altera/components/doc
mkdir -p ${DIR_RELEASE}/altera/openmac/src
mkdir -p ${DIR_RELEASE}/altera/pdi/src
mkdir -p ${DIR_RELEASE}/common/lib/src
mkdir -p ${DIR_RELEASE}/common/openmac/src
mkdir -p ${DIR_RELEASE}/common/pdi/src
mkdir -p ${DIR_RELEASE}/common/powerlink/src
mkdir -p ${DIR_RELEASE}/common/spi/src
mkdir -p ${DIR_RELEASE}/doc
mkdir -p ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/data
mkdir -p ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/doc
mkdir -p ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/hdl/vhdl
mkdir -p ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/data
mkdir -p ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/doc
mkdir -p ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/hdl/vhdl
mkdir -p ${DIR_RELEASE}/xilinx/openmac/src
mkdir -p ${DIR_RELEASE}/xilinx/pdi/src

# copy docs
echo "copy docs..."
cp doc/01_POWERLINK-IP-Core_Altera.pdf            ${DIR_RELEASE}/doc
cp doc/01_POWERLINK-IP-Core_Xilinx.pdf            ${DIR_RELEASE}/doc
cp doc/02_POWERLINK-IP-Core_Generic.pdf           ${DIR_RELEASE}/doc
cp doc/03_OpenMAC.pdf                             ${DIR_RELEASE}/doc

# copy Altera POWERLINK
echo "copy altera powerlink ipcore..."
cp common/powerlink/revision.txt                  ${DIR_RELEASE}/common/powerlink
cp altera/components/doc/index.pdf                ${DIR_RELEASE}/altera/components/doc
cp altera/components/img/*.*                      ${DIR_RELEASE}/altera/components/img
cp altera/components/powerlink_hw.tcl             ${DIR_RELEASE}/altera/components
cp altera/openmac/src/dpr_16_16.mif               ${DIR_RELEASE}/altera/openmac/src
cp altera/openmac/src/dpr_16_32.mif               ${DIR_RELEASE}/altera/openmac/src
cp altera/pdi/src/pdi_dpr.mif                     ${DIR_RELEASE}/altera/pdi/src
cp common/powerlink/src/powerlink.vhd             ${DIR_RELEASE}/common/powerlink/src
cp altera/openmac/src/openMAC_DPR.vhd             ${DIR_RELEASE}/altera/openmac/src
cp altera/openmac/src/openMAC_DMAFifo.vhd         ${DIR_RELEASE}/altera/openmac/src
cp common/openmac/src/openFILTER.vhd              ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openHUB.vhd                 ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC.vhd                 ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_Ethernet.vhd        ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_cmp.vhd             ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_phyAct.vhd          ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_DMAmaster.vhd       ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/dma_handler.vhd             ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/master_handler.vhd          ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_PHYMI.vhd           ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_rmii2mii.vhd        ${DIR_RELEASE}/common/openmac/src
cp altera/pdi/src/pdi_dpr.vhd                     ${DIR_RELEASE}/altera/pdi/src
cp common/pdi/src/pdi.vhd                         ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_par.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_tripleVBufLogic.vhd         ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_apIrqGen.vhd                ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_controlStatusReg.vhd        ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_event.vhd                   ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_led.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_simpleReg.vhd               ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/portio.vhd                      ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/portio_cnt.vhd                  ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_spi.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/lib/src/cntRtl.vhd                      ${DIR_RELEASE}/common/lib/src
cp common/lib/src/synchronizerRtl.vhd             ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd             ${DIR_RELEASE}/common/lib/src
cp common/lib/src/nShiftRegRtl.vhd                ${DIR_RELEASE}/common/lib/src
cp common/spi/src/spiSlave-e.vhd                  ${DIR_RELEASE}/common/spi/src
cp common/spi/src/spiSlave-rtl_sclk-a.vhd         ${DIR_RELEASE}/common/spi/src
cp common/lib/src/addr_decoder.vhd                ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedet.vhd                     ${DIR_RELEASE}/common/lib/src
cp common/lib/src/req_ack.vhd                     ${DIR_RELEASE}/common/lib/src
cp common/lib/src/sync.vhd                        ${DIR_RELEASE}/common/lib/src
cp common/lib/src/slow2fastSync.vhd               ${DIR_RELEASE}/common/lib/src
cp common/lib/src/memMap.vhd                      ${DIR_RELEASE}/common/lib/src
cp common/lib/src/global.vhd                      ${DIR_RELEASE}/common/lib/src

# copy Xilinx AXI POWERLINK
echo "copy xilinx axi powerlink ipcore..."
cp common/powerlink/revision.txt                  ${DIR_RELEASE}/common/powerlink
cp common/lib/src/addr_decoder.vhd                ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedet.vhd                     ${DIR_RELEASE}/common/lib/src
cp common/lib/src/memMap.vhd                      ${DIR_RELEASE}/common/lib/src
cp common/lib/src/req_ack.vhd                     ${DIR_RELEASE}/common/lib/src
cp common/lib/src/sync.vhd                        ${DIR_RELEASE}/common/lib/src
cp common/lib/src/slow2fastSync.vhd               ${DIR_RELEASE}/common/lib/src
cp common/lib/src/clkXingRtl.vhd                  ${DIR_RELEASE}/common/lib/src
cp common/lib/src/global.vhd                      ${DIR_RELEASE}/common/lib/src
cp xilinx/openmac/src/async_fifo_ctrl.vhd         ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/fifo_read.vhd               ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/fifo_write.vhd              ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/n_synchronizer.vhd          ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/ipif_master_handler.vhd     ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/openMAC_DMAFifo.vhd         ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/openMAC_DPR.vhd             ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/pdi/src/pdi_dpr.vhd                     ${DIR_RELEASE}/xilinx/pdi/src
cp common/openmac/src/dma_handler.vhd             ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/master_handler.vhd          ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openFILTER.vhd              ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openHUB.vhd                 ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_16to32conv.vhd      ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_cmp.vhd             ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_DMAmaster.vhd       ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_Ethernet.vhd        ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_phyAct.vhd          ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_PHYMI.vhd           ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_rmii2mii.vhd        ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC.vhd                 ${DIR_RELEASE}/common/openmac/src
cp common/lib/src/cntRtl.vhd                      ${DIR_RELEASE}/common/lib/src
cp common/lib/src/synchronizerRtl.vhd             ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd             ${DIR_RELEASE}/common/lib/src
cp common/lib/src/nShiftRegRtl.vhd                ${DIR_RELEASE}/common/lib/src
cp common/spi/src/spiSlave-e.vhd                  ${DIR_RELEASE}/common/spi/src
cp common/spi/src/spiSlave-rtl_sclk-a.vhd         ${DIR_RELEASE}/common/spi/src
cp common/pdi/src/pdi_apIrqGen.vhd                ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_controlStatusReg.vhd        ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_event.vhd                   ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_led.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_par.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_simpleReg.vhd               ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_spi.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_tripleVBufLogic.vhd         ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi.vhd                         ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/portio_cnt.vhd                  ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/portio.vhd                      ${DIR_RELEASE}/common/pdi/src
cp common/powerlink/src/powerlink.vhd             ${DIR_RELEASE}/common/powerlink/src
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.mdd     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.mpd     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.mui     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.pao     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.tcl     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/doc/index.pdf                     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/doc
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/hdl/vhdl/axi_powerlink.vhd        ${DIR_RELEASE}/xilinx/library/pcores/${DIR_AXI_POWERLINK}/hdl/vhdl

# copy Xilinx PLB POWERLINK
echo "copy xilinx plb powerlink ipcore..."
cp common/powerlink/revision.txt                  ${DIR_RELEASE}/common/powerlink
cp common/lib/src/addr_decoder.vhd                ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedet.vhd                     ${DIR_RELEASE}/common/lib/src
cp common/lib/src/memMap.vhd                      ${DIR_RELEASE}/common/lib/src
cp common/lib/src/req_ack.vhd                     ${DIR_RELEASE}/common/lib/src
cp common/lib/src/sync.vhd                        ${DIR_RELEASE}/common/lib/src
cp common/lib/src/slow2fastSync.vhd               ${DIR_RELEASE}/common/lib/src
cp common/lib/src/global.vhd                      ${DIR_RELEASE}/common/lib/src
cp xilinx/openmac/src/async_fifo_ctrl.vhd         ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/fifo_read.vhd               ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/fifo_write.vhd              ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/n_synchronizer.vhd          ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/ipif_master_handler.vhd     ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/openMAC_DMAFifo.vhd         ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/openmac/src/openMAC_DPR.vhd             ${DIR_RELEASE}/xilinx/openmac/src
cp xilinx/pdi/src/pdi_dpr.vhd                     ${DIR_RELEASE}/xilinx/pdi/src
cp common/openmac/src/dma_handler.vhd             ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/master_handler.vhd          ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openFILTER.vhd              ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openHUB.vhd                 ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_16to32conv.vhd      ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_cmp.vhd             ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_DMAmaster.vhd       ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_Ethernet.vhd        ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_phyAct.vhd          ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_PHYMI.vhd           ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC_rmii2mii.vhd        ${DIR_RELEASE}/common/openmac/src
cp common/openmac/src/openMAC.vhd                 ${DIR_RELEASE}/common/openmac/src
cp common/lib/src/cntRtl.vhd                      ${DIR_RELEASE}/common/lib/src
cp common/lib/src/synchronizerRtl.vhd             ${DIR_RELEASE}/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd             ${DIR_RELEASE}/common/lib/src
cp common/lib/src/nShiftRegRtl.vhd                ${DIR_RELEASE}/common/lib/src
cp common/spi/src/spiSlave-e.vhd                  ${DIR_RELEASE}/common/spi/src
cp common/spi/src/spiSlave-rtl_sclk-a.vhd         ${DIR_RELEASE}/common/spi/src
cp common/pdi/src/pdi_apIrqGen.vhd                ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_controlStatusReg.vhd        ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_event.vhd                   ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_led.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_par.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_simpleReg.vhd               ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_spi.vhd                     ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi_tripleVBufLogic.vhd         ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/pdi.vhd                         ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/portio_cnt.vhd                  ${DIR_RELEASE}/common/pdi/src
cp common/pdi/src/portio.vhd                      ${DIR_RELEASE}/common/pdi/src
cp common/powerlink/src/powerlink.vhd             ${DIR_RELEASE}/common/powerlink/src
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.mdd     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.mpd     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.mui     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.pao     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.tcl     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/doc/index.pdf                     ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/doc
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/hdl/vhdl/plb_powerlink.vhd        ${DIR_RELEASE}/xilinx/library/pcores/${DIR_PLB_POWERLINK}/hdl/vhdl
