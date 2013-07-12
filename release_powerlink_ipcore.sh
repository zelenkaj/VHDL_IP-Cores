#!/bin/bash
#

DIR_AXI_POWERLINK="axi_powerlink_v0_30_a"
DIR_PLB_POWERLINK="plb_powerlink_v0_30_a"

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
mkdir -p release/altera/components/doc
mkdir -p release/altera/openmac/src
mkdir -p release/altera/pdi/src
mkdir -p release/common/lib/src
mkdir -p release/common/openmac/src
mkdir -p release/common/pdi/src
mkdir -p release/common/powerlink/src
mkdir -p release/common/spi/src
mkdir -p release/doc
mkdir -p release/xilinx/library/pcores/$DIR_AXI_POWERLINK/data
mkdir -p release/xilinx/library/pcores/$DIR_AXI_POWERLINK/doc
mkdir -p release/xilinx/library/pcores/$DIR_AXI_POWERLINK/hdl/vhdl
mkdir -p release/xilinx/library/pcores/$DIR_PLB_POWERLINK/data
mkdir -p release/xilinx/library/pcores/$DIR_PLB_POWERLINK/doc
mkdir -p release/xilinx/library/pcores/$DIR_PLB_POWERLINK/hdl/vhdl
mkdir -p release/xilinx/openmac/src
mkdir -p release/xilinx/pdi/src

# copy docs
echo "copy docs..."
cp doc/01_POWERLINK-IP-Core_Altera.pdf            release/doc
cp doc/01_POWERLINK-IP-Core_Xilinx.pdf            release/doc
cp doc/02_POWERLINK-IP-Core_Generic.pdf           release/doc
cp doc/03_OpenMAC.pdf                             release/doc

# copy Altera POWERLINK
echo "copy altera powerlink ipcore..."
cp common/powerlink/revision.txt                  release/common/powerlink
cp altera/components/doc/index.pdf                release/altera/components/doc
cp altera/components/img/*.*                      release/altera/components/img
cp altera/components/powerlink_hw.tcl             release/altera/components
cp altera/openmac/src/dpr_16_16.mif               release/altera/openmac/src
cp altera/openmac/src/dpr_16_32.mif               release/altera/openmac/src
cp altera/pdi/src/pdi_dpr.mif                     release/altera/pdi/src
cp common/powerlink/src/powerlink.vhd             release/common/powerlink/src
cp altera/openmac/src/openMAC_DPR.vhd             release/altera/openmac/src
cp altera/openmac/src/openMAC_DMAFifo.vhd         release/altera/openmac/src
cp common/openmac/src/openFILTER.vhd              release/common/openmac/src
cp common/openmac/src/openHUB.vhd                 release/common/openmac/src
cp common/openmac/src/openMAC.vhd                 release/common/openmac/src
cp common/openmac/src/openMAC_Ethernet.vhd        release/common/openmac/src
cp common/openmac/src/openMAC_cmp.vhd             release/common/openmac/src
cp common/openmac/src/openMAC_phyAct.vhd          release/common/openmac/src
cp common/openmac/src/openMAC_DMAmaster.vhd       release/common/openmac/src
cp common/openmac/src/dma_handler.vhd             release/common/openmac/src
cp common/openmac/src/master_handler.vhd          release/common/openmac/src
cp common/openmac/src/openMAC_PHYMI.vhd           release/common/openmac/src
cp common/openmac/src/openMAC_rmii2mii.vhd        release/common/openmac/src
cp altera/pdi/src/pdi_dpr.vhd                     release/altera/pdi/src
cp common/pdi/src/pdi.vhd                         release/common/pdi/src
cp common/pdi/src/pdi_par.vhd                     release/common/pdi/src
cp common/pdi/src/pdi_tripleVBufLogic.vhd         release/common/pdi/src
cp common/pdi/src/pdi_apIrqGen.vhd                release/common/pdi/src
cp common/pdi/src/pdi_controlStatusReg.vhd        release/common/pdi/src
cp common/pdi/src/pdi_event.vhd                   release/common/pdi/src
cp common/pdi/src/pdi_led.vhd                     release/common/pdi/src
cp common/pdi/src/pdi_simpleReg.vhd               release/common/pdi/src
cp common/pdi/src/portio.vhd                      release/common/pdi/src
cp common/pdi/src/portio_cnt.vhd                  release/common/pdi/src
cp common/pdi/src/pdi_spi.vhd                     release/common/pdi/src
cp common/lib/src/cntRtl.vhd                      release/common/lib/src
cp common/lib/src/synchronizerRtl.vhd             release/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd             release/common/lib/src
cp common/lib/src/nShiftRegRtl.vhd                release/common/lib/src
cp common/spi/src/spiSlave-e.vhd                  release/common/spi/src
cp common/spi/src/spiSlave-rtl_sclk-a.vhd         release/common/spi/src
cp common/lib/src/addr_decoder.vhd                release/common/lib/src
cp common/lib/src/edgedet.vhd                     release/common/lib/src
cp common/lib/src/req_ack.vhd                     release/common/lib/src
cp common/lib/src/sync.vhd                        release/common/lib/src
cp common/lib/src/slow2fastSync.vhd               release/common/lib/src
cp common/lib/src/memMap.vhd                      release/common/lib/src
cp common/lib/src/global.vhd                      release/common/lib/src

# copy Xilinx AXI POWERLINK
echo "copy xilinx axi powerlink ipcore..."
cp common/powerlink/revision.txt                  release/common/powerlink
cp common/lib/src/addr_decoder.vhd                release/common/lib/src
cp common/lib/src/edgedet.vhd                     release/common/lib/src
cp common/lib/src/memMap.vhd                      release/common/lib/src
cp common/lib/src/req_ack.vhd                     release/common/lib/src
cp common/lib/src/sync.vhd                        release/common/lib/src
cp common/lib/src/slow2fastSync.vhd               release/common/lib/src
cp common/lib/src/clkXingRtl.vhd                  release/common/lib/src
cp common/lib/src/global.vhd                      release/common/lib/src
cp xilinx/openmac/src/async_fifo_ctrl.vhd         release/xilinx/openmac/src
cp xilinx/openmac/src/fifo_read.vhd               release/xilinx/openmac/src
cp xilinx/openmac/src/fifo_write.vhd              release/xilinx/openmac/src
cp xilinx/openmac/src/n_synchronizer.vhd          release/xilinx/openmac/src
cp xilinx/openmac/src/ipif_master_handler.vhd     release/xilinx/openmac/src
cp xilinx/openmac/src/openMAC_DMAFifo.vhd         release/xilinx/openmac/src
cp xilinx/openmac/src/openMAC_DPR.vhd             release/xilinx/openmac/src
cp xilinx/pdi/src/pdi_dpr.vhd                     release/xilinx/pdi/src
cp common/openmac/src/dma_handler.vhd             release/common/openmac/src
cp common/openmac/src/master_handler.vhd          release/common/openmac/src
cp common/openmac/src/openFILTER.vhd              release/common/openmac/src
cp common/openmac/src/openHUB.vhd                 release/common/openmac/src
cp common/openmac/src/openMAC_16to32conv.vhd      release/common/openmac/src
cp common/openmac/src/openMAC_cmp.vhd             release/common/openmac/src
cp common/openmac/src/openMAC_DMAmaster.vhd       release/common/openmac/src
cp common/openmac/src/openMAC_Ethernet.vhd        release/common/openmac/src
cp common/openmac/src/openMAC_phyAct.vhd          release/common/openmac/src
cp common/openmac/src/openMAC_PHYMI.vhd           release/common/openmac/src
cp common/openmac/src/openMAC_rmii2mii.vhd        release/common/openmac/src
cp common/openmac/src/openMAC.vhd                 release/common/openmac/src
cp common/lib/src/cntRtl.vhd                      release/common/lib/src
cp common/lib/src/synchronizerRtl.vhd             release/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd             release/common/lib/src
cp common/lib/src/nShiftRegRtl.vhd                release/common/lib/src
cp common/spi/src/spiSlave-e.vhd                  release/common/spi/src
cp common/spi/src/spiSlave-rtl_sclk-a.vhd         release/common/spi/src
cp common/pdi/src/pdi_apIrqGen.vhd                release/common/pdi/src
cp common/pdi/src/pdi_controlStatusReg.vhd        release/common/pdi/src
cp common/pdi/src/pdi_event.vhd                   release/common/pdi/src
cp common/pdi/src/pdi_led.vhd                     release/common/pdi/src
cp common/pdi/src/pdi_par.vhd                     release/common/pdi/src
cp common/pdi/src/pdi_simpleReg.vhd               release/common/pdi/src
cp common/pdi/src/pdi_spi.vhd                     release/common/pdi/src
cp common/pdi/src/pdi_tripleVBufLogic.vhd         release/common/pdi/src
cp common/pdi/src/pdi.vhd                         release/common/pdi/src
cp common/pdi/src/portio_cnt.vhd                  release/common/pdi/src
cp common/pdi/src/portio.vhd                      release/common/pdi/src
cp common/powerlink/src/powerlink.vhd             release/common/powerlink/src
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.mdd     release/xilinx/library/pcores/$DIR_AXI_POWERLINK/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.mpd     release/xilinx/library/pcores/$DIR_AXI_POWERLINK/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.mui     release/xilinx/library/pcores/$DIR_AXI_POWERLINK/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.pao     release/xilinx/library/pcores/$DIR_AXI_POWERLINK/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/data/axi_powerlink_v2_1_0.tcl     release/xilinx/library/pcores/$DIR_AXI_POWERLINK/data
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/doc/index.pdf                     release/xilinx/library/pcores/$DIR_AXI_POWERLINK/doc
cp xilinx/library/pcores/axi_powerlink_vX_YY_Z/hdl/vhdl/axi_powerlink.vhd        release/xilinx/library/pcores/$DIR_AXI_POWERLINK/hdl/vhdl

# copy Xilinx PLB POWERLINK
echo "copy xilinx plb powerlink ipcore..."
cp common/powerlink/revision.txt                  release/common/powerlink
cp common/lib/src/addr_decoder.vhd                release/common/lib/src
cp common/lib/src/edgedet.vhd                     release/common/lib/src
cp common/lib/src/memMap.vhd                      release/common/lib/src
cp common/lib/src/req_ack.vhd                     release/common/lib/src
cp common/lib/src/sync.vhd                        release/common/lib/src
cp common/lib/src/slow2fastSync.vhd               release/common/lib/src
cp common/lib/src/global.vhd                      release/common/lib/src
cp xilinx/openmac/src/async_fifo_ctrl.vhd         release/xilinx/openmac/src
cp xilinx/openmac/src/fifo_read.vhd               release/xilinx/openmac/src
cp xilinx/openmac/src/fifo_write.vhd              release/xilinx/openmac/src
cp xilinx/openmac/src/n_synchronizer.vhd          release/xilinx/openmac/src
cp xilinx/openmac/src/ipif_master_handler.vhd     release/xilinx/openmac/src
cp xilinx/openmac/src/openMAC_DMAFifo.vhd         release/xilinx/openmac/src
cp xilinx/openmac/src/openMAC_DPR.vhd             release/xilinx/openmac/src
cp xilinx/pdi/src/pdi_dpr.vhd                     release/xilinx/pdi/src
cp common/openmac/src/dma_handler.vhd             release/common/openmac/src
cp common/openmac/src/master_handler.vhd          release/common/openmac/src
cp common/openmac/src/openFILTER.vhd              release/common/openmac/src
cp common/openmac/src/openHUB.vhd                 release/common/openmac/src
cp common/openmac/src/openMAC_16to32conv.vhd      release/common/openmac/src
cp common/openmac/src/openMAC_cmp.vhd             release/common/openmac/src
cp common/openmac/src/openMAC_DMAmaster.vhd       release/common/openmac/src
cp common/openmac/src/openMAC_Ethernet.vhd        release/common/openmac/src
cp common/openmac/src/openMAC_phyAct.vhd          release/common/openmac/src
cp common/openmac/src/openMAC_PHYMI.vhd           release/common/openmac/src
cp common/openmac/src/openMAC_rmii2mii.vhd        release/common/openmac/src
cp common/openmac/src/openMAC.vhd                 release/common/openmac/src
cp common/lib/src/cntRtl.vhd                      release/common/lib/src
cp common/lib/src/synchronizerRtl.vhd             release/common/lib/src
cp common/lib/src/edgedetectorRtl.vhd             release/common/lib/src
cp common/lib/src/nShiftRegRtl.vhd                release/common/lib/src
cp common/spi/src/spiSlave-e.vhd                  release/common/spi/src
cp common/spi/src/spiSlave-rtl_sclk-a.vhd         release/common/spi/src
cp common/pdi/src/pdi_apIrqGen.vhd                release/common/pdi/src
cp common/pdi/src/pdi_controlStatusReg.vhd        release/common/pdi/src
cp common/pdi/src/pdi_event.vhd                   release/common/pdi/src
cp common/pdi/src/pdi_led.vhd                     release/common/pdi/src
cp common/pdi/src/pdi_par.vhd                     release/common/pdi/src
cp common/pdi/src/pdi_simpleReg.vhd               release/common/pdi/src
cp common/pdi/src/pdi_spi.vhd                     release/common/pdi/src
cp common/pdi/src/pdi_tripleVBufLogic.vhd         release/common/pdi/src
cp common/pdi/src/pdi.vhd                         release/common/pdi/src
cp common/pdi/src/portio_cnt.vhd                  release/common/pdi/src
cp common/pdi/src/portio.vhd                      release/common/pdi/src
cp common/powerlink/src/powerlink.vhd             release/common/powerlink/src
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.mdd     release/xilinx/library/pcores/$DIR_PLB_POWERLINK/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.mpd     release/xilinx/library/pcores/$DIR_PLB_POWERLINK/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.mui     release/xilinx/library/pcores/$DIR_PLB_POWERLINK/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.pao     release/xilinx/library/pcores/$DIR_PLB_POWERLINK/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/data/plb_powerlink_v2_1_0.tcl     release/xilinx/library/pcores/$DIR_PLB_POWERLINK/data
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/doc/index.pdf                     release/xilinx/library/pcores/$DIR_PLB_POWERLINK/doc
cp xilinx/library/pcores/plb_powerlink_vX_YY_Z/hdl/vhdl/plb_powerlink.vhd        release/xilinx/library/pcores/$DIR_PLB_POWERLINK/hdl/vhdl
