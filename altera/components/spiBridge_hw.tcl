# -----------------------------------------------------------------------------
# spiBridge_hw.tcl
# -----------------------------------------------------------------------------
#
#    (c) B&R, 2013
#
#    Redistribution and use in source and binary forms, with or without
#    modification, are permitted provided that the following conditions
#    are met:
#
#    1. Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
#    3. Neither the name of B&R nor the names of its
#       contributors may be used to endorse or promote products derived
#       from this software without prior written permission. For written
#       permission, please contact office@br-automation.com
#
#    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
#    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
#    COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
#    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
#    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
#    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#    POSSIBILITY OF SUCH DAMAGE.
#
# -----------------------------------------------------------------------------

package require -exact sopc 10.1

# -----------------------------------------------------------------------------
# module
# -----------------------------------------------------------------------------
set_module_property NAME spi_bridge
set_module_property VERSION 0.0.1
set_module_property INTERNAL false
set_module_property GROUP "Bridges and Adapters"
set_module_property AUTHOR "B&R"
set_module_property DISPLAY_NAME "SPI Bridge"
set_module_property TOP_LEVEL_HDL_FILE "../../altera/spi/src/alteraSpiBridgeRtl.vhd"
set_module_property TOP_LEVEL_HDL_MODULE alteraSpiBridge
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE false
set_module_property ELABORATION_CALLBACK elaboration_callback
set_module_property ANALYZE_HDL true

# -----------------------------------------------------------------------------
# file sets
# -----------------------------------------------------------------------------
add_file "../../altera/components/sdc/spiBridge-aclk.sdc" {SYNTHESIS}
add_file "../../altera/spi/src/alteraSpiBridgeRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/lib/src/edgedetectorRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/lib/src/global.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/lib/src/synchronizerRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/spi/src/protStreamRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/spi/src/spiBridgeRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/spi/src/spiSlave-e.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/spi/src/spiSlave-rtl_aclk-a.vhd" {SYNTHESIS SIMULATION}

# -----------------------------------------------------------------------------
# VHDL parameters
# -----------------------------------------------------------------------------
add_parameter           gRegisterSize   NATURAL             8
set_parameter_property  gRegisterSize   DEFAULT_VALUE       8
set_parameter_property  gRegisterSize   TYPE                NATURAL
set_parameter_property  gRegisterSize   DERIVED             true
set_parameter_property  gRegisterSize   HDL_PARAMETER       true
set_parameter_property  gRegisterSize   VISIBLE             false

add_parameter           gPolarity       NATURAL             0
set_parameter_property  gPolarity       DEFAULT_VALUE       0
set_parameter_property  gPolarity       TYPE                NATURAL
set_parameter_property  gPolarity       DERIVED             true
set_parameter_property  gPolarity       HDL_PARAMETER       true
set_parameter_property  gPolarity       VISIBLE             false

add_parameter           gPhase          NATURAL             0
set_parameter_property  gPhase          DEFAULT_VALUE       0
set_parameter_property  gPhase          TYPE                NATURAL
set_parameter_property  gPhase          DERIVED             true
set_parameter_property  gPhase          HDL_PARAMETER       true
set_parameter_property  gPhase          VISIBLE             false

add_parameter           gShiftDir       NATURAL             0
set_parameter_property  gShiftDir       DEFAULT_VALUE       0
set_parameter_property  gShiftDir       TYPE                NATURAL
set_parameter_property  gShiftDir       DERIVED             true
set_parameter_property  gShiftDir       HDL_PARAMETER       true
set_parameter_property  gShiftDir       VISIBLE             false

add_parameter           gBusDataWidth   NATURAL             32
set_parameter_property  gBusDataWidth   DEFAULT_VALUE       32
set_parameter_property  gBusDataWidth   TYPE                NATURAL
set_parameter_property  gBusDataWidth   DERIVED             true
set_parameter_property  gBusDataWidth   AFFECTS_GENERATION  true
set_parameter_property  gBusDataWidth   HDL_PARAMETER       true
set_parameter_property  gBusDataWidth   VISIBLE             false

add_parameter           gBusAddrWidth   NATURAL             8
set_parameter_property  gBusAddrWidth   DEFAULT_VALUE       8
set_parameter_property  gBusAddrWidth   TYPE                NATURAL
set_parameter_property  gBusAddrWidth   DERIVED             true
set_parameter_property  gBusAddrWidth   AFFECTS_GENERATION  true
set_parameter_property  gBusAddrWidth   HDL_PARAMETER       true
set_parameter_property  gBusAddrWidth   VISIBLE             false

add_parameter           gWrBufBase      NATURAL             0
set_parameter_property  gWrBufBase      DEFAULT_VALUE       0
set_parameter_property  gWrBufBase      TYPE                NATURAL
set_parameter_property  gWrBufBase      DERIVED             true
set_parameter_property  gWrBufBase      HDL_PARAMETER       true
set_parameter_property  gWrBufBase      VISIBLE             false

add_parameter           gWrBufSize      NATURAL             128
set_parameter_property  gWrBufSize      DEFAULT_VALUE       128
set_parameter_property  gWrBufSize      TYPE                NATURAL
set_parameter_property  gWrBufSize      DERIVED             true
set_parameter_property  gWrBufSize      HDL_PARAMETER       true
set_parameter_property  gWrBufSize      VISIBLE             false

add_parameter           gRdBufBase      NATURAL             128
set_parameter_property  gRdBufBase      DEFAULT_VALUE       128
set_parameter_property  gRdBufBase      TYPE                NATURAL
set_parameter_property  gRdBufBase      DERIVED             true
set_parameter_property  gRdBufBase      HDL_PARAMETER       true
set_parameter_property  gRdBufBase      VISIBLE             false

add_parameter           gRdBufSize      NATURAL             128
set_parameter_property  gRdBufSize      DEFAULT_VALUE       128
set_parameter_property  gRdBufSize      TYPE                NATURAL
set_parameter_property  gRdBufSize      DERIVED             true
set_parameter_property  gRdBufSize      HDL_PARAMETER       true
set_parameter_property  gRdBufSize      VISIBLE             false

# -----------------------------------------------------------------------------
# System Info parameters
# -----------------------------------------------------------------------------
add_parameter           sys_bdgAddrw    NATURAL             10
set_parameter_property  sys_bdgAddrw    SYSTEM_INFO         {ADDRESS_WIDTH bridge}
set_parameter_property  sys_bdgAddrw    DERIVED             true
set_parameter_property  sys_bdgAddrw    VISIBLE             false

add_parameter           sys_bdgDataw    NATURAL             32
set_parameter_property  sys_bdgDataw    SYSTEM_INFO         {MAX_SLAVE_DATA_WIDTH bridge}
set_parameter_property  sys_bdgDataw    DERIVED             true
set_parameter_property  sys_bdgDataw    VISIBLE             false

# -----------------------------------------------------------------------------
# GUI parameters
# -----------------------------------------------------------------------------
add_parameter           gui_cpol        NATURAL             0
set_parameter_property  gui_cpol        ALLOWED_RANGES      {0 1}
set_parameter_property  gui_cpol        DISPLAY_NAME        "Clock Polarity"

add_parameter           gui_cpha        NATURAL             0
set_parameter_property  gui_cpha        ALLOWED_RANGES      {0 1}
set_parameter_property  gui_cpha        DISPLAY_NAME        "Clock Phase"

add_parameter           gui_shiftdir    NATURAL             0
set_parameter_property  gui_shiftdir    ALLOWED_RANGES      {"0:LSB first" "1:MSB first"}
set_parameter_property  gui_shiftdir    DISPLAY_NAME        "Shift Direction"

add_parameter           gui_regsize     NATURAL             8
set_parameter_property  gui_regsize     ALLOWED_RANGES      {8 16 32}
set_parameter_property  gui_regsize     DISPLAY_NAME        "Register Size"
set_parameter_property  gui_regsize     DISPLAY_UNITS       "Bits"

add_parameter           gui_readsize    NATURAL             128
set_parameter_property  gui_readsize    DISPLAY_NAME        "Read Buffer Size"
set_parameter_property  gui_readsize    DISPLAY_UNITS       "Bytes"

add_parameter           gui_writesize   NATURAL             128
set_parameter_property  gui_writesize   DISPLAY_NAME        "Write Buffer Size"
set_parameter_property  gui_writesize   DISPLAY_UNITS       "Bytes"

add_parameter           gui_readbase    NATURAL             0
set_parameter_property  gui_readbase    DISPLAY_NAME        "Read Buffer Base"
set_parameter_property  gui_readbase    DISPLAY_HINT        "HEXADECIMAL"

add_parameter           gui_writebase   NATURAL             128
set_parameter_property  gui_writebase   DISPLAY_NAME        "Write Buffer Base"
set_parameter_property  gui_writebase   DISPLAY_HINT        "HEXADECIMAL"

add_parameter           gui_enconmem    BOOLEAN             false
set_parameter_property  gui_enconmem    DISPLAY_NAME        "Enable Continuous Memory Map"

# -----------------------------------------------------------------------------
# GUI configuration
# -----------------------------------------------------------------------------
set spiSetGroup         "SPI Settings"
add_display_item        "" ${spiSetGroup}                   GROUP
add_display_item        ${spiSetGroup}  gui_cpol            PARAMETER
add_display_item        ${spiSetGroup}  gui_cpha            PARAMETER
add_display_item        ${spiSetGroup}  gui_shiftdir        PARAMETER
add_display_item        ${spiSetGroup}  gui_regsize         PARAMETER

set bridgeSetGroup      "Bridge Settings"
add_display_item        "" ${bridgeSetGroup}                GROUP
add_display_item        ${bridgeSetGroup} gui_enconmem      PARAMETER
add_display_item        ${bridgeSetGroup} gui_readbase      PARAMETER
add_display_item        ${bridgeSetGroup} gui_readsize      PARAMETER
add_display_item        ${bridgeSetGroup} gui_writebase     PARAMETER
add_display_item        ${bridgeSetGroup} gui_writesize     PARAMETER

# -----------------------------------------------------------------------------
# callbacks
# -----------------------------------------------------------------------------
proc elaboration_callback {} {
    # -------------------------------------------------------------------------
    # predefined values

    # -------------------------------------------------------------------------
    # Process GUI checks

    # Check if continuous memory map is enabled, control read/write bases
    if {[get_parameter_value gui_enconmem]} {
        set_parameter_property gui_readbase ENABLED false
        set_parameter_property gui_writebase ENABLED false
    } else {
        set_parameter_property gui_readbase ENABLED true
        set_parameter_property gui_writebase ENABLED true
    }

    # -------------------------------------------------------------------------
    # Get SPI settings
    set_parameter_value gRegisterSize   [get_parameter_value gui_regsize]
    set_parameter_value gPolarity       [get_parameter_value gui_cpol]
    set_parameter_value gPhase          [get_parameter_value gui_cpha]
    set_parameter_value gShiftDir       [get_parameter_value gui_shiftdir]

    # -------------------------------------------------------------------------
    # Get Bridge settings
    set_parameter_value gBusAddrWidth   [get_parameter_value sys_bdgAddrw]
    set_parameter_value gBusDataWidth   [get_parameter_value sys_bdgDataw]

    # Check if continuous memory map is enabled
    if {[get_parameter_value gui_enconmem]} {
        set readBase    0
        set writeBase   [expr $readBase + [get_parameter_value gui_readsize]]
    } else {
        set readBase    [get_parameter_value gui_readbase]
        set writeBase   [get_parameter_value gui_writebase]
    }

    set_parameter_value gRdBufBase  $readBase
    set_parameter_value gRdBufSize  [get_parameter_value gui_readsize]
    set_parameter_value gWrBufBase  $writeBase
    set_parameter_value gWrBufSize  [get_parameter_value gui_writesize]
}

# -----------------------------------------------------------------------------
# connection points
# -----------------------------------------------------------------------------
# connection point r0
add_interface r0 reset end
set_interface_property r0 associatedClock c0
set_interface_property r0 synchronousEdges DEASSERT
set_interface_property r0 ENABLED true

add_interface_port r0 rsi_r0_reset reset Input 1

# connection point c0
add_interface c0 clock end
set_interface_property c0 clockRate 0
set_interface_property c0 ENABLED true

add_interface_port c0 csi_c0_clock clk Input 1

# connection point spi
add_interface spi conduit end
set_interface_property spi ENABLED true

add_interface_port spi coe_spi_clk export Input 1
add_interface_port spi coe_spi_sel_n export Input 1
add_interface_port spi coe_spi_mosi export Input 1
add_interface_port spi coe_spi_miso export Output 1

# connection point bridge
add_interface bridge avalon start
set_interface_property bridge associatedClock c0
set_interface_property bridge associatedReset r0
set_interface_property bridge burstOnBurstBoundariesOnly false
set_interface_property bridge linewrapBursts false
set_interface_property bridge ENABLED true

add_interface_port bridge avm_bridge_address address Output gbusaddrwidth
add_interface_port bridge avm_bridge_byteenable byteenable Output (gbusdatawidth/8)
add_interface_port bridge avm_bridge_write write Output 1
add_interface_port bridge avm_bridge_writedata writedata Output gbusdatawidth
add_interface_port bridge avm_bridge_read read Output 1
add_interface_port bridge avm_bridge_readdata readdata Input gbusdatawidth
add_interface_port bridge avm_bridge_waitrequest waitrequest Input 1

# connection point sync
add_interface sync interrupt start
set_interface_property sync associatedAddressablePoint bridge
set_interface_property sync associatedClock c0
set_interface_property sync associatedReset r0
set_interface_property sync irqScheme INDIVIDUAL_REQUESTS
set_interface_property sync ENABLED true

add_interface_port sync inr_sync_irq irq Input 1
