# -----------------------------------------------------------------------------
# mpxMaster_hw.tcl
# -----------------------------------------------------------------------------
#
#    (c) B&R, 2014
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

# -----------------------------------------------------------------------------
# PACKAGES
# -----------------------------------------------------------------------------
# Insert local packages.
source "../../common/util/tcl/ipcoreUtil.tcl"
source "../../altera/components/tcl/qsysUtil.tcl"

# Use SOPC version 10.1
package require -exact sopc 10.1

# Use package ipcoreUtil for general functions...
package require ipcoreUtil 0.0.1

# Use package qsysUtil for Qsys helpers...
package require qsysUtil 0.0.1

# -----------------------------------------------------------------------------
# module
# -----------------------------------------------------------------------------
set_module_property NAME mpxMaster
set_module_property VERSION 1.0.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property AUTHOR "B&R"
set_module_property DISPLAY_NAME "Parallel Mux AD-Bus Master"
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE false
set_module_property VALIDATION_CALLBACK validation_callback
set_module_property ELABORATION_CALLBACK elaboration_callback
set_module_property ANALYZE_HDL false
set_module_property ICON_PATH "img/br.png"

# -----------------------------------------------------------------------------
# file sets
# -----------------------------------------------------------------------------
add_fileset             QUARTUS_SYNTH QUARTUS_SYNTH fileset_callback
set_fileset_property    QUARTUS_SYNTH TOP_LEVEL     mpxMaster

# -----------------------------------------------------------------------------
# VHDL parameters
# -----------------------------------------------------------------------------
set hdlParamVisible TRUE

qsysUtil::addHdlParam  gDataWidth   NATURAL 16  $hdlParamVisible
qsysUtil::addHdlParam  gAddrWidth   NATURAL 16  $hdlParamVisible
qsysUtil::addHdlParam  gAddrLow     NATURAL 1   $hdlParamVisible
qsysUtil::addHdlParam  gAdWidth     NATURAL 1   $hdlParamVisible

# -----------------------------------------------------------------------------
# System Info parameters
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# GUI parameters
# -----------------------------------------------------------------------------
qsysUtil::addGuiParam  gui_dataWidth NATURAL 16 "Data width"    "Bits" "8 16 32"
qsysUtil::addGuiParam  gui_addrWidth NATURAL 16 "Address width" "Bits" "1:32"

# -----------------------------------------------------------------------------
# GUI configuration
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# callbacks
# -----------------------------------------------------------------------------
proc validation_callback {} {
    # Get configured width
    set dataBits  [get_parameter_value gui_dataWidth]
    set addrBits  [get_parameter_value gui_addrWidth]

    # Get data width in bytes
    set dataBytes [expr int( $dataBits / 8 )]

    # Get log2 of data bytes
    set dataBytesLog2 [expr int( log($dataBytes) / log(2) )]

    # Get max bits
    if { $dataBits > $addrBits } {
        set maxBits $dataBits
    } else {
        set maxBits $addrBits
    }

    # Assign HDL generics
    set_parameter_value gDataWidth  $dataBits
    set_parameter_value gAddrWidth  $addrBits
    set_parameter_value gAddrLow    $dataBytesLog2
    set_parameter_value gAdWidth    $maxBits
}

proc elaboration_callback {} {
}

proc fileset_callback { entityName } {
    send_message INFO "Generating entity $entityName"

    add_fileset_file "mpxMaster-rtl-ea.vhd" VHDL PATH "../../common/parallelInterface/src/mpxMaster-rtl-ea.vhd"
}

# -----------------------------------------------------------------------------
# connection points
# -----------------------------------------------------------------------------
# connection point c0
add_interface c0 clock end
set_interface_property c0 clockRate 0
set_interface_property c0 ENABLED true

add_interface_port c0 iClk clk Input 1

# connection point r0
add_interface r0 reset end
set_interface_property r0 associatedClock c0
set_interface_property r0 synchronousEdges DEASSERT
set_interface_property r0 ENABLED true

add_interface_port r0 iRst reset Input 1

# connection point s0
add_interface s0 avalon end
set_interface_property s0 addressUnits WORDS
set_interface_property s0 associatedClock c0
set_interface_property s0 associatedReset r0
set_interface_property s0 bitsPerSymbol 8
set_interface_property s0 burstOnBurstBoundariesOnly false
set_interface_property s0 burstcountUnits WORDS
set_interface_property s0 explicitAddressSpan 0
set_interface_property s0 linewrapBursts false
set_interface_property s0 maximumPendingReadTransactions 0
set_interface_property s0 ENABLED true

add_interface_port s0 iSlv_address address Input gaddrwidth-gAddrLow
add_interface_port s0 iSlv_read read Input 1
add_interface_port s0 oSlv_readdata readdata Output gdatawidth
add_interface_port s0 iSlv_write write Input 1
add_interface_port s0 iSlv_writedata writedata Input gdatawidth
add_interface_port s0 oSlv_waitrequest waitrequest Output 1
add_interface_port s0 iSlv_byteenable byteenable Input gdatawidth/8

# connection point mpx0
add_interface mpx0 conduit end
set_interface_property mpx0 associatedClock c0
set_interface_property mpx0 associatedReset r0
set_interface_property mpx0 ENABLED true

add_interface_port mpx0 oMpxMst_cs export Output 1
add_interface_port mpx0 iMpxMst_ad_i export Input gadwidth
add_interface_port mpx0 oMpxMst_ad_o export Output gadwidth
add_interface_port mpx0 oMpxMst_ad_oen export Output 1
add_interface_port mpx0 oMpxMst_be export Output gdatawidth/8
add_interface_port mpx0 oMpxMst_ale export Output 1
add_interface_port mpx0 oMpxMst_wr export Output 1
add_interface_port mpx0 oMpxMst_rd export Output 1
add_interface_port mpx0 iMpxMst_ack export Input 1