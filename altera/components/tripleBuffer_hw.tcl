# -----------------------------------------------------------------------------
# tripleBuffer_hw.tcl
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
set_module_property NAME tripleBuffer
set_module_property VERSION 0.0.1
set_module_property INTERNAL false
set_module_property GROUP "Memory"
set_module_property AUTHOR "B&R"
set_module_property DISPLAY_NAME "Triple Buffer"
set_module_property TOP_LEVEL_HDL_FILE "../../altera/tripleBuffer/src/alteraTripleBufferRtl.vhd"
set_module_property TOP_LEVEL_HDL_MODULE alteraTripleBuffer
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE false
set_module_property ELABORATION_CALLBACK elaboration_callback
set_module_property ANALYZE_HDL TRUE

# -----------------------------------------------------------------------------
# file sets
# -----------------------------------------------------------------------------
add_file "../../altera/tripleBuffer/src/alteraTripleBufferRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../altera/lib/src/dpRam-rtl-a.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/lib/src/addrDecodeRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/lib/src/binaryEncoderRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/lib/src/dpRam-e.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/lib/src/edgedetectorRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/lib/src/global.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/tripleBuffer/src/tripleBridgeRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/tripleBuffer/src/tripleBufferPkg.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/tripleBuffer/src/tripleBufferRtl.vhd" {SYNTHESIS SIMULATION}
add_file "../../common/tripleBuffer/src/tripleLogicRtl.vhd" {SYNTHESIS SIMULATION}

source "../../common/tripleBuffer/tcl/calcTriBuf.tcl"

# -----------------------------------------------------------------------------
# VHDL parameters
# -----------------------------------------------------------------------------
add_parameter           gAddressWidth   NATURAL
set_parameter_property  gAddressWidth   DEFAULT_VALUE       3
set_parameter_property  gAddressWidth   TYPE                NATURAL
set_parameter_property  gAddressWidth   DERIVED             true
set_parameter_property  gAddressWidth   HDL_PARAMETER       true
set_parameter_property  gAddressWidth   AFFECTS_GENERATION  true
set_parameter_property  gAddressWidth   VISIBLE             false

add_parameter           gInputBuffers   NATURAL
set_parameter_property  gInputBuffers   DEFAULT_VALUE       2
set_parameter_property  gInputBuffers   TYPE                NATURAL
set_parameter_property  gInputBuffers   DERIVED             true
set_parameter_property  gInputBuffers   HDL_PARAMETER       true
set_parameter_property  gInputBuffers   VISIBLE             false

add_parameter           gInputBase      STRING
set_parameter_property  gInputBase      DEFAULT_VALUE       "241404"
set_parameter_property  gInputBase      TYPE                STRING
set_parameter_property  gInputBase      DERIVED             true
set_parameter_property  gInputBase      HDL_PARAMETER       true
set_parameter_property  gInputBase      VISIBLE             false

add_parameter           gTriBufOffset   STRING
set_parameter_property  gTriBufOffset   DEFAULT_VALUE       "403020201000"
set_parameter_property  gTriBufOffset   TYPE                STRING
set_parameter_property  gTriBufOffset   DERIVED             true
set_parameter_property  gTriBufOffset   HDL_PARAMETER       true
set_parameter_property  gTriBufOffset   VISIBLE             false

add_parameter           gPortAconfig    STRING
set_parameter_property  gPortAconfig    DEFAULT_VALUE       "10"
set_parameter_property  gPortAconfig    TYPE                STRING
set_parameter_property  gPortAconfig    DERIVED             true
set_parameter_property  gPortAconfig    HDL_PARAMETER       true
set_parameter_property  gPortAconfig    VISIBLE             false

add_parameter           gPortAstream    NATURAL
set_parameter_property  gPortAstream    DEFAULT_VALUE       0
set_parameter_property  gPortAstream    TYPE                NATURAL
set_parameter_property  gPortAstream    DERIVED             true
set_parameter_property  gPortAstream    HDL_PARAMETER       true
set_parameter_property  gPortAstream    VISIBLE             false

# -----------------------------------------------------------------------------
# System Info parameters
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# GUI parameters
# -----------------------------------------------------------------------------
add_parameter           gui_listSizeCon     INTEGER_LIST
set_parameter_property  gui_listSizeCon     DEFAULT_VALUE   12,16,32,36,36
set_parameter_property  gui_listSizeCon     DISPLAY_NAME    "Consumer Buffer Size"
set_parameter_property  gui_listSizeCon     UNITS           "Bytes"

add_parameter           gui_listSizePro     INTEGER_LIST
set_parameter_property  gui_listSizePro     DEFAULT_VALUE   16,32,36,36
set_parameter_property  gui_listSizePro     DISPLAY_NAME    "Producer Buffer Size"
set_parameter_property  gui_listSizePro     UNITS           "Bytes"

add_parameter           gui_enableStream    BOOLEAN
set_parameter_property  gui_enableStream    DEFAULT_VALUE   false
set_parameter_property  gui_enableStream    DISPLAY_NAME    "Stream Access at Port A"

# -----------------------------------------------------------------------------
# GUI configuration
# -----------------------------------------------------------------------------
add_display_item        "" tableCon                         GROUP TABLE
add_display_item        tableCon gui_listSizeCon            PARAMETER

add_display_item        "" tablePro                         GROUP TABLE
add_display_item        tablePro gui_listSizePro            PARAMETER

# -----------------------------------------------------------------------------
# callbacks
# -----------------------------------------------------------------------------
proc elaboration_callback {} {
    # -------------------------------------------------------------------------
    # predefined values

    # Consumer ack base address
    set ackRegSize  4
    set roleConVal  0
    set roleProVal  1

    # -------------------------------------------------------------------------
    # Process GUI checks

    # Check if port a stream option is enabled
    if {[get_parameter_value gui_enableStream]} {
        set portAstream 1
    } else {
        set portAstream 0
    }

    # -------------------------------------------------------------------------
    # Process GUI tables

    # Get table values as lists
    set bufSizeCon [procBufferSize gui_listSizeCon]
    set bufSizePro [procBufferSize gui_listSizePro]

    # Get number of consumer and producer buffers
    set numCon [llength $bufSizeCon]
    set numPro [llength $bufSizePro]

    # Concatenate consumer & producer list
    set bufSize [concat $bufSizeCon $bufSizePro]

    # Get total number of buffer (=gInputBuffers)
    set num [llength $bufSize]

    # -------------------------------------------------------------------------
    # Get Input Memory Map

    # Get input memory map (add size of consumer ack register!)
    set inMemMap [getMemoryMap $ackRegSize $bufSize]

    # Get offsets of input memory map (=CMACRO TBUF_OFFSET)
    set inMemMapOffset [lreplace $inMemMap end ""]

    # Reverse input memory map
    set inMemMap_rev [lsort -integer -decreasing $inMemMap]

    # Set base of consumer and producer ack register
    set baseConAck  0
    set baseProAck  [lindex $inMemMap end]

    # Get memory span
    set inMemSpan   [expr $baseProAck + $ackRegSize]

    # Get log2 of memory span (=gAddressWidth)
    set log2inMemSpan [expr int(ceil(log($inMemSpan)/log(2)))]

    # Convert inMemMap into hex values
    set pos         [expr int(ceil(double($log2inMemSpan)/4))]
    set inMemMap_revHex [convDecToHex $inMemMap_rev $pos]

    # "Convert" list into string and omit spaces (=gInputBase)
    set inMemMap_string [getStringStream $inMemMap_revHex]

    # -------------------------------------------------------------------------
    # Get Triple Buffer Memory Map
    set triMem [calcTriBufOffset $bufSize]

    # Reverse Triple Buffer Memory Map
    set triMem_rev [lsort -integer -decreasing $triMem]

    # Get Triple Buffer Memory Map span
    set triMemSpan   [lindex $triMem end]

    # Get log2 of memory span
    set log2triMemSpan [expr int(ceil(log($triMemSpan)/log(2)))]

    # Convert Triple Buffer Memory Map into hex values
    set pos         [expr int(ceil(double($log2triMemSpan)/4))]
    set triMem_revHex [convDecToHex $triMem_rev $pos]

    # "Convert" list into string and omit spaces (=gTriBufOffset)
    set triMem_string [getStringStream $triMem_revHex]

    # -------------------------------------------------------------------------
    # Get Producer/Consumer
    set lstIsPro_rev ""

    # Set is producer
    for {set i 0} {$i<$numPro} {incr i} {
        set lstIsPro_rev [concat $lstIsPro_rev $roleProVal]
    }

    # Set is not producer (=consumer)
    for {set i 0} {$i<$numCon} {incr i} {
        set lstIsPro_rev [concat $lstIsPro_rev $roleConVal]
    }

    # Get re-reversed
    set lstIsPro [lsort -integer -increasing $lstIsPro_rev]

    # Convert to string (=gPortAconfig)
    set isPro_string [getStringStream $lstIsPro_rev]

    # -------------------------------------------------------------------------
    # Forward VHDL Generics

    set_parameter_value gInputBuffers   $num
    set_parameter_value gAddressWidth   $log2inMemSpan
    set_parameter_value gInputBase      $inMemMap_string
    set_parameter_value gTriBufOffset   $triMem_string
    set_parameter_value gPortAconfig    $isPro_string
    set_parameter_value gPortAstream    $portAstream

    # -------------------------------------------------------------------------
    # Forward C Macros
    setListCmacro   "TBUF_SIZE"             $bufSize
    setListCmacro   "TBUF_OFFSET"           $inMemMapOffset
    setValCmacro    "TBUF_OFFSET_CONACK"    $baseConAck
    setValCmacro    "TBUF_SIZE_CONACK"      $ackRegSize
    setValCmacro    "TBUF_OFFSET_PROACK"    $baseProAck
    setValCmacro    "TBUF_SIZE_PROACK"      $ackRegSize
    setListCmacro   "TBUF_PORTA_ISPRODUCER" $lstIsPro
    setValCmacro    "TBUF_NUM_CON"          $numCon
    setValCmacro    "TBUF_NUM_PRO"          $numPro
}

# -----------------------------------------------------------------------------
# internal functions
# -----------------------------------------------------------------------------
# Process buffer sizes out of GUI tables
proc procBufferSize { tab } {
    # get buffer size list
    set lst         [split [get_parameter_value $tab] ","]
    set alignment   4

    # check buffer sizes for alignment
    if {[checkSizeAlign $lst $alignment] != 0} {
        send_message error "Alignment error in Table \"[get_parameter_property $tab DISPLAY_NAME]\""
    }

    return $lst
}

# Check size list for alignment
proc checkSizeAlign { lst alnm } {
    foreach val $lst {
        if {[expr $val % $alnm] != 0} {
            return -1
        }
    }
    return 0
}

# Get memory map of provided buffer sizes
proc getMemoryMap { base lstSize } {
    set lstBase $base
    set accu    $base

    foreach size $lstSize {
        set lstBase [concat $lstBase [expr $accu + $size]]
        set accu [expr $accu + $size]
    }

    return $lstBase
}

# Convert list values into hex
proc convDecToHex { lst pos } {
    set tmp ""

    foreach i $lst {
        set tmp [concat $tmp [format %0${pos}X $i]]
    }

    return $tmp
}

# Get string stream from list
proc getStringStream { lst } {
    set tmp ""

    foreach i $lst {
        set tmp [format "%s%s" $tmp $i]
    }

    return $tmp
}

# Set value as CMACRO
proc setValCmacro { name val } {
    set_module_assignment embeddedsw.CMacro.${name}             $val
}

# Set list as CMACRO
proc setListCmacro { name lst } {
    set cnt 0

    foreach i $lst {
        set_module_assignment embeddedsw.CMacro.${name}${cnt}   $i
        incr cnt
    }
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

# connection point porta
add_interface porta avalon end
set_interface_property porta addressAlignment DYNAMIC
set_interface_property porta addressUnits WORDS
set_interface_property porta associatedClock c0
set_interface_property porta associatedReset r0
set_interface_property porta burstOnBurstBoundariesOnly false
set_interface_property porta explicitAddressSpan 0
set_interface_property porta holdTime 0
set_interface_property porta isMemoryDevice false
set_interface_property porta isNonVolatileStorage false
set_interface_property porta linewrapBursts false
set_interface_property porta maximumPendingReadTransactions 0
set_interface_property porta printableDevice false
set_interface_property porta readLatency 0
set_interface_property porta readWaitTime 1
set_interface_property porta setupTime 0
set_interface_property porta timingUnits Cycles
set_interface_property porta writeWaitTime 0
set_interface_property porta ENABLED true

add_interface_port porta avs_porta_address address Input "(gAddressWidth) - 2"
add_interface_port porta avs_porta_byteenable byteenable Input 4
add_interface_port porta avs_porta_write write Input 1
add_interface_port porta avs_porta_read read Input 1
add_interface_port porta avs_porta_writedata writedata Input 32
add_interface_port porta avs_porta_readdata readdata Output 32
add_interface_port porta avs_porta_waitrequest waitrequest Output 1

# connection point portb
add_interface portb avalon end
set_interface_property portb addressAlignment DYNAMIC
set_interface_property portb addressUnits WORDS
set_interface_property portb associatedClock c0
set_interface_property portb associatedReset r0
set_interface_property portb burstOnBurstBoundariesOnly false
set_interface_property portb explicitAddressSpan 0
set_interface_property portb holdTime 0
set_interface_property portb isMemoryDevice false
set_interface_property portb isNonVolatileStorage false
set_interface_property portb linewrapBursts false
set_interface_property portb maximumPendingReadTransactions 0
set_interface_property portb printableDevice false
set_interface_property portb readLatency 0
set_interface_property portb readWaitTime 1
set_interface_property portb setupTime 0
set_interface_property portb timingUnits Cycles
set_interface_property portb writeWaitTime 0
set_interface_property portb ENABLED true

add_interface_port portb avs_portb_address address Input "(gAddressWidth) - 2"
add_interface_port portb avs_portb_byteenable byteenable Input 4
add_interface_port portb avs_portb_write write Input 1
add_interface_port portb avs_portb_read read Input 1
add_interface_port portb avs_portb_writedata writedata Input 32
add_interface_port portb avs_portb_readdata readdata Output 32
add_interface_port portb avs_portb_waitrequest waitrequest Output 1
