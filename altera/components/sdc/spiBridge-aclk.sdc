###############################################################################
# SPI Bridge Timing Constraints for
#  Asynchronous Clock Architecture
###############################################################################

###############################################################################
# I/O MIN/MAX DELAY [ns]
set spi_in_max      10.0
set spi_in_min      1.0
set spi_out_max     10.0
set spi_out_min     0.0

###############################################################################
# SPI CLOCK RATE
set spi_clk_rate    12.5MHz

###############################################################################
# HIERARCHY
set instSpiBridge   *alteraSpiBridge
set instSpiCore     $instSpiBridge*spiSlave

###############################################################################
# REGISTERS

## Capture register
set reg_spiCap [get_registers ${instSpiCore}*spiCap]

## Output register is the last shift register ff
    set instSpiReg "${instSpiCore}*spiReg"
    set spiRegCnt 0
    foreach_in_collection reg [get_registers ${instSpiReg}[?]] {
        incr spiRegCnt
    }
    set spiRegCnt [expr $spiRegCnt - 1]
set reg_spiOut [get_registers "${instSpiReg}[${spiRegCnt}]"]

###############################################################################
# PINS

## Spi select
set pin_sel [get_pins -hierarchical *spiCap|ena]

###############################################################################
# CLK

## Get driving clock of spi capture register, which is the spi clk...
set fanins [get_fanins $reg_spiCap -clock]

## Create for every found fanin clock a clock
foreach_in_collection fanin_keeper $fanins {
    set clk [get_node_info $fanin_keeper -name]
    create_clock -period $spi_clk_rate -name spi_clk [get_ports $clk]
}

###############################################################################
# SETUP / HOLD Timing for inputs

## MOSI
set_max_delay -from [get_ports *] -to ${reg_spiCap} $spi_in_max
set_min_delay -from [get_ports *] -to ${reg_spiCap} $spi_in_min

###############################################################################
# CLOCK TO OUTPUT Timing for outputs

## MISO
set_max_delay -from ${reg_spiOut} -to [get_ports *] $spi_out_max
set_min_delay -from ${reg_spiOut} -to [get_ports *] $spi_out_min

###############################################################################
# TIMING IGNORE

## nSEL
set_false_path -from [get_ports *] -to ${pin_sel}

###############################################################################
# CLOCK GROUPS
set_clock_groups -asynchronous -group spi_clk
