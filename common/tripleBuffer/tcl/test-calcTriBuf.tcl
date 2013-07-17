#!/usr/bin/tclsh
# This file tests the procedures provided in calcTriBuf.tcl

source calcTriBuf.tcl

# Test calcTriBufOffset results:
# * compare function result with golden list
# * check if address translation generates continuous access
proc test_calcTriBufOffset { } {
    set listSize [list 24 4 16 32]
    set listTriBase_Gold [list 0 24 48 48 52 56 56 72 88 88 120 152]

    set listTriBase [calcTriBufOffset $listSize]

    if {$listTriBase != $listTriBase_Gold} {
        return 1
    }

    #check if the translation generates continuous access
    set j 0
    set k 0
    set addrStart 0
    set lastTranAddr 0
    foreach n $listSize  {
        #each buffer is checked by considering each triple buffer
        for {set i 0} {$i<3} {incr i} {
            set offset [lindex $listTriBase $j]
            set size [lindex $listSize $k]

            for {set addr $addrStart} {$addr<[expr $n + $addrStart]} {incr addr} {
                #obtain translated address by adding the LUT offset to addr
                set tranAddr [expr $addr + $offset]

                #check if the last and the current translated address differ by
                # one (except very first translation)
                if {$lastTranAddr > 0} {
                    if {[expr $tranAddr - 1] != $lastTranAddr} {
                        return 2
                    }
                }
                set lastTranAddr $tranAddr
            }
            incr j
        }
        set addrStart [expr $addrStart + $n]
        incr k
    }

    return 0
}

#test code
puts "--- calcTriBuf.tcl ---"
set ret 0

puts "-> test_calcTriBufOffset"
set ret [test_calcTriBufOffset]
if {$ret != 0} {
    puts "failure ($ret)"
    exit $ret
} else {
    puts "success"
}

exit $ret
