#!/usr/bin/tclsh
# This file provides functions to calculate the triple buffer memory mapping.

# Gets the internal triple buffer memory mapping (translation value in LUT)
proc calcTriBufOffset { listSize } {
    set listTriBase ""
    set accu 0
    set baseOffset 0

    foreach mSize $listSize {
        for {set j 0} {$j<3} {incr j} {
            set tmp [expr $accu - $baseOffset]
            set listTriBase [concat $listTriBase $tmp]
            set accu [expr $accu + $mSize]
        }
        set baseOffset [expr $baseOffset + $mSize]
    }

    return $listTriBase
}
