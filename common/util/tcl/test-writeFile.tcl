#!/usr/bin/tclsh
# This file tests the procedures provided in writeFile.tcl

source writeFile.tcl

# Test the writeFile_* function by creating a test header file, calling every
# function once at least. Afterwards the created file is compared with the
# golden one.
proc test_writeFile { } {
    set ret                 0
    set testFileName        "writeFile.h"
    set goldenTestFileName  "golden-writeFile.h"
    set fileId              [writeFile_open $testFileName]

    # Write the file header
    writeFile_header $fileId

    # Write some cmacros
    writeFile_cmacro $fileId "HELLO_IT" 555
    writeFile_cmacro $fileId "GO_4"     "0xC00FFEE"
    writeFile_emptyLine $fileId
    writeFile_cmacro $fileId "THIS_IS"  "TRUE"
    writeFile_cmacro $fileId "AND_NOT"  "FALSE"
    writeFile_emptyLine $fileId

    # Write some string lines
    writeFile_string $fileId "//If you read this, you have too much time!"
    writeFile_emptyLine $fileId
    writeFile_string $fileId "//Just kidding :)"

    # Close the file
    writeFile_close $fileId

    # Get lines of the files
    set goldenFile [getFileLines $goldenTestFileName]
    set testFile [getFileLines $testFileName]

    # Get the golden time stamp line to skip it
    set goldenTimeStamp [lindex $goldenFile 5]

    set ret [compFileLines $goldenFile $testFile $goldenTimeStamp]

    return $ret
}

# Procedure to open file and split into lines
proc getFileLines { fileName } {
    # open the file
    set fileId [open $fileName]

    # read the file and split line by line
    set listLines [split [read $fileId] "\n"]

    # close the file
    close $fileId

    return $listLines
}

# Procedure to compare file lines
proc compFileLines { linesA linesB skipLine } {
    set misCnt 0
    set lengthA [llength $linesA]
    set lengthB [llength $linesB]

    if { $lengthA != $lengthB } {
        return -1
    }

    for {set cnt 0} {$cnt<$lengthA} {incr cnt} {
        set lineA [lindex $linesA $cnt]
        set lineB [lindex $linesB $cnt]

        # Check if line A equals the skipLine
        if { $lineA == $skipLine } {
            # overwrite lineB
            set lineB $lineA
        }

        if { $lineA != $lineB } {
            puts "line $cnt: Mismatch!\n"
            puts "A: $lineA\n"
            puts "B: $lineB\n"
            incr misCnt
        }
    }

    return $misCnt
}

#test code
puts "--- writeFile.tcl ---"
set ret 0

puts "-> test_writeFile"
set ret [test_writeFile]
if {$ret != 0} {
    puts "failure ($ret)"
    exit $ret
} else {
    puts "success"
}

exit $ret
