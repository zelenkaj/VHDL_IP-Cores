#!/usr/bin/tclsh
# This file provides the generation callback invoked by nios2-bsp generator.

# Generation callback for nios2-bsp
proc generationCallback { instName tgtDir bspDir } {
    set fileName        "appif-cfg.h"
    set listCmacro_off  [list   "TBUF_OFFSET_CONACK" \
                                "TBUF_OFFSET" \
                                "TBUF_OFFSET_PROACK" ]
    set listCmacro_siz  [list   "TBUF_SIZE_CONACK" \
                                "TBUF_SIZE" \
                                "TBUF_SIZE_PROACK" ]
    set listCmacro_oth  [list   "TBUF_NUM_CON" "TBUF_NUM_PRO" \
                                "TBUF_PORTA_ISPRODUCER"]

    # Get path of this file
    set thisFileLoc [pwd]

    # Get writeFile_* functions by relative path
    source "${thisFileLoc}/../../../common/util/tcl/writeFile.tcl"

    puts ""
    puts "***********************************************************"
    puts ""
    puts " Create ${fileName} with settings from ${instName} ..."
    puts ""

    # Get all cmacro not offset or size names
    set listCmacroName_oth ""
    foreach cmacro $listCmacro_oth {
        set listCmacroName_oth [concat $listCmacroName_oth [getAllCmacros $cmacro]]
    }

    # Get all values
    set listCmacroValue_oth [getCmacroValues $listCmacroName_oth]

    # Get all cmacro offset names
    set listCmacroName_off ""
    foreach cmacro $listCmacro_off {
        set listCmacroName_off [concat $listCmacroName_off [getAllCmacros $cmacro]]
    }

    # Get all offset values
    set listCmacroValue_off [getCmacroValues $listCmacroName_off]

    # Get all cmacro size names
    set listCmacroName_siz ""
    foreach cmacro $listCmacro_siz {
        set listCmacroName_siz [concat $listCmacroName_siz [getAllCmacros $cmacro]]
    }

    # Get all size values
    set listCmacroValue_siz [getCmacroValues $listCmacroName_siz]

    # Open file in target directory
    set fid [writeFile_open "${tgtDir}/${fileName}"]

    # Write header
    writeFile_header $fid

    # Write IFNDEF stuff
    writeFile_emptyLine $fid
    writeFile_string $fid "#ifndef __APPIF_CFG_H__"
    writeFile_string $fid "#define __APPIF_CFG_H__"
    writeFile_emptyLine $fid

    # Write offset/size pairs
    foreach off_name $listCmacroName_off off_val $listCmacroValue_off \
            siz_name $listCmacroName_siz size_val $listCmacroValue_siz {
        writeFile_cmacro $fid $off_name $off_val
        writeFile_cmacro $fid $siz_name $size_val
        writeFile_emptyLine $fid
    }

    # Write other macros
    foreach cmacro $listCmacroName_oth value $listCmacroValue_oth {
        writeFile_cmacro $fid $cmacro $value
    }
    writeFile_emptyLine $fid

    # Write TBUF initialization vector
    writeFile_string $fid "#define TBUF_INIT_VEC { \\"

    # Get number of buffer, to know the vector length
    set numOfBuf [llength $listCmacroName_off]

    set cnt 0
    foreach off_name $listCmacroName_off siz_name $listCmacroName_siz {
        set tmpString "                        "
        set tmpString "${tmpString}{ ${off_name}, ${siz_name} }"

        incr cnt

        if { $cnt < $numOfBuf } {
            set tmpString "${tmpString}, "
        }

        set tmpString "${tmpString} \\"

        writeFile_string $fid $tmpString
    }

    writeFile_string $fid "                      }"

    writeFile_emptyLine $fid
    writeFile_string $fid "#endif"
    writeFile_close $fid

    puts "***********************************************************"
    puts ""

}

# This procedure tries to find the corresponding cmacros in sopcinfo.
# It starts without counting value and afterwards with counting value 0.
# Hence, the following cmacros are found (e.g.):
# THIS_IS_SOMETHING
# or
# THIS_IS_COUNTING0 THIS_IS_COUNTING1 ...
# or
# THIS_IS_ANY THIS_IS_ANY0 THIS_IS_ANY1 ...
#
# Note that cmacros that do not start with 0 are not found!
proc getAllCmacros { cmacro } {
    set listCmacro ""
    set cnt 0

    # Try without counting value
    if { [get_module_assignment embeddedsw.CMacro.$cmacro] != "" } {
        set listCmacro [concat $listCmacro $cmacro]
    }

    # Now try with counting value
    while {1} {
        set tmp ${cmacro}${cnt}
        if { [get_module_assignment embeddedsw.CMacro.$tmp] != "" } {
            set listCmacro [concat $listCmacro $tmp]
        } else {
            return $listCmacro
        }

        incr cnt
    }
}

# This procedure gets the values of a list of cmacros
proc getCmacroValues { listCmacros } {
    set tmp ""
    foreach cmacro $listCmacros {
        set tmp [concat $tmp [get_module_assignment embeddedsw.CMacro.$cmacro]]
    }

    return $tmp
}
