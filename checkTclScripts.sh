#!/bin/bash
# Runs tcl scripts provided in TCL_LIST

echo
echo "Starting TCL script checks..."
echo

#store current path
export ORIGIN_DIR=.

#find all tcl scripts starting with "test" and store in list
TCL_LIST=`find $ORIGIN_DIR -name "test-*.tcl"`

#loop through list
for TCL in $TCL_LIST
do
    TCL_DIR=`dirname $TCL`
    TCL_SRC=`basename $TCL`

    #change to directoy
    pushd $TCL_DIR

    tclsh $TCL_SRC
    RET=$?

    popd

    if [ $RET -ne 0 ]; then
        exit $RET
    fi
done

exit 0
