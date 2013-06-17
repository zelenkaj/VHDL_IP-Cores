#!/bin/bash
# Runs all available shell scripts that start with "tb".
# The compilation/simulation results are copied to results directory.

echo
echo "Starting simulation of all available shell-scripts..."
echo

#store current path
export ORIGIN_DIR=.

#find all shell scripts starting with "tb" and store in list
TBSH_LIST=`find $ORIGIN_DIR -name "tb*.sh"`

#loop through tb*.sh list
for TBSH in $TBSH_LIST
do
    TBSH_DIR=`dirname $TBSH`
    TBSH_SRC=`basename $TBSH`

    #change to directory of current tb*.sh
    pushd $TBSH_DIR

    #run sh
    chmod +x $TBSH_SRC
    ./$TBSH_SRC

    #store return
    RET=$?

    popd

    #copy work to results
    mkdir results -p
    mv $TBSH_DIR/_out_* ./results -fu

    #check return
    if [ $RET -ne 0 ]; then
        exit 1
    fi
done

exit 0
