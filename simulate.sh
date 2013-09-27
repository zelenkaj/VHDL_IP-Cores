#!/bin/bash
# Runs all available shell scripts that start with "tb".
# The compilation/simulation results are copied to results directory.
# The script provides it's runtime after completion.

DIR_RESULTS=results

#Get start time.
TIME_START=$(date +"%s")

#Kill results directory
rm ${DIR_RESULTS} -rf

echo
echo "Starting simulation of all available shell-scripts..."
echo

#store current path
export ORIGIN_DIR=.

#find all shell scripts starting with "tb" and store in list
TBSH_LIST=`find $ORIGIN_DIR -name "tb*.sh"`

#loop through tb*.sh list
RET=1
for TBSH in $TBSH_LIST
do
    TBSH_DIR=`dirname $TBSH`
    TBSH_SRC=`basename $TBSH`

    echo "Run testbench in path ${TBSH}..."

    #change to directory of current tb*.sh
    pushd $TBSH_DIR >> /dev/null

    #run sh
    chmod +x $TBSH_SRC
    ./$TBSH_SRC >> /dev/null

    #store return
    RET=$?

    popd >> /dev/null

    #copy work to results
    mkdir ${DIR_RESULTS} -p
    mv $TBSH_DIR/_out_* ./${DIR_RESULTS}

    #check return
    if [ $RET -ne 0 ]; then
        echo "-> ERROR!"
        #Print transcript of faulty simulation
        echo
        echo "###############################################################################"
        echo "### TRANSCRIPT (${TBSH_DIR}/transcript)"
        pushd $TBSH_DIR >> /dev/null
        cat transcript
        popd >> /dev/null
        echo "###############################################################################"
        break
    else
        echo "-> SUCCESSFUL!"
    fi
    echo
done

#Get completion time, calculate duration time and give seconds in time format.
TIME_COMPLETE=$(date +"%s")
SEC_DUR=$(( $TIME_COMPLETE - $TIME_START ))
TIME_DUR=$(date -u -d @${SEC_DUR} +"%T")

echo
if [ $RET -ne 0 ]; then
    printf "Simulation completed with errors! (RET=${RET} "
else
    printf "Simulation completed successful! ("
fi

echo "Runtime=${TIME_DUR})"

exit 0
