#!/bin/bash
# Compile all needed libraries.

DIR_TOOLS=tools

#store current path
export ORIGIN_DIR=.

echo
echo "Compile libraries..."
echo

LIBST_LIST="\
common/lib/sh/libcommon.settings \
common/util/sh/libutil.settings \
"

RET=1
for LIBSET in $LIBST_LIST
do
    echo "###############################################################################"
    echo "# Compile library $LIBSET"

    chmod +x ./${DIR_TOOLS}/msim-vcomLib.sh
    ./${DIR_TOOLS}/msim-vcomLib.sh ${LIBSET}
    RET=$?

    #check return
    if [ $RET -ne 0 ]; then
        echo "-> ERROR!"
        break
    else
        echo "-> SUCCESSFUL!"
    fi

    echo "###############################################################################"
    echo
done

exit $RET
