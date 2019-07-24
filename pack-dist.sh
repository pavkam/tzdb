#!/bin/bash
#
# Packs TZDB into the distribution directory and kame an "unified" unit file for easy distribution.
#

cleanup () {
  rm -fr $REPO/xaa 2> /dev/null
  rm -fr $REPO/xab 2> /dev/null
  rm -fr $REPO/xac 2> /dev/null
  rm -fr $REPO/xad 2> /dev/null
}

REPO=`dirname "$0"`

if [ ! -d "$REPO/src/TZDBPK" ]; then
    echo "[ERR] Script located in '$REPO' but cannot find required sub-directories. Make sure you have full repo downloaded."
    exit 1
fi

echo "Running in '$REPO' path."

rm -fr $REPO/dist 2> /dev/null
mkdir $REPO/dist

# Split the file into pieces based in includes .
split -p '\{\$INCLUDE.*\}' $REPO/src/TZDBPK/TZDB.pas
if [ "$?" -ne 0 ] || [ ! -e "$REPO/xaa" ] || [ ! -e "$REPO/xab" ] || [ ! -e "$REPO/xac" ] || [ -e "$REPO/xad" ]; then
    cleanup

    echo "[ERR] Failed to split the TZDB unit file into chunks."
    exit 1
fi

# We have three chunks in here. Assemble them into final file.
cat $REPO/src/TZDBPK/Version.inc > $REPO/dist/TZDB.pas
cat $REPO/xab | sed "s/{\$INCLUDE.*}//g" >> $REPO/dist/TZDB.pas
cat $REPO/src/TZDBPK/TZDB.inc >> $REPO/dist/TZDB.pas
cat $REPO/xac | sed "s/{\$INCLUDE.*}//g" >> $REPO/dist/TZDB.pas

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to build a packaged unit file."
    exit 1
fi

cleanup

echo "The process has finished! Whoop Whoop!"
