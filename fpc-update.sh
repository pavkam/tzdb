#!/bin/bash
#
# This script pulls all the latest data from CLDR and IANA databases, builds the TZCompile project and updates the DB
# Requires a UNIX-like system with FreePascal installed.
#
# Enjoy!
#

REPO_DIR=`dirname "$0"`
TZ_DIR=$REPO_DIR/tz_database_latest
CLDR_DIR=$REPO_DIR/cldr
OUT_DIR=$REPO_DIR/bin

if [ ! -d "$TZ_DIR" ] || [ ! -d "$CLDR_DIR" ]; then
    echo "[ERR] Script located in '$REPO_DIR' but cannot find required sub-directories. Make sure you have full repo downloaded."
    exit 1
fi

echo "Running in '$REPO_DIR' path."

FPC_MAJOR=`fpc -iV | sed 's/\([0-9]*\)\.[0-9]*\.[0-9]*/\1/g' || 0`

if [ $FPC_MAJOR -eq 0 ]; then
    echo "[ERR] FreePascal compiler not installed."
    exit 1
fi

if [ $FPC_MAJOR -lt 3 ]; then
    echo "[ERR] Expected at least FreePascal version 3."
    exit 1
fi

echo "Found FreePascal version `fpc -iV` installed."

rm -fr "$OUT_DIR" 2> /dev/null
mkdir $OUT_DIR