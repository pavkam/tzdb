#!/bin/bash
#
# This script pulls all the latest data from CLDR and IANA databases, builds the TZCompile project and updates the DB
# Requires a UNIX-like system with FreePascal installed.
#
# Enjoy!
#

REPO=`dirname "$0"`

if [ ! -d "$REPO/tz_database_latest" ] || [ ! -e "$REPO/cldr/windowsZones.xml" ] || [ ! -d "$REPO/src/TZDBPK" ] || [ ! -e "$REPO/src/TZCompile/TZCompile.dpr" ]; then
    echo "[ERR] Script located in '$REPO' but cannot find required sub-directories. Make sure you have full repo downloaded."
    exit 1
fi

echo "Running in '$REPO' path."
echo "Updating CLDR Windows mappings..."
wget https://raw.githubusercontent.com/unicode-org/cldr/master/common/supplemental/windowsZones.xml -q -O ./cldr/windowsZoned.xml

if [ "$?" -ne 0 ]; then
    echo "[WARN] Failed pulling down updated CLDR Windows zone information from GitHub."
fi

cat "$REPO/cldr/windowsZones.xml" | sed -n 's/<mapZone other="\(.*\)".*territory="001" type="\(.*\)"\/>/try GlobalCache.AddAlias("\1", "\2"); except end;/p' | sed "s/\"/'/g" > $REPO/src/TZCompile/WindowsTZ.inc

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to convert CLDR xml file to inc."
    exit 1
fi

ALIASES=`wc -l $REPO/src/TZCompile/WindowsTZ.inc | sed 's/\([0-9]\) .*/\1/g'`
echo "Created $ALIASES aliases."

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

rm -fr "$REPO/bin" 2> /dev/null
mkdir $REPO/bin

fpc "$REPO/src/TZCompile/TZCompile.dpr" -FEbin -FUbin
if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to compile the TZCompile program."
    exit 1
fi

$REPO/bin/TZCompile $REPO/tz_database_latest $REPO/src/TZDBPK/TZDB.inc.temp
if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to process latest TZDB data."
    exit 1
fi

rm $REPO/src/TZDBPK/TZDB.inc
mv $REPO/src/TZDBPK/TZDB.inc.temp $REPO/src/TZDBPK/TZDB.inc

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to finalize the process."
    exit 1
fi

