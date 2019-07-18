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
echo "Pulling the latest CLDR data from GitHub..."
wget https://raw.githubusercontent.com/unicode-org/cldr/master/common/supplemental/windowsZones.xml -q -O ./cldr/windowsZones.xml

if [ "$?" -ne 0 ]; then
    echo "[WARN] Failed pulling down updated CLDR Windows zone information from GitHub."
fi

echo "Converting the latest CLDR xml file to inc..."
cat $REPO/cldr/windowsZones.xml | sed -n 's/<mapZone other="\(.*\)".*territory="001" type="\(.*\)"\/>/GlobalCache.AddAlias("\1", "\2");/p' | sed "s/\"/'/g" > $REPO/src/TZCompile/WindowsTZ.inc

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

rm -fr $REPO/bin 2> /dev/null
mkdir $REPO/bin

fpc $REPO/src/TZCompile/TZCompile.dpr -FEbin -FUbin
if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to compile the TZCompile program."
    exit 1
fi

echo "Pulling the latest TZDB database from IANA ..."

rm -rf $REPO/iana_temp 2> /dev/null
wget -q https://www.iana.org/time-zones/repository/tzdata-latest.tar.gz
mkdir $REPO/iana_temp
tar -xf tzdata-latest.tar.gz -C $REPO/iana_temp
if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to pull the latest TZDB tar ball."
    exit 1;
fi

rm tzdata-latest.tar.gz

IANAV=`cat $REPO/iana_temp/version`
echo "Current TZDB database version is v$IANAV."
FILES=( africa antarctica asia australasia backward backzone etcetera europe northamerica pacificnew southamerica systemv )
for fn in "${FILES[@]}"; do
    echo "Replacing file $fn ..."
    cp $REPO/iana_temp/$fn $REPO/tz_database_latest/$fn
done

rm -rf $REPO/iana_temp

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to replace required TZ files from the IANA archive."
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

echo "Updating README with the new version..."
cat $REPO/README.md | sed "s/\(.*\*\*\)[0-9]*[a-z]*\(\*\*.*\)/\1$IANAV\2/g" > $REPO/README.md.tmp
rm $REPO/README.md
mv $REPO/README.md.tmp $REPO/README.md

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to update README.md with the newest DB version."
    exit 1
fi

echo "The process has finished! Whoop Whoop!"