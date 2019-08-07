#!/bin/bash
#
# This script pulls all the latest data from CLDR and IANA databases, builds the TZCompile project and updates the DB
# Requires a UNIX-like system with FreePascal installed.
#
# Enjoy!
#

REPO=`dirname "$0"`
echo "Running in '$REPO' path."

TZDB_PAS=$REPO/src/TZDBPK/TZDB.pas

if [ "$1" != "" ]; then
  INPUT_VER=$1
else
  INPUT_VER=`cat $TZDB_PAS | sed -n "s/.*CComponentVersion\ *=\ *'\(.*\)';.*/\1/p"`
fi

IFS='.'; DOT_ARR=($INPUT_VER); unset IFS;
VER_0=${DOT_ARR[0]}
VER_1=${DOT_ARR[1]}
VER_2=${DOT_ARR[2]}
VER_3=${DOT_ARR[3]}

if [[ $VER_0 =~ ^[0-9]+$ ]] && [[ $VER_1 =~ ^[0-9]+$ ]] && [[ $VER_2 =~ ^[0-9]+$ ]] && [[ $VER_3 =~ ^[0-9]+$ ]]; then
  if [ "$1" == "" ]; then
    # increment the build number
    ((VER_3++))
  fi
  echo "Will bump the version of the project to $VER_0.$VER_1.$VER_2.$VER_3."
else
  echo "[ERR] Invalid version info provided: $1. Expected 'n.n.n.n' format."
  exit 1
fi

if [ ! -d "$REPO/tz_database_latest" ] || [ ! -e "$REPO/cldr/windowsZones.xml" ] || [ ! -d "$REPO/src/TZDBPK" ] || [ ! -e "$REPO/src/TZCompile/TZCompile.dpr" ]; then
    echo "[ERR] Script located in '$REPO' but cannot find required sub-directories. Make sure you have full repo downloaded."
    exit 1
fi

echo "Pulling the latest CLDR data from GitHub..."

CLDR_XML=$REPO/cldr/windowsZones.xml
wget https://raw.githubusercontent.com/unicode-org/cldr/master/common/supplemental/windowsZones.xml -q -O $CLDR_XML.tmp
if [ "$?" -ne 0 ]; then
    echo "[WARN] Failed pulling down updated CLDR Windows zone information from GitHub."
    rm $CLDR_XML.tmp
else
    rm $CLDR_XML
    mv $CLDR_XML.tmp $CLDR_XML
fi

echo "Converting the latest CLDR xml file to inc..."
CLDR_INC=$REPO/src/TZCompile/WindowsTZ.inc
cat $CLDR_XML | sed -n 's/<mapZone other="\(.*\)".*territory="001" type="\(.*\)"\/>/GlobalCache.AddAlias("\1", "\2");/p' | sed "s/\"/'/g" > $CLDR_INC.tmp

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to convert CLDR xml file to inc."
    exit 1
fi

rm $CLDR_INC
mv $CLDR_INC.tmp $CLDR_INC

ALIASES=`wc -l $CLDR_INC | sed 's/\([0-9]\) .*/\1/g'`
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
FILES=( africa antarctica asia australasia backward backzone etcetera europe factory northamerica pacificnew southamerica systemv )
for fn in "${FILES[@]}"; do
    echo "Replacing file $fn ..."
    cp $REPO/iana_temp/$fn $REPO/tz_database_latest/$fn
done

rm -rf $REPO/iana_temp

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to replace required TZ files from the IANA archive."
    exit 1
fi

# Compile the IANA DB

TZDB_INC=$REPO/src/TZDBPK/TZDB.inc
$REPO/bin/TZCompile $REPO/tz_database_latest $TZDB_INC.temp $IANAV
if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to process latest TZDB data."
    exit 1
fi

rm $TZDB_INC
mv $TZDB_INC.temp $TZDB_INC

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to finalize the process."
    exit 1
fi

# Run tests

fpc $REPO/src/TZTest/TZTest.dpr -Fu$REPO/src/TZDBPK -Fu$REPO/src/TZDBLIB -FEbin -FUbin
if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to compile the TZTest program."
    exit 1
fi

$REPO/bin/TZTest --format=plain --all --progress --sparse
if [ "$?" -ne 0 ]; then
    echo "[ERR] Tests failed! Please correct the errors before continuing."
    exit 1
fi

# Update the README

echo "Updating README with the new version '$IANAV'..."
README=$REPO/README.md
cat $README | sed "s/\(.*\*\*\)[0-9]*[a-z]*\(\*\*.*\)/\1$IANAV\2/g" > $README.tmp
if [ "$?" -ne 0 ]; then
  echo "[ERR] Failed to update README.md file with the IANA DB version."
  exit 1
fi

rm $README
mv $README.tmp $README

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to finalize the update of README."
    exit 1
fi

# Bump the version

replace_tokens () {
  cat $1 | sed "s/$2/\1$3\2/g" > $1.tmp
  if [ "$?" -ne 0 ]; then
      exit 1
  fi
  rm $1
  mv $1.tmp $1
  if [ "$?" -ne 0 ]; then
      exit 1
  fi
}

VER_MAJ="$VER_0.$VER_1"
VER_FULL="$VER_0.$VER_1.$VER_2.$VER_3"

DPROJ_FILES=`find $REPO -type f | grep .dproj`
for DPROJ in $DPROJ_FILES; do
  echo "Bumping the version of file '$DPROJ' to '$VER_FULL'..."
  cp $DPROJ $DPROJ.1

  replace_tokens $DPROJ.1 '\(<VerInfo_Keys>.*FileVersion=\)[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*\(.*<\/VerInfo_Keys>\)' $VER_FULL
  replace_tokens $DPROJ.1 '\(<VerInfo_Keys>.*ProductVersion=\)[0-9]*\.[0-9]*\(.*<\/VerInfo_Keys>\)' $VER_MAJ
  replace_tokens $DPROJ.1 '\(.*<VersionInfoKeys Name="FileVersion">\)[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*\(<\/VersionInfoKeys>\)' $VER_FULL
  replace_tokens $DPROJ.1 '\(.*<VersionInfoKeys Name="ProductVersion">\)[0-9]*\.[0-9]*\(<\/VersionInfoKeys>\)' $VER_MAJ
  replace_tokens $DPROJ.1 '\(.*<VersionInfo Name="MajorVer">\)[0-9]*\(<\/VersionInfo>\)' $VER_0
  replace_tokens $DPROJ.1 '\(.*<VersionInfo Name="MinorVer">\)[0-9]*\(<\/VersionInfo>\)' $VER_1
  replace_tokens $DPROJ.1 '\(.*<VersionInfo Name="Release">\)[0-9]*\(<\/VersionInfo>\)' $VER_2
  replace_tokens $DPROJ.1 '\(.*<VersionInfo Name="Build">\)[0-9]*\(<\/VersionInfo>\)' $VER_3
  replace_tokens $DPROJ.1 '\(.*<VerInfo_MajorVer>\)[0-9]*\(<\/VerInfo_MajorVer>\)' $VER_0
  replace_tokens $DPROJ.1 '\(.*<VerInfo_MinorVer>\)[0-9]*\(<\/VerInfo_MinorVer>\)' $VER_1
  replace_tokens $DPROJ.1 '\(.*<VerInfo_Release>\)[0-9]*\(<\/VerInfo_Release>\)' $VER_2
  replace_tokens $DPROJ.1 '\(.*<VerInfo_Build>\)[0-9]*\(<\/VerInfo_Build>\)' $VER_3

  if [ "$?" -ne 0 ]; then
      echo "[ERR] Failed to bump versions in file '$DPROJ'!"
      exit 1
  fi

  rm $DPROJ
  mv $DPROJ.1 $DPROJ

  if [ "$?" -ne 0 ]; then
      echo "[ERR] Failed to bump versions in file '$DPROJ'!"
      exit 1
  fi
done

# update the version in the .pas module as well
cat $TZDB_PAS | sed "s/\(.*CComponentVersion\ *=\ *'\).*\(';.*\)/\1$VER_FULL\2/g" > $TZDB_PAS.tmp
if [ "$?" -ne 0 ]; then
  echo "[ERR] Failed to update TZDB.pas file with the bumped version."
  exit 1
fi
rm $TZDB_PAS
mv $TZDB_PAS.tmp $TZDB_PAS

# Merge the files into one
DIST=$REPO/dist
if [ ! -d "$DIST" ]; then
    echo "[WARN] Unable to locate the distribution dorectory. Skipping the last step."
    echo "DONE!"
    exit 0
fi

echo "Merging the TZDB components into one source file..."

TZDB_PAS_DIST=$DIST/TZDB.pas
rm $TZDB_PAS_DIST 2> /dev/null
cp $REPO/media/logo-64x64.ico $DIST/TZDB.ico
cat $REPO/src/TZDBPK/TZDBPK.dpk | sed 's/TZDBPK/TZDB/g' > $DIST/TZDB.dpk 

cleanup () {
  rm -fr $REPO/xx00 2> /dev/null
  rm -fr $REPO/xx01 2> /dev/null
  rm -fr $REPO/xx02 2> /dev/null
}

# Split the file into pieces based in includes .
csplit -s $TZDB_PAS '/{\$INCLUDE.*}/' {1} 2> /dev/null
if [ "$?" -ne 0 ] || [ ! -e "$REPO/xx00" ] || [ ! -e "$REPO/xx00" ] || [ ! -e "$REPO/xx00" ]; then
    cleanup

    echo "[ERR] Failed to split the TZDB unit file into chunks."
    exit 1
fi

# We have three chunks in here. Assemble them into final file.
cat $REPO/src/TZDBPK/Version.inc > $TZDB_PAS_DIST
cat $REPO/xx01 | sed "s/{\$INCLUDE.*}//g" >> $TZDB_PAS_DIST
cat $REPO/src/TZDBPK/TZDB.inc >> $TZDB_PAS_DIST
cat $REPO/xx02 | sed "s/{\$INCLUDE.*}//g" >> $TZDB_PAS_DIST

if [ "$?" -ne 0 ]; then
    echo "[ERR] Failed to build a packaged unit file."
    exit 1
fi

cleanup

echo "Downloading the API documentation..."
wget https://raw.githubusercontent.com/wiki/pavkam/tzdb/API-Documentation.md -q -O $DIST/API.md

if [ "$?" -ne 0 ]; then
    echo "[WARN] Failed to download the API documentation."
fi

echo "DONE!"