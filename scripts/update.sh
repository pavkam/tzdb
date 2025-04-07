#!/bin/bash
#
# This script pulls all the latest data from CLDR and IANA databases, builds the TZCompile project and updates the DB
# Requires a UNIX-like system with FreePascal installed.
#
# Enjoy!
#

LOG_FILE=./update-compile.log

function error() {
    echo "❌ ${1}"

    if [ -f "${LOG_FILE}" ]; then
        cat ${LOG_FILE}
        rm ${LOG_FILE} > /dev/null 2>&1
    fi

    exit 1
}

function warn() {
    echo "⚠️ ${1}"
}

function info() {
    echo "► ${1}"
}

function ok() {
    echo "✅ ${1}"
}

TZDB_PAS=./src/TZDBPK/TZDB.pas
if [ ! -e "${TZDB_PAS}" ]; then
    error "Invalid current directory. Please run this script from the root of the repository."
fi

INPUT_VER=`cat ${TZDB_PAS} | sed -n "s/.*CComponentVersion\ *=\ *'\(.*\)';.*/\1/p"`
CUSTOM_VER=0
CI=0

if [ "${1}" == "ci" ]; then
    CI=1
    if ! command -v git &> /dev/null; then
        error "CI mode only supported when git is installed!"
    fi
elif [ "${1}" != "" ]; then
    INPUT_VER=${1}
    CUSTOM_VER=1
fi

IFS='.'; DOT_ARR=(${INPUT_VER}); unset IFS;
VER_0=${DOT_ARR[0]}
VER_1=${DOT_ARR[1]}
VER_2=${DOT_ARR[2]}
VER_3=${DOT_ARR[3]}

if [[ ${VER_0} =~ ^[0-9]+$ ]] && [[ ${VER_1} =~ ^[0-9]+$ ]] && [[ ${VER_2} =~ ^[0-9]+$ ]] && [[ ${VER_3} =~ ^[0-9]+$ ]]; then
    if [ ${CUSTOM_VER} -eq 0 ]; then
        # Increment the build number
        ((VER_3++))
    fi
    info "Will bump the version of the project to ${VER_0}.${VER_1}.${VER_2}.${VER_3}."
else
    error "Invalid version info provided: '${INPUT_VER}'. Expected 'n.n.n.n' format."
fi

if [ ! -d "./tz_database_latest" ] || [ ! -e "./cldr/converter.xsl" ] || [ ! -d "./src/TZDBPK" ] || [ ! -e "./src/TZCompile/TZCompile.dpr" ]; then
    error "Cannot find required sub-directories. Make sure you have full repo downloaded."
fi

info "Pulling the latest CLDR data from GitHub..."

CLDR_XML=./cldr/windowsZones.xml
wget https://raw.githubusercontent.com/unicode-org/cldr/master/common/supplemental/windowsZones.xml -q -O ${CLDR_XML}.tmp > ${LOG_FILE} 2>&1
if [ "$?" -ne 0 ]; then
    warn "Failed pulling down updated CLDR Windows zone information from GitHub."
    rm ${CLDR_XML}.tmp
else
    rm ${CLDR_XML}
    mv ${CLDR_XML}.tmp ${CLDR_XML}
fi

info "Converting the latest CLDR xml file to inc..."
CLDR_INC=./src/TZCompile/WindowsTZ.inc
cat ${CLDR_XML} | sed -n 's/<mapZone other="\(.*\)".*territory="001" type="\(.*\)"\/>/GlobalCache.AddAlias("\1", "\2");/p' | sed "s/\"/'/g" > ${CLDR_INC}.tmp 2> ${LOG_FILE} || error "Failed to convert CLDR xml file to inc."

rm ${CLDR_INC} > ${LOG_FILE} 2>&1 || error "Failed to remove the old CLDR inc file."
mv ${CLDR_INC}.tmp ${CLDR_INC} > ${LOG_FILE} 2>&1 || error "Failed to move the new CLDR inc file."

ALIASES=$(wc -l ${CLDR_INC} | sed 's/\([0-9]*\)\ .*/\1/g') || error "Failed to count the number of aliases."
info "Created ${ALIASES} aliases."

FPC_MAJOR=$(fpc -iV | sed 's/\([0-9]*\)\.[0-9]*\.[0-9]*/\1/g' || 0) || error "Failed to get the FreePascal major version."

if [ ${FPC_MAJOR} -eq 0 ]; then
    error "FreePascal compiler not installed."
fi

if [ ${FPC_MAJOR} -lt 3 ]; then
    error "Expected at least FreePascal version 3."
fi

info "Found FreePascal version `fpc -iV` installed."

rm -fr ./bin > ${LOG_FILE} 2>&1 || error "Failed to remove the bin directory."
mkdir -p ./bin > ${LOG_FILE} 2>&1 || error "Failed to create the bin directory."

fpc ./src/TZCompile/TZCompile.dpr -FEbin -FUbin > ${LOG_FILE} 2>&1 || error "Failed to compile the TZCompile program."

info "Pulling the latest TZDB database from IANA ..."

rm -rf ./iana_temp > ${LOG_FILE} 2>&1 || error "Failed to remove the iana_temp directory."
wget -q https://www.iana.org/time-zones -O ./time-zones.html > ${LOG_FILE} 2>&1 || error "Failed to pull the latest TZDB database from IANA."

IANA_GZ_URL=$( cat ./time-zones.html | sed -nr 's/.*href="(.*tzdata.*\.tar\.gz)".*/\1/p' ) || error "Failed to extract the IANA TZDB tar ball URL."
rm ./time-zones.html > ${LOG_FILE} 2>&1 || error "Failed to remove the time-zones.html file."

wget -q "${IANA_GZ_URL}" > ${LOG_FILE} 2>&1 || error "Failed to pull the latest TZDB tar ball."

mv ./tzdata*.tar.gz ./tzdata-latest.tar.gz > ${LOG_FILE} 2>&1 || error "Failed to move the latest TZDB tar ball."
mkdir -p ./iana_temp > ${LOG_FILE} 2>&1 || error "Failed to create the iana_temp directory."
tar -xf tzdata-latest.tar.gz -C ./iana_temp > ${LOG_FILE} 2>&1 || error "Failed to extract the latest TZDB tar ball."

rm tzdata-latest.tar.gz > ${LOG_FILE} 2>&1

info "Pulling the latest ICU zones ..."

wget -q https://raw.githubusercontent.com/unicode-org/icu/refs/heads/main/icu4c/source/tools/tzcode/icuzones -O ./iana_temp/icuzones > ${LOG_FILE} 2>&1 || error "Failed to pull the latest ICU zones."

IANA_VERSION=$(cat ./iana_temp/version) || error "Failed to extract the IANA version."
info "Current TZDB database version is v${IANA_VERSION}."

FILES=( africa antarctica asia australasia backward backzone etcetera europe factory northamerica southamerica icuzones )
for fn in "${FILES[@]}"; do
    info "Replacing file ${fn} ..."
    cp ./iana_temp/${fn} ./tz_database_latest/${fn} > ${LOG_FILE} 2>&1 || error "Failed to replace file ${fn}."
done

rm -rf ./iana_temp > ${LOG_FILE} 2>&1 || error "Failed to remove the IANA temp directory."

# CI mode checks
if [ ${CI} -eq 1 ]; then
    CHK=$(git status -s ./cldr ./tz_database_latest) || error "Failed to check the status of the repository."
    if [ "${CHK}" == "" ]; then
        warn "Nothing has been updated. Finishing the rest of the process."
        exit 0
    fi

    git add ./cldr ./tz_database_latest > ${LOG_FILE} 2>&1 || error "Failed to add the changes to the remote repository."
    git commit -m "chore: update to the latest tzdb/cldr" > ${LOG_FILE} 2>&1 || error "Failed to commit the changes to the remote repository."
fi

# Compile the IANA DB
TZDB_INC=./src/TZDBPK/TZDB.inc
./bin/TZCompile ./tz_database_latest ${TZDB_INC}.temp ${IANA_VERSION} > ${LOG_FILE} 2>&1 || error "Failed to process latest TZDB data."

rm ${TZDB_INC} > ${LOG_FILE} 2>&1 || error "Failed to remove the old TZDB inc file."
mv ${TZDB_INC}.temp ${TZDB_INC} > ${LOG_FILE} 2>&1 || error "Failed to move the new TZDB inc file."

# Run tests
./scripts/run_tests.sh || error "Tests failed!"

# Update the README
info "Updating README with the new version '${IANA_VERSION}'..."
README=./README.md
cat ${README} | sed "s/\(.*\*\*\)[0-9]*[a-z]*\(\*\*.*\)/\1${IANA_VERSION}\2/g" > ${README}.tmp 2> ${LOG_FILE} || error "Failed to update README.md file with the IANA DB version."

rm ${README} > ${LOG_FILE} 2>&1 || error "Failed to remove the old README.md file."
mv ${README}.tmp ${README} > ${LOG_FILE} 2>&1 || error "Failed to move the new README.md file."

# Bump the version
replace_tokens () {
  cat ${1} | sed "s/${2}/\1${3}\2/g" > ${1}.tmp 2> ${LOG_FILE} || error "Failed to bump versions in file '${1}'!"
  rm ${1} > ${LOG_FILE} 2>&1 || error "Failed to remove the old file '${1}'!"
  mv ${1}.tmp ${1} > ${LOG_FILE} 2>&1 || error "Failed to move the new file '${1}'!"
}

VER_MAJ="${VER_0}.${VER_1}"
VER_FULL="${VER_0}.${VER_1}.${VER_2}.${VER_3}"

DPROJ_FILES=$(find . -type f | grep .dproj)
for DPROJ in ${DPROJ_FILES}; do
  info "Bumping the version of file '${DPROJ}' to '${VER_FULL}'..."
  cp ${DPROJ} ${DPROJ}.1 > ${LOG_FILE} 2>&1

  replace_tokens ${DPROJ}.1 '\(<VerInfo_Keys>.*FileVersion=\)[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*\(.*<\/VerInfo_Keys>\)' ${VER_FULL}
  replace_tokens ${DPROJ}.1 '\(<VerInfo_Keys>.*ProductVersion=\)[0-9]*\.[0-9]*\(.*<\/VerInfo_Keys>\)' ${VER_MAJ}
  replace_tokens ${DPROJ}.1 '\(.*<VersionInfoKeys Name="FileVersion">\)[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*\(<\/VersionInfoKeys>\)' ${VER_FULL}
  replace_tokens ${DPROJ}.1 '\(.*<VersionInfoKeys Name="ProductVersion">\)[0-9]*\.[0-9]*\(<\/VersionInfoKeys>\)' ${VER_MAJ}
  replace_tokens ${DPROJ}.1 '\(.*<VersionInfo Name="MajorVer">\)[0-9]*\(<\/VersionInfo>\)' ${VER_0}
  replace_tokens ${DPROJ}.1 '\(.*<VersionInfo Name="MinorVer">\)[0-9]*\(<\/VersionInfo>\)' ${VER_1}
  replace_tokens ${DPROJ}.1 '\(.*<VersionInfo Name="Release">\)[0-9]*\(<\/VersionInfo>\)' ${VER_2}
  replace_tokens ${DPROJ}.1 '\(.*<VersionInfo Name="Build">\)[0-9]*\(<\/VersionInfo>\)' ${VER_3}
  replace_tokens ${DPROJ}.1 '\(.*<VerInfo_MajorVer>\)[0-9]*\(<\/VerInfo_MajorVer>\)' ${VER_0}
  replace_tokens ${DPROJ}.1 '\(.*<VerInfo_MinorVer>\)[0-9]*\(<\/VerInfo_MinorVer>\)' ${VER_1}
  replace_tokens ${DPROJ}.1 '\(.*<VerInfo_Release>\)[0-9]*\(<\/VerInfo_Release>\)' ${VER_2}
  replace_tokens ${DPROJ}.1 '\(.*<VerInfo_Build>\)[0-9]*\(<\/VerInfo_Build>\)' ${VER_3}

  rm ${DPROJ} > ${LOG_FILE} 2>&1 || error "Failed to remove the old file '${DPROJ}'!"
  mv ${DPROJ}.1 ${DPROJ} > ${LOG_FILE} 2>&1 || error "Failed to move the new file '${DPROJ}'!"
done

# update the version in the .pas module as well
cat ${TZDB_PAS} | sed "s/\(.*CComponentVersion\ *=\ *'\).*\(';.*\)/\1${VER_FULL}\2/g" > ${TZDB_PAS}.tmp 2> ${LOG_FILE} || error "Failed to update TZDB.pas file with the bumped version."

rm ${TZDB_PAS} > ${LOG_FILE} 2>&1 || error "Failed to remove the old TZDB.pas file."
mv ${TZDB_PAS}.tmp ${TZDB_PAS} > ${LOG_FILE} 2>&1 || error "Failed to move the new TZDB.pas file."

# Merge the files into one
DIST=./dist
if [ ! -d "${DIST}" ]; then
    warn "Unable to locate the distribution directory. Skipping the last step."
    exit 0
fi

info "Merging the TZDB components into one source file..."

TZDB_PAS_DIST=${DIST}/TZDB.pas
rm ${TZDB_PAS_DIST} > ${LOG_FILE} 2>&1
cp ./media/logo-64x64.ico ${DIST}/TZDB.ico > ${LOG_FILE} 2>&1
cat ./src/TZDBPK/TZDBPK.dpk | sed 's/TZDBPK/TZDB/g' > ${DIST}/TZDB.dpk 2> ${LOG_FILE}

cleanup () {
  rm -fr ./xx00 > ${LOG_FILE} 2>&1
  rm -fr ./xx01 > ${LOG_FILE} 2>&1
  rm -fr ./xx02 > ${LOG_FILE} 2>&1
}


# Split the file into pieces based in includes .
csplit -s ${TZDB_PAS} '/{\$INCLUDE.*}/' {1} 2> ${LOG_FILE}
if [ "$?" -ne 0 ] || [ ! -e "./xx00" ] || [ ! -e "./xx01" ] || [ ! -e "./xx02" ]; then
    cleanup
    error "Failed to split the TZDB unit file into chunks."
fi

# We have three chunks in here. Assemble them into final file.
echo -e "{ WARNING: This file is auto-generated by update-compile.sh. Do not edit it manually! }\nunit TZDB;\n" > ${TZDB_PAS_DIST} 2> ${LOG_FILE} || error "Failed to create the TZDB.pas file."

cat ./src/TZDBPK/Version.inc >> ${TZDB_PAS_DIST} 2> ${LOG_FILE} || error "Failed to append the Version.inc file to the TZDB.pas file."
cat ./xx01 | sed "s/{\$INCLUDE.*}//g" >> ${TZDB_PAS_DIST} 2> ${LOG_FILE} || error "Failed to append the first chunk to the TZDB.pas file."
cat ./src/TZDBPK/TZDB.inc >> ${TZDB_PAS_DIST} 2> ${LOG_FILE} || error "Failed to append the TZDB.inc file to the TZDB.pas file."
cat ./xx02 | sed "s/{\$INCLUDE.*}//g" >> ${TZDB_PAS_DIST} 2> ${LOG_FILE} || error "Failed to append the second chunk to the TZDB.pas file."

cleanup

info "Downloading the API documentation..."
wget https://raw.githubusercontent.com/wiki/pavkam/tzdb/API-Documentation.md -q -O ${DIST}/API.md > ${LOG_FILE} 2>&1 || error "Failed to download the API documentation."

# Finish up in CI mode.

if [ ${CI} -eq 1 ]; then
    TAG="${VER_0}.${VER_1}.${VER_2}.${VER_3}.${IANA_VERSION}"

    git add . > ${LOG_FILE} 2>&1 || error "Failed to add the changes to the remote repository."
    git commit -m "chore: update to IANA DB v${IANA_VERSION} and bump version" > ${LOG_FILE} 2>&1 || error "Failed to commit the changes to the remote repository."
    git tag "${TAG}" > ${LOG_FILE} 2>&1 || error "Failed to tag the changes to the remote repository."
    git push origin "${TAG}" > ${LOG_FILE} 2>&1 || error "Failed to push the changes to the remote repository."
    git push > ${LOG_FILE} 2>&1 || error "Failed to push the changes to the remote repository."
fi

ok "Finished!"
