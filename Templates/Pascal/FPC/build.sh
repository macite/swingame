#!/bin/bash

#
# Step 1: Detect the operating system
#
MAC="Mac OS X"
WIN="Windows"
LIN="Linux"

if [ `uname` = "Darwin" ]; then
    OS=$MAC
elif [ `uname` = "Linux" ]; then
    OS=$LIN
else
    OS=$WIN
fi

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd`
cd "$APP_PATH"

GAME_NAME=${APP_PATH##*/}

if [ "$OS" = "$MAC" ]; then
    ICON="SwinGame.icns"
else
    ICON="SwinGame"
fi

FULL_APP_PATH=$APP_PATH
APP_PATH="."

GAME_MAIN=""

#Set the basic paths
OUT_DIR="${APP_PATH}/bin"
FULL_OUT_DIR="${FULL_APP_PATH}/bin"
TMP_DIR="${APP_PATH}/tmp"
SRC_DIR="${APP_PATH}/src"

LOG_FILE="${APP_PATH}/out.log"

PAS_FLAGS="-O3 -vw"
SG_INC="-Fu${APP_PATH}/lib/"

FPC_BIN=`which fpc`
if [ -z "${FPC_BIN}" ]; then
    echo
    echo "I cannot find the Free Pascal Compiler."
    echo "Please make sure you have installed it"
    echo " - use the default location (no spaces in path)"
    echo " - also restarted your computer after install"
    echo
    exit -1
fi

if [ "$OS" = "$MAC" ]; then
#   Look for crt1.o
    if [ ! -f /usr/lib/crt1.o ]; then
        echo
        echo "I cannot find the required libraries."
        echo "Please try running the following command:"
        echo "  xcode-select --install"
        echo
        exit -1
    fi
fi

FPC_VER=`${FPC_BIN} -iV`

FPC_MAJOR_VER=`echo ${FPC_VER} | awk -F'.' '{print $1}'`
FPC_MINOR_VER=`echo ${FPC_VER} | awk -F'.' '{print $2}'`
FPC_LESSR_VER=`echo ${FPC_VER} | awk -F'.' '{print $3}'`


CLEAN="N"


Usage()
{
    echo "Usage: [-c] [-h] [name]"
    echo
    echo "Compiles your game into an executable application."
    echo "Output is located in $OUT_DIR."
    echo
    echo "Options:"
    echo " -c   Perform a clean rather than a build"
    echo " -h   Show this help message "
    echo " -i [icon] Change the icon file"
    echo " -r   Create a release version that does not include debug information"
    exit 0
}

RELEASE=""

# Flags to indicate which architecture for Windows only
SG_WIN32=true
SG_WIN64=false

while getopts chri:w: o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
    r)  RELEASE="Y" ;;
    i)  ICON="$OPTARG";;
    w)  if [ "${OPTARG}" = "in32" ]; then
            SG_WIN32=true
            SG_WIN64=false
        elif [ "${OPTARG}" = "in64" ]; then
            SG_WIN32=false
            SG_WIN64=true
        fi
        ;;
    esac
done

shift $((${OPTIND}-1))

if [ "$OS" = "$MAC" ]; then
    TMP_DIR="${TMP_DIR}/mac"
    LIB_DIR="${APP_PATH}/lib/mac"
    PAS_FLAGS="-dSWINGAME_SDL2 -k\"-lz\" -k\"-lbz2\" -k\"-lstdc++\" -k\"-lm\" -k\"-lc\" -k\"-lc++\" -k\"-lcurl\""
elif [ "$OS" = "$WIN" ]; then
    if [ ${SG_WIN32} = true ]; then
      LIB_DIR="${APP_PATH}/lib/win32"
      TMP_DIR="${TMP_DIR}/win32"
    else
      LIB_DIR="${APP_PATH}/lib/win64"
      TMP_DIR="${TMP_DIR}/win64"

      FPC_BIN=`which ppcrossx64`
      if [ -z "${FPC_BIN}" ]; then
          echo
          echo "I cannot find the 64bit Free Pascal Compiler."
          echo "Please make sure you have installed it"
          echo " - use the default location (no spaces in path)"
          echo " - also restarted your computer after install"
          exit -1
      fi
    fi
    PAS_FLAGS="${PAS_FLAGS} -dSWINGAME_SDL2 -k-L'${LIB_DIR}' -k-lsgsdl2"
else
    LIB_DIR="${FULL_APP_PATH}/lib/linux"
    TMP_DIR="${TMP_DIR}/unx"
    PAS_FLAGS="${PAS_FLAGS} -dSWINGAME_SDL2 -k-lm -k-lc"
fi

#
# Change directories based on release or debug builds
#
if [ -n "${RELEASE}" ]; then
    OUT_DIR="${OUT_DIR}/Release"
    FULL_OUT_DIR="${FULL_OUT_DIR}/Release"
    TMP_DIR="${TMP_DIR}/Release"
else
    PAS_FLAGS="-gw -vw ${PAS_FLAGS}"
    OUT_DIR="${OUT_DIR}/Debug"
    FULL_OUT_DIR="${FULL_OUT_DIR}/Debug"
    TMP_DIR="${TMP_DIR}/Debug"
fi

if [ ! -d ${TMP_DIR} ]; then
    mkdir -p ${TMP_DIR}
fi

if [ -f "${LOG_FILE}" ]; then
    rm -f "${LOG_FILE}"
fi


DoExitCompile ()
{
    echo "An error occurred while compiling";
    cat out.log

    if [ "$OS" = "$LIN" ]; then
        echo ""
        echo "Make sure you have the required libraries installed:"
        echo "sudo apt-get install fpc curl libsdl1.2-dev libsdl-gfx1.2-dev libsdl-image1.2-dev libsdl-mixer1.2-dev libsdl-ttf2.0-dev libsdl-net* libsmpeg*"
    fi

    exit 1;
}

CleanTmp()
{
    if [ -d "${TMP_DIR}" ]
    then
        rm -rf "${TMP_DIR}"
    fi
    mkdir "${TMP_DIR}"
}

doDriverMessage()
{
    echo "  ... Using SDL2 Driver"
}

doBasicMacCompile()
{
    mkdir -p ${TMP_DIR}
    echo "  ... Compiling $GAME_MAIN"

    FRAMEWORKS='-framework Cocoa -framework AudioToolbox -framework AudioUnit -framework CoreAudio -framework IOKit -framework OpenGL -framework Carbon -framework ForceFeedback -framework CoreVideo'

    STATIC_LIBS=`cd ${LIB_DIR};ls -f *.a | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-l%s ", substr(patharr[idx],4)) }'`

    ${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -S2 -Sh -FE"${OUT_DIR}" -FU"${TMP_DIR}" -Fu"${LIB_DIR}" -Fi"${SRC_DIR}" -k"-F'${LIB_DIR}' ${FRAMEWORKS}" -k"${STATIC_LIBS}" -o"${GAME_NAME}" "${SRC_DIR}/${GAME_MAIN}" > "${LOG_FILE}"  2> "${LOG_FILE}"
    if [ $? != 0 ]; then DoExitCompile; fi
}

doMacPackage()
{
    GAMEAPP_PATH="${FULL_OUT_DIR}/${GAME_NAME}.app"
    if [ -d "${GAMEAPP_PATH}" ]
    then
        echo "  ... Removing old application"
        rm -rf "${GAMEAPP_PATH}"
    fi

    echo "  ... Creating Application Bundle"

    mkdir "${GAMEAPP_PATH}"
    mkdir "${GAMEAPP_PATH}/Contents"
    mkdir "${GAMEAPP_PATH}/Contents/MacOS"
    mkdir "${GAMEAPP_PATH}/Contents/Resources"
    # mkdir "${GAMEAPP_PATH}/Contents/Frameworks"
    # echo "  ... Adding Private Frameworks"
    # cp -R -p "${LIB_DIR}/"*.framework "${GAMEAPP_PATH}/Contents/Frameworks/"

    mv "${FULL_OUT_DIR}/${GAME_NAME}" "${GAMEAPP_PATH}/Contents/MacOS/"
    echo "<?xml version='1.0' encoding='UTF-8'?>\
    <!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\
    <plist version=\"1.0\">\
    <dict>\
            <key>CFBundleDevelopmentRegion</key>\
            <string>English</string>\
            <key>CFBundleExecutable</key>\
            <string>${GAME_NAME}</string>\
            <key>CFBundleIconFile</key>\
            <string>${ICON}</string>\
            <key>CFBundleIdentifier</key>\
            <string>au.edu.swinburne.${GAME_NAME}</string>\
            <key>CFBundleInfoDictionaryVersion</key>\
            <string>6.0</string>\
            <key>CFBundleName</key>\
            <string>${GAME_NAME}</string>\
            <key>CFBundlePackageType</key>\
            <string>APPL</string>\
            <key>CFBundleSignature</key>\
            <string>SWIN</string>\
            <key>CFBundleVersion</key>\
            <string>1.0</string>\
            <key>CSResourcesFileMapped</key>\
            <true/>\
    </dict>\
    </plist>" >> "${GAMEAPP_PATH}/Contents/Info.plist"

    echo "APPLSWIN" >> "${GAMEAPP_PATH}/Contents/PkgInfo"

    RESOURCE_DIR="${GAMEAPP_PATH}/Contents/Resources"
}

doLinuxCompile()
{
    "${APP_PATH}/lib/makelib.sh"
    mkdir -p ${TMP_DIR}
    echo "  ... Compiling $GAME_MAIN"

    ${FPC_BIN}  -Fl"${LIB_DIR}" -k"-rpath=\$ORIGIN --enable-new-dtags" ${PAS_FLAGS} ${SG_INC} -S2 -Sh -FE${OUT_DIR} -FU${TMP_DIR} -Fu${LIB_DIR} -Fi${SRC_DIR} -o"${GAME_NAME}" ${SRC_DIR}/${GAME_MAIN} > "${LOG_FILE}" 2> "${LOG_FILE}"
    if [ $? != 0 ]; then
        DoExitCompile;
    fi
}

doLinuxPackage()
{
    cp -p -f "${FULL_APP_PATH}/lib/linux"/*.so "${FULL_OUT_DIR}"
    RESOURCE_DIR="${FULL_OUT_DIR}/Resources"
}

doWindowsCompile()
{
    mkdir -p ${TMP_DIR}

    echo "  ... Compiling $GAME_MAIN"

    #
    # If using full path then you need to escape the /c/ used by MSYS
    #
    # LIB_DIR=`echo $LIB_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    # TMP_DIR=`echo $TMP_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    # SRC_DIR=`echo $SRC_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    # OUT_DIR=`echo $OUT_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    # SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`
    # SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`
    # SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`

    echo "  ... Creating Resources"
    windres ${SRC_DIR}/SwinGame.rc ${SRC_DIR}/GameLauncher.res
    if [ $? != 0 ]; then DoExitCompile; fi

    ${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -Sh -FE${OUT_DIR} -FU${TMP_DIR} -Fu${LIB_DIR} -Fi${SRC_DIR} -o"${GAME_NAME}.exe" ${SRC_DIR}/${GAME_MAIN} > "${LOG_FILE}" 2> "${LOG_FILE}"
    if [ $? != 0 ]; then DoExitCompile; fi

}

doWindowsPackage()
{
    RESOURCE_DIR=${FULL_OUT_DIR}/Resources

    echo "  ... Copying libraries"
    cp -p -f "${LIB_DIR}"/*.dll "${FULL_OUT_DIR}"
}


copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2

    cd "${FROM_DIR}"

    # Create directory structure
    find . -mindepth 1 ! -path \*.svn\* ! -path \*/. -type d -exec mkdir -p "${TO_DIR}/{}" \;
    if [ $? != 0 ]; then DoExitCompile; fi

    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p "{}" "${TO_DIR}/{}"  \;
    if [ $? != 0 ]; then DoExitCompile; fi
}

#
# Copy Resources from standard location to $RESOURCE_DIR
#
doCopyResources()
{
    echo "  ... Copying Resources into $GAME_NAME"

    copyWithoutSVN "${FULL_APP_PATH}/Resources" "${RESOURCE_DIR}"
}

#
# Locate GameMain.pas
#
locateGameMain()
{
  cd "${SRC_DIR}"
  fileList=$(find "." -maxdepth 1 -type f -name \*.pas)
  FILE_COUNT=$(echo "$fileList" | tr " " "\n" | wc -l)

  if [ ${FILE_COUNT} = 1 ]; then
    GAME_MAIN=${fileList[0]}
  else
    echo "Select the file to compile for your game"
    PS3="File number: "

    select fileName in $fileList; do
        if [ -n "$fileName" ]; then
            GAME_MAIN=${fileName}
        fi

        break
    done
  fi

  cd "${FULL_APP_PATH}"

  if [ ! -f "${SRC_DIR}/${GAME_MAIN}" ]; then
    echo "Cannot find file to compile, was looking for ${GAME_MAIN}"
    exit -1
  fi
}

locateGameMain

if [ $CLEAN = "N" ]
then
    if [ ! -d "${OUT_DIR}" ]
    then
        mkdir -p "${OUT_DIR}"
    fi

    echo "--------------------------------------------------"
    echo "          Creating $GAME_NAME"
    echo "          for $OS"
    echo "--------------------------------------------------"
    echo "  Running script from $FULL_APP_PATH"
    echo "  Saving output to $OUT_DIR"
    echo "  Compiler flags ${SG_INC} ${PAS_FLAGS}"
    echo "--------------------------------------------------"
    doDriverMessage
    echo "  ... Creating ${GAME_NAME}"

    if [ "$OS" = "$MAC" ]; then
        HAS_LION=false
        OS_VER=`sw_vers -productVersion | awk -F . '{print $1"."$2}'`
        OS_VER_MINOR=`sw_vers -productVersion | awk -F . '{print $2}'`

        if [ $OS_VER_MINOR -ge "7" ]; then
            # Is Lion or later = has PIE
            HAS_LION=true
        fi

        if [ $HAS_LION = true ]; then
            if (( ($FPC_MAJOR_VER == 2) && ($FPC_MINOR_VER == 6) && (FPC_LESSR_VER == 0) )); then
                PAS_FLAGS="$PAS_FLAGS -k-macosx_version_min -k${OS_VER} -k-no_pie"
            elif (( $FPC_MAJOR_VER == 3 )); then
                PAS_FLAGS="$PAS_FLAGS -WM10.${OS_VER_MINOR}"
            else
                PAS_FLAGS="$PAS_FLAGS -WM10.7"
            fi
        else
            PAS_FLAGS="${PAS_FLAGS} -dNO_ARC"
            if (( ($FPC_MAJOR_VER == 2) && ($FPC_MINOR_VER == 6) && (FPC_LESSR_VER > 0) )); then
                PAS_FLAGS="$PAS_FLAGS -WM10.5"
            fi
        fi

        doBasicMacCompile
        doMacPackage
    elif [ "$OS" = "$LIN" ]; then
        doLinuxCompile
        doLinuxPackage
    else
        doWindowsCompile
        doWindowsPackage
    fi

    doCopyResources
else
    CleanTmp
    rm -rf "${OUT_DIR}"
    mkdir "${OUT_DIR}"
    echo    ... Cleaned
fi

#remove temp files on success
rm -f ${LOG_FILE} 2>> /dev/null

echo "  Finished"
echo "--------------------------------------------------"
