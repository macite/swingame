#!/bin/bash

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"
FULL_APP_PATH=$APP_PATH
APP_PATH="."

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

#
# Set the basic paths
#
OUT_DIR="${APP_PATH}/bin"
FULL_OUT_DIR="${FULL_APP_PATH}/bin"
TMP_DIR="${APP_PATH}/tmp"
SRC_DIR="${APP_PATH}/src"
LOG_FILE="${APP_PATH}/out.log"

if [ "$OS" = "$MAC" ]; then
    ICON="SwinGame.icns"
else
    ICON="SwinGame"
fi

Usage()
{
    echo "Usage: [-c] [-h] src_name"
    echo 
    echo "Compiles your game into an executable application."
    echo "Output is located in $OUT_DIR."
    echo
    echo "Options:"
    echo " -c   Perform a clean rather than a build"
    echo " -h   Show this help message "
    exit 0
}

while getopts chb:g:s: o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    h)  Usage ;;
    esac
done

shift $((${OPTIND}-1))

#
# Set compiler path and options
#
FPC_BIN=`which fpc`
if [ -z "${FPC_BIN}" ]; then
    echo
    echo "I cannot find the Free Pascal Compiler."
    echo "Please make sure you have installed it"
    echo " - use the default location (no spaces in path)"
    echo " - also restarted your computer after install"
    exit -1
fi

FPC_VER=`${FPC_BIN} -iV`

FPC_MAJOR_VER=`echo ${FPC_VER} | awk -F'.' '{print $1}'`
FPC_MINOR_VER=`echo ${FPC_VER} | awk -F'.' '{print $2}'`
FPC_LESSR_VER=`echo ${FPC_VER} | awk -F'.' '{print $3}'`

if [ "$OS" = "$WIN" ]; then
    PAS_FLAGS="-g -Ci -gc -Ct -dTrace"
else
    PAS_FLAGS="-g -Ci -gw -Ct -dTrace"
fi

if [ "$OS" = "$MAC" ]; then
    FPC_BIN=`which ppcx64`
    TMP_DIR="${APP_PATH}/tmp/sdl2"
    LIB_DIR="${APP_PATH}/staticlib/mac"
    PAS_FLAGS="${PAS_FLAGS} -dSWINGAME_SDL2 -k\"-lz\" -k\"-lbz2\" -k\"-lstdc++\" -k\"-lm\" -k\"-lc\" -k\"-lc++\" -k\"-lcurl\""
elif [ "$OS" = "$WIN" ]; then
    LIB_DIR="${APP_PATH}/lib/win"
    PAS_FLAGS="${PAS_FLAGS} -dSWINGAME_SDL2 -k-L'${LIB_DIR}' -k-lstdc++ -k-lsgsdl2"
else # Linux
    PAS_FLAGS="${PAS_FLAGS} -dSWINGAME_SDL2 -k\"-lm\" -k\"-lc\" -k\"-lsgsdl2\""

    if [[ ! -f /usr/lib/libsgsdl2.so ]]; then
        echo "SwinGame driver must be installed. Super user permissions required."
        echo "Installing 3rd Party Libraries"
        sudo apt-get install build-essential libsdl2-dev libsdl2-gfx-dev libsdl2-image-dev libsdl2-mixer-dev libsdl2-net-dev libsdl2-ttf-dev
        if [ $? != 0 ]; then echo "Failed to install"; exit 1; fi
        echo
        echo "Installing SwinGame driver"
        sudo install ./staticlib/sdl2/linux/libsgsdl2.so -t /usr/lib/
        if [ $? != 0 ]; then echo "Failed to install"; exit 1; fi
    fi
fi

# echo ${PAS_FLAGS}

DRV_LIB=`find ./libsrc -type d ! -path \*.svn\* | awk -F . '{print "-Fu"$0}'`
SG_INC="-Fi${APP_PATH}/libsrc -Fu${APP_PATH}/libsrc -Fu${APP_PATH}/src ${DRV_LIB}"
CLEAN="N"

#
# Create SwinGame.pas if missing
#
if [[ ! -f ./test/SwinGame.pas ]]; then
  echo " Creating SwinGame.pas"
  cd ../Tools/SGWrapperGen
  python create_pascal_library.py
  cd "${FULL_APP_PATH}"
  cp ../Generated/Pascal/lib/SwinGame.pas ./test/SwinGame.pas
fi


#
# Set game name
#
GAME_NAME="Test"

#
# Locate GameMain.pas
#
locateGameMain()
{
  cd "${FULL_APP_PATH}/test"
  fileList=$(ls *.pas) #$(find "." -maxdepth 1 -type f -name \*.pas)

  FILE_COUNT=$(echo "$fileList" | tr " " "\n" | wc -l)
  
  if [[ ${FILE_COUNT} = 1 ]]; then
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
  
  cd ${FULL_APP_PATH}
}

#
# Get the name of the source file
#
locateGameMain
SRC_FILE=$GAME_MAIN

if [ ! -f ./test/$SRC_FILE ]; then
    echo "Unable to locate source file"
    exit 1
fi

#
# Remove old log file
#
if [ -f "${LOG_FILE}" ]
then
    rm -f "${LOG_FILE}"
fi


DoExitCompile ()
{ 
    echo "An error occurred while compiling"; 
    cat out.log
    exit 1; 
}

CleanTmp()
{
    if [ -d "${APP_PATH}/tmp" ]
    then
        rm -rf "${APP_PATH}/tmp"
    fi
    mkdir -p "${TMP_DIR}"
    
}

DoDriverMessage()
{
  echo "  ... Using SGSDL2 Driver"
}

doBasicMacCompile()
{
    mkdir -p ${TMP_DIR}/${1}
    
    echo "  ... Compiling $GAME_NAME - $1"
    
    FRAMEWORKS='-framework AudioToolbox -framework AudioUnit -framework CoreAudio -framework CoreVideo -framework IOKit -framework OpenGL -framework Carbon -framework ForceFeedback'

    STATIC_LIBS=`cd ${LIB_DIR};ls -f *.a | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-l%s ", substr(patharr[idx],4)) }'`    
    
    ${FPC_BIN} ${PAS_FLAGS} ${SG_INC} -Mobjfpc -gl -gw2 -Sew -Sh -FE"${TMP_DIR}/${1}" -FU"${TMP_DIR}/${1}" -Fu"${LIB_DIR}" -Fi"${SRC_DIR}" -k"${STATIC_LIBS}" -k"-rpath @loader_path/../Frameworks" -k"-F${LIB_DIR} -framework Cocoa ${FRAMEWORKS}" -k"-lbz2" $2 -o"${OUT_DIR}/${GAME_NAME}" "./test/${SRC_FILE}" > ${LOG_FILE} 2> ${LOG_FILE}
    
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
    mkdir "${GAMEAPP_PATH}/Contents/Frameworks"

    # echo "  ... Added Private Frameworks"
    # cp -R -p "${LIB_DIR}/"*.framework "${GAMEAPP_PATH}/Contents/Frameworks/"

    mv "${OUT_DIR}/${GAME_NAME}" "${APP_PATH}/bin/${GAME_NAME}.app/Contents/MacOS/" 
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
    mkdir -p ${TMP_DIR}
    echo "  ... Compiling $GAME_NAME"
    
    ${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -Sh -FE${OUT_DIR} -FU${TMP_DIR} -Fu${LIB_DIR} -Fi${SRC_DIR} -o${GAME_NAME} ./test/${SRC_FILE} > ${LOG_FILE}
    if [ $? != 0 ]; then DoExitCompile; fi
}

doLinuxPackage()
{
    RESOURCE_DIR="${FULL_OUT_DIR}/Resources"
}

doWindowsCompile()
{
    mkdir -p ${TMP_DIR}
    
    echo "  ... Compiling $GAME_NAME"
    
    LIB_DIR=`echo $LIB_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    TMP_DIR=`echo $TMP_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    SRC_DIR=`echo $SRC_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    OUT_DIR=`echo $OUT_DIR | sed 's/\/\(.\)\//\1:\//'`          #awk '{sub("/c/", "c:/"); print}'`
    SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`
    SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`
    SG_INC=`echo $SG_INC | sed 's/\/\(.\)\//\1:\//'`            #awk '{sub("/c/", "c:/"); print}'`
    
    #echo "  ... Creating Resources"
    #windres ${SRC_DIR}/SwinGame.rc ${SRC_DIR}/GameLauncher.res
    #if [ $? != 0 ]; then DoExitCompile; fi
    
    ${FPC_BIN} ${SG_INC} -S2 -Sh ${PAS_FLAGS} -FE${OUT_DIR} -FU${TMP_DIR} -Fu${LIB_DIR} -Fi${LIB_DIR} ${PAS_FLAGS} -o${GAME_NAME}.exe ./test/${SRC_FILE} > ${LOG_FILE}
    #${FPC_BIN}  ${PAS_FLAGS} ${SG_INC} -Mobjfpc -Sh -FE${TMP_DIR} -Fi${LIB_DIR} -FU${TMP_DIR} -s ./test/${SRC_FILE} > ${LOG_FILE}
    if [ $? != 0 ]; then DoExitCompile; fi
}

doWindowsPackage()
{
    RESOURCE_DIR="${FULL_OUT_DIR}/Resources"
    
    echo "  ... Copying libraries"
    cp -p -f "${LIB_DIR}"/*.dll "${OUT_DIR}"
}

copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 ! -path \*.svn\* ! -path \*/. -type d -exec mkdir -p "${TO_DIR}/{}" \;
    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
}

#
# Copy Resources from standard location to $RESOURCE_DIR
#
doCopyResources()
{
    echo "  ... Copying Resources into $GAME_NAME"
    
    copyWithoutSVN "${FULL_APP_PATH}/test/Resources" "${RESOURCE_DIR}"
}


if [ $CLEAN = "N" ]
then
    CleanTmp
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
    echo "  Compiler flags ${SG_INC} ${C_FLAGS}"
    echo "--------------------------------------------------"
    echo "  ... Creating ${GAME_NAME}"    
    DoDriverMessage;

    if [ "$OS" = "$MAC" ]; then
        HAS_PPC=false
        HAS_i386=false
        HAS_LEOPARD_SDK=false
        HAS_LION=false
        OS_VER=`sw_vers -productVersion | awk -F . '{print $1"."$2}'`
        OS_VER_MINOR=`sw_vers -productVersion | awk -F . '{print $2}'`
        
        if [ $OS_VER_MINOR -ge "7" ]; then
            # Is Lion or later = has PIE
            HAS_LION=true
        fi

        if [ -f /usr/libexec/as/ppc/as ]; then
            HAS_PPC=true
        fi
        
        if [ -f /usr/libexec/as/i386/as ]; then
            HAS_i386=true
        fi
        
        if [ -d /Developer/SDKs/MacOSX10.5.sdk ]; then
            HAS_LEOPARD_SDK=true
        fi
        
        if [ $HAS_LION = true ]; then
            if (( ($FPC_MAJOR_VER == 2) && ($FPC_MINOR_VER == 6) && (FPC_LESSR_VER == 0) )); then
                PAS_FLAGS="$PAS_FLAGS -k-macosx_version_min -k10.7 -k-no_pie"
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
#rm -rf ${TMP_DIR} 2>> /dev/null

echo "  Finished"
echo "--------------------------------------------------"
cd "${FULL_APP_PATH}"
if [ "$OS" = "$MAC" ]; then
    ${APP_PATH}/bin/Test.app/Contents/MacOS/Test
else
    ${APP_PATH}/bin/Test
fi
