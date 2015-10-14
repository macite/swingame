#!/bin/sh
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
FULL_APP_PATH=$APP_PATH
APP_PATH="."


#Set the basic paths
OUT_DIR="${APP_PATH}/bin"
FULL_OUT_DIR="${FULL_APP_PATH}/bin"
BIN_DIR="${APP_PATH}/bin"
SRC_DIR="${APP_PATH}/src"
LIB_DIR="${APP_PATH}/lib"
LOG_FILE="${APP_PATH}/out.log"

GMCS_FLAGS="-target:exe -r:./lib/SwinGame.dll" #" -r:Microsoft.VisualBasic"
CS_FLAGS="-optimize+"
SG_INC="-I${APP_PATH}/lib/"

if [ "$OS" = "$WIN" ]; then
   export PATH=$APP_PATH/lib:/c/Program\ Files\ \(x86\)/Mono/bin/:/c/Program\ Files/Mono/bin/:$PATH:/c/Windows/Microsoft.NET/Framework/v4.0.30319
   GMCS_FLAGS="$GMCS_FLAGS -platform:x86"
fi

#Locate the compiler...
GMCS_BIN=`which mcs 2>> /dev/null`
if [ -z "$GMCS_BIN" ]; then
    #try locating mcs
    GMCS_BIN=`which gmcs 2>> /dev/null`
    if [ -z "$GMCS_BIN" ]; then
        #try locating gmcs
        GMCS_BIN=`which csc 2>> /dev/null`

        if [ -z "$GMCS_BIN" ]; then
            #no compiler found :(
            echo "Unable to find a C# compiler. Install Mono or add it to your path."
            exit -1
        fi

        echo "-------------------------------------------------------------------------------"
        echo "                   !!WARNING!! Using the default C# compiler."
        echo "-------------------------------------------------------------------------------"
        echo ""
        echo " This compiler does not support some C# 6.0 features used in the template."
        echo ""
        echo " To use the default compiler:"
        echo "   1: Remove 'using static SwinGameSDK.SwinGame;'"
        echo "   2: Add 'SwinGame.' to the front of each call to a SwinGame function. "
        echo "      For example, change 'OpenGraphicsWindow(...) to"
        echo "                          'SwinGame.OpenGraphicsWindow(...)'"
        echo ""
        echo " ... or install Mono from http://www.mono-project.com/download/#download-win"
        echo ""
        echo "-------------------------------------------------------------------------------"
        echo ""
        sleep 4
    fi
fi

if [ "$OS" = "$MAC" ]; then
    ICON="SwinGame.icns"
else
    ICON="SwinGame"
fi

CLEAN="N"

#
# Library versions
#
OPENGL=false
SDL_13=true
SDL_12=false

Usage()
{
    echo "Usage: [-c] [-h] [-d] [name]"
    echo 
    echo "Compiles your game into an executable application."
    echo "Output is located in $FULL_OUT_DIR."
    echo
    echo "Options:"
    echo " -c   Perform a clean rather than a build"
    echo " -r   Release build"
    echo " -h   Show this help message "
    echo " -i [icon] Change the icon file"
    exit 0
}

RELEASE=""

while getopts chri:g:b:s: o
do
    case "$o" in
    c)  CLEAN="Y" ;;
    s)  if [ "${OPTARG}" = "dl12" ]; then
            SDL_12=true
            SDL_13=false
            OPENGL=false
        fi 
        ;;
    b)  if [ "${OPTARG}" = "adass" ]; then
            SDL_12=false
            SDL_13=true
            OPENGL=false
        fi 
        ;;
    h)  Usage ;;
    g)  if [ "${OPTARG}" = "odly" ]; then
            SDL_12=false
            SDL_13=false
            OPENGL=true
        fi 
        ;;
    d)  RELEASE="Y" ;;
    i)  ICON="$OPTARG";;
    ?)  Usage
    esac
done

shift $((${OPTIND}-1))

if [ "a$1a" != "aa" ]; then
    GAME_NAME=$1
fi

if [ "$OS" = "$MAC" ]; then
    if [ ${SDL_13} = true ]; then
      TMP_DIR="${TMP_DIR}/badass"
      LIB_DIR="${APP_PATH}/lib/sdl13"
    elif [ ${OPENGL} = true ]; then
        TMP_DIR="${TMP_DIR}/godly"
      LIB_DIR="${APP_PATH}/lib/godly"
    else
      TMP_DIR="${TMP_DIR}/sdl12"
      LIB_DIR="${APP_PATH}/lib/mac"
    fi
elif [ "$OS" = "$WIN" ]; then
    #
    # This needs 1.3 versions of SDL for Windows...
    # along with function sdl_gfx, sdl_ttf, sdl_image, sdl_mixer
    #
    
    # if [ ${SDL_13} = true ]; then
    #   LIB_DIR="${APP_PATH}/lib/sdl13/win"
    # elif [ ${OPENGL} = true ]; then
    #   LIB_DIR="${APP_PATH}/lib/sdl13/win"
    # else
    SDL_13=false
    OPENGL=false
    LIB_DIR="${APP_PATH}/lib/win"
    # fi
fi

#
# Change directories based on release or debug builds
#
if [ -n "${RELEASE}" ]; then
    CS_FLAGS="-optimize+"
    OUT_DIR="${OUT_DIR}/Release"
    FULL_OUT_DIR="${FULL_OUT_DIR}/Release"
else
    CS_FLAGS="-debug -define:DEBUG"
    OUT_DIR="${OUT_DIR}/Debug"
    FULL_OUT_DIR="${FULL_OUT_DIR}/Debug"
fi

if [ -f "${LOG_FILE}" ]
then
    rm -f "${LOG_FILE}"
fi


doMacPackage()
{
    GAMEAPP_PATH="${FULL_OUT_DIR}/${GAME_NAME}.app"
    if [ -d "${GAMEAPP_PATH}" ] 
    then
        echo "  ... Removing old application"
        rm -rf "${GAMEAPP_PATH}"
    fi
    
    echo "  ... Creating Application Bundle"
    
    macpack -m winforms -n "${GAME_NAME}" -o "${OUT_DIR}" "${OUT_DIR}/${GAME_NAME}.exe"
    # mkdir "${GAMEAPP_PATH}/Contents/Frameworks"
    
    # echo "  ... Adding Private Frameworks"
    # cp -R -p "${LIB_DIR}/"*.framework "${GAMEAPP_PATH}/Contents/Frameworks/"
    # cp -R -p "./lib/SwinGame.dll" "${GAMEAPP_PATH}/Contents/Resources/"
    
    # pushd . >> /dev/null
    # cd "${GAMEAPP_PATH}/Contents/Resources"
    # ln -s ../Frameworks/SGSDK.framework/SGSDK libSGSDK.dylib
    # ln -s ../Frameworks ./Frameworks #Silly macpac uses ./bin folder
    # popd >> /dev/null
    
    cp "${LIB_DIR}/libSGSDK.dylib" "${GAMEAPP_PATH}/Contents/Resources/libSGSDK.dylib"
    cp -R -p "./lib/SwinGame.dll" "${GAMEAPP_PATH}/Contents/Resources/"
    
    rm -f "${OUT_DIR}/${GAME_NAME}.exe"
    
    if [ -f "${EXECUTABLE_NAME}.mdb" ]
    then
        echo "  ... Adding Debug Information"
        mv "${EXECUTABLE_NAME}.mdb" "${PRODUCT_NAME}.app/Contents/Resources"
    fi
    
    echo "  ... Adding Application Information"
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

doCompile()
{
    if [ ! -d ${OUT_DIR} ]; then
        mkdir -p ${OUT_DIR}
    fi
    
    if [ "$OS" = "$WIN" ]; then
        "${GMCS_BIN}" ${GMCS_FLAGS} ${CS_FLAGS} -out:"${OUT_DIR}/${GAME_NAME}.exe" `find ${APP_PATH} -mindepth 2 -exec ${APP_PATH}/lib/cygpath -ma {} \; | grep [.]cs$` >> ${LOG_FILE}
    else
        "${GMCS_BIN}" ${GMCS_FLAGS} ${CS_FLAGS} -out:"${OUT_DIR}/${GAME_NAME}.exe" `find ${APP_PATH} -mindepth 2 | grep [.]cs$` >> ${LOG_FILE}
    fi

    if [ $? != 0 ]; then echo "Error compiling."; cat ${LOG_FILE}; exit 1; fi
}

doLinuxPackage()
{
    echo "  ... Copying SwinGame Library"
    cp -R -p "./lib/SwinGame.dll" "${OUT_DIR}/"
    RESOURCE_DIR="${FULL_OUT_DIR}/Resources"
}

doWindowsPackage()
{
    RESOURCE_DIR=${FULL_OUT_DIR}/Resources
    
    echo "  ... Copying libraries"
    cp -p -f "${LIB_DIR}"/*.dll "${OUT_DIR}"
    cp -R -p "./lib/SwinGame.dll" "${OUT_DIR}"
}

copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2

    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 -type d ! -path \*.svn\* -exec sh -c "if [ ! -d '${TO_DIR}/{}' ]; then mkdir -p '${TO_DIR}/{}'; fi" \;
    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
}

#
# Copy Resources from standard location to $RESOURCE_DIR
#
doCopyResources()
{
    echo "  ... Copying Resources into $GAME_NAME"
    
    copyWithoutSVN "${APP_PATH}/Resources" "${RESOURCE_DIR}"
}


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
    echo "  Compiler flags ${CS_FLAGS}"
    echo "--------------------------------------------------"
    echo "  ... Creating ${GAME_NAME}"
    doCompile
    
    if [ "$OS" = "$MAC" ]; then
        doMacPackage
    elif [ "$OS" = "$LIN" ]; then
        doLinuxPackage
    else
        doWindowsPackage
    fi
    
    doCopyResources
else
    CleanTmp
    rm -rf "${BIN_DIR}"
    mkdir -p "${BIN_DIR}"
    echo    ... Cleaned
fi

#remove temp files on success
rm -f ${LOG_FILE} 2>> /dev/null

echo "  Finished"
echo "--------------------------------------------------"