#!/bin/bash


# declare variables

# if [ "${1}" = "-iphone" ]; then
#   iphone=true
# elif [ "${1}" = "-ipad" ]; then
#   ipad=true
# fi

export PATH=${PATH}:/usr/local/bin/

# Get path to script - ensure we start here
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"


FULL_APP_PATH=$APP_PATH
APP_PATH="."

GAME_NAME=${FULL_APP_PATH##*/}

OUT_DIR="${APP_PATH}/bin"
LIB_DIR="${APP_PATH}/lib/godly/ios"
TMP_DIR="${APP_PATH}/tmp"
SRC_DIR="${APP_PATH}/src"

LOG_FILE="${APP_PATH}/out.log"

SG_INC="-Fi${APP_PATH}/lib -Fu${APP_PATH}/lib -Fu${APP_PATH}/src"

locateIOSSDK()
{
  BASE_XCODE_PLATFORMS="/Applications/Xcode.app/Contents/Developer/Platforms"
  BASE_IOS_SIM="${BASE_XCODE_PLATFORMS}/iPhoneSimulator.platform/Developer/SDKs"
  BASE_IOS_DEV="${BASE_XCODE_PLATFORMS}/iPhoneOS.platform/Developer/SDKs"

  if [ ! -d "$BASE_XCODE_PLATFORMS" ]; then
    echo "Unable to locate XCode SDKs. Ensure you have XCode 4+ at ${BASE_XCODE_PLATFORMS}"
    exit -1
  fi

  if [ ! -d "$BASE_IOS_SIM" ]; then
    echo "Unable to locate XCode iPhoneSimulator. Ensure you have XCode 4+ at ${BASE_IOS_SIM}"
    exit -1
  fi
  if [ ! -d "$BASE_IOS_DEV" ]; then
    echo "Unable to locate XCode iPhoneSimulator. Ensure you have XCode 4+ at ${BASE_IOS_DEV}"
    exit -1
  fi

  #
  # Find the iOS Simulator
  #
  pushd "${BASE_IOS_SIM}" >> /dev/null

  fileList=$(find "." -maxdepth 1 -type d -name iPhoneSimulator\*.sdk)
  # FILE_COUNT=$(echo "$fileList" | tr " " "\n" | wc -l)
  
  # if [ ${FILE_COUNT} = 1 ]; then
    IOS_SIM_SDK_DIR="${BASE_IOS_SIM}"`echo ${fileList[0]} | awk '{split($0,names," "); idx=1; while(names[idx+1] != "") { idx++ } printf("%s",names[idx])}' | sed "s|[.]/|/|"`
  # else
  #   echo "Select the iOS Simulator to use"
  #   PS3="File number: "
  
  #   select fileName in $fileList; do
  #       if [ -n "$fileName" ]; then
  #           IOS_SIM_SDK_DIR="${BASE_IOS_SIM}${fileName}"
  #       fi      
  #       break
  #   done
  # fi

  popd >> /dev/null

  #
  # Find the iOS Simulator
  #
  pushd "${BASE_IOS_DEV}" >> /dev/null

  fileList=$(find "." -maxdepth 1 -type d -name iPhone\*.sdk)
  # FILE_COUNT=$(echo "$fileList" | tr " " "\n" | wc -l)
  
  # if [ ${FILE_COUNT} = 1 ]; then
    IOS_DEV_SDK_DIR="${BASE_IOS_DEV}"`echo ${fileList[0]} | awk '{split($0,names," "); idx=1; while(names[idx+1] != "") { idx++ } printf("%s",names[idx])}' | sed "s|[.]/|/|"`
  # else
  #   echo "Select the iOS version to use"
  #   PS3="File number: "
  
  #   select fileName in $fileList; do
  #       if [ -n "$fileName" ]; then
  #           IOS_DEV_SDK_DIR="${BASE_IOS_DEV}${fileName}"
  #       fi      
  #       break
  #   done
  # fi

  popd >> /dev/null

  # echo ${IOS_SIM_SDK_DIR}
  # echo ${IOS_DEV_SDK_DIR}
}

locateIOSSDK


IPHONE_SDK_ARM=${IOS_DEV_SDK_DIR}

if [ ! -d ${IPHONE_SDK_ARM} ]; then
  echo "Unable to find iOS SDK"
  exit -1
fi

IPHONE_SDK_SIM=${IOS_SIM_SDK_DIR}

if [ ! -d ${IPHONE_SDK_SIM} ]; then
  echo "Unable to find iOS Simulator SDK"
  exit -1
fi



# process options
while getopts n: o
do
    case "$o" in
    n)  GAME_NAME="${OPTARG}";;
    esac
done

#check for the compilers...
PPC_386_BIN=`which ppc386 2>> /dev/null`
if [ -z "$PPC_386_BIN" ]; then
  echo "Unable to find a Pascal compiler. Install fpc-intel compiler."
  open "http://sourceforge.net/projects/freepascal/files/Mac%20OS%20X/2.6.0/"
  exit -1
fi

PPC_ARM_BIN=`which ppcarm 2>> /dev/null`
if [ -z "$PPC_ARM_BIN" ]; then
  echo "Unable to find a Pascal ARM compiler. Install fpc-arm-iOS compiler."
  open "http://sourceforge.net/projects/freepascal/files/Mac%20OS%20X/2.6.0/"
  exit -1
fi




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

DoExitCompile ()
{ 
    echo "An error occurred while compiling"; 
    cat out.log
    exit 1;  
}

locateGameMain

echo "--------------------------------------------------"
echo "          Creating $GAME_NAME"
echo "          for iOS"
echo "--------------------------------------------------"
echo "  Running script from $APP_PATH"
#echo "  Compiler flags ${SG_INC} ${C_FLAGS}"
echo "--------------------------------------------------"
echo "  ... Compiling ${GAME_NAME}"

if [ ! -d ${TMP_DIR} ]; then
    mkdir -p "${TMP_DIR}"
    mkdir "${TMP_DIR}/arm" "${TMP_DIR}/i386"
fi

if [ ! -d ${OUT_DIR} ]; then
    mkdir "${OUT_DIR}"
fi

ppcarm -Tdarwin -WP7.0 -gw -S2 -Sew -Cparmv7 -Cfvfpv2 -Sh ${SG_INC} -XX -XR"${IPHONE_SDK_ARM}" -gltw -FE"tmp/arm" -FU"tmp/arm" -Fi"src" -Fu"${LIB_DIR}" -k"/usr/lib/libbz2.dylib" -k"${LIB_DIR}/*.a" -k"-framework AudioToolbox -framework QuartzCore -framework OpenGLES -framework CoreGraphics" -k"-framework MobileCoreServices" -k"-framework ImageIO" -k"-framework UIKit -framework Foundation -framework CoreAudio" -k-no_order_inits -XMSDL_main -dIOS -dSWINGAME_OPENGL -dSWINGAME_SDL13 -o"${TMP_DIR}/${GAME_NAME}.arm" "src/${GAME_MAIN}" -k"-lstdc++" -XMSDL_main > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
   DoExitCompile; 
fi

ppc386 -Tiphonesim -WP7.0 -gw -S2 -Sew -Sh ${SG_INC} -XX -XR"${IPHONE_SDK_SIM}" -gltw -FE"tmp/i386" -FU"tmp/i386" -Fi"src" -Fu"${LIB_DIR}" -k"/usr/lib/libbz2.dylib" -k"${LIB_DIR}/*.a" -o"${TMP_DIR}/${GAME_NAME}.i386" "src/${GAME_MAIN}" -k"-framework AudioToolbox" -k"-framework QuartzCore" -k"-framework OpenGLES" -k"-framework CoreGraphics" -k"-framework MobileCoreServices" -k"-framework ImageIO" -k"-framework UIKit" -k"-framework Foundation" -k"-framework CoreAudio" -k"-lstdc++" -XMSDL_main -k-no_order_inits -dIOS -dSWINGAME_OPENGL -dSWINGAME_SDL13 > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
   DoExitCompile;
fi

# exit

lipo -create -output "${OUT_DIR}/${GAME_NAME}" "${TMP_DIR}/${GAME_NAME}.i386" "${TMP_DIR}/${GAME_NAME}.arm" > ${LOG_FILE} 2> ${LOG_FILE}

# mv "${TMP_DIR}/${GAME_NAME}.i386" "${OUT_DIR}/${GAME_NAME}"

dsymutil --verbose "${OUT_DIR}/${GAME_NAME}" -o "${OUT_DIR}/${GAME_NAME}.app.dSYM" > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
    echo 'Failed to create debug symbols'
    cat out.log
    exit 1;
fi

echo "  ... Creating XCode Project"
python ${APP_PATH}/tools/create_xcode_project.py "${GAME_NAME}" > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
    echo 'Failed to create xcode project'
    cat out.log
    exit 1;
fi

echo "  ... Preparing App"
killall -c waxsim 2>/dev/null
xcodebuild -project iOS.xcodeproj -target iOS -sdk iphonesimulator7.0 -arch i386 -configuration Debug > ${LOG_FILE} 2> ${LOG_FILE}
if [ $? != 0 ]; then
    echo 'Failed to create app'
    cat out.log
    exit 1;
fi

echo "  ... Opening App"
./tools/waxsim -s 7.0 ./build/Debug-iphonesimulator/iOS.app > ${LOG_FILE} 2> ${LOG_FILE} &
osascript ./tools/ShowSimulator.scpt > ${LOG_FILE} 2> ${LOG_FILE}

# open "${GAME_NAME}.xcodeproj"

echo "--------------------------------------------------"
echo "Finished!"