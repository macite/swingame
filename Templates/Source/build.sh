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

#
# Step 2: Move to the directory containing the script
#
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd`
cd "$APP_PATH"
FULL_APP_PATH=$APP_PATH
APP_PATH="."

#
# Step 3: Setup options
#
STATIC_NAME="libSGSDK.a"

PAS_FLAGS=""
EXTRA_OPTS="-O1 -fPIC -Sewn -vwn -dSWINGAME_LIB"

VERSION_NO=4.0
VERSION=4.0

DEBUG=false
CLEAN=false
INSTALL=false
STATIC=false
FRAMEWORK=false
IOS=false
SG_WIN32=true
SG_WIN64=false

#
# Step 4: Usage message and process command line arguments
#
Usage()
{
  echo "Usage: ./build.sh [-c] [-d] [-i] [-h] [-IOS] [-framework] [-static] [version] [-win32|-win64]"
  echo
  echo "Creates and Compiles the native SGSDK library."
  echo
  echo "Options:"
  echo " -c     Perform a clean rather than a build"
  echo " -d     Compile with debug symbols"
  echo " -static  Compile as a static library"
  echo " -h     Show this help message"
  echo " -i     Install after compiling"
  echo " -win32   Compile a 32bit version -- needs to be run in 32bit mingw terminal"
  echo " -win64   Compile a 64bit version -- needs to be run in 64bit mingw terminal"
  echo
  echo "Requires:"
  echo " - Free Pascal Compiler"

  if [ "$OS" = "$LIN" ]; then
    echo " - sgsdl2"
  fi
  exit 0
}

#
# Read and process command line arguments
#
while getopts chdif:I:s:w: o
do
  case "$o" in
  c)  CLEAN=true ;;
  h)  Usage ;;
  f)  if [ "${OPTARG}" = "ramework" ]; then
      FRAMEWORK=true
    fi
    ;;
  s)  if [ "${OPTARG}" = "tatic" ]; then
      STATIC=true
    fi
    ;;
  d)  EXTRA_OPTS="-vwn -gw -dTRACE -dSWINGAME_LIB"
    DEBUG=true ;;
  I)  if [ "${OPTARG}" = "OS" ]; then
      IOS=true
    fi;;
  i)  INSTALL=true;;
  w)  if [ "${OPTARG}" = "in32" ]; then
      SG_WIN32=true
      SG_WIN64=false
    elif [ "${OPTARG}" = "in64" ]; then
      SG_WIN32=false
      SG_WIN64=true
    fi
    ;;
  [?]) print >&2 "Usage: $0 [-c] [-d] [-i] [-h] [version]"
     exit -1;;
  esac
done

shift $((${OPTIND}-1))

if [ "a$1" != "a" ]; then
  VERSION=$1
fi

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
  IOS_SIM_SDK_DIR="${BASE_IOS_SIM}"`echo $fileList | awk '{split($0,names," "); idx=1; while(names[idx+1] != "") { idx++ } printf("%s",names[idx])}' | sed "s|[.]/|/|"`
  # else
  #   echo "Select the iOS Simulator to use"
  #   PS3="File number: "

  #   select fileName in $fileList; do
  #     if [ -n "$fileName" ]; then
  #       IOS_SIM_SDK_DIR="${BASE_IOS_SIM}${fileName}"
  #     fi
  #     break
  #   done
  # fi
  # echo $IOS_SIM_SDK_DIR

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
  #     if [ -n "$fileName" ]; then
  #       IOS_DEV_SDK_DIR="${BASE_IOS_DEV}${fileName}"
  #     fi
  #     break
  #   done
  # fi

  popd >> /dev/null

  # echo ${IOS_SIM_SDK_DIR}
  # echo ${IOS_DEV_SDK_DIR}
}

#
# Step 5: Set the paths to local variables
#

TMP_DIR="${APP_PATH}/tmp/"
SDK_SRC_DIR="${APP_PATH}/src"
LOG_FILE="${APP_PATH}/tmp/out.log"

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

if [ ${IOS} = true ]; then
  locateIOSSDK

  IPHONE_SDK_ARM="${IOS_DEV_SDK_DIR}"
  IPHONE_SDK_SIM="${IOS_SIM_SDK_DIR}"

  if [ ! -d ${IPHONE_SDK_ARM} ]; then
    echo "Unable to find iOS SDK"
    exit -1
  fi

  if [ ! -d ${IPHONE_SDK_SIM} ]; then
    echo "Unable to find iOS Simulator SDK"
    exit -1
  fi

  #check for the compilers...
  PPC_386_BIN=`which ppc386 2>> /dev/null`
  if [ -z "$PPC_386_BIN" ]; then
    echo "Unable to find a Pascal Intel compiler. Install fpc-intel compiler."
    open "http://sourceforge.net/projects/freepascal/files/Mac%20OS%20X/2.6.0/"
    exit -1
  fi

  PPC_ARM_BIN=`which ppcarm 2>> /dev/null`
  if [ -z "$PPC_ARM_BIN" ]; then
    echo "Unable to find a Pascal ARM compiler. Install fpc-arm-iOS compiler."
    open "http://sourceforge.net/projects/freepascal/files/Mac%20OS%20X/2.6.0/"
    exit -1
  fi

  OUT_DIR="${APP_PATH}/bin/ios"
  FULL_OUT_DIR="${FULL_APP_PATH}/bin/ios"
  # VERSION_DIR="${OUT_DIR}/SGSDK.framework/Versions/${VERSION}"
  # HEADER_DIR="${VERSION_DIR}/Headers"
  # RESOURCES_DIR="${VERSION_DIR}/Resources"
  # CURRENT_DIR="${OUT_DIR}/SGSDK.framework/Versions/Current"

  LIB_DIR="${APP_PATH}/staticlib/godly/ios"

elif [ "$OS" = "$MAC" ]; then
  if [ "$FPC_VER" != "2.6.4" ]; then
    echo 'FPC needs to be 2.6.4'
    exit
  fi

  OUT_DIR="${APP_PATH}/bin/mac"
  FULL_OUT_DIR="${FULL_APP_PATH}/bin/mac"
  VERSION_DIR="${OUT_DIR}/SGSDK.framework/Versions/${VERSION}"
  HEADER_DIR="${VERSION_DIR}/Headers"
  RESOURCES_DIR="${VERSION_DIR}/Resources"
  CURRENT_DIR="${OUT_DIR}/SGSDK.framework/Versions/Current"

  # Set lib dir
  LIB_DIR="${APP_PATH}/staticlib/mac"

  PAS_FLAGS="${PAS_FLAGS} -dSWINGAME_SDL2 -k\"-lz\" -k\"-lbz2\" -k\"-lstdc++\" -k\"-lm\" -k\"-lc\" -k\"-lc++\" -k\"-lcurl\""

  #
  # Setup framework/dylib details
  #
  if [ ${FRAMEWORK} = true ]; then
    OUT_FILE="SGSDK"
    INSTALL_NAME="@rpath/SGSDK.framework/Versions/${VERSION}/${OUT_FILE}"
  else
    OUT_FILE="libSGSDK.dylib"
    INSTALL_NAME="@rpath/${OUT_FILE}"
  fi

elif [ "$OS" = "$WIN" ]; then
  if [ ${SG_WIN32} = true ]; then
    LIB_DIR="${APP_PATH}/lib/win32"
    OUT_DIR="${APP_PATH}/bin/win32"
    FULL_OUT_DIR="${FULL_APP_PATH}/bin/win32"
  else
    LIB_DIR="${APP_PATH}/lib/win64"
    OUT_DIR="${APP_PATH}/bin/win64"
    FULL_OUT_DIR="${FULL_APP_PATH}/bin/win64"

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

  # FPC paths require c:/
  SDK_SRC_DIR=`echo $SDK_SRC_DIR | sed 's/\/\(.\)\//\1:\//'`
  OUT_DIR=`echo $OUT_DIR | sed 's/\/\(.\)\//\1:\//'`
else #linux
  OUT_DIR="${APP_PATH}/bin/linux"
fi

if [ ${INSTALL} = true ]; then
  if [ $OS = "$MAC" ]; then
    INSTALL_DIR=`cd ~/Library/Frameworks; pwd`
  elif [ "$OS" = "$LIN" ]; then
    INSTALL_DIR="/usr/lib"
    HEADER_DIR="/usr/include/sgsdk"
  else #windows
    INSTALL_DIR="$SYSTEMROOT/System32"
  fi
fi

#
# Step 6: Setup log file
#
if [ -f "${LOG_FILE}" ]
then
  rm -f "${LOG_FILE}"
fi

#
# Step 7: Declare functions for compiling
#

DoDriverMessage()
{
  echo "  ... Using SGSDK Backend Driver"
}

DisplayHeader()
{
  echo "--------------------------------------------------"
  echo "      Creating SwinGame Dynamic Library"
  echo "         for $OS"
  echo "--------------------------------------------------"
  echo "  Running script from $FULL_APP_PATH"
  echo "  Saving output to $OUT_DIR"
  
  if [ "$OS" = "$MAC" ]; then
    echo "  Copying Frameworks from ${LIB_DIR}"
  elif [ "$OS" = "$WIN" ]; then
    echo "  Copying libraries from ${LIB_DIR}"
  fi
  
  echo "  Compiling with $EXTRA_OPTS"
  if [ $INSTALL = true ]; then
    echo "  Installing to $INSTALL_DIR"
    if [ "$OS" = "$LIN" ]; then
      echo "  Copying headers to $HEADER_DIR"
    fi
  fi
  
  echo "--------------------------------------------------"
  DoDriverMessage
}

# Clean up the temporary directory
CleanTmp()
{
  if [ -d "${TMP_DIR}" ]
  then
    rm -rf "${TMP_DIR}"
  fi
}

PrepareTmp()
{
  CleanTmp
  mkdir -p "${TMP_DIR}"
}

#
# Compile for Mac - manually assembles and links files
# argument 1 is arch
#
doMacCompile()
{
  PrepareTmp

  ARCH="$1"
  LINK_OPTS="$2"

  FRAMEWORKS='-framework AudioToolbox -framework AudioUnit -framework CoreAudio -framework CoreVideo -framework IOKit -framework OpenGL -framework Carbon -framework ForceFeedback'

  # FRAMEWORKS=`cd ${LIB_DIR};ls -d *.framework | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-framework %s ", patharr[idx]) }'`

  STATIC_LIBS=`cd ${LIB_DIR};ls -f *.a | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("-l%s ", substr(patharr[idx],4)) }'`

  if [ ${DEBUG} = true ] ; then
    EXTRA_OPTS="$EXTRA_OPTS -Xs"
  fi
  
  # Compile...
  "${FPC_BIN}" ${PAS_FLAGS} -S2 -Sh ${EXTRA_OPTS} -FE"${TMP_DIR}" -FU"${TMP_DIR}" -k"$LINK_OPTS -L'${TMP_DIR}' -F'${LIB_DIR}' -current_version '${VERSION_NO}'" -k"-lbz2" -k"-lstdc++" -k"-install_name '${INSTALL_NAME}'" -k"-rpath @loader_path/../Frameworks -rpath @executable_path/../Frameworks -rpath ../Frameworks -rpath ." -k"-L ${LIB_DIR}" -k"${STATIC_LIBS}" -k" ${FRAMEWORKS} -framework Cocoa" "${SDK_SRC_DIR}/SGSDK.pas"  >> "${LOG_FILE}"
  
  if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat "${LOG_FILE}"; exit 1; fi
  rm -f "${LOG_FILE}"

  mv ${TMP_DIR}/libSGSDK.dylib ${OUT_DIR}/libSGSDK${1}.dylib
  
  if [ $STATIC = true ]; then
    ar -rcs ${OUT_DIR}/${STATIC_NAME}.${ARCH} ${TMP_DIR}/*.o
  fi
}

DoExitCompile ()
{
  echo "An error occurred while compiling";
  cat tmp/out.log

  if [ "$OS" = "$LIN" ]; then
    echo ""
    echo "Make sure you have the required libraries installed:"
    echo "sudo apt-get install fpc curl libsdl1.2-dev libsdl-gfx1.2-dev libsdl-image1.2-dev libsdl-mixer1.2-dev libsdl-ttf2.0-dev libsdl-net* libsmpeg*"
  fi

  exit 1;
}

doIOSCompile()
{
  TMP_DIR="${APP_PATH}/tmp/ios"
  PrepareTmp
  mkdir "${TMP_DIR}/arm" "${TMP_DIR}/i386"

  "${PPC_ARM_BIN}" -Cn -gw -S2 -Sew -Cparmv7 -Cfvfpv2 -Sh ${SG_INC} -XX -k-ios_version_min -k5.0 -XR"${IPHONE_SDK_ARM}" -gltw -FE"${TMP_DIR}/arm" -FU"${TMP_DIR}/arm" -Fi"src" -Fu"${LIB_DIR}" -k"/usr/lib/libbz2.dylib" -k"${LIB_DIR}/*.a" -k"-framework AudioToolbox -framework QuartzCore -framework OpenGLES -framework CoreGraphics" -k"-framework MobileCoreServices" -k"-framework ImageIO" -k"-framework UIKit -framework Foundation -framework CoreAudio" -k-no_order_inits -XMSDL_main -dIOS -dSWINGAME_OPENGL -dSWINGAME_SDL13 -o"${TMP_DIR}/SGSDK.arm" "${SDK_SRC_DIR}/SGSDK.pas" > ${LOG_FILE} 2> ${LOG_FILE}
  
  if [ $? != 0 ]; then
    DoExitCompile;
  fi

  ar -rcs ${TMP_DIR}/${STATIC_NAME}.arm ${TMP_DIR}/arm/*.o

  "${PPC_386_BIN}" -Tiphonesim -WP7.0 -Cn -gw -S2 -Sew -Sh ${SG_INC} -XX -XR"${IPHONE_SDK_SIM}" -gltw -FE"${TMP_DIR}/i386" -FU"${TMP_DIR}/i386" -Fi"src" -Fu"${LIB_DIR}" -k"/usr/lib/libbz2.dylib" -k"${LIB_DIR}/*.a" -o"${TMP_DIR}/${GAME_NAME}.i386" "${SDK_SRC_DIR}/SGSDK.pas" -k-framework -kAudioToolbox -k-framework -kQuartzCore -k-framework -kOpenGLES -k-framework -kCoreGraphics -k"-framework MobileCoreServices" -k"-framework ImageIO" -k-framework -kUIKit -k-framework -kFoundation -k-framework -kCoreAudio -k-no_order_inits -XMSDL_main -dIOS -dSWINGAME_OPENGL -dSWINGAME_SDL13 > ${LOG_FILE} 2> ${LOG_FILE}
  
  if [ $? != 0 ]; then
    DoExitCompile;
  fi

  ar -rcs ${TMP_DIR}/${STATIC_NAME}.i386 ${TMP_DIR}/i386/*.o

  lipo -create -output "${OUT_DIR}/${STATIC_NAME}" "${TMP_DIR}/${STATIC_NAME}.i386" "${TMP_DIR}/${STATIC_NAME}.arm" > ${LOG_FILE} 2> ${LOG_FILE}
}

#
# Create fat dylib containing x64 and i386 code
#
doLipo()
{
  echo "  ... Creating Universal Binary"
  lipo -arch ${1} "${OUT_DIR}/libSGSDK${1}.dylib" -arch ${2} "${OUT_DIR}/libSGSDK${2}.dylib" -output "${OUT_DIR}/libSGSDK.dylib" -create

  rm -rf "${OUT_DIR}/libSGSDK${1}.dylib"
  rm -rf "${OUT_DIR}/libSGSDK${2}.dylib"
  
  if [ $STATIC = true ]; then
    lipo -arch ${1} "${OUT_DIR}/${STATIC_NAME}.${1}" -arch ${2} "${OUT_DIR}/${STATIC_NAME}.${2}" -output "${OUT_DIR}/${STATIC_NAME}" -create
    rm -f "${OUT_DIR}/${STATIC_NAME}.${1}"
    rm -f "${OUT_DIR}/${STATIC_NAME}.${2}"
  fi
}

#
# Create Mac Framework
#
doCreateFramework()
{
  echo "  ... Creating Framework version ${VERSION}"
  
  if [ ! -d "${OUT_DIR}/SGSDK.framework" ]
  then
    mkdir "${OUT_DIR}/SGSDK.framework"
    mkdir "${OUT_DIR}/SGSDK.framework/Versions"
    ADD_SYMLINKS=true
  else
    rm -rf "${OUT_DIR}/SGSDK.framework/Versions/${VERSION}"
    ADD_SYMLINKS=false
  fi

  mkdir "${VERSION_DIR}"
  mkdir "${HEADER_DIR}"
  mkdir "${RESOURCES_DIR}"

  cp "${SDK_SRC_DIR}/SGSDK.h" "${HEADER_DIR}/SGSDK.h"
  cp "${SDK_SRC_DIR}/Types.h" "${HEADER_DIR}/Types.h"
  mv "${OUT_DIR}/libSGSDK.dylib" "${VERSION_DIR}/SGSDK"

  echo "<?xml version='1.0' encoding='UTF-8'?>\
  <!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\
  <plist version=\"1.0\">\
  <dict>\
      <key>CFBundleName</key>\
      <string>SGSDK</string>\
      <key>CFBundleIdentifier</key>\
      <string>au.edu.swinburne.SGSDK</string>\
      <key>CFBundleVersion</key>\
      <string>${VERSION}</string>\
      <key>CFBundleDevelopmentRegion</key>\
      <string>English</string>\
      <key>CFBundleExecutable</key>\
      <string>SGSDK</string>\
      <key>CFBundleInfoDictionaryVersion</key>\
      <string>6.0</string>\
      <key>CFBundlePackageType</key>\
      <string>FMWK</string>\
      <key>CFBundleSignature</key>\
      <string>SWIN</string>\
  </dict>\
  </plist>" >> "${RESOURCES_DIR}/Info.plist"

  cd "${OUT_DIR}/SGSDK.framework"
  #link is relative to Current link...
  if [ -d ./Versions/Current ]
  then
    rm ./Versions/Current
  fi
  ln -f -s "./${VERSION}" "./Versions/Current"

  if [ $ADD_SYMLINKS = true ]
  then
    ln -f -s "./Versions/Current/Resources" "./Resources"
    ln -f -s "./Versions/Current/Headers" "./Headers"
    ln -f -s "./Versions/Current/SGSDK" "./SGSDK"
  fi
}

#
# Step 8: Do each OSs create/install process...
#
if [ $CLEAN = false ]
then
  if [ ! -d "${OUT_DIR}" ]
  then
    mkdir -p "${OUT_DIR}"
  fi

  if [ "$OS" = "$MAC" ]; then
    DisplayHeader

    if [ ${IOS} = true ]; then
      doIOSCompile
      
      echo "  Finished"
      echo "--------------------------------------------------"
      exit
    fi

    HAS_i386=false
    HAS_x64=false

    OS_VER=`sw_vers -productVersion | awk -F . '{print $1"."$2}'`
    OS_VER_MINOR=`sw_vers -productVersion | awk -F . '{print $2}'`
    XCODE_PREFIX=''

    USR_PATHS='/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr'
    if [ ! -d $USR_PATHS ]; then
      USR_PATHS='/usr'
    fi

    if [ -d /Applications/Xcode.app/Contents ]; then
      if [ -d /Applications/Xcode.app/Contents/Developer/Platforms ]; then
        XCODE_PREFIX='/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform'
      else
        XCODE_PREFIX='/Applications/Xcode.app/Contents'
      fi
    fi

    if [ -f "${USR_PATHS}/libexec/as/i386/as" ]; then
      HAS_i386=true
    fi

    if [ -f "${USR_PATHS}/libexec/as/x86_64/as" ]; then
      HAS_x64=true
    fi

    PAS_FLAGS="$PAS_FLAGS -WM10.7"

    SDK_PATH="${XCODE_PREFIX}/Developer/SDKs/MacOSX${OS_VER}.sdk"
    if [ ! -d ${SDK_PATH} ]; then
      echo "Unable to locate MacOS SDK. ${SDK_PATH}"
      exit -1
    fi
    SDK_FLAGS="-syslibroot ${SDK_PATH} -macosx_version_min 10.7"

    echo "  ... Compiling Library"

    if [[ $HAS_i386 = true && $HAS_x64 = true ]]; then
      echo "  ... Building Universal Binary"

      #Compile i386 version of library
      FPC_BIN=`which ppc386`
      doMacCompile "i386" "$SDK_FLAGS"

      #Compile ppc version of library
      FPC_BIN=`which ppcx64`
      doMacCompile "x86_64" "$SDK_FLAGS"

      #Combine into a fat dylib
      doLipo "i386" "x86_64"
    else
      #Compile i386 version of library
      FPC_BIN=`which ppc386`
      doMacCompile "i386" "$SDK_FLAGS"

      mv ${OUT_DIR}/libSGSDKi386.dylib ${OUT_DIR}/libSGSDK.dylib

      if [ $STATIC = true ]; then
        ar -rcs ${OUT_DIR}/${STATIC_NAME} ${TMP_DIR}/*.o
      fi
    fi


    #Convert into a Framework
    if [ ${FRAMEWORK} = true ]; then
      doCreateFramework

      if [ $INSTALL = true ]
      then
        echo "  ... Installing SwinGame"
        if [ ! -d "${INSTALL_DIR}" ]
        then
          mkdir -p "${INSTALL_DIR}"
        fi

        doCopyFramework()
        {
          # $1 = framework
          # $2 = dest
          fwk_name=${1##*/} # ## = delete longest match for */... ie all but file name

          if [ -d "${2}/${fwk_name}" ]
          then
            #framework exists at destn, just copy the version details
            rm $2/${fwk_name}/Versions/Current
            cp -p -R -f "${1}/Versions/"* "${2}/${fwk_name}/Versions"
          else
            cp -p -R "$1" "$2"
          fi
        }

        doCopyFramework "${FULL_OUT_DIR}"/SGSDK.framework "${INSTALL_DIR}"
        for file in `find ${LIB_DIR} -depth 1 | grep [.]framework$`
        do
          doCopyFramework ${file} "${INSTALL_DIR}"
        done
      fi # install
    else #not framework = dylib
      mv ${OUT_DIR}/libSGSDK.dylib ${OUT_DIR}/libSGSDK${NAME_SUFFIX}.dylib
    fi # framework

  elif [ "$OS" = "$WIN" ]
  then
    DisplayHeader

    PrepareTmp

    ${FPC_BIN} -S2 -Sh $EXTRA_OPTS -FE"${TMP_DIR}" -FU"${TMP_DIR}" "${SDK_SRC_DIR}/SGSDK.pas" >> "${LOG_FILE}"
    if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat "${LOG_FILE}"; exit 1; fi

    mv "${TMP_DIR}/SGSDK.dll" "${OUT_DIR}/SGSDK.dll"

    if [ $INSTALL = true ]
    then
      echo "  ... Installing SwinGame"

      cp -p -f "${OUT_DIR}/SGSDK.dll" ${INSTALL_DIR}
    fi
    CleanTmp
  else #os = Linux
    DisplayHeader
    
    if [ ! -e "/usr/lib/libsgsdl2.so" ] && [ ! -e "$APP_PATH/sgsdl2/libsgsdl2.so" ]; then
      "$APP_PATH/sgsdl2/build.sh"
    fi

    if [ -e "$APP_PATH/sgsdl2/libsgsdl2.so" ]; then
      EXTRA_OPTS="${EXTRA_OPTS} -Fl$APP_PATH/sgsdl2"
    fi

    PrepareTmp

    echo "  ... Compiling Library"
    fpc -S2 -k"-rpath=\$ORIGIN" -Sh $EXTRA_OPTS -FE"${TMP_DIR}" -FU"${TMP_DIR}" "${SDK_SRC_DIR}/SGSDK.pas" >> ${LOG_FILE}
    if [ $? != 0 ]; then echo "Error compiling SGSDK"; cat ${LOG_FILE}; exit 1; fi

    mv "${TMP_DIR}/libSGSDK.so" "${OUT_DIR}/libSGSDK.so.${VERSION}"
    ln -s -f "libSGSDK.so.${VERSION}" "${OUT_DIR}/libsgsdk.so"
    ln -s -f "libSGSDK.so.${VERSION}" "${OUT_DIR}/libSGSDK.so"

    if [ $INSTALL = true ]
    then
      echo "  ... Installing SwinGame -- needs sudo permission"
      cd "$FULL_APP_PATH"
      sudo install "${OUT_DIR}"/libSGSDK.* "${INSTALL_DIR}"
      sudo install "${OUT_DIR}"/libsgsdk.* "${INSTALL_DIR}"

      if [ ! -d ${HEADER_DIR} ]
      then
        echo "  ... Creating header directory"
        sudo mkdir -p ${HEADER_DIR}
        sudo chmod a+r ${HEADER_DIR}
      fi

      echo "  ... Copying header files"
      sudo cp "${SDK_SRC_DIR}/SGSDK.h" "${HEADER_DIR}/sgsdk.h"
      sudo cp "${SDK_SRC_DIR}/Types.h" "${HEADER_DIR}/Types.h"
    fi
    CleanTmp
  fi
else
  echo "--------------------------------------------------"
  echo "        SwinGame Dynamic Library"
  echo "--------------------------------------------------"
  CleanTmp
  rm -rf "${OUT_DIR}"
  mkdir -p "${OUT_DIR}"
  echo  ... Cleaned
fi

rm -f ${LOG_FILE}
# rm -rf ${TMP_DIR} 2>>/dev/null

echo "  Finished"
echo "--------------------------------------------------"
