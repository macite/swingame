#!/bin/bash

#clean
# rm -rf tmp
# mkdir tmp

# declare variables
# 
# 
# 
# if [ "${1}" = "-iphone" ]; then
#   iphone=true
# elif [ "${1}" = "-ipad" ]; then
#   ipad=true
# fi
# 
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"
FULL_APP_PATH=$APP_PATH

GAME_NAME=${APP_PATH##*/}

APP_PATH="."

OUT_DIR="${APP_PATH}/bin"
SRC_DIR="${APP_PATH}/src"

LOG_FILE="${APP_PATH}/out.log"
SG_INC="-Fi${APP_PATH}/lib -Fu${APP_PATH}/lib -Fu${APP_PATH}/src ${DRV_LIB}"

IPHONE_SDK_ARM="/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.0.sdk"

if [ ! -d ${IPHONE_SDK_ARM} ]; then
  IPHONE_SDK_ARM="/Applications/Xcode.app/Contents${IPHONE_SDK_ARM}"
  if [ ! -d ${IPHONE_SDK_ARM} ]; then
    IPHONE_SDK_ARM="/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk"
    if [ ! -d ${IPHONE_SDK_ARM} ]; then
      IPHONE_SDK_ARM="/Applications/Xcode.app/Contents${IPHONE_SDK_ARM}"
      if [ ! -d ${IPHONE_SDK_ARM} ]; then
        echo "Unable to find iOS SDK"
        exit -1
      fi
    fi
  fi
fi

IPHONE_SDK_SIM="/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk"

if [ ! -d ${IPHONE_SDK_SIM} ]; then
  IPHONE_SDK_SIM="/Applications/Xcode.app/Contents${IPHONE_SDK_SIM}"
  if [ ! -d ${IPHONE_SDK_SIM} ]; then
    IPHONE_SDK_SIM="/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk"
    if [ ! -d ${IPHONE_SDK_SIM} ]; then
      IPHONE_SDK_SIM="/Applications/Xcode.app/Contents${IPHONE_SDK_SIM}"
      if [ ! -d ${IPHONE_SDK_SIM} ]; then
        echo "Unable to find iOS Simulator SDK"
        exit -1
      fi
    fi
  fi
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
  
  cd ${FULL_APP_PATH}
  
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


#Generates UUID and save to txt file for future use.



locateGameMain

echo "--------------------------------------------------"
echo "          Creating $GAME_NAME"
echo "          for iOS"
echo "--------------------------------------------------"
echo "  Running script from $FULL_APP_PATH"
echo "  Saving output to $OUT_DIR"
#echo "  Compiler flags ${SG_INC} ${C_FLAGS}"
echo "--------------------------------------------------"
echo "  ... Compiling ${GAME_NAME}"

gcc -g -XX -k-ios_version_min -k5.0 -XR"${IPHONE_SDK_SIM}" -gltw -FE"tmp" -FU"tmp" -Fi"src" -Fu"lib/mac/static_libraries" -k"/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk/usr/lib/libbz2.dylib" -k"lib/mac/static_libraries/libSDL.a" -k"lib/mac/static_libraries/libpng.a" -k"lib/mac/static_libraries/libSDL_gfx.a" -k"lib/mac/static_libraries/libSDL_mixer.a" -k"lib/mac/static_libraries/libSDL_image.a" -k"lib/mac/static_libraries/libSDL_ttf.a" -k"lib/mac/static_libraries/libfreetype.a" -k"lib/mac/static_libraries/libogg.a" -k"lib/mac/static_libraries/libvorbis.a"  -Fu/Developer/ObjectivePascal/units/i386-iphonesim -Tiphonesim  -o"./${GAME_NAME}" "src/${GAME_MAIN}" -k-framework -kAudioToolbox -k-framework -kQuartzCore -k-framework -kOpenGLES -k-framework -kCoreGraphics -k"-framework MobileCoreServices" -k"-framework ImageIO" -k-framework -kUIKit -k-framework -kFoundation -k-framework -kCoreAudio -k-no_order_inits -XMSDL_main -dIOS -dSWINGAME_SDL13 > ${LOG_FILE} 2> ${LOG_FILE}

if [ $? != 0 ]; then
   DoExitCompile;
fi




OUT_APP_DIR="${OUT_DIR}${GAME_NAME}.app/"


echo "  ... Installing ${GAME_NAME} to iOS Simulator."
#move file to simulator install folder

if ! [ -d "${OUT_APP_DIR}" ]; then
  mkdir -p "${OUT_APP_DIR}"
fi


mv "${GAME_NAME}" "${OUT_APP_DIR}${GAME_NAME}"

#generate info.plist and pkginfo
if [ "${iphone}" = true ]; then

  echo "<?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
  <dict>
      <key>CFBundleIdentifier</key>
      <string>com.yourcompany.${GAME_NAME}</string>
      <key>CFBundleName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleDisplayName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>CFBundleDevelopmentRegion</key>
      <string>English</string>
      <key>CFBundleExecutable</key>
      <string>${GAME_NAME}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleSignature</key>
      <string>SWIN</string>
      <key>CFBundleVersion</key>
      <string>1.0</string>
      <key>DTPlatformName</key>
      <string>iphonesimulator</string>
      <key>DTSDKName</key>
      <string>iphonesimulator5.0</string>
      <key>UIDeviceFamily</key>
      <array>
          <integer>1</integer>
          <integer>2</integer>
      </array>
      <key>CFBundleSUpportedPlatforms</key>
      <array>
          <string>iPhoneSimulator</string>
      </array>
  </dict>
  </plist>" > "${OUT_APP_DIR}/Info.plist"

elif [ "${ipad}" = true ]; then
  echo "<?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
  <dict>
      <key>CFBundleIdentifier</key>
      <string>com.yourcompany.${GAME_NAME}</string>
      <key>CFBundleName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleDisplayName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>CFBundleDevelopmentRegion</key>
      <string>English</string>
      <key>CFBundleExecutable</key>
      <string>${GAME_NAME}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleSignature</key>
      <string>SWIN</string>
      <key>CFBundleVersion</key>
      <string>1.0</string>
      <key>DTPlatformName</key>
      <string>iphonesimulator</string>
      <key>DTSDKName</key>
      <string>iphonesimulator5.0</string>
      <key>UIDeviceFamily</key>
      <array>
          <integer>2</integer>
      </array>
      <key>CFBundleSUpportedPlatforms</key>
      <array>
          <string>iPhoneSimulator</string>
      </array>
  </dict>
  </plist>" > "${OUT_APP_DIR}/Info.plist"
#else generate for both.
else
  echo "<?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
  <dict>
      <key>CFBundleIdentifier</key>
      <string>com.yourcompany.${GAME_NAME}</string>
      <key>CFBundleName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleDisplayName</key>
      <string>${GAME_NAME}</string>
      <key>CFBundleInfoDictionaryVersion</key>
      <string>6.0</string>
      <key>CFBundleDevelopmentRegion</key>
      <string>English</string>
      <key>CFBundleExecutable</key>
      <string>${GAME_NAME}</string>
      <key>CFBundlePackageType</key>
      <string>APPL</string>
      <key>CFBundleSignature</key>
      <string>SWIN</string>
      <key>CFBundleVersion</key>
      <string>1.0</string>
      <key>DTPlatformName</key>
      <string>iphonesimulator</string>
      <key>DTSDKName</key>
      <string>iphonesimulator5.0</string>
      <key>UIDeviceFamily</key>
      <array>
          <integer>1</integer>
          <integer>2</integer>
      </array>
      <key>CFBundleSUpportedPlatforms</key>
      <array>
          <string>iPhoneSimulator</string>
      </array>
  </dict>
  </plist>" > "${OUT_APP_DIR}/Info.plist"
fi
echo "APPLSWIN" > "${OUT_APP_DIR}/PkgInfo"

copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 ! -path \*.svn\* ! -path \*/. -type d -exec mkdir -p "${TO_DIR}/{}" \;
    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
    cd "${FULL_APP_PATH}"
}


echo "  ... Copying resources."
mkdir -p "${OUT_DIR}Resources"



copyWithoutSVN "./Resources" "${OUT_DIR}/Resources"
#setting icon
cp "./Resources/SwinGame.ico" "${OUT_APP_DIR}/icon.png"
#setting pre splash
cp "./Resources/images/Swinburne.jpg" "${OUT_APP_DIR}/default.png"
rm -f ${LOG_FILE} 2>> /dev/null
echo "  ... Starting iOS Simulator."
#restart simulator
osascript <<EOF
tell application "System Events" 
  set SimulatorOpen to ("iPhone Simulator") 
end tell
tell application "iPhone Simulator"
  if (SimulatorOpen contains "iPhone Simulator") is true then 
      quit
      delay 1
    end if
    activate
end tell
EOF

echo "--------------------------------------------------"
echo "Finished!"
