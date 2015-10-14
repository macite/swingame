#!/bin/sh

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
# Step 2: Determine game name
#
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

GAME_NAME=${APP_PATH##*/}

if [ "$OS" = "$MAC" ]; then
    EXE_PATH=$APP_PATH/bin/Debug/${GAME_NAME}.app/Contents/MacOS/${GAME_NAME}
elif [ "$OS" = "$LIN" ]; then
    EXE_PATH=$APP_PATH/bin/Debug/${GAME_NAME}.exe
else #Windows
    EXE_PATH=$APP_PATH/bin/Debug/${GAME_NAME}.exe
fi

VERSION="Debug Version from /bin/Debug"

if [ ! -f "${EXE_PATH}" ]; then
    if [ "$OS" = "$MAC" ]; then
        EXE_PATH=$APP_PATH/bin/Release/${GAME_NAME}.app/Contents/MacOS/${GAME_NAME}
    elif [ "$OS" = "$LIN" ]; then
        EXE_PATH=$APP_PATH/bin/Release/${GAME_NAME}.exe
    else #Windows
        EXE_PATH=$APP_PATH/bin/Release/${GAME_NAME}.exe
    fi
    VERSION="Release Version from /bin/Release"
fi

if [ ! -f "${EXE_PATH}" ]; then
    echo "Please build the game using ./build.sh" >&2
    exit -1
fi

echo "Running ${VERSION}"

if [ "$OS" = "$MAC" ]; then
    "${EXE_PATH}"
elif [ "$OS" = "$LIN" ]; then
    mono "$EXE_PATH"
else #Windows
    "${EXE_PATH}"
fi

