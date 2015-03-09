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
    EXE_PATH=$APP_PATH/bin/Debug/${GAME_NAME}
else #Windows
    EXE_PATH=$APP_PATH/bin/Debug/${GAME_NAME}.exe
fi

VERSION="Debug Version from /bin/Debug"

if [ ! -f "${EXE_PATH}" ]; then
    if [ "$OS" = "$MAC" ]; then
        EXE_PATH=$APP_PATH/bin/Release/${GAME_NAME}.app/Contents/MacOS/${GAME_NAME}
    elif [ "$OS" = "$LIN" ]; then
        EXE_PATH=$APP_PATH/bin/Release/${GAME_NAME}
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

    if [ ! -f "${APP_PATH}/lib/bring_fg.scpt" ]; then

printf "on run argv \n\
    try \n\
        Delay(0.5) \n\
        set proc to \"\" & item 1 of argv & \"\" \n\
        tell application \"System Events\" \n\
          tell process proc \n\
              set frontmost to true \n\
          end tell \n\
        end tell \n\
    on error \n\
    end try \n\
    return \n\
end run " >> "${APP_PATH}/lib/bring_fg.scpt"
    fi

    osascript "${APP_PATH}/lib/bring_fg.scpt" "${GAME_NAME}" & "$EXE_PATH"
else
    "$EXE_PATH"
fi