#!/bin/sh

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

GAME_NAME=${APP_PATH##*/}

if [ -z "$APP_PATH" ]; then
    exit -1
fi

echo "--------------------------------------------------"
echo "          Cleaning Up $GAME_NAME"
echo "--------------------------------------------------"

rm -rf "${APP_PATH}/tmp"
mkdir "${APP_PATH}/tmp"
echo " * Cleaned tmp folder"

rm -rf "${APP_PATH}/bin"
mkdir "${APP_PATH}/bin"
echo " * Cleaned bin folder"

echo "--------------------------------------------------"
