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

if [ ! "$OS" = "$LIN" ]; then
  exit 0
fi

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd`
cd "$APP_PATH"

rm -f bin/linux/libSGSDK.so.4.0 "$APP_PATH/linux/libsgsdk.so"
rm -f "$APP_PATH/sgsdk_source/sgsdl2/libsgsdl2.so" "$APP_PATH/linux/libsgsdl2.so"

echo " * Cleaned libsgsdl2"
