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
  echo "Library compilation not supported. Use the supplied sgsdl2 static or dynamic library."
  exit 1
fi

INSTALL=false

#
# Read and process command line arguments
#
while getopts i o
do
  case "$o" in
  i)  INSTALL=true;;
  [?]) print >&2 "Usage: $0 [-i]"
     exit -1;;
  esac
done

shift $((${OPTIND}-1))

# Move to src dir
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd`
cd "$APP_PATH"

if [ ! -d "sgsdk_source" ]; then
  echo "  ... Extracting sgsdk source"
  unzip sgsdk_source.zip > /dev/null
fi

cd sgsdk_source
./build.sh
if [ $? != 0 ]; then echo "Library compilation failed!"; exit 1; fi

if [ ! -d "$APP_PATH/linux" ]; then
  mkdir "$APP_PATH/linux"
fi

cp bin/linux/libSGSDK.so.4.0 "$APP_PATH/linux/libsgsdk.so"
cp "$APP_PATH/sgsdk_source/sgsdl2/libsgsdl2.so" "$APP_PATH/linux/libsgsdl2.so"
