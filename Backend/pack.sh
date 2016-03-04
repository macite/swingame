#!/bin/sh

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd`
cd "$APP_PATH"


echo "Pack the CoreSDK into a simple to compile package for Linux builds"

mkdir -p ../Dist/Source/sgsdl2
mkdir -p ../Dist/Source/sgsdl2/src
mkdir -p ../Dist/Source/sgsdl2/include

cp Core/src/*.cpp ../Dist/Source/sgsdl2/src
cp Core/include/*.h ../Dist/Source/sgsdl2/include

cp SGSDL2/src/*.cpp ../Dist/Source/sgsdl2/src
cp SGSDL2/src/*.h ../Dist/Source/sgsdl2/include

cp SGSDL2/projects/bash/dist-build.sh ../Dist/Source/sgsdl2/build.sh
chmod a+x ../Dist/Source/sgsdl2/build.sh

echo "done... see ../Dist/Source/sgsdl2"
