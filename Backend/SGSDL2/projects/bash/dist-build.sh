#!/bin/sh

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd`
cd "$APP_PATH"

echo "Building SGSDL2 for Linux -- connecting SwinGame to hardware access libraries"
echo ""
echo "Ensure you have the necessary libraries installed..."
echo "sudo apt-get install build-essential libsdl2-dev libsdl2-gfx-dev libsdl2-image-dev libsdl2-mixer-dev libsdl2-net-dev libsdl2-ttf-dev libcurl-dev"
echo ""

# Now lets build this library :)
echo "Compiling... libsgsdl2.so"
clang++ -shared -Wl,-soname,libsgsdl2.so -fPIC -std=c++11 -Weverything -Wno-padded -Wno-missing-prototypes -Wno-c++98-compat -lSDL2 -lSDL2_mixer -lSDL2_image -lSDL2_gfx -lSDL2_ttf -lSDL2_net -lcurl -I./include/ ./src/*.cpp -o libsgsdl2.so -g

echo "Installing... requires sudo"
sudo install libsgsdl2.so -t /usr/lib/
