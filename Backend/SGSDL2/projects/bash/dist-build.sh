#!/bin/sh

APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd`
cd "$APP_PATH"

echo "  ... Building SGSDL2 for Linux"
echo "  ... -- connecting SwinGame to hardware access libraries"
echo "  ... Ensure you have the necessary libraries installed..."
echo "      sudo apt-get install build-essential fpc clang libsdl2-dev libsdl2-gfx-dev libsdl2-image-dev libsdl2-mixer-dev libsdl2-net-dev libsdl2-ttf-dev libcurl-dev"
echo ""

CLANG=`which clang++`
if [ ! -f "$CLANG" ]; then
  echo "Could not find clang compiler. Please check that compiler and libraries are installed."
  exit 1
fi

SDL_CFG=`which sdl2-config`
if [ ! -f "$SDL_CFG" ]; then
  echo "Could not find SDL2. Ensure that all required libraries are installed."
  exit 1
fi

CRL_CFG=`which curl-config`
if [ ! -f "$CRL_CFG" ]; then
  echo "Could not fild libcurl-dev. Ensure all required libraries are installed"
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

# Now lets build this library :)
echo "  ... Compiling libsgsdl2.so"
clang++ -shared -Wl,-soname,libsgsdl2.so,-rpath=\$ORIGIN -fPIC -std=c++11 -lSDL2 -lSDL2_mixer -lSDL2_image -lSDL2_gfx -lSDL2_ttf -lSDL2_net -lcurl -I./include/ `sdl2-config --cflags` `curl-config --cflags` ./src/*.cpp -o libsgsdl2.so -g > out.log 2> out.log

if [ $? != 0 ]; then echo "Error compiling libsgsdl2.so"; cat out.log; exit 1; fi

if [ ${INSTALL} = true ]; then
  echo "  ... Installing -- requires sudo"
  sudo install libsgsdl2.so -t /usr/lib/
fi
