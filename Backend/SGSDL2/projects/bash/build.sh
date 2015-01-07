#!/bin/bash


#sudo apt-get install build-essential libsdl2-dev libsdl2-gfx-dev libsdl2-image-dev libsdl2-mixer-dev libsdl2-net-dev libsdl2-ttf-dev

rm libsgsdl2.so
clang++ -shared -fPIC -std=c++11 -Weverything -Wno-padded -Wno-missing-prototypes -Wno-c++98-compat -lSDL2 -lSDL2_mixer -lSDL2_image -lSDL2_gfx -lSDL2_ttf -lSDL_net -I../../../Core/include/ -I../../../Core/src/ ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -o libsgsdl2.so -g

sudo install libsgsdl2.so -t /usr/lib/

cp libsgsdl2.so ../../../../CoreSDK/staticlib/sdl2/linux