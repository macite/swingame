#!/bin/bash
clang++ -shared -fPIC -std=c++11 -Weverything -Wno-padded -Wno-missing-prototypes $(pkg-config --cflags --libs sdl2) -lSDL2_mixer -lSDL2_image -lSDL2_gfx -I../../../Core/include/ -I../../../Core/src/ ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -o libsgsdl2.so
sudo install libsgsdl2.so -t /usr/lib/
