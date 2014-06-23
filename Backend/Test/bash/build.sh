#!/bin/bash
rm RunTests.out
clang++ -std=c++11 ../src/*.cpp -I../../SGSDL2/src/ -I../../Core/src/ \
-I../../Core/include/ -I/usr/include/SDL2 -lSDL2 -lSDL2_mixer -lSDL2_ttf \
-lsgsdl2 -g -o RunTests.out
./RunTests.out
