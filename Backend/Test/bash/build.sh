#!/bin/bash
rm ../Resources/a.out
clang++ -std=c++11 ../src/*.cpp -I../../SGSDL2/src/ -I../../Core/src/ -I../../Core/include/ -I/usr/include/SDL2 -lSDL2 -lSDL2_mixer -lsgsdl2
mv a.out ../Resources/a.out
cd ../Resources
./a.out
