#!/bin/bash
#rm libsgsdl2.so
#clang++ -shared -fPIC -std=c++11 -Weverything -Wno-padded -Wno-missing-prototypes -Wno-c++98-compat -lSDL2 -lSDL2_mixer -lSDL2_image -lSDL2_gfx -I../../../Core/include/ -I../../../Core/src/ ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -o libsgsdl2.so -g
#sudo install libsgsdl2.so -t /usr/lib/

#export PKG_CONFIG_PATH=/c/msys64/mingw64/lib/pkgconfig

SDL_LIBS=("sdl2" "SDL2_mixer" "SDL2_image" "SDL2_ttf" "SDL2_net" "SDL2_gfx")

ALL_SDL2_LIBS=`pkg-config ${SDL_LIBS[@]} freetype2 libpng mad flac libtiff-4 libwebp vorbis vorbisfile ogg libmikmod libmodplug --static --libs`

echo $ALL_SDL2_LIBS

g++ -shared -DBUILDING_DLL -std=c++11 -o libSGSDL2-64.dll -L/mingw64/lib -I/mingw64/include -I/mingw64/include/SDL2 -I../../../Core/include/ -I../../../Core/src ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} -liconv -lmodplug -lFLAC -lvorbisfile -lvorbis -logg -lmodplug -lmikmod -lpthread -lstdc++ -Wl,--out-implib,libSGSDL2-64.dll.a

mkdir sgsdl2
cd sgsdl2
g++ -c -DBUILDING_DLL -std=c++11 -L/mingw64/lib -I/mingw64/include -I/mingw64/include/SDL2 -I../../../../Core/include/ -I../../../../Core/src ../../../../SGSDL2/src/*.cpp ../../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} -liconv -lmodplug -lFLAC -lvorbisfile -lvorbis -logg -lmodplug -lmikmod -lpthread -lstdc++
cd ..

LIBS_ARR=($ALL_SDL2_LIBS)
for lib_file in "${SDL_LIBS[@]}"
do
  mkdir "$lib_file"
  cd "$lib_file"
  ar -x /mingw64/lib/lib${lib_file}.a
  ar r ../libSGSDL2-64.a *.o
  cd ..
  rm -rf "$lib_file"
done

ar r libSGSDL2-64.a sgsdl2/*.o
rm -rf sgsdl2

ranlib libSGSDL2-64.a
