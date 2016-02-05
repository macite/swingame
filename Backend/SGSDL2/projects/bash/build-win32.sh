#!/bin/bash
#rm libsgsdl2.so
#clang++ -shared -fPIC -std=c++11 -Weverything -Wno-padded -Wno-missing-prototypes -Wno-c++98-compat -lSDL2 -lSDL2_mixer -lSDL2_image -lSDL2_gfx -I../../../Core/include/ -I../../../Core/src/ ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -o libsgsdl2.so -g
#sudo install libsgsdl2.so -t /usr/lib/

#export PKG_CONFIG_PATH=/c/msys32/mingw32/lib/pkgconfig

SDL_LIBS=("sdl2" "SDL2_mixer" "SDL2_image" "SDL2_ttf" "SDL2_net" "SDL2_gfx")

ALL_SDL2_LIBS=`pkg-config ${SDL_LIBS[@]} freetype2 libpng mad flac libtiff-4 libwebp vorbis vorbisfile ogg libmikmod libmodplug --static --libs`

echo $ALL_SDL2_LIBS

echo "Creating shared library"
g++ -shared -DBUILDING_DLL -std=c++11 -o libSGSDL2-32.dll -L/mingw32/lib -I/mingw32/include -I/mingw32/include/SDL2 -I../../../Core/include/ -I../../../Core/src ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} -liconv -lmodplug -lFLAC -lvorbisfile -lvorbis -logg -lmodplug -lmikmod -lpthread -lstdc++ -Wl,--out-implib,libSGSDL2-32.dll.a

echo "Creating static library"
echo " - compile backend code"
mkdir sgsdl2
cd sgsdl2
g++ -c -DBUILDING_DLL -std=c++11 -L/mingw32/lib -I/mingw32/include -I/mingw32/include/SDL2 -I../../../../Core/include/ -I../../../../Core/src ../../../../SGSDL2/src/*.cpp ../../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} -liconv -lmodplug -lFLAC -lvorbisfile -lvorbis -logg -lmodplug -lmikmod -lpthread -lstdc++
cd ..

echo " - merge other SDL2 libraries"
LIBS_ARR=($ALL_SDL2_LIBS)
for lib_file in "${SDL_LIBS[@]}"
do
  mkdir "$lib_file"
  cd "$lib_file"
  ar -x /mingw32/lib/lib${lib_file}.a
  ar r ../libSGSDL2-32.a *.o
  cd ..
  rm -rf "$lib_file"
done

ar r libSGSDL2-32.a sgsdl2/*.o
rm -rf sgsdl2

ranlib libSGSDL2-32.a
