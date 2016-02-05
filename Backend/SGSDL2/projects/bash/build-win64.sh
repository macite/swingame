#!/bin/bash
#rm libsgsdl2.so
#clang++ -shared -fPIC -std=c++11 -Weverything -Wno-padded -Wno-missing-prototypes -Wno-c++98-compat -lSDL2 -lSDL2_mixer -lSDL2_image -lSDL2_gfx -I../../../Core/include/ -I../../../Core/src/ ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -o libsgsdl2.so -g
#sudo install libsgsdl2.so -t /usr/lib/

#export PKG_CONFIG_PATH=/c/msys64/mingw64/lib/pkgconfig

SDL_LIBS=("sdl2" "SDL2_mixer" "SDL2_image" "SDL2_ttf" "SDL2_net" "SDL2_gfx")
#CURL_LIBS=`curl-config --static-libs`
#ALL_SDL2_LIBS=`pkg-config libssl libcrypto ${SDL_LIBS[@]} freetype2 libpng mad flac libtiff-4 libwebp vorbis vorbisfile ogg libmikmod libmodplug --static --libs`

CURL_LIBS=""
ALL_SDL2_LIBS="-LC:/msys64/mingw64/lib -lrtmp -lssl -lws2_32 -lgdi32 -lcrypt32 -lcrypto -lws2_32 -lgdi32 -lcrypt32 -lSDL2_mixer -lSDL2_image -lSDL2_ttf -lSDL2_net -lSDL2_gfx -lmingw32 -lSDL2main -lSDL2 -lmingw32 -lSDL2main -lSDL2 -Wl,--no-undefined -lm -ldinput8 -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 -loleaut32 -lshell32 -lversion -luuid -static-libgcc -lfreetype -lz -lbz2 -lharfbuzz -lglib-2.0 -lintl -pthread -lws2_32 -lole32 -lwinmm -lshlwapi -lintl -lpng16 -lz -lmad -lFLAC -lm -ltiff -llzma -ljpeg -lz -lwebp -lm -lvorbisfile -lvorbis -lm -logg -lmikmod -lm -lmodplug -lm -lidn -lssh2 -lssl -lcrypto -lwldap32 -lws2_32 -liconv -lFLAC -lvorbisfile -lvorbis -logg -lmodplug -lmikmod -lpthread -lIphlpapi"

echo $ALL_SDL2_LIBS
echo "Creating shared library"
g++ -shared -DBUILDING_DLL -DCURL_STATICLIB -std=c++11 -o libSGSDL2-64.dll -L/mingw64/lib -I/mingw64/include -I/mingw64/include/SDL2 -I../../../Core/include/ -I../../../Core/src ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp /mingw64/lib/libcurl.a -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} ${CURL_LIBS} -Wl,--out-implib,libSGSDL2-64.dll.a

echo "Creating static library"
echo " - compile backend code"
mkdir sgsdl2
cd sgsdl2
g++ -c -DBUILDING_DLL -DCURL_STATICLIB -std=c++11 -L/mingw64/lib -I/mingw64/include -I/mingw64/include/SDL2 -I../../../../Core/include/ -I../../../../Core/src ../../../../SGSDL2/src/*.cpp ../../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} ${CURL_LIBS} -liconv -lmodplug -lFLAC -lvorbisfile -lvorbis -logg -lmodplug -lmikmod -lpthread -lstdc++ -lIphlpapi
cd ..

echo " - merge other SDL2 libraries"
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
