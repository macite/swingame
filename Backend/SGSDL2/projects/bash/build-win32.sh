#!/bin/bash
#rm libsgsdl2.so
#clang++ -shared -fPIC -std=c++11 -Weverything -Wno-padded -Wno-missing-prototypes -Wno-c++98-compat -lSDL2 -lSDL2_mixer -lSDL2_image -lSDL2_gfx -I../../../Core/include/ -I../../../Core/src/ ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -o libsgsdl2.so -g
#sudo install libsgsdl2.so -t /usr/lib/

#export PKG_CONFIG_PATH=/c/msys32/mingw32/lib/pkgconfig

SDL_LIBS=("sdl2" "SDL2_mixer" "SDL2_image" "SDL2_ttf" "SDL2_net" "SDL2_gfx")
#CURL_LIBS=`curl-config --static-libs`
#ALL_SDL2_LIBS=`pkg-config ${SDL_LIBS[@]} freetype2 libpng mad flac libtiff-4 libwebp vorbis vorbisfile ogg libmikmod libmodplug libcurl --static --libs`
ALL_SDL2_LIBS="-L../../lib/win32 -L/usr/local/cross-tools/i686-w64-mingw32/lib -L/mingw32/lib -lpng16 -lcurl -lSDL2 -lSDL2main -lSDL2_mixer -lSDL2_image -ltiff -lSDL2_ttf -lSDL2_net -lssl -lcrypto -lSDL2_gfx -lmingw32 -Wl,--no-undefined -ldinput8 -ldxguid -ldxerr8 -luser32 -lgdi32 -limm32 -loleaut32 -lshell32 -lversion -luuid -static-libgcc -lfreetype -lz -lbz2 -lharfbuzz -lglib-2.0 -lpthread -lws2_32 -lole32 -lwinmm -lshlwapi -lintl -lmad -lFLAC -llzma -ljpeg -lwebp -lm -lvorbisfile -lvorbis -logg -lmikmod -lmodplug -lstdc++ -lidn -lrtmp -lssh2 -lwldap32 -lIphlpapi -liconv -lstdc++"
echo $ALL_SDL2_LIBS

echo "Creating shared library"
g++ -shared -DBUILDING_DLL -DCURL_STATICLIB -std=c++11 -o libSGSDL2-32.dll -I/mingw32/include -I/mingw32/include/SDL2 -I../../../Core/include/ -I../../../Core/src ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} -Wl,--out-implib,libSGSDL2-32.dll.a

echo "Creating static library"
echo " - compile backend code"
mkdir sgsdl2
cd sgsdl2
g++ -c -DBUILDING_DLL -std=c++11 -L/mingw32/lib -I/mingw32/include -I/mingw32/include/SDL2 -I../../../../Core/include/ -I../../../../Core/src ../../../../SGSDL2/src/*.cpp ../../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS}
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

cp libSGSDL2-32.dll ../../../../CoreSDK/lib/win32/libSGSDL2.dll
cp libSGSDL2-32.dll.a ../../../../CoreSDK/lib/win32/libSGSDL2.dll.a
cp libSGSDL2-32.a ../../../../CoreSDK/staticlib/win32/libSGSDL2.a
