#!/bin/bash

ALL_SDL2_LIBS="-L../../lib/win32 -L/mingw32/lib -L/usr/lib -lSDL2main -Wl,--no-undefined"

DLLS=`cd ../../lib/win32;ls -d *.dll | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("../../lib/win32/%s.dll ", patharr[idx]) }'`

echo $DLLS

INC_SDL="-I/mingw32/include -I/mingw32/include/libpng16 -I../../external/SDL/include -I../../external/SDL_gfx -I../../external/SDL_image -I../../external/SDL_mixer -I../../external/SDL_net -I../../external/SDL_ttf -I../../lib/win_inc"

OTHER_LIB="-lstdc++ -lpthread"

echo $ALL_SDL2_LIBS
echo "Creating shared library"
g++ -m32 -static-libstdc++ -static-libgcc ${INC_SDL} -L/mingw32/bin -Wl,-Bdynamic ${DLLS} -shared -DBUILDING_DLL -std=c++11 -o libSGSDL2-32.dll -L/mingw32/lib -I/mingw32/include -I/mingw32/include/SDL2 -I../../../Core/include/ -I../../../Core/src ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -Wl,-Bstatic ${ALL_SDL2_LIBS} ${OTHER_LIB} -static-libstdc++ -static-libgcc -Wl,--out-implib,libSGSDL2-32.dll.a -static-libstdc++ -static-libgcc



# echo "Creating static library"
# echo " - compile backend code"
# mkdir sgsdl2
# cd sgsdl2
# g++ ${INC_SDL} -c -DBUILDING_DLL -DCURL_STATICLIB -std=c++11 -L/mingw32/lib -I/mingw32/include -I/mingw32/include/SDL2 -I../../../../Core/include/ -I../../../../Core/src ../../../../SGSDL2/src/*.cpp ../../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} -liconv -lmodplug -lFLAC -lvorbisfile -lvorbis -logg -lmodplug -lmikmod -lpthread -lstdc++ -lIphlpapi
# cd ..
#
# echo " - merge other SDL2 libraries"
# LIBS_ARR=($ALL_SDL2_LIBS)
# for lib_file in "${SDL_LIBS[@]}"
# do
#   mkdir "$lib_file"
#   cd "$lib_file"
#   ar -x /mingw32/lib/lib${lib_file}.a
#   ar r ../libSGSDL2-32.a *.o
#   cd ..
#   rm -rf "$lib_file"
# done
#
# ar r libSGSDL2-32.a sgsdl2/*.o
# rm -rf sgsdl2
#
# ranlib libSGSDL2-32.a

cp libSGSDL2-32.dll ../../../../CoreSDK/lib/win32/libSGSDL2.dll
cp libSGSDL2-32.dll.a ../../../../CoreSDK/lib/win32/libSGSDL2.dll.a
cp ../../lib/win32/*.dll ../../../../CoreSDK/lib/win32/
# cp libSGSDL2-32.a ../../../../CoreSDK/staticlib/win32/libSGSDL2.a
