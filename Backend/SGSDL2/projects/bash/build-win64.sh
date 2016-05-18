#!/bin/bash

ALL_SDL2_LIBS="-L../../lib/win64 -L/mingw64/lib -L/usr/lib -lSDL2 -lSDL2main -lSDL2_mixer -lSDL2_image -lSDL2_ttf -lSDL2_net -lSDL2_gfx -Wl,--no-undefined"

DLLS=`cd ../../lib/win64;ls -d *.dll | awk -F . '{split($1,patharr,"/"); idx=1; while(patharr[idx+1] != "") { idx++ } printf("../../lib/win64/%s.dll ", patharr[idx]) }'`

echo $DLLS

INC_SDL="-I/mingw64/include -I/mingw64/include/libpng16 -I../../external/SDL/include -I../../external/SDL_gfx -I../../external/SDL_image -I../../external/SDL_mixer -I../../external/SDL_net -I../../external/SDL_ttf -I../../lib/win_inc"

OTHER_LIB="-ldinput8 -ldxguid -ldxerr8 -luser32 -lgdi32 -limm32 -loleaut32 -lshell32 -lversion -luuid -lws2_32 -lole32 -lwinmm -liphlpapi -lstdc++ -lpthread"

echo $ALL_SDL2_LIBS
echo "Creating shared library"
g++ -m64 ${INC_SDL} -L/mingw64/bin -Wl,-Bdynamic ${DLLS} -shared -DBUILDING_DLL -std=c++11 -o libSGSDL2-64.dll -L/mingw64/lib -I/mingw64/include -I/mingw64/include/SDL2 -I../../../Core/include/ -I../../../Core/src ../../../SGSDL2/src/*.cpp ../../../Core/src/*.cpp -Wl,-Bstatic -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} ${OTHER_LIB} -Wl,--out-implib,libSGSDL2-64.dll.a



# echo "Creating static library"
# echo " - compile backend code"
# mkdir sgsdl2
# cd sgsdl2
# g++ ${INC_SDL} -c -DBUILDING_DLL -DCURL_STATICLIB -std=c++11 -L/mingw64/lib -I/mingw64/include -I/mingw64/include/SDL2 -I../../../../Core/include/ -I../../../../Core/src ../../../../SGSDL2/src/*.cpp ../../../../Core/src/*.cpp -static -static-libstdc++ -static-libgcc ${ALL_SDL2_LIBS} -liconv -lmodplug -lFLAC -lvorbisfile -lvorbis -logg -lmodplug -lmikmod -lpthread -lstdc++ -lIphlpapi
# cd ..
#
# echo " - merge other SDL2 libraries"
# LIBS_ARR=($ALL_SDL2_LIBS)
# for lib_file in "${SDL_LIBS[@]}"
# do
#   mkdir "$lib_file"
#   cd "$lib_file"
#   ar -x /mingw64/lib/lib${lib_file}.a
#   ar r ../libSGSDL2-64.a *.o
#   cd ..
#   rm -rf "$lib_file"
# done
#
# ar r libSGSDL2-64.a sgsdl2/*.o
# rm -rf sgsdl2
#
# ranlib libSGSDL2-64.a

cp libSGSDL2-64.dll ../../../../CoreSDK/lib/win64/libSGSDL2.dll
cp libSGSDL2-64.dll.a ../../../../CoreSDK/lib/win64/libSGSDL2.dll.a
cp ../../lib/win64/*.dll ../../../../CoreSDK/lib/win64/

# cp libSGSDL2-64.a ../../../../CoreSDK/staticlib/win64/libSGSDL2.a
