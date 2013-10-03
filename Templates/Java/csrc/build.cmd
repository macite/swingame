@echo off

javah -classpath ..\bin swingame.platform.NativeCore swingame.platform.NativeInput swingame.platform.NativeGraphics
gcc -I"%JAVA_HOME%"\include\ -I"%JAVA_HOME%"\include\win32\ -L..\clib -lSGSDK -shared -Wl,--add-stdcall-alias -o ..\clib\JavaSwinGame.dll *.c