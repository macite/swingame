#!/bin/sh

javah -classpath ../bin swingame.platform.NativeInput swingame.platform.NativeGraphics

gcc -dynamiclib -arch i386 -o ../clib/libJavaSwinGame.jnilib -I/Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/JavaVM.framework/Headers  -framework JavaVM -framework Cocoa -framework Foundation -F../../../Dist/Source/bin -framework SGSDK *.c