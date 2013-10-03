#!/bin/bash

#script needs to be ran from inside the libpng folder. 
# script also needs the makefile to be in the libpng folder which should be in the same directory as this script.

mkdir build
mkdir build/armv6
mkdir build/armv7
mkdir build/i386
mkdir build/x86_64
mkdir build/universal

make
lipo -create -output "build/universal/libpng.a" "build/armv7/libpng.a" "build/armv6/libpng.a" "build/i386/libpng.a" "build/x86_64/libpng.a"