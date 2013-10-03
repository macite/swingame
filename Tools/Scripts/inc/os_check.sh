#!/bin/bash

#
# This script contains the sh script functions related detecting the
# operating system for SwinGame
#

MAC="Mac OS X"
WIN="Windows"
LIN="Linux"

if [ `uname` = "Darwin" ]; then
    OS=$MAC
elif [ `uname` = "Linux" ]; then
    OS=$LIN
else
    OS=$WIN
fi
