#!/bin/bash

AR_BIN='ar rcs'

ARM_ARCHIVE='arm_fpc.a'
I386_ARCHIVE='i386_fpc.a'
UNIVERSAL_LIB='fpc.a'

ARM_OBJ_LIST=$(find "/usr/local/lib/fpc/2.6.0/units/arm-darwin" -maxdepth 2 -type f -name \*.o ! -name freetype\*)

I386_OBJ_LIST=$(find "/usr/local/lib/fpc/2.6.0/units/i386-iphonesim" -maxdepth 2 -type f -name \*.o ! -name freetype\*)



if [ -f ${ARM_ARCHIVE} ];
then 
  rm ${ARM_ARCHIVE}
fi

if [ -f ${I386_ARCHIVE} ];
then 
  rm ${I386_ARCHIVE}
fi

echo 'Archiving FPC arm.'

${AR_BIN} ${ARM_ARCHIVE} ${ARM_OBJ_LIST}

echo 'Archiving FPC i386.'

${AR_BIN} ${I386_ARCHIVE} ${I386_OBJ_LIST}


if [ -f ${UNIVERSAL_LIB} ];
then 
  rm ${UNIVERSAL_LIB}
fi

echo 'Making universal FPC static lib'
lipo -create -output ${UNIVERSAL_LIB} ${ARM_ARCHIVE} ${I386_ARCHIVE}

echo 'finished.'