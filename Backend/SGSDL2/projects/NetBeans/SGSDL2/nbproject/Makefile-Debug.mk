#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=MinGW-Windows
CND_DLIB_EXT=dll
CND_CONF=Debug
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/_ext/766657836/sgBackendUtils.o \
	${OBJECTDIR}/_ext/1386528437/SGSDL2Audio.o \
	${OBJECTDIR}/_ext/1386528437/SGSDL2Core.o \
	${OBJECTDIR}/_ext/1386528437/SGSDL2Graphics.o \
	${OBJECTDIR}/_ext/1386528437/SGSDL2Input.o \
	${OBJECTDIR}/_ext/1386528437/SGSDL2Network.o \
	${OBJECTDIR}/_ext/1386528437/SGSDL2Text.o \
	${OBJECTDIR}/_ext/1386528437/SGSDL2Utils.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=-L../../../lib/win32 -lpng -lz

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/libSGSDL2.${CND_DLIB_EXT}

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/libSGSDL2.${CND_DLIB_EXT}: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.cc} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/libSGSDL2.${CND_DLIB_EXT} ${OBJECTFILES} ${LDLIBSOPTIONS} -lmingw32 -lSDL2 -lSDL2_mixer -lSDL2_gfx -lSDL2_image -ldinput8 -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 -loleaut32 -lshell32 -lversion -luuid -lSDL2_ttf -lSDL2_net -shared

${OBJECTDIR}/_ext/766657836/sgBackendUtils.o: ../../../../Core/src/sgBackendUtils.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/766657836
	${RM} "$@.d"
	$(COMPILE.cc) -g -s -I../../../../Core/include -I../../../external/SDL/include -I../../../external/SDL_gfx -I../../../external/SDL_image -I../../../external/SDL_mixer -I../../../external/SDL_net -I../../../external/SDL_ttf -I../../../external/PNG/libpng-1.6.18 -std=c++11  -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/766657836/sgBackendUtils.o ../../../../Core/src/sgBackendUtils.cpp

${OBJECTDIR}/_ext/1386528437/SGSDL2Audio.o: ../../../src/SGSDL2Audio.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1386528437
	${RM} "$@.d"
	$(COMPILE.cc) -g -s -I../../../../Core/include -I../../../external/SDL/include -I../../../external/SDL_gfx -I../../../external/SDL_image -I../../../external/SDL_mixer -I../../../external/SDL_net -I../../../external/SDL_ttf -I../../../external/PNG/libpng-1.6.18 -std=c++11  -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1386528437/SGSDL2Audio.o ../../../src/SGSDL2Audio.cpp

${OBJECTDIR}/_ext/1386528437/SGSDL2Core.o: ../../../src/SGSDL2Core.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1386528437
	${RM} "$@.d"
	$(COMPILE.cc) -g -s -I../../../../Core/include -I../../../external/SDL/include -I../../../external/SDL_gfx -I../../../external/SDL_image -I../../../external/SDL_mixer -I../../../external/SDL_net -I../../../external/SDL_ttf -I../../../external/PNG/libpng-1.6.18 -std=c++11  -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1386528437/SGSDL2Core.o ../../../src/SGSDL2Core.cpp

${OBJECTDIR}/_ext/1386528437/SGSDL2Graphics.o: ../../../src/SGSDL2Graphics.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1386528437
	${RM} "$@.d"
	$(COMPILE.cc) -g -s -I../../../../Core/include -I../../../external/SDL/include -I../../../external/SDL_gfx -I../../../external/SDL_image -I../../../external/SDL_mixer -I../../../external/SDL_net -I../../../external/SDL_ttf -I../../../external/PNG/libpng-1.6.18 -std=c++11  -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1386528437/SGSDL2Graphics.o ../../../src/SGSDL2Graphics.cpp

${OBJECTDIR}/_ext/1386528437/SGSDL2Input.o: ../../../src/SGSDL2Input.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1386528437
	${RM} "$@.d"
	$(COMPILE.cc) -g -s -I../../../../Core/include -I../../../external/SDL/include -I../../../external/SDL_gfx -I../../../external/SDL_image -I../../../external/SDL_mixer -I../../../external/SDL_net -I../../../external/SDL_ttf -I../../../external/PNG/libpng-1.6.18 -std=c++11  -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1386528437/SGSDL2Input.o ../../../src/SGSDL2Input.cpp

${OBJECTDIR}/_ext/1386528437/SGSDL2Network.o: ../../../src/SGSDL2Network.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1386528437
	${RM} "$@.d"
	$(COMPILE.cc) -g -s -I../../../../Core/include -I../../../external/SDL/include -I../../../external/SDL_gfx -I../../../external/SDL_image -I../../../external/SDL_mixer -I../../../external/SDL_net -I../../../external/SDL_ttf -I../../../external/PNG/libpng-1.6.18 -std=c++11  -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1386528437/SGSDL2Network.o ../../../src/SGSDL2Network.cpp

${OBJECTDIR}/_ext/1386528437/SGSDL2Text.o: ../../../src/SGSDL2Text.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1386528437
	${RM} "$@.d"
	$(COMPILE.cc) -g -s -I../../../../Core/include -I../../../external/SDL/include -I../../../external/SDL_gfx -I../../../external/SDL_image -I../../../external/SDL_mixer -I../../../external/SDL_net -I../../../external/SDL_ttf -I../../../external/PNG/libpng-1.6.18 -std=c++11  -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1386528437/SGSDL2Text.o ../../../src/SGSDL2Text.cpp

${OBJECTDIR}/_ext/1386528437/SGSDL2Utils.o: ../../../src/SGSDL2Utils.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1386528437
	${RM} "$@.d"
	$(COMPILE.cc) -g -s -I../../../../Core/include -I../../../external/SDL/include -I../../../external/SDL_gfx -I../../../external/SDL_image -I../../../external/SDL_mixer -I../../../external/SDL_net -I../../../external/SDL_ttf -I../../../external/PNG/libpng-1.6.18 -std=c++11  -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1386528437/SGSDL2Utils.o ../../../src/SGSDL2Utils.cpp

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/libSGSDL2.${CND_DLIB_EXT}

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
