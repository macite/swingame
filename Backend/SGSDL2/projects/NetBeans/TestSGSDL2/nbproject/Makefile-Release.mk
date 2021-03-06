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
CND_PLATFORM=MinGW_1-Windows
CND_DLIB_EXT=dll
CND_CONF=Release
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/_ext/1446273465/main.o \
	${OBJECTDIR}/_ext/1446273465/test_audio.o \
	${OBJECTDIR}/_ext/1446273465/test_input.o \
	${OBJECTDIR}/_ext/1446273465/test_network.o \
	${OBJECTDIR}/_ext/1446273465/test_text.o


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
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/testsgsdl2.exe

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/testsgsdl2.exe: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.cc} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/testsgsdl2 ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/_ext/1446273465/main.o: ../../../../Test/src/main.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1446273465
	${RM} "$@.d"
	$(COMPILE.cc) -O2 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1446273465/main.o ../../../../Test/src/main.cpp

${OBJECTDIR}/_ext/1446273465/test_audio.o: ../../../../Test/src/test_audio.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1446273465
	${RM} "$@.d"
	$(COMPILE.cc) -O2 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1446273465/test_audio.o ../../../../Test/src/test_audio.cpp

${OBJECTDIR}/_ext/1446273465/test_input.o: ../../../../Test/src/test_input.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1446273465
	${RM} "$@.d"
	$(COMPILE.cc) -O2 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1446273465/test_input.o ../../../../Test/src/test_input.cpp

${OBJECTDIR}/_ext/1446273465/test_network.o: ../../../../Test/src/test_network.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1446273465
	${RM} "$@.d"
	$(COMPILE.cc) -O2 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1446273465/test_network.o ../../../../Test/src/test_network.cpp

${OBJECTDIR}/_ext/1446273465/test_text.o: ../../../../Test/src/test_text.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/1446273465
	${RM} "$@.d"
	$(COMPILE.cc) -O2 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/_ext/1446273465/test_text.o ../../../../Test/src/test_text.cpp

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/testsgsdl2.exe

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
