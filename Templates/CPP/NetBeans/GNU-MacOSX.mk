SWINGAME_FLAGS=-Wl,-rpath,@loader_path/../Frameworks -arch i386 -F./lib/sdl13/mac -framework SGSDK
SWINGAME_LINK_FLAGS=

.build-platform:
	${MKDIR} -p ${CND_DISTDIR}/${CONF}/Frameworks
	${CP} -r lib/sdl13/mac/SGSDK.framework ${CND_DISTDIR}/${CONF}/Frameworks

.clean-platform:
	${RM} -r ${CND_DISTDIR}/${CONF}/Frameworks