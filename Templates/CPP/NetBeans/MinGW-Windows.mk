SWINGAME_FLAGS=-L./lib/win -static-libgcc -march=i386 -lsgsdk
SWINGAME_LINK_FLAGS=

.build-platform:
	${CP} lib/win/*.dll ${CND_DISTDIR}/${CONF}/MinGW-Windows/
	${CP} -r Resources ${CND_DISTDIR}/${CONF}/MinGW-Windows/

.clean-platform:
