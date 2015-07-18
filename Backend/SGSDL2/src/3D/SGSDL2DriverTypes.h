//
//  SGSDL2DriverTypes.h
//  sgsdl2
//
//  Created by James Ferguson on 28/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_SGSDL2DriverTypes_h
#define sgsdl2_SGSDL2DriverTypes_h

// TODO include windows version
#ifdef __APPLE__
#include <OpenGL/gl3.h>
#endif


// Wrappers for OpenGL types, incase this is ever built for 64 bit platforms.
// Or for DX
typedef GLbyte SGbyte;
typedef GLubyte SGubyte;
typedef GLshort SGshort;
typedef GLushort SGushort;
typedef GLint SGint;
typedef GLuint SGuint;
typedef GLfloat SGfloat;
typedef GLdouble SGdouble;
typedef GLenum SGenum;
typedef GLvoid SGvoid;

#endif
