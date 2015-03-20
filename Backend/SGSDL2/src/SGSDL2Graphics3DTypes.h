//
//  SGSDL2Graphics3DTypes.h
//  sgsdl2
//
//  Created by James Ferguson on 14/03/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_SGSDL2Graphics3DTypes_h
#define sgsdl2_SGSDL2Graphics3DTypes_h



// Holds all the data for a single geometry.
// If any of the GLuint variables are zero, it means
// that no data has been associated with this geometry yet.
struct sgsdl2_geometry
{
	// The vao associated with this geometry
	GLuint vao;
	
	// Each of the vertex buffers.
	GLuint vertex_buffer;
	GLuint color_buffer;
	GLuint texcoords_buffer;
	GLuint indices_buffer;
	
	// Only the length of indices is needed during rendering
	GLint num_of_indices;
};


// Wrapper for an opengl texture
struct sgsdl2_texture
{
	// Handle for the texture
	GLuint handle;
	
};


#endif
