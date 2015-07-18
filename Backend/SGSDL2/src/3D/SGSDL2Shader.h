//
//  SGSDL2Shader.h
//  sgsdl2
//
//  Created by James Ferguson on 6/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Shader__
#define __sgsdl2__SGSDL2Shader__

#include "SGSDL2Types.h"
#include <string>

using namespace std;

// Generates and compiles a single shader and passes identifier into shader_object
// Returns true if sucessful
bool sgsdl2_make_shader(string const source_path, GLenum const shader_type, GLuint &handle);

// Generates and links a shader program using the passed in array of shaders.
// Shaders are attached in the order they are passed in.
// Returns true if sucessful
bool sgsdl2_make_shader_program(GLuint const * const shaders, int const count, GLuint &program);

bool sgsdl2_validate_program(GLuint program);

void sgsdl2_delete_shader(GLuint &handle);

void sgsdl2_delete_shader_program(GLuint &handle);

void sgsdl2_bind_data_to_shader(SGuint shader, sgsdl2_shader_interface interface);

#endif /* defined(__sgsdl2__SGSDL2Shader__) */
