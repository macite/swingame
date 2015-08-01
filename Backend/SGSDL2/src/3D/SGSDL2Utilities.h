//
//  SGSDL2Utilities.h
//  sgsdl2
//
//  Created by James Ferguson on 4/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Utilities__
#define __sgsdl2__SGSDL2Utilities__

#include "sgInterfaces.h"
#include <string>
#include "glm/glm.hpp"
//#include <assimp/matrix4x4.h>
//#include <assimp/vector3.h>
//#include <assimp/quaternion.h>
#include <assimp/types.h>

using namespace std;
using namespace glm;

vec3 sgsdl2_make_vec3_color(sg_color color);

void sgsdl2_print_opengl_version();

// Reads the entire contents of a file into content
// Returns true upon success
bool sgsdl2_read_file_contents(string const path, string &content);

bool sgsdl2_check_opengl_error(string prompt = "");

void sgsdl2_update_opengl_render(sg_drawing_surface *surface);

void sgsdl2_clear_opengl_window(sg_color color);

string sgsdl2_uni_light_name(int i, const char *str);

// Generates a euler angle vector from a quaternion vector. Used by the importer.
vec3 sgsdl2_angles_from_quat(vec4 quat);

// Turns an array of vectors into a flat array of floats. Used by the importer.
void sgsdl2_flatten_array(aiVector3D *vectors, unsigned int size, float *&flattened_array, unsigned int &new_size, unsigned int dimension = 3);


// Conversion functions
sg_color sgsdl2_color(aiColor4D);

vec3 sgsdl2_vec3(aiVector3D);

vec4 sgsdl2_vec4(aiQuaternion);

mat4 sgsdl2_mat4(aiMatrix4x4);

#endif /* defined(__sgsdl2__SGSDL2Utilities__) */
