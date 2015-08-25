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


// Error messages
#define ERR_NODE_NO_ROOT			"Node does not belong to a scene."
#define ERR_SCENE_NO_ROOT			"Scene is missing a root node."
#define ERR_NODE_NO_PARENT			"Node does not have a parent node."
#define ERR_LIGHT_ALREADY_EXISTS	"Node already possesses a light."
#define ERR_MISSING_LIGHT			"Node does not have a light attached."
#define ERR_MESH_NO_MAT				"Mesh does not have a material."
#define ERR_IMAGE_COULDNT_LOAD		"Image could not be found."
#define ERR_MISSING_CAMERA			"Node does not have a camera attached."
#define ERR_MISSING_SHADER			"There is no shader that can render this mesh."
#define ERR_SCENE_COULDNT_IMPORT	"Scene could not be imported."
#define ERR_IMAGE_IMPORT_FAILED		"Import of an image failed."
#define ERR_CAMERA_ALREADY_EXISTS	"Node already has a camera attached."
#define ERR_MESH_ALREADY_EXISTS		"Node already has a mesh attached."
#define ERR_VAO_MISSING				"Mesh is missing it's VAO."
#define ERR_IMPLICIT_DELETE_ATTRIB	"Mesh already had an attribute at that index. It will automatically be removed."




void sgsdl2_print_opengl_version();

// Reads the entire contents of a file into content
// Returns true upon success
bool sgsdl2_read_file_contents(string const path, string &content);

void sgsdl2_print_error(const char *error);

bool sgsdl2_check_opengl_error(string prompt = "");

void sgsdl2_update_opengl_render(sg_drawing_surface *surface);

void sgsdl2_clear_opengl_window(sg_color color);

string sgsdl2_uni_light_name(int i, const char *str);

// Generates a euler angle vector from a quaternion vector. Used by the importer.
vec3 sgsdl2_angles_from_quat(vec4 quat);

// Turns an array of vectors into a flat array of floats. Used by the importer.
void sgsdl2_flatten_array(aiVector3D *vectors, unsigned int size, float *&flattened_array, unsigned int &new_size, unsigned int dimension = 3);


// Conversion functions
vec3 sgsdl2_make_vec3_color(sg_color color);

sg_color sgsdl2_color(aiColor4D);

vec3 sgsdl2_vec3(aiVector3D);

vec4 sgsdl2_vec4(aiQuaternion);

mat4 sgsdl2_mat4(aiMatrix4x4);


#endif /* defined(__sgsdl2__SGSDL2Utilities__) */
