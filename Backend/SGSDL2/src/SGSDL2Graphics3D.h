//
//  SGSDL2Graphics3D.h
//  sgsdl2
//
//  Created by James Ferguson on 11/03/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Graphics3D__
#define __sgsdl2__SGSDL2Graphics3D__

#ifdef __linux__
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#else
#include <SDL.h>
#include <SDL_image.h>
#endif

#ifdef __APPLE__
#include <OpenGL/gl3.h>
#endif

#include "SGSDL2Graphics3DTypes.h"
#include "sgBackendTypes.h"
#include "sgInterfaces.h"
#include <string>

using namespace std;


void sgsdl2_load_graphics3d_fns(sg_interface *functions);

//
// To be moved to the interface
//

// Renders a wireframe with the given color.
// data is an array of 3 coordinate points.
void sgsdl2_draw_mesh(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz);

// Used for testing 3d
void sgsdl2_draw_test(sg_drawing_surface *surface);



//
// Geometry
//
#pragma mark Geometry

// Creates and empty geometry
sgsdl2_geometry sgsdl2_make_geometry();

// Creates a new vbo with the given vertex data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the vertices array
// dimensions is the number of floats per vertex, ie. 3 for 3D.
void sgsdl2_attach_vertices(sgsdl2_geometry &geometry, GLfloat const * const vertices, GLuint const count, GLint const dimensions);

// Creates a new vbo with the given index data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the indices array
void sgsdl2_attach_indices(sgsdl2_geometry &geometry, GLushort const * const indices, GLuint const count);

// Creates a new vbo with the given color data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the color array
// dimensions is the number of floats per color, ie. 3 for rgb.
void sgsdl2_attach_colors(sgsdl2_geometry &geometry, const GLfloat *colors, const GLuint count, const GLint dimensions);

// Creates a new vbo with the given color data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the color array
// dimensions is the number of floats per color, ie. 3 for rgb.
void sgsdl2_attach_texcoords(sgsdl2_geometry &geometry, const GLfloat *coords, const GLuint count);

// Creates a geometry with all the given data.
// If any of the arrays posses a count that is zero, that data is ignored.
// This does not apply for vertices and indices as they must exist
// for a geometry to be rendered

//sgsdl2_geometry sgsdl2_make_geometry(GLfloat *vertices, GLuint vert_count, GLuint indices, GLuint index_count);
//sgsdl2_geometry sgsdl2_make_geometry(GLfloat *vertices, GLuint vert_count, GLfloat colors, GLuint color_count, GLuint indices, GLuint index_count);

// Returns true if the geometry has the minimum amount of information to be rendered.
// This does not validate the data on the opengl side,
// just that the required buffers are non-zero
bool sgsdl2_can_geometry_be_rendered(sgsdl2_geometry const geometry);

// Deletes each of the VBOs attached to this geometry
void sgsdl2_delete_geometry(sgsdl2_geometry geometry);



//
// Textures
//
#pragma mark Textures

// Creates an empty texture
sgsdl2_texture sgsdl2_make_texture();

// Changes the wrapping parameters for a texture
void sgsdl2_change_texture_wrapping(sgsdl2_texture const texture, GLint const wrapping_s = GL_REPEAT, GLint const wrapping_t = GL_REPEAT, sg_color const color = {0, 0, 0, 1});

// Changes the filtering for a texture
// Min is when the image is too small, mag is when the image is too big
void sgsdl2_change_texture_filtering(sgsdl2_texture const texture, GLint const min = GL_LINEAR, GLint const mag = GL_LINEAR);

// Generates texture mipmaps
void sgsdl2_generate_texture_mipmaps(sgsdl2_texture const texture);

// Attaches image data to the texture
bool sgsdl2_attach_texture_image(sgsdl2_texture const texture, string const image_path, GLenum format);
bool sgsdl2_attach_texture_image(sgsdl2_texture const texture, const char * image_path, GLenum format);



//
// Rendering
//
#pragma mark Rendering

// Renders a geometry using the default shaders.
// transform is a 4x4 matrix representing the geometry location, direction and up vectors.
// SHOULD NOT BE USED
void sgsdl2_quick_render_geometry(sgsdl2_geometry const geometry, float const * const transform);

// Renders a geometry in a solid color
void sgsdl2_solid_render_geometry(sgsdl2_geometry const geometry, sg_color const color, GLuint const shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform);

// Just renders a geometry
void sgsdl2_render_geometry(sgsdl2_geometry const geometry, GLuint const shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform);

// Renders geometry with a texture
void sgsdl2_texture_render_geometry(sgsdl2_geometry geometry, sgsdl2_texture texture, GLuint shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform);

// Returns the handle of the default shaders that is best equipped to render the given geometry.
GLuint sgsdl2_select_shader(sgsdl2_geometry const geometry);



//
// Shaders
//
#pragma mark Shaders

// Generates and compiles a single shader and passes identifier into shader_object
// Returns true if sucessful
bool sgsdl2_make_shader(string const source_path, GLenum const shader_type, GLuint &handle);

// Generates and links a shader program using the passed in array of shaders.
// Shaders are attached in the order they are passed in.
// Returns true if sucessful
bool sgsdl2_make_shader_program(GLuint const * const shaders, int const count, GLuint &program);

void sgsdl2_delete_shader(GLuint &handle);

void sgsdl2_delete_shader_program(GLuint &handle);



//
// Utility
//

// Calls any necessary glEnable()s
void sgsdl2_prepare_for_3d();

// Reads the entire contents of a file into content
// Returns true upon success
bool sgsdl2_read_file_contents(string const path, string &content);

bool sgsdl2_check_opengl_error(string prompt = "");

void checkSDLError(int line = -1);

void sgsdl2_print_opengl_version();

// Updates opengl rendering onto the window
void sgsdl2_update_opengl_render(sg_drawing_surface *surface);

void sgsdl2_clear_opengl_window(sg_drawing_surface *surface, sg_color);



#endif /* defined(__sgsdl2__SGSDL2Graphics3D__) */
