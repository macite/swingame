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
// Scenes
//
#pragma mark Scenes

// Creates an empty scene
sgsdl2_scene* sgsdl2_make_scene();

// Compiles the default shaders that are used behind the scenes
void sgsdl2_compile_default_shaders(sgsdl2_scene * const scene);

// Sets the active camera for a scene
void sgsdl2_set_active_camera(sgsdl2_scene * const scene, sgsdl2_camera * const new_active_cam);

// Adds an element to the root set of this scene
void sgsdl2_add_element_to_root(sgsdl2_scene * const scene, sgsdl2_scene_element * const element);

// Removes an element from the root set of this scene
// Assumes the element has no parent
void sgsdl2_remove_element_from_root(sgsdl2_scene_element * const element);

// Removes an element from the light cache
void sgsdl2_remove_light_from_cache(sgsdl2_light * const light);

// Starts caching a new element
// Assumes the element has already been added to the node tree.
//void sgsdl2_add_element_to_caches(sgsdl2_scene_element * const element);

// Removes an element from any scene caches
// Used to seperate the node tree from the cache system
//void sgsdl2_remove_element_from_caches(sgsdl2_scene_element * const element);

// Adds an element reference to the root set of this scene
// Alias of sgsdl2_add_element_to_root();
void sgsdl2_add_element(sgsdl2_scene * const scene, sgsdl2_scene_element * const element, sgsdl2_scene_element * const parent = nullptr);

// Depending on where the element is in the tree, removes it from the root set,
// or dettaches it from its parent
void sgsdl2_remove_element(sgsdl2_scene_element * const element);

// Attaches an element to a parent (automatically detaches it from its previous
// parent if it had one)
void sgsdl2_attach_element(sgsdl2_scene_element * const parent, sgsdl2_scene_element * const child);

// Dettaches an element from its parent and makes its new parent the root scene
void sgsdl2_dettach_from_parent(sgsdl2_scene_element * const element);

// Flags a shader to be updated by the scene
void sgsdl2_add_shader(sgsdl2_scene * const scene, GLuint const shader);

// shader will no longer be updated by the scene
void sgsdl2_remove_shader(sgsdl2_scene * const scene, GLuint const shader);

// Cleans up all the resources used by a scene and then deletes the pointer
void sgsdl2_delete_scene(sgsdl2_scene *scene);



//
// Transforms
//
#pragma mark Transforms

// Invalidates the model transform on the given element
void sgsdl2_invalidate_transform(sgsdl2_scene_element * const element);

// Calculates the model transform for a given element
// Walking the parent chain if needed
Matrix4f sgsdl2_calculate_model_transform(sgsdl2_scene_element * const element);

// Calculates the view transform for an element
Matrix4f sgsdl2_calculate_view_transform(sgsdl2_scene_element * const element);

// Calculates a proj transform for a camera
__attribute__((deprecated))
Matrix4f sgsdl2_calculate_proj_transform(sgsdl2_camera const * const camera, float aspect);

// Calculates a proj transform for a camera using the scenes viewport for aspect calculations
Matrix4f sgsdl2_calculate_proj_transform(sgsdl2_camera const * const camera);

// Returns the shadow transform matrix
Matrix4f sgsdl2_calculate_shadow_transform(sgsdl2_light const * const light);


// Recalculates the matrices for a camera
//void sgsdl2_recalculate_camera_matrices(sgsdl2_camera * const camera);



//
// Elements
//
#pragma mark Elements

// Creates an empty element at the origin facing -z
//sgsdl2_scene_element* sgsdl2_make_element();

// Creates an element at the given location
//sgsdl2_scene_element* sgsdl2_make_element(Vector3f location, Vector3f direction, Vector3f up);



//
// Cameras
//
#pragma mark Cameras

// Creates the default camera
sgsdl2_camera* sgsdl2_make_camera();

// Creates a camera at a location
sgsdl2_camera* sgsdl2_make_camera(Vector3f const location, Vector3f const direction, Vector3f const up);

// Sets the camera frustum based on an fov
void sgsdl2_set_camera_frustum(sgsdl2_camera *camera, float fovx, float fovy, float near, float far);



//
// Geometry
//
#pragma mark Geometry

// Creates a basic geometry object
sgsdl2_geometry* sgsdl2_make_geometry();

// Creates a geometry object at a location
sgsdl2_geometry* sgsdl2_make_geometry(Vector3f const location, Vector3f const direction, Vector3f const up);

// Creates a new vbo with the given vertex data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the vertices array
// dimensions is the number of floats per vertex, ie. 3 for 3D.
void sgsdl2_attach_vertices(sgsdl2_geometry *geometry, GLfloat const * const vertices, GLuint const count, GLint const dimensions);

// Creates a new vbo with the given normal data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the normals array
// dimensions is the number of floats per normal, ie. 3 for 3D.
void sgsdl2_attach_normals(sgsdl2_geometry *geometry, GLfloat const * const normals, GLuint const count, GLint const dimensions);

// Creates a new vbo with the given index data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the indices array
void sgsdl2_attach_indices(sgsdl2_geometry *geometry, GLushort const * const indices, GLuint const count);

// Creates a new vbo with the given color data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the color array
// dimensions is the number of floats per color, ie. 3 for rgb.
void sgsdl2_attach_colors(sgsdl2_geometry *geometry, const GLfloat *colors, const GLuint count, const GLint dimensions);

// Creates a new vbo with the given color data in it
// and attaches it to an existing geometry,
// deleting the existing vbo if needed.
// Assumes the data is tightly packed.
// count is the number of elements in the color array
// dimensions is the number of floats per color, ie. 3 for rgb.
void sgsdl2_attach_texcoords(sgsdl2_geometry *geometry, const GLfloat *coords, const GLuint count);

// Creates a geometry with all the given data.
// If any of the arrays posses a count that is zero, that data is ignored.
// This does not apply for vertices and indices as they must exist
// for a geometry to be rendered

//sgsdl2_geometry sgsdl2_make_geometry(GLfloat *vertices, GLuint vert_count, GLuint indices, GLuint index_count);
//sgsdl2_geometry sgsdl2_make_geometry(GLfloat *vertices, GLuint vert_count, GLfloat colors, GLuint color_count, GLuint indices, GLuint index_count);

// Returns true if the geometry has the minimum amount of information to be rendered.
// This does not validate the data on the opengl side,
// just that the required buffers are non-zero
bool sgsdl2_can_geometry_be_rendered(sgsdl2_geometry const * const geometry);

// Deletes each of the VBOs attached to this geometry and then deletes the pointer
void sgsdl2_delete_geometry(sgsdl2_geometry *geometry);



//
// Lights
//
#pragma mark Lights

// Creates a basic white light
sgsdl2_light* sgsdl2_make_light();

// Creates a basic white light at a location
sgsdl2_light* sgsdl2_make_light(Vector3f const location, Vector3f const direction, Vector3f const up);

// Creates a custom light
sgsdl2_light* sgsdl2_make_light(Vector3f const location, Vector3f const direction, Vector3f const up, Vector3f const color, float intensity, float attenuation);

// Generates a camera at the position of the given light
sgsdl2_camera* sgsdl2_generate_camera_at(sgsdl2_light const * const light);

// Deletes a light and removes it from any scene caches and then deletes the pointer
void sgsdl2_delete_light(sgsdl2_light *light);



//
// Textures
//
#pragma mark Textures

// Creates an empty texture
sgsdl2_texture* sgsdl2_make_texture();

// Changes the wrapping parameters for a texture
// The value of -1 means don't change that value
void sgsdl2_change_texture_wrapping(sgsdl2_texture const * const texture, GLint const wrapping_s = -1, GLint const wrapping_t = -1, sg_color const color = {0, 0, 0, 1});

// Changes the filtering for a texture
// Min is when the image is too small, mag is when the image is too big ???
// The value of -1 means don't change that value
void sgsdl2_change_texture_filtering(sgsdl2_texture const * const texture, GLint const min = -1, GLint const mag = -1);

// Generates texture mipmaps
void sgsdl2_generate_texture_mipmaps(sgsdl2_texture const * const texture);

// Attaches image data to the texture
bool sgsdl2_attach_texture_image(sgsdl2_texture const * const texture, string const image_path, GLenum format);
bool sgsdl2_attach_texture_image(sgsdl2_texture const * const texture, const char * image_path, GLenum format);

// Deletes the texture from opengl then deletes the pointer
void sgsdl2_delete_texture(sgsdl2_texture *texture);



//
// Array Textures
//
#pragma mark Array Textures

// Generates an array texture object
sgsdl2_array_texture sgsdl2_make_array_texture(int num_of_levels, int width, int height, GLint internal_format, GLenum format, GLenum type);



//
// Rendering
//
#pragma mark Rendering

// Renders a geometry using the default shaders.
// transform is a 4x4 matrix representing the geometry location, direction and up vectors.
// SHOULD NOT BE USED
//void sgsdl2_quick_render_geometry(sgsdl2_geometry const geometry, float const * const transform);

// Renders a single piece of geometry in a solid color
__attribute__((deprecated))
void sgsdl2_solid_render_geometry(sgsdl2_geometry const * const geometry, sg_color const color, GLuint const shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform);

__attribute__((deprecated))
// Renders a single piece of geometry using vertex colors
void sgsdl2_render_geometry(sgsdl2_geometry const * const geometry, GLuint const shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform);

__attribute__((deprecated))
// Renders a single piece of geometry with a texture
void sgsdl2_texture_render_geometry(sgsdl2_geometry const * const geometry, sgsdl2_texture texture, GLuint shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform);

// Performs a full render of the scene
void sgsdl2_render_scene(sgsdl2_scene *scene);

// Prepares all the lighting data
void sgsdl2_prepare_lighting(sgsdl2_scene *scene);

// Prepares a single light
void sgsdl2_recalculate_light(sgsdl2_light *light);

// Assigns the light a level in the global shadow map array
void sgsdl2_allocate_shadow_map_location(sgsdl2_light *light);

//  Frees a reserved shadow map location
void sgsdl2_deallocate_shadow_map_location(sgsdl2_light *light);

// Reallocates the light texture
__attribute__((deprecated))
void sgsdl2_generate_shadow_map_texture(sgsdl2_light *light);

// Contains the scene iteration logic
void sgsdl2_perform_render_pass(sgsdl2_scene * const scene, sgsdl2_render_profile profile);

// Renders a single element and all its children recursively
void sgsdl2_render_element(sgsdl2_scene_element *element, sgsdl2_render_profile profile);

// Passes the global matrices to the shader
void sgsdl2_pass_scene_data_to_shader(GLuint shader, sgsdl2_camera * const camera, sgsdl2_geometry * const geometry);

// Passes data about the camera to the shader
void sgsdl2_pass_camera_data_to_shader(GLuint shader, sgsdl2_camera const * const scene);

// Passes the light array to the given shader
void sgsdl2_pass_light_data_to_shader(GLuint shader, vector<sgsdl2_light*> lights);

// Passes the material data to the shader
void sgsdl2_pass_material_data_to_shader(GLuint shader, sgsdl2_geometry * const geometry);

void sgsdl2_perform_render(sgsdl2_geometry const * const geometry);

// Returns the handle of the default shaders that is best equipped to render the given geometry.
__attribute__((deprecated))
GLuint sgsdl2_select_shader(sgsdl2_geometry * const geometry);



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
