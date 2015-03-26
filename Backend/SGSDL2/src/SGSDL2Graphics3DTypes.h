//
//  SGSDL2Graphics3DTypes.h
//  sgsdl2
//
//  Created by James Ferguson on 14/03/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_SGSDL2Graphics3DTypes_h
#define sgsdl2_SGSDL2Graphics3DTypes_h

#include "sgInterfaces.h"
#include "Vector3f.h"
#include "Matrix4f.h"
#include <vector>

using namespace std;

// Constants used by the shader property of sgsdl2_geometry
// Shader has not been selected, one of the defaults will automatically be selected each frame
#define SHADER_UNSELECTED 0
// The geometry wants to use the default solid shader
#define SHADER_DEFAULT_SOLID -1
// The geometry wants to use the default vertex color shader
#define SHADER_DEFAULT_VERTEX_COLOR -2
// The geometry wants to use the default texture shader
#define SHADER_DEFAULT_TEXTURE -3


// Holds all visual elements related to a single gl context
struct sgsdl2_scene;

// The base type of visual element stored in scene
// Stores a transform representing the location of the object
struct sgsdl2_scene_element;

// A light source
// inherits from scene_element
// At the moment only point lights are supported
struct sgsdl2_light;

// A piece of geometry
// inherits from scene_element
// If any of the GLuint variables are zero, it means
// that no data has been associated with this geometry yet.
struct sgsdl2_geometry;

// A camera
// inherits from scene_element
struct sgsdl2_camera;

// Wrapper for an opengl texture
struct sgsdl2_texture;

// Encapsulates all the properties of a material
struct sgsdl2_material;



///////////////////////////////////////////
// Enums
///////////////////////////////////////////


// The type of rendering pass to perform
enum class sgsdl2_render_mode
{
	DEPTH_ONLY,
	FULL
};


// Represents the available types of scene_elements
enum class sgsdl2_scene_element_type
{
	LIGHT,
	GEOMETRY,
	CAMERA
};


// What method is used to render a piece of geometry
enum class sgsdl2_render_type
{
	SOLID_COLOR,
	VERTEX_COLORS,
	TEXTURE
};


// Type of shadows generated by lights
enum class sgsdl2_shadowing_type
{
	NONE,
	STATIC,
	DYNAMIC
};



///////////////////////////////////////////
// Implementation
///////////////////////////////////////////


struct sgsdl2_texture
{
	// Handle for the texture
	GLuint handle;
};


struct sgsdl2_material
{
	// The shader to use with this material
	int shader;
	
	sg_color diffuse_color;
	sg_color specular_color;
	float specular_exponent;
	GLuint texture;
};


struct sgsdl2_scene
{
	// The window that this scene is rendering to
	sg_drawing_surface *surface;
	
	// Pointer to the currently active camera
	sgsdl2_camera *active_camera;
	
	// All elements that belong to this scene
	vector<sgsdl2_scene_element*> elements;
	
	// Cached array of all lights in the scene
	vector<sgsdl2_light*> lights;
	
	// Pointer to the shadow map atlas
	GLuint shadow_map;
	
	// All the shaders used in this scene
	vector<GLuint> shaders;
	
	// Default shaders used for each rendering method
	GLuint default_solid_shader;
	GLuint default_vertex_color_shader;
	GLuint default_texture_shader;
};


struct sgsdl2_scene_element
{
	// Transform vectors
	Vector3f location;
	Vector3f direction;
	Vector3f up;
	
	// Cached transform matrix
	Matrix4f transformMatrix;
	bool transformIsValid;
	
	// The parent is the element that this element is attached to
	sgsdl2_scene_element *parent;
	// Elements that are attached to this element
	vector<sgsdl2_scene_element*> children;
	// Short cut pointer to the root scene
	sgsdl2_scene *root;
	
	// The subtype of this element (camera, geometry, light)
	sgsdl2_scene_element_type type;
};


struct sgsdl2_light : public sgsdl2_scene_element
{
	// In other words, the color
	Vector3f intensities;
	
	// How quickly the light drops off with distance
	// (however will always follow inverse square law)
	float attenuation;
	
	// How much of this light is ambient
	float ambientCoefficient;
	
	// Type of shadows the light casts
	sgsdl2_shadowing_type shadow_type;
	
	// Layer of the shadow map atlas that holds this current shadow map
	GLuint shadow_map_layer;
	
	// Whether the shadow map is valid. Does nothing if shadow type is DYNAMIC or NONE
	bool is_shadow_map_valid;
};


struct sgsdl2_geometry : public sgsdl2_scene_element
{
	// The vao associated with this geometry
	GLuint vao;
	
	// Each of the vertex buffers.
	// Attributes
	GLuint vertex_buffer;
	GLuint normal_buffer;
	GLuint color_buffer;
	GLuint texcoords_buffer;
	GLuint indices_buffer;
	
	// Only the length of indices is needed during rendering
	GLint num_of_indices;
	
	// Uniforms
	sg_color color;
	sgsdl2_texture *texture;
	
	// The shader to use with this object
	int shader;
	
	// An override that will force the piece of geometry to be rendered in a solid color
	bool render_solid_color;
	
	// The material
	sgsdl2_material *material;
};


struct sgsdl2_camera : public sgsdl2_scene_element
{
	// How wide and high the view of the camera is in radians
	float field_of_view;
	
	// The near and far clipping distances
	float near_z;
	float far_z;
	
	// Whether or no this camera is currently active
	bool is_main;
	
	// The cached camera transforms
	Matrix4f view_trans;
	Matrix4f proj_trans;
	bool is_view_valid;
};


#endif
