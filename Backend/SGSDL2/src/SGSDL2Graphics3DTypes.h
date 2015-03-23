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



///////////////////////////////////////////
// Enums
///////////////////////////////////////////


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



///////////////////////////////////////////
// Implementation
///////////////////////////////////////////


struct sgsdl2_texture
{
	// Handle for the texture
	GLuint handle;
};


struct sgsdl2_scene
{
	// The window that this scene is rendering to
	sg_drawing_surface *surface;
	
	// Pointer to the currently active camera
	sgsdl2_camera *active_camera;
	
	// All elements that belong to this scene
	vector<sgsdl2_scene_element*> elements;
	
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
};


struct sgsdl2_geometry : public sgsdl2_scene_element
{
	// The vao associated with this geometry
	GLuint vao;
	
	// Each of the vertex buffers.
	// Attributes
	GLuint vertex_buffer;
	GLuint color_buffer;
	GLuint texcoords_buffer;
	GLuint indices_buffer;
	
	// Only the length of indices is needed during rendering
	GLint num_of_indices;
	
	// Uniforms
	sg_color color;
	sgsdl2_texture *texture;
	
	// The shader to use with this object
	GLuint shader;
	
	// An override that will force the piece of geometry to be rendered in a solid color
	bool render_solid_color;
	
	// The render method of this geometry
//	sgsdl2_render_type render_method;
};


struct sgsdl2_camera : public sgsdl2_scene_element
{
	// How wide and high the view of the camera is in radians
	float field_of_view;
	
	// The near and far clipping distances
	float near_z;
	float far_z;
	
	// Whether or no this camera is currently active
};


#endif
