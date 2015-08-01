//
//  SGSDL2Types.h
//  sgsdl2
//
//  Created by James Ferguson on 28/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_SGSDL2Types_h
#define sgsdl2_SGSDL2Types_h

#include <vector>
#include "glm/glm.hpp"
#include "glm/ext.hpp"
#include "sgInterfaces.h"
#include "SGSDL2DriverTypes.h"


using namespace std;
using namespace glm;


// Constants used by the shader property of sgsdl2_geometry
// Shader has not been selected, one of the defaults will automatically be selected each frame
#define SHADER_UNSELECTED 0
// The geometry wants to use the default solid shader
#define SHADER_DEFAULT_SOLID -1
// The geometry wants to use the default vertex color shader
#define SHADER_DEFAULT_VERTEX_COLOR -2
// The geometry wants to use the default texture shader
#define SHADER_DEFAULT_TEXTURE -3

// Reserved attribute locations
#define MESH_ATTR_VERTICES	1
#define MESH_ATTR_NORMALS	2
#define MESH_ATTR_TEXCOORDS	3
//#define MESH_ATTR_INDICES	4
#define MESH_ATTR_LAST_RES	8

// The names of the attributes in the shader. Used for older shaders
#define SHAD_ATTR_NAME_VERTICES "location"
#define SHAD_ATTR_NAME_NORMALS "normal"
#define SHAD_ATTR_NAME_TEXCOORDS "uv"

// Names of uniforms
// Uniform names
#define SHAD_UNI_NAME_PROJ		 				"proj"
#define SHAD_UNI_NAME_VIEW		 				"view"
#define SHAD_UNI_NAME_MODEL 					"model"
#define SHAD_UNI_NAME_NORM_MODEL	 			"normalModel"
#define SHAD_UNI_NAME_MVP						"mvp"
#define SHAD_UNI_NAME_SHADOW_MAP				"shadowMap"
#define SHAD_UNI_NAME_CAMERA_LOC				"cameraLocation"
#define SHAD_UNI_NAME_DIFFUSE					"material.diffuseColor"
#define SHAD_UNI_NAME_SPECULAR					"material.specularColor"
#define SHAD_UNI_NAME_SPECULAR_EXPONENT			"material.specularExponent"
#define SHAD_UNI_NAME_SPECULAR_INTENSITY		"material.specularIntensity"
#define SHAD_UNI_NAME_DIFFUSE_TEXTURE			"material.diffuseTexture"
#define SHAD_UNI_NAME_SPECULAR_TEXTURE			"material.specularTexture"
#define SHAD_UNI_NAME_NORMAL_MAP				"material.normalMap"
#define SHAD_UNI_NAME_LIGHT_ARRAY				"lights"
#define SHAD_UNI_NAME_LIGHT_COUNT				"numberOfLights"
#define SHAD_UNI_NAME_LIGHT_LOCATION(n)			sgsdl2_uni_light_name(i, "location").c_str()
#define SHAD_UNI_NAME_LIGHT_FORWARD(n) 			sgsdl2_uni_light_name(i, "forward").c_str()
#define SHAD_UNI_NAME_LIGHT_TRANS(n) 			sgsdl2_uni_light_name(i, "transform").c_str()
#define SHAD_UNI_NAME_LIGHT_INTENSITIES(n) 		sgsdl2_uni_light_name(i, "intensities").c_str()
#define SHAD_UNI_NAME_LIGHT_ATTENUATION(n)		sgsdl2_uni_light_name(i, "attenuation").c_str()
#define SHAD_UNI_NAME_LIGHT_AMBIENT(n)			sgsdl2_uni_light_name(i, "ambientCoefficient").c_str()
#define SHAD_UNI_NAME_LIGHT_COS_OUTER(n)		sgsdl2_uni_light_name(i, "cosOuterCone").c_str()
#define SHAD_UNI_NAME_LIGHT_COS_INNER(n)		sgsdl2_uni_light_name(i, "cosInnerCone").c_str()
#define SHAD_UNI_NAME_LIGHT_CASTS_SHADOWS(n)	sgsdl2_uni_light_name(i, "castsShadows").c_str()
#define SHAD_UNI_NAME_LIGHT_SHADOW_MAP(n)		sgsdl2_uni_light_name(i, "shadowMapLevel").c_str()
#define SHAD_UNI_NAME_LIGHT_TYPE(n)				sgsdl2_uni_light_name(i, "type").c_str()

// Shader texture locations
#define SHAD_TEX_INDEX_DIFFUSE 1
#define SHAD_TEX_INDEX_SPECULAR 2
#define SHAD_TEX_INDEX_NORMAL 3
#define SHAD_TEX_INDEX_DEPTH 4

// Maximum number of lights in the scene. Must be the same as defined in the shaders.
#define MAX_LIGHTING_COUNT 16

// Dimensions of shadow maps
#define SHAD_MAP_SIZE 1024

// Default shader values
#define SHAD_NONE			0
#define SHAD_DEFAULT_FULL	-1
#define SHAD_DEFAULT_DEPTH	-2



//
// Core scene elements
//
#pragma mark Core scene elements

// Holds all visual elements related to a single gl context
struct sgsdl2_scene;

// Nodes are responsible for the tree structure of a scene
// Each node can hold one light, piece of geometry and one camera, as well as any number of child nodes.
// Each node also possesses a transform that offsets it from its parent node.
struct sgsdl2_node;

// Defines a light source in the scene.
// Multiple different lighting types are supported, some with custom options.
struct sgsdl2_light;

struct sgsdl2_mesh;

// A camera is the viewport through which the scene is rendered.
struct sgsdl2_camera;

// A material describes how the surface of a mesh is rendered.
// The material holds the texture that is to be applied to the mesh and the shader to be used.
struct sgsdl2_material;



//
// Backend wrappers
//
#pragma mark Backend wrappers

// Controls creation, storage, settings and deletion of a texture object
struct sgsdl2_texture;

// Encapsulates a lot of the functions to do with managing an array texture
// Tracks what levels of the texture are in use.
struct sgsdl2_array_texture;

// Shader program wrapper
struct sgsdl2_shader;

// A generic vertex attribute that defines at what index the data should be passed to the shader at.
struct sgsdl2_attribute;


// These three structs represent the exact data that will be passed to the shader.
// The data is represented in each struct in the exact same way that it will be declared in the shader.
// They are seperated into three different levels because some data is stays consistent each frame and some changes depending on which mesh is being rendered.
// Exceptions apply to GLSL types in which case they represented by their integer handle.

// Stores computed values about the scene that are about to be passed to the shader
// Camera transforms, lighting, shadowmaps and global state setttings
//struct sgsdl2_scene_state;

// Stores computed values about the node that are about to be passed to the shader
// Model transform
struct sgsdl2_node_state;

// Stores computed values about the mesh that are about to be passed to the shader
// A material state is mainly different to a material because it uses vec3 to represent color.
// Color, texture, specular intensity.
struct sgsdl2_material_state;

// Used to wrap the lighting info that is passed to the shader
struct sgsdl2_light_state;

struct sgsdl2_camera_state;

struct sgsdl2_lighting_state;

// Represents all the data that will be passed to each shader
struct sgsdl2_shader_interface;

// Data inside a renderer is used for frame rendering and should not be edited from an external source.
struct sgsdl2_renderer;

struct sgsdl2_render_options;


//
// Enums
//
#pragma mark Enums

// The method of lighting used by a light element
enum class sgsdl2_light_type : int
{
	POINT 		= 1,	// Constant lighting in all directions
	DIRECTIONAL	= 2,	// Uses an orthogonal matrix to calculate directions
	SPOT		= 3 	// Similar to point but light comes out in a cone
};

// Type of shadows generated by lights
enum class sgsdl2_shadowing_type
{
	NONE,
	STATIC,
	DYNAMIC
};

// Type of transformation used by a camera element
enum class sgsdl2_camera_type
{
	ORTHOGONAL,
	PERSPECTIVE
};

// How the scene should render the scene
enum class sgsdl2_shader_mode
{
	//	WIREFRAME,
	//	SOLID,
	//	TEXTURED,
	//	BASIC_LIGHTING,
	//	DETAILED_LIGHTING
	FULL,
	DEPTH
};

enum class sgsdl2_render_type
{
//	WIREFRAME,
//	SOLID,
//	TEXTURED,
//	BASIC_LIGHTING,
//	DETAILED_LIGHTING
	FULL,
	DEPTH
}__attribute__((deprecated));








//
// Implementation
//
#pragma mark Implementation
	
struct sgsdl2_light_state
{
	// Position in world space
	vec3 location;
	
	// Direction in world space
	// Note: this is the -z axis transformed by the global transform for the owning node.
	vec3 forward;
	
	// The world to shadow coords transform
	mat4 depth_transform;
	
	// Color of the light multiplied by its intesity
	vec3 intensities;
	
	// Attenuation
	SGfloat attenuation_cutoff;
	
	// Portion of the light that contributes to ambient lighting
	SGfloat ambient_coefficient;
	
	// The cosine of the angle from the center to the edge of the outer cone of the spot light
	SGfloat cos_outer_cone;
	
	// The cosine of the angle from the center to the edge of the inner cone of the spot light
	SGfloat cos_inner_cone;
	
	// True when the light should be tested for shadows
	bool casts_shadows;
	
	// Level of the shadow map that is owned by this light
	SGuint shadow_map_level;
	
	// Type of the light as an integer
	SGuint type;
};

struct sgsdl2_camera_state
{
	mat4 view;
	mat4 proj;
	vec3 camera_loc;
};

struct sgsdl2_lighting_state
{
	sgsdl2_light_state lights[MAX_LIGHTING_COUNT];
	SGint lighting_count;
	SGuint shadow_map;
};

struct sgsdl2_material_state
{
	vec3 diffuse_color;
	SGuint diffuse_texture = 0;
	vec3 specular_color;
	SGuint specular_texture = 0;
	SGfloat specular_exponent;
	SGfloat specular_intensity;
	SGuint normal_map = 0;
};

struct sgsdl2_node_state
{
	mat4 model;
	mat4 normal_model;
};

struct sgsdl2_shader_interface
{
	sgsdl2_lighting_state lighting;		// Frame constant
	sgsdl2_camera_state camera;			// Pass constant
	sgsdl2_material_state material;		// Mesh constant
	sgsdl2_node_state node;				// Mesh constant
	
	// Misc data
	mat4 mvp;
};
	
struct sgsdl2_render_options
{
	//	bool enable_lighting;
	sgsdl2_shader_mode mode;
};

struct sgsdl2_renderer
{
	sgsdl2_render_options opts;
	sgsdl2_shader_interface interface;
	sgsdl2_scene *scene;
};

struct sgsdl2_attribute
{
	SGuint handle = 0;
	
	// Note: when using custom locations some are reserved. Probs 1-10
	SGuint location = 0;
};


	
	
struct sgsdl2_scene
{
	// The window that this scene is rendering to
	sg_drawing_surface *surface = nullptr;

	// The root node of the scene graph
	sgsdl2_node *root_node = nullptr;

	// Pointer to the currently active camera
	sgsdl2_camera *active_camera = nullptr;
	
//	sgsdl2_renderer renderer;

	// Cached array of all lights in the scene
	vector<sgsdl2_light*> lights;

	// Array of shadow maps used by lights in the scene
//	sgsdl2_array_texture* shadow_map_array = nullptr;
	SGuint shadow_map_array = 0;
	bool occupied_shadow_map_levels[MAX_LIGHTING_COUNT];

	// Default shaders are stored in this array so they can be automatically destroyed when the scene is deleted.
	vector<SGuint> shaders;

	SGuint default_shader;
	SGuint default_depth_shader;
	
	// A 1x1 white texture stored with minimum bit depth to save space.
	// It is used as the default texture for diffuse and specular textures when non is specified.
	SGuint white_texture;
};


struct sgsdl2_node
{
	// Transform vectors
	vec3 location = vec3(0, 0, 0);
	vec3 rotation = vec3(0, 0, 0);
	vec3 scale = vec3(1, 1, 1);

	// The parent node to this one.
	// If it is null, indicates that it is the root node of the scene
	sgsdl2_node *parent = nullptr;

	// Nodes that are attached to this node.
	vector<sgsdl2_node*> children;

	// The scene that this node belongs to.
	sgsdl2_scene *root = nullptr;

//	sgsdl2_geometry *geometry;
	sgsdl2_mesh *mesh;
	sgsdl2_camera *camera;
	sgsdl2_light *light;
};


struct sgsdl2_light
{
	// Weak ptr to parent node
	sgsdl2_node *parent;

	// Whether the light is currently active.
	// An inactive light would not contribute any light to the scene.
	bool is_active = true;

	// Color of the light
	// Should be
	sg_color color;

	// How powerful the light is
	float intensity;

	// How much of this light is ambient
	float ambient_coefficient;

	// How quickly the light drops off with distance
	// (however will always follow inverse square law)
	float attenuation_cutoff;

	// The radius of the physical light body. Must be greater than zero.
	float radius;

	// The type of light
	sgsdl2_light_type type;

	// Type of shadows the light casts
	sgsdl2_shadowing_type shadow_type;

	// Level of the shadow map in the array texture
	// -1 means none has been assigned to the light and one will be chosen next render.
	// Note: a value of zero is valid.
	int shadow_map_level = -1;

	// When this is set to true, the shadow map will be recalculated on the next render pass.
	__attribute__((deprecated))
	bool is_shadowmap_invalid;

	// For spot lights, indicates the shape of the light cone.
	float cos_inner_cone;
	float cos_outer_cone;

	// For orthogonal lights, indicates the size of the area affected by the light.
	// This should be made as small as possible because the size of the shadow map is fixed.
	float width;
	float height;
};


struct sgsdl2_geometry
{
	// Indicates that this geometry is hidden.
	bool is_hidden;

	// Collection of meshes that make this object.
	vector<sgsdl2_mesh*> meshes;
}__attribute__((deprecated));


struct sgsdl2_mesh
{
	// Weak ptr to parent node
	sgsdl2_node *parent;

	// Indicates whether this mesh is active or not.
	// An inactive mesh is not rendered in anyway.
	bool is_active = true;

	// The vao associated with this mesh
	SGuint vao = 0;

	// Array of attribute buffers
	vector<sgsdl2_attribute> attributes;

	// Handle to the indices buffer
	SGuint indices_handle = 0;

	// Only the length of indices is needed during rendering
	SGuint indices_count;

	// The surface apperance of the mesh.
	sgsdl2_material *material;

	// Indicates that this mesh is visible.
	// An invisible mesh is not rendered in the main pass but can still cast shadows.
	// To completely hide a mesh and it's shadows use is_active.
	bool is_visible = true;

	// Indicates that this mesh casts a shadow
	// A mesh that does not cast a shadow is faster to render as it is not included in depth passes.
	bool casts_shadows = true;

	// Indicates that this mesh is rendered with shadows on it.
	// A mesh that does not accept shadows is faster to render.
	bool recieves_shadows = true;
};


struct sgsdl2_camera
{
	// Weak ptr to parent node
	sgsdl2_node *parent;

	// Type of camera projection
	sgsdl2_camera_type type;

	// The near and far clipping distances
	float near;
	float far;

	// Dimensions of the frustum
	float left;
	float right;
	float top;
	float bottom;
};

struct sgsdl2_material
{
	// NOTE: shaders are signed to allow for constants that represent scene level shaders to be used (ie SHAD_DEFAULT_FULL).
	
	// Default shader to use in the main render pass
	__attribute__((deprecated))
	SGint shader;

	// Shader to use for any depth render passes.
	__attribute__((deprecated))
	SGint depth_shader;

	sg_color diffuse_color;
	SGuint diffuse_texture = 0;
	sg_color specular_color;
	SGuint specular_texture = 0;
	float specular_exponent;
	float specular_intensity;
	SGuint normal_map = 0;
};

struct sgsdl2_texture
{
	// Handle for the texture
	SGuint handle = 0;
	
//	int width;
//	int height;
};

	
	
	/////////////////////////////////////////////
	// Unused
	/////////////////////////////////////////////

struct sgsdl2_uniform
{
	const char *name;
	void *value;
	int count;
	// Type
};


struct sgsdl2_shader
{
	SGuint handle = 0;

	// Set to true when the shader expects to be supplied with the material texture
	bool wants_texture;

	// Indicates that the texture wants the lighting data from the scene.
	// Note: if the shader is being used to render shadow maps, it will not be passed scene lighting data.
	bool wants_lighting;
}__attribute__((deprecated));


#endif
