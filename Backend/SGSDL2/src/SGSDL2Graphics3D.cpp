
//  SGSDL2Graphics3D.cpp
//  sgsdl2
//
//  Created by James Ferguson on 11/03/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Graphics3D.h"

// For sg_window_be
#include "SGSDL2Graphics.h"
#include <iostream>
#include <fstream>
#include <assert.h>
#include "WavefrontConverterCPP.h"

using namespace std;

// For reference:
//#define GL_NO_ERROR                       0
//#define GL_INVALID_ENUM                   0x0500 = 1280
//#define GL_INVALID_VALUE                  0x0501 = 1281
//#define GL_INVALID_OPERATION              0x0502 = 1282
//#define GL_OUT_OF_MEMORY                  0x0505 = 1283


// Uniform names
#define SHAD_SOLID_COLOR 			"solidColor"
#define SHAD_PROJ_MATRIX 			"proj"
#define SHAD_VIEW_MATRIX 			"view"
#define SHAD_MODEL_MATRIX 			"model"
#define SHAD_NORM_MODEL_MATRIX 		"normalModel"
#define SHAD_LIGHTS_ARRAY			"lights"
#define SHAD_SHADOW_MAP				"shadowMap"
#define SHAD_CAMERA_POS				"cameraPosition"

#define SHAD_MAT_DIFFUSE_COLOR		"material.diffuseColor"
#define SHAD_MAT_SPECULAR_COLOR		"material.specularColor"
#define SHAD_MAT_SPECULAR_EXPONENT	"material.specularExponent"
#define SHAD_MAT_SPECULAR_INTENSITY	"material.specularIntensity"
#define SHAD_MAT_TEXTURE			"material.texture"
#define SHAD_MAT_USE_TEXTURE		"material.useTexture"
// Attribute names
#define SHAD_VERTICES				"position"
#define SHAD_NORMALS				"normal"
#define SHAD_COLORS					"vertexColor"
#define SHAD_TEX_COORDS				"texCoord"
// Attribute locations
#define SHAD_LOC_VERTICES 			1
#define SHAD_LOC_NORMALS			2
#define SHAD_LOC_COLORS 			3
#define SHAD_LOC_TEX_COORDS			4

#define SHAD_LIGHT_VERT_PATH "shaders/light.vert"
#define SHAD_LIGHT_FRAG_PATH "shaders/light.frag"
#define SHAD_SHADOW_VERT_PATH "shaders/shadowmap.vert"
#define SHAD_SHADOW_FRAG_PATH "shaders/shadowmap.frag"

// GL_TEXTURE0 is reserved for unused textures
#define SHADOW_TEX GL_TEXTURE1
#define SHADOW_TEX_VAL 1
#define MATERIAL_TEX GL_TEXTURE2
#define MATERIAL_TEX_VAL 2


//
// Scenes
//
#pragma mark Scenes

sgsdl2_scene* sgsdl2_make_scene()
{
	sgsdl2_scene *scene = new sgsdl2_scene();
	scene->active_camera = nullptr;
	scene->elements = vector<sgsdl2_scene_element*>();
	scene->shadow_map_width = 4096;
	scene->shadow_map_height = 4096;
	scene->shadow_map_array = sgsdl2_make_array_texture(16,
														scene->shadow_map_width,
														scene->shadow_map_height,
														GL_DEPTH_COMPONENT16,
														GL_DEPTH_COMPONENT,
														GL_FLOAT);
	
	return scene;
}

void sgsdl2_compile_default_shaders(sgsdl2_scene * const scene)
{
	GLuint shaders[2];

	// Lights
	sgsdl2_make_shader(SHAD_LIGHT_VERT_PATH, GL_VERTEX_SHADER, shaders[0]);
	sgsdl2_make_shader(SHAD_LIGHT_FRAG_PATH, GL_FRAGMENT_SHADER, shaders[1]);
	sgsdl2_make_shader_program(shaders, 2, scene->default_shader);
	
	// Shadow
	sgsdl2_make_shader(SHAD_SHADOW_VERT_PATH, GL_VERTEX_SHADER, shaders[0]);
	sgsdl2_make_shader(SHAD_SHADOW_FRAG_PATH, GL_FRAGMENT_SHADER, shaders[1]);
	sgsdl2_make_shader_program(shaders, 2, scene->default_shadow_shader);
	sgsdl2_check_opengl_error("compile_default_shaders: ");
}

void sgsdl2_set_active_camera(sgsdl2_scene * const scene, sgsdl2_camera * const new_active_cam)
{
	scene->active_camera = new_active_cam;
}

void sgsdl2_create_geometry_objects_from_file(const char *file_path, sgsdl2_geometry ***objects, int *count)
{
	int int_count;
	obj_data *int_objects;
	convert_file(file_path, &int_objects, &int_count);
	
	if (int_count > 0)
	{
		*objects = (sgsdl2_geometry**) malloc(sizeof(sgsdl2_geometry*) * (unsigned int) int_count);
		*count = int_count;
		
		for (int i = 0; i < int_count; i++)
		{
			(*objects)[i] = sgsdl2_make_geometry();
			sgsdl2_attach_vertices((*objects)[i], int_objects[i].vertices, int_objects[i].vertices_count, 3);
			sgsdl2_attach_indices((*objects)[i], int_objects[i].indices, int_objects[i].indices_count);
			if (int_objects[i].tex_coords_count > 0)
			{
				sgsdl2_attach_texcoords((*objects)[i], int_objects[i].tex_coords, int_objects[i].tex_coords_count);
			}
			if (int_objects[i].normals_count > 0)
			{
				sgsdl2_attach_normals((*objects)[i], int_objects[i].normals, int_objects[i].normals_count, 3);
			}
		}
	}
	delete[] int_objects;
}

void sgsdl2_add_geometry_from_file(sgsdl2_scene * const scene, const char *file_path)
{
	int count = 0;
	sgsdl2_geometry **objects;
	sgsdl2_create_geometry_objects_from_file(file_path, &objects, &count);
	
	for (int i = 0; i < count; i++)
	{
		sgsdl2_add_element_to_root(scene, objects[i]);
	}
	// Don't need to delete objects as they now belong to the scene
}

void sgsdl2_add_element_to_root(sgsdl2_scene * const scene, sgsdl2_scene_element * const element)
{
	// Cache it if it is a light
	if (element->type == sgsdl2_scene_element_type::LIGHT)
	{
		scene->lights.push_back(static_cast<sgsdl2_light*>(element));
	}
	
	scene->elements.push_back(element);
	element->root = scene;
}

void sgsdl2_remove_element_from_root(sgsdl2_scene_element * const element)
{
	// Element must belong to a scene for it to be removed
	if (element->root != nullptr)
	{
		// Also remove it from the cache if it is a light
		if (element->type == sgsdl2_scene_element_type::LIGHT)
		{
			sgsdl2_light *light = static_cast<sgsdl2_light*>(element);
			sgsdl2_remove_light_from_cache(light);
		}
		
		// Remove the element from root set
		for (vector<sgsdl2_scene_element*>::iterator it = element->root->elements.begin();
			 it != element->root->elements.end(); ++it)
		{
			if (*it == element)
			{
				element->root->elements.erase(it);
				continue;
			}
		}
	}
}

void sgsdl2_remove_light_from_cache(sgsdl2_light * const light)
{
	for (vector<sgsdl2_light*>::iterator it = light->root->lights.begin();
		 it != light->root->lights.end();
		 ++it)
	{
		if (*it == light)
		{
			light->root->lights.erase(it);
			return;
		}
	}
	cout << "Warning: Light element was not correctly added to scene cache." << endl;
}

void sgsdl2_add_element(sgsdl2_scene * const scene, sgsdl2_scene_element * const element)
{
	sgsdl2_add_element_to_root(scene, element);
}

void sgsdl2_remove_element(sgsdl2_scene_element * const element)
{
	// The element must belong to a scene
	if (element->root == nullptr)
	{
		cout << "Warning: attempted to remove an element that did not belong to a scene." << endl;
		return;
	}
	
	// Check if the element has children
	if (element->children.size() > 0)
	{
		// To improve efficiency, the elements are removed last first.
		for (unsigned long i = element->children.size(); i > 0; i--)
		{
			sgsdl2_remove_element(element->children[i]);
		}
	}
	
	// Remove it from the scene cache if it is a light
	if (element->type == sgsdl2_scene_element_type::LIGHT)
	{
		sgsdl2_remove_light_from_cache(static_cast<sgsdl2_light*>(element));
	}
	
	// If the element belonged to a parent
	if (element->parent != nullptr)
	{
		// Remove this element from the parent's list of its children
		for (vector<sgsdl2_scene_element*>::iterator it = element->parent->children.begin();
			 it != element->parent->children.end();
			 ++it)
		{
			if (*it == element)
			{
				element->parent->children.erase(it);
				element->parent = nullptr;
				break;
			}
		}
	}
	
	else
	{
		// Remove the element from the root set
		for (vector<sgsdl2_scene_element*>::iterator it = element->root->elements.begin();
			 it != element->root->elements.end(); ++it)
		{
			if (*it == element)
			{
				element->root->elements.erase(it);
				break;
			}
		}
	}
	
	element->root = nullptr;
}

void sgsdl2_attach_element(sgsdl2_scene_element * const parent, sgsdl2_scene_element * const child)
{
	// Child must not have a previous parent
//	if (child->parent != nullptr)
//	{
//		cout << "Error@attach_element: Child must not have a previous parent. Use move_element instead." << endl;
//		return;
//	}
	
	// Child and parent must not belong to different scenes
	if (child->root != nullptr && child->parent->root != child->root)
	{
		cout << "Error@attach_element: Parent and child must belong to the same scene." << endl;
		return;
	}
	
	// Ensure that the element is not attached to another parent
	if (child->root != nullptr)
	{
		sgsdl2_remove_element(child);
	}
	
	parent->children.push_back(child);
	child->parent = parent;
	child->root = parent->root;
	
	// Add it to the cache if it is a light
	if (child->type == sgsdl2_scene_element_type::LIGHT)
	{
		parent->root->lights.push_back(static_cast<sgsdl2_light*>(child));
	}
}

void sgsdl2_dettach_from_parent(sgsdl2_scene_element * const element)
{
	for (vector<sgsdl2_scene_element*>::iterator it = element->parent->children.begin();
		 it != element->parent->children.end();
		 ++it)
	{
		if (*it == element)
		{
			element->parent->children.erase(it);
			element->parent = nullptr;
			element->root->elements.push_back(element);
		}
	}
}

void sgsdl2_add_shader(sgsdl2_scene * const scene, GLuint const shader)
{
	scene->shaders.push_back(shader);
}

void sgsdl2_remove_shader(sgsdl2_scene * const scene, GLuint const shader)
{
	for (vector<GLuint>::iterator it = scene->shaders.begin();
		 it != scene->shaders.end();
		 ++it)
	{
		if (*it == shader)
		{
			scene->shaders.erase(it);
		}
	}
}

void sgsdl2_clear_scene(sgsdl2_scene *scene)
{
	// To improve efficiency, the elements are removed last first.
	for (unsigned long i = scene->elements.size(); i > 0; i--)
	{
		sgsdl2_remove_element(scene->elements[i - 1]);
	}
}

void sgsdl2_delete_scene(sgsdl2_scene *scene)
{
#pragma unused(scene)
	// TODO clean up
}



//
// Transforms
//
#pragma mark Transforms

void sgsdl2_invalidate_transform(sgsdl2_scene_element * const element)
{
	element->transformIsValid = false;
}

Matrix4f sgsdl2_calculate_model_transform(sgsdl2_scene_element * const element)
{
	// Recalculate this transform in needed
	if (!element->transformIsValid)
	{
		element->transformMatrix = makeMatrix4fFromReverseLookAt(element->location, addVector3f(element->location, element->direction), element->up);;
		element->transformIsValid = true;
	}
	
	Matrix4f trans = element->transformMatrix;
	if (element->parent != nullptr)
	{
		Matrix4f parent_trans = sgsdl2_calculate_model_transform(element->parent);
		// Premultiply by parent matrix
		trans = multiplyMatrixByMatrix4f(parent_trans, element->transformMatrix);
	}
	return trans;
}

Matrix4f sgsdl2_calculate_view_transform(sgsdl2_scene_element * const element)
{
	Matrix4f trans = makeMatrix4fFromLookAt(element->location,
											addVector3f(element->location, element->direction),
											element->up);
	if (element->parent != nullptr)
	{
		Matrix4f parent_trans = sgsdl2_calculate_view_transform(element->parent);
		// Post multiply by parent matrix
		trans = multiplyMatrixByMatrix4f(parent_trans, trans);
	}
	return trans;
}

//Matrix4f sgsdl2_calculate_proj_transform(sgsdl2_camera const * const camera, float aspect)
//{
//	Matrix4f trans;
//	if (camera->camera_type == sgsdl2_camera_type::PERSPECTIVE)
//	{
////		trans = makeMatrix4fFromProjection(camera->field_of_view, aspect, camera->near, camera->far);
//		trans = makeMatrix4fFromFrustum(camera->left, camera->right, camera->top, camera->bottom, camera->near, camera->far);
//	}
//	else
//	{
//		trans = makeMatrix4fFromOrtho(camera->left, camera->right, camera->top, camera->bottom, camera->near, camera->far);
//	}
//	return trans;
//}

Matrix4f sgsdl2_calculate_proj_transform(sgsdl2_camera const * const camera)
{
	Matrix4f trans;
	if (camera->camera_type == sgsdl2_camera_type::PERSPECTIVE)
	{
		//		trans = makeMatrix4fFromProjection(camera->field_of_view, aspect, camera->near, camera->far);
		trans = makeMatrix4fFromFrustum(camera->left, camera->right, camera->top, camera->bottom, camera->near, camera->far);
	}
	else
	{
		trans = makeMatrix4fFromOrtho(camera->left, camera->right, camera->top, camera->bottom, camera->near, camera->far);
	}
	return trans;
}

Matrix4f sgsdl2_calculate_shadow_projection(sgsdl2_light const * const light, bool normalize)
{
	Matrix4f trans;
	if (light->light_type == sgsdl2_light_type::DIRECTIONAL)
	{
		trans = makeMatrix4fFromOrtho(-light->width / 2 ,
									  light->width / 2,
									  light->height / 2,
									  -light->height / 2,
									  light->radius,
									  light->cutoff);
	}
	else
	{
		float dist = light->radius * tanf(acosf(light->cos_outer_cone));
		trans = makeMatrix4fFromFrustum(-dist, dist, dist, -dist, light->radius, light->cutoff);
	}
	
	// Matrix will transform depth coords from [-1,1] to [0, 1]
	if (normalize)
	{
//		trans = multiplyMatrixByMatrix4f({{
//			0.5f, 0.0f, 0.0f, 0.0f,
//			0.0f, 0.5f, 0.0f, 0.0f,
//			0.0f, 0.0f, 0.5f, 0.0f,
//			0.5f, 0.5f, 0.5f, 1.0f
//		}}, trans);
	}
	
//	trans = multiplyMatrixByMatrix4f({{
//		0.5f, 0.0f, 0.0f, 0.0f,
//		0.0f, 0.5f, 0.0f, 0.0f,
//		0.0f, 0.0f, 0.5f, 0.0f,
//		0.5f, 0.5f, 0.5f, 1.0f
//	}}, trans);
	
	return trans;
}

Matrix4f sgsdl2_calculate_shadow_transform(sgsdl2_light * const light, bool normalize)
{
	Matrix4f trans = sgsdl2_calculate_view_transform(light);
	trans = multiplyMatrixByMatrix4f(sgsdl2_calculate_shadow_projection(light, normalize), trans);
	return trans;
}

Vector3f sgsdl2_calculate_world_location(sgsdl2_scene_element * const element)
{
	Vector4f loc = multiplyVectorByMatrix4f({{0, 0, 0, 1}}, sgsdl2_calculate_model_transform(element));
	Vector3f loc3f = {{loc.x, loc.y, loc.z}};
	return loc3f;
}



//
// Cameras
//
#pragma mark Cameras

sgsdl2_camera* sgsdl2_make_camera()
{
	return sgsdl2_make_camera({{0, 0, 10}}, {{0, 0, -1}}, {{0, 1, 0}});
}

sgsdl2_camera* sgsdl2_make_camera(Vector3f const location, Vector3f const direction, Vector3f const up)
{
	// TODO this should call the next function down
	sgsdl2_camera *camera = new sgsdl2_camera();
	camera->near = 1;
	camera->far = 100;
	camera->left = -1;
	camera->right = 1;
	camera->top = 1;
	camera->bottom = -1;
	camera->location = location;
	camera->direction = direction;
	camera->up = up;
	camera->type = sgsdl2_scene_element_type::CAMERA;
	camera->parent = nullptr;
	return camera;
}

void sgsdl2_set_camera_frustum(sgsdl2_camera *camera, float fovx, float fovy, float near, float far)
{
	camera->near = near;
	camera->far = far;
	camera->right = near * tanf(fovx / 2);
	camera->top = near * tanf(fovy / 2);
	camera->left = -camera->right;
	camera->bottom = -camera->top;
}



//
// Geometry
//
#pragma mark Geometry

sgsdl2_geometry* sgsdl2_make_geometry()
{
	sgsdl2_geometry *geometry = new sgsdl2_geometry();
	geometry->location = {{0, 0, 0}};
	geometry->direction = {{0, 0, -1}};
	geometry->up = {{0, 1, 0}};
	geometry->vertex_buffer = 0;
	geometry->color_buffer = 0;
	geometry->indices_buffer = 0;
	geometry->texcoords_buffer = 0;
	geometry->type = sgsdl2_scene_element_type::GEOMETRY;
	geometry->render_solid_color = false;
	glGenVertexArrays(1, &geometry->vao);
	sgsdl2_check_opengl_error("make_geometry: ");
	return geometry;
}

sgsdl2_geometry* sgsdl2_make_geometry(Vector3f const location, Vector3f const direction, Vector3f const up)
{
	sgsdl2_geometry *geometry = sgsdl2_make_geometry();
	geometry->location = location;
	geometry->direction = direction;
	geometry->up = up;
	sgsdl2_check_opengl_error("make_geometry: ");
	return geometry;
}

void sgsdl2_attach_vertices(sgsdl2_geometry *geometry, GLfloat const * const vertices, GLuint const count, GLint const dimensions)
{
	// Delete the previous buffer if needed
	if (geometry->vertex_buffer != 0)
	{
		glDeleteBuffers(1, &geometry->vertex_buffer);
	}
	
	glGenBuffers(1, &geometry->vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, geometry->vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLfloat) * count), vertices, GL_STATIC_DRAW);
	sgsdl2_check_opengl_error("attach_vertices@buffer_data: ");
	
	// Specify the format of the data
	glBindVertexArray(geometry->vao);
	glEnableVertexAttribArray(SHAD_LOC_VERTICES);
	glVertexAttribPointer(SHAD_LOC_VERTICES, dimensions, GL_FLOAT, false, 0, 0);
	sgsdl2_check_opengl_error("attach_vertices@data_format: ");
	
	// Unbind buffers
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

void sgsdl2_attach_normals(sgsdl2_geometry *geometry, GLfloat const * const normals, GLuint const count, GLint const dimensions)
{
	// Delete the previous buffer if needed
	if (geometry->normal_buffer != 0)
	{
		glDeleteBuffers(1, &geometry->normal_buffer);
	}
	
	glGenBuffers(1, &geometry->normal_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, geometry->normal_buffer);
	glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLfloat) * count), normals, GL_STATIC_DRAW);
	sgsdl2_check_opengl_error("attach_normals@buffer_data: ");
	
	// Specify the format of the data
	glBindVertexArray(geometry->vao);
	glEnableVertexAttribArray(SHAD_LOC_NORMALS);
	glVertexAttribPointer(SHAD_LOC_NORMALS, dimensions, GL_FLOAT, false, 0, 0);
	sgsdl2_check_opengl_error("attach_normals@data_format: ");
	
	// Unbind buffers
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

void sgsdl2_attach_indices(sgsdl2_geometry *geometry, GLushort const * const indices, GLuint const count)
{
	// Delete the previous buffer if needed
	if (geometry->indices_buffer != 0)
	{
		glDeleteBuffers(1, &geometry->indices_buffer);
		geometry->num_of_indices = 0;
	}
	
	glGenBuffers(1, &geometry->indices_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry->indices_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLushort) * count), indices, GL_STATIC_DRAW);
	geometry->num_of_indices = (GLint) count;
	sgsdl2_check_opengl_error("attach_indices: ");
	
	
	// Unbind buffers
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
}

void sgsdl2_attach_colors(sgsdl2_geometry *geometry, GLfloat const * const colors, GLuint const count, GLint const dimensions)
{
	// Delete the previous buffer if needed
	if (geometry->color_buffer != 0)
	{
		glDeleteBuffers(1, &geometry->color_buffer);
	}
	
	glGenBuffers(1, &geometry->color_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, geometry->color_buffer);
	glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLfloat) * count), colors, GL_STATIC_DRAW);
	sgsdl2_check_opengl_error("attach_colors@buffer_data: ");
	
	// Specify the format of the data
	glBindVertexArray(geometry->vao);
	glEnableVertexAttribArray(SHAD_LOC_COLORS);
	glVertexAttribPointer(SHAD_LOC_COLORS, dimensions, GL_FLOAT, false, 0, 0);
	sgsdl2_check_opengl_error("attach_colors@data_format: ");
	
	// Unbind buffers
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

void sgsdl2_attach_texcoords(sgsdl2_geometry *geometry, const GLfloat *coords, const GLuint count)
{
	// Delete the previous buffer if needed
	if (geometry->texcoords_buffer != 0)
	{
		glDeleteBuffers(1, &geometry->texcoords_buffer);
	}
	
	glGenBuffers(1, &geometry->texcoords_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, geometry->texcoords_buffer);
	glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLfloat) * count), coords, GL_STATIC_DRAW);
	sgsdl2_check_opengl_error("attach_texcoords@buffer_data: ");
	
	// Specify the format of the data
	glBindVertexArray(geometry->vao);
	glEnableVertexAttribArray(SHAD_LOC_TEX_COORDS);
	glVertexAttribPointer(SHAD_LOC_TEX_COORDS, 2, GL_FLOAT, false, 0, 0);
	sgsdl2_check_opengl_error("attach_texcoords@data_format: ");
	
	// Unbind buffers
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

bool sgsdl2_can_geometry_be_rendered(sgsdl2_geometry const * const geometry)
{
	// Geometry must have a valid vao, vertex buffer and index buffer
	return (geometry->vao != 0) && (geometry->vertex_buffer != 0) && (geometry->indices_buffer != 0);
}

void sgsdl2_delete_geometry(sgsdl2_geometry *geometry)
{
	glDeleteBuffers(1, &geometry->vertex_buffer);
	glDeleteBuffers(1, &geometry->indices_buffer);
	glDeleteBuffers(1, &geometry->color_buffer);
	glDeleteBuffers(1, &geometry->texcoords_buffer);
	glDeleteVertexArrays(1, &geometry->vao);
	delete geometry;
}



//
// Lights
//
#pragma mark Lights

sgsdl2_light* sgsdl2_make_light()
{
	sgsdl2_light *light = new sgsdl2_light();
	light->light_type = sgsdl2_light_type::SPOT;
	light->cos_inner_cone = cosf((float)(10.0 * M_PI / 180.0));
	light->cos_outer_cone = cosf((float)(M_PI_4));
	light->color = {{1, 1, 1}};
	light->intensity = 1;
	light->attenuation = 1;
	light->cutoff = 100;
	light->radius = 0.1f;
	light->type = sgsdl2_scene_element_type::LIGHT;
	light->shadow_type = sgsdl2_shadowing_type::DYNAMIC;
	light->active = true;
	light->shadow_map_level = -1;
	return light;
}

sgsdl2_light* sgsdl2_make_light(Vector3f const location, Vector3f const direction, Vector3f const up)
{
	sgsdl2_light *light = sgsdl2_make_light();
	light->location = location;
	light->direction = direction;
	light->up = up;
	return light;
}

sgsdl2_light* sgsdl2_make_light(Vector3f const location, Vector3f const direction, Vector3f const up, Vector3f const color, float intensity, float attenuation)
{
	sgsdl2_light *light = sgsdl2_make_light(location, direction, up);
	light->color = color;
	light->intensity = intensity;
	light->attenuation = attenuation;
	return light;
}

sgsdl2_camera* sgsdl2_generate_camera_at(sgsdl2_light* light)
{
	sgsdl2_camera *camera = sgsdl2_make_camera();
	camera->location = light->location;
	camera->direction = light->direction;
	camera->up = light->up;
	camera->near = light->radius;
	camera->far = light->cutoff;
	
	if (light->light_type == sgsdl2_light_type::DIRECTIONAL)
	{
		camera->camera_type = sgsdl2_camera_type::ORTHOGONAL;
		camera->left = -light->width / 2;
		camera->right = light->width / 2;
		camera->top = light->height / 2;
		camera->bottom = -light->height / 2;
	}
	else
	{
		camera->camera_type = sgsdl2_camera_type::PERSPECTIVE;
		
		if (light->light_type == sgsdl2_light_type::SPOT)
		{
//			camera->field_of_view = acosf(light->cos_outer_cone);
			camera->right = light->radius * tanf(acosf(light->cos_outer_cone));
			camera->top = camera->right;
			camera->left = -camera->right;
			camera->bottom = -camera->top;
		}
		else
		{
//			camera->field_of_view = (float) M_PI_4;
		}
	}
	
	return camera;
}

// Deletes a light and removes it from any scene caches
void sgsdl2_delete_light(sgsdl2_light *light)
{
	// TODO
	sgsdl2_deallocate_shadow_map_location(light);
	delete light;
}



//
// Textures
//
#pragma mark Textures

sgsdl2_texture* sgsdl2_make_texture()
{
	sgsdl2_texture* texture = new sgsdl2_texture;
	glGenTextures(1, &texture->handle);
	return texture;
}

void sgsdl2_change_texture_wrapping(sgsdl2_texture const * const texture, GLint const wrapping_s, GLint const wrapping_t, sg_color const color)
{
	glBindTexture(GL_TEXTURE_2D, texture->handle);
	if (wrapping_s != -1)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrapping_s);
	}
	if (wrapping_t != -1)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapping_t);
	}
	glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, &color.r);
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_wrapping: ");
}

void sgsdl2_change_texture_filtering(sgsdl2_texture const * const texture, GLint const min, GLint const mag)
{
	glBindTexture(GL_TEXTURE_2D, texture->handle);
	if (min != -1)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, min);
	}
	if (mag != -1)
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mag);
	}
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_filtering: ");
}

void sgsdl2_generate_texture_mipmaps(sgsdl2_texture const * const texture)
{
	glBindTexture(GL_TEXTURE_2D, texture->handle);
	glGenerateMipmap(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_image: ");
}

bool sgsdl2_attach_texture_image(sgsdl2_texture const * const texture, string const image_path, GLenum format)
{
	SDL_Surface *image = IMG_Load(image_path.c_str());
	if (!image)
		return false;
	
	glBindTexture(GL_TEXTURE_2D, texture->handle);
	glTexImage2D(GL_TEXTURE_2D, 0, (GLint) format, image->w, image->h, 0, format, GL_UNSIGNED_BYTE, image->pixels);
	glBindTexture(GL_TEXTURE_2D, 0);
	return !sgsdl2_check_opengl_error("texture_image: ");
}

bool sgsdl2_attach_texture_image(sgsdl2_texture const * const texture, const char *image_path, GLenum format)
{
	SDL_Surface *image = IMG_Load(image_path);
	if (!image)
		return false;

	glBindTexture(GL_TEXTURE_2D, texture->handle);
	glTexImage2D(GL_TEXTURE_2D, 0, (GLint) format, image->w, image->h, 0, format, GL_UNSIGNED_BYTE, image->pixels);
	glBindTexture(GL_TEXTURE_2D, 0);
	return !sgsdl2_check_opengl_error("texture_image: ");
}

void sgsdl2_delete_texture(sgsdl2_texture *texture)
{
	glDeleteTextures(1, &texture->handle);
	delete texture;
}


//
// Array Textures
//
#pragma mark Array Textures

sgsdl2_array_texture sgsdl2_make_array_texture(int num_of_levels, int width, int height, GLint internal_format, GLenum format, GLenum type)
{
	int max_opengl_levels;
	glGetIntegerv(GL_MAX_ARRAY_TEXTURE_LAYERS, &max_opengl_levels);
	
	if (num_of_levels > max_opengl_levels)
	{
		cout << "Requested number of levels for array texture is higher than the maximum allowed in opengl." << endl;
		num_of_levels = max_opengl_levels;
	}
	
	sgsdl2_array_texture result;
	result.width = width;
	result.height = height;
	result.num_of_levels = num_of_levels;
	result.occupied_levels = new bool[num_of_levels];
	fill_n(result.occupied_levels, num_of_levels, false);
	
	glGenTextures(1, &result.handle);
	glBindTexture(GL_TEXTURE_2D_ARRAY, result.handle);
	glTexImage3D(GL_TEXTURE_2D_ARRAY, 0, internal_format, width, height, num_of_levels, 0, format, type, NULL);
	
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
	
	int mode, func;
	glGetTexParameteriv(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_COMPARE_FUNC, &func);
	glGetTexParameteriv(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_COMPARE_MODE, &mode);
	
	glBindTexture(GL_TEXTURE_2D_ARRAY, 0);
	
	sgsdl2_check_opengl_error("make_array_texture: ");
	
	return result;
}





//
// Rendering
//
#pragma mark Rendering

void sgsdl2_render_scene(sgsdl2_scene *scene)
{
	if (scene->active_camera == nullptr)
	{
		cout << "Error@render_scene: Scene does not have an active camera, nothing will be rendered.";
		return;
	}
	
	// Recalculates shadowmaps if needed
	sgsdl2_prepare_lighting(scene);
	
	
	
	// Perform the main render pass
	sgsdl2_render_profile profile;
	profile.use_lights = true;
	profile.use_material = true;
	profile.shader_override = 0;
	profile.override_camera = false;
	
	sgsdl2_perform_render_pass(scene, profile);
}

void sgsdl2_render_shadowmap(sgsdl2_scene *scene, sgsdl2_light *light)
{
	
}

void sgsdl2_prepare_lighting(sgsdl2_scene *scene)
{
	// Generate a frame buffer
	GLuint framebuffer = 0;
	glGenFramebuffers(1, &framebuffer);
	glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
	glDrawBuffer(GL_NONE); // No color buffer is drawn to.
	glViewport(0, 0, scene->shadow_map_width, scene->shadow_map_width);
	
	// Rerender each needed shadow map
	for (vector<sgsdl2_light*>::iterator it = scene->lights.begin();
		 it != scene->lights.end();
		 ++it)
	{
		if ((*it)->shadow_type != sgsdl2_shadowing_type::NONE)
		{
			// Light does not have an allocated shadow map level
			if ((*it)->shadow_map_level == -1)
			{
				sgsdl2_allocate_shadow_map_location(*it);
			}
			
			// Check if the light's texture needs to be rerendered
			// Light is dynamic or it is invalid
			if ((*it)->shadow_type == sgsdl2_shadowing_type::DYNAMIC
				|| (*it)->shadow_map_needs_rerender)
			{
				sgsdl2_recalculate_light(*it);
			}
		}
	}
	
	// Reset the buffer
	glBindFramebuffer(GL_FRAMEBUFFER, 0);	
	glDrawBuffer(GL_BACK);
	glViewport(0, 0, scene->surface->width, scene->surface->height);
	glDeleteFramebuffers(1, &framebuffer);
}

void sgsdl2_recalculate_light(sgsdl2_light *light)
{
	// Assumes there is a valid texture
	glFramebufferTextureLayer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, light->root->shadow_map_array.handle, 0, light->shadow_map_level);
	sgsdl2_check_opengl_error();
	
	GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (status == GL_FRAMEBUFFER_COMPLETE)
	{
		// Render all the geometry again
		sgsdl2_render_profile profile;
		profile.use_material = false;
		profile.use_lights = false;
		profile.shader_override = light->root->default_shadow_shader;
		profile.override_camera = true;
		profile.view_transform = sgsdl2_calculate_view_transform(light);
		profile.proj_transform = sgsdl2_calculate_shadow_projection(light);
		profile.camera_location = light->location;
		
//		glClearDepth(0);
		glClear(GL_DEPTH_BUFFER_BIT);
		sgsdl2_perform_render_pass(light->root, profile);
	}
	else
	{
		cout << "Could not render to framebuffer." << endl;
		cout << "Framebuffer status: " << status << endl;
	}
}

void sgsdl2_allocate_shadow_map_location(sgsdl2_light *light)
{
	for (int i = 0; i < light->root->shadow_map_array.num_of_levels; i++)
	{
		if (!light->root->shadow_map_array.occupied_levels[i])
		{
			light->shadow_map_level = i;
			light->root->shadow_map_array.occupied_levels[i] = true;
			break;
		}
	}
}

void sgsdl2_deallocate_shadow_map_location(sgsdl2_light *light)
{
	light->root->shadow_map_array.occupied_levels[light->shadow_map_level] = false;
	light->shadow_map_level = -1;
}

void sgsdl2_perform_render_pass(sgsdl2_scene * const scene, sgsdl2_render_profile profile)
{
	// Start calculating global data
	sgsdl2_render_data data;
	
	// Camera data
	if (profile.override_camera)
	{
		memcpy(data.view, profile.view_transform.m, sizeof(float) * 16);
		memcpy(data.proj, profile.proj_transform.m, sizeof(float) * 16);
		memcpy(data.camera_pos, profile.camera_location.v, sizeof(float) * 3);
	}
	else
	{
		Matrix4f view = sgsdl2_calculate_view_transform(scene->active_camera);
		memcpy(data.view, view.m, sizeof(float) * 16);
		Matrix4f proj = sgsdl2_calculate_proj_transform(scene->active_camera);
		memcpy(data.proj, proj.m, sizeof(float) * 16);
		Vector3f pos = sgsdl2_calculate_world_location(scene->active_camera);
		memcpy(data.camera_pos, pos.v, sizeof(float) * 3);
	}
	
	// Lighting data
	if (profile.use_lights)
	{
		data.shadow_map = scene->shadow_map_array.handle;
		
		int count = 0;
		for (unsigned int i = 0; i < scene->lights.size(); i++)
		{
			if (scene->lights[i]->active)
			{
				sgsdl2_light_data light;
				
				Vector3f light_pos = sgsdl2_calculate_world_location(scene->lights[i]);
				memcpy(light.position, light_pos.v, sizeof(float) * 3);
				
				Matrix4f light_model_trans = sgsdl2_calculate_model_transform(scene->lights[i]);
				// With the current layout, the fowards direction is positive z
				Vector4f light_dir = multiplyVectorByMatrix4f({{0, 0, 1, 0}}, light_model_trans);
				memcpy(light.direction, light_dir.v, sizeof(float) * 3);
				
				Matrix4f shadow_trans = sgsdl2_calculate_shadow_transform(scene->lights[i], true);
				memcpy(light.shadow_transform, shadow_trans.m, sizeof(float) * 16);
				
				light.intensities[0] = scene->lights[i]->color.x * scene->lights[i]->intensity;
				light.intensities[1] = scene->lights[i]->color.y * scene->lights[i]->intensity;
				light.intensities[2] = scene->lights[i]->color.z * scene->lights[i]->intensity;
				
				light.attenuation = scene->lights[i]->attenuation;
				light.ambient_coefficient = scene->lights[i]->ambient_coefficient;
				light.cos_outer_cone = scene->lights[i]->cos_outer_cone;
				light.cos_inner_cone = scene->lights[i]->cos_inner_cone;
				light.type = (int) scene->lights[i]->light_type;
				light.shadow_map_level = scene->lights[i]->shadow_map_level;
				light.casts_shadows = (scene->lights[i]->shadow_type == sgsdl2_shadowing_type::NONE)? false : true;
				
				data.lights[count] = light;
				count++;
			}
		}
		data.num_of_lights = count;
	}
	else
	{
		data.num_of_lights = 0;
	}
	
	// Iterate through the scene
	for (vector<sgsdl2_scene_element*>::iterator it = scene->elements.begin();
		 it != scene->elements.end();
		 ++it)
	{
		sgsdl2_render_element(*it, profile, data);
	}
}

void sgsdl2_render_element(sgsdl2_scene_element *element, sgsdl2_render_profile profile, sgsdl2_render_data data)
{
	if (element->type == sgsdl2_scene_element_type::GEOMETRY)
	{
		// Cast to geometry
		sgsdl2_geometry *geometry = static_cast<sgsdl2_geometry*>(element);
		
		// Check that it can be rendered
		if (!sgsdl2_can_geometry_be_rendered(geometry))
		{
			cout << "Geometry does not have the required data to be rendered" << endl;
			return;
		}
		
		// Finish filling in the data (have to clone it so we don't modifiy data when we enter recursion at the end of this function)
		sgsdl2_render_data current_data = data;
		
		Matrix4f model = sgsdl2_calculate_model_transform(geometry);
		memcpy(current_data.model, model.m, sizeof(float) * 16);
		
		Matrix4f view, proj;
		memcpy(view.m, current_data.view, sizeof(float) * 16);
		memcpy(proj.m, current_data.proj, sizeof(float) * 16);
		Matrix4f mvc = multiplyMatrixByMatrix4f(proj, multiplyMatrixByMatrix4f(view, model));
		memcpy(current_data.mvc, model.m, sizeof(float) * 16);
		
		current_data.material = *geometry->material;
		
		// Determine the right shader if not specified
		GLuint shader = sgsdl2_select_shader(geometry, profile);
		glUseProgram(shader);
		
		if (profile.use_material)
		{
			// Assigns the material data and model matrices
			sgsdl2_pass_material_data_to_shader(shader, current_data);
		}
		
		if (profile.use_lights)
		{
			// Assigns light uniforms and shadow map array
			sgsdl2_pass_light_data_to_shader(shader, current_data);
		}
		else
		{
			// Tell the shader there are no lights
			glUniform1i(glGetUniformLocation(shader, "numberOfLights"), 0);
		}
		
		sgsdl2_pass_scene_data_to_shader(shader, current_data);
		sgsdl2_perform_render(geometry);
		
		glUseProgram(0);
	}
	
	// Render children
	for (vector<sgsdl2_scene_element*>::iterator it = element->children.begin();
		 it != element->children.end();
		 ++it)
	{
		sgsdl2_render_element(*it, profile, data);
	}
}

void sgsdl2_pass_scene_data_to_shader(GLuint shader, sgsdl2_render_data data)
{
	// View
	glUniformMatrix4fv(glGetUniformLocation(shader, SHAD_VIEW_MATRIX), 1, false, data.view);
	
	// Proj
	glUniformMatrix4fv(glGetUniformLocation(shader, SHAD_PROJ_MATRIX), 1, false, data.proj);
	
	// Model
	glUniformMatrix4fv(glGetUniformLocation(shader, SHAD_MODEL_MATRIX), 1, false, data.model);
	
	// MVC
	// TODO
	
	// Normal model
	// TODO
	
	// Camera location
	glUniform3fv(glGetUniformLocation(shader, SHAD_CAMERA_POS), 1, data.camera_pos);
	
	sgsdl2_check_opengl_error("pass_scene_data_to_shaders: ");
}

void sgsdl2_pass_light_data_to_shader(GLuint shader, sgsdl2_render_data data)
{
	for (int i = 0; i < data.num_of_lights; i++)
	{
		sgsdl2_light_data light = data.lights[i];
		
		string uniform_name = SHAD_LIGHTS_ARRAY;
		uniform_name += "[" + to_string(i) + "]";
		
		int pos_loc = glGetUniformLocation(shader, (uniform_name + ".position").c_str());
		int dir_loc = glGetUniformLocation(shader, (uniform_name + ".direction").c_str());
		int trans_loc = glGetUniformLocation(shader, (uniform_name + ".transform").c_str());
		int intesity_loc = glGetUniformLocation(shader, (uniform_name + ".intensities").c_str());
		int atten_loc = glGetUniformLocation(shader, (uniform_name + ".attenuation").c_str());
		int amb_loc = glGetUniformLocation(shader, (uniform_name + ".ambientCoefficient").c_str());
		int inner_angle_loc = glGetUniformLocation(shader, (uniform_name + ".cosInnerCone").c_str());
		int outer_angle_loc = glGetUniformLocation(shader, (uniform_name + ".cosOuterCone").c_str());
		int type_loc = glGetUniformLocation(shader, (uniform_name + ".lightType").c_str());
		int casts_loc = glGetUniformLocation(shader, (uniform_name + ".castsShadows").c_str());
		int map_loc = glGetUniformLocation(shader, (uniform_name + ".shadowMapLevel").c_str());
		
		// Location needs to be transformed
		glUniform3fv(pos_loc, 1, light.position);
		glUniform3fv(dir_loc, 1, light.direction);
		glUniform3fv(intesity_loc, 1, light.intensities);
		glUniform1f(atten_loc, light.attenuation);
		glUniform1f(amb_loc, light.ambient_coefficient);
		glUniform1f(inner_angle_loc, light.cos_inner_cone);
		glUniform1f(outer_angle_loc, light.cos_outer_cone);
		glUniform1i(type_loc, light.type);
		glUniform1i(casts_loc, light.casts_shadows);
		glUniform1i(map_loc, light.shadow_map_level);
		glUniformMatrix4fv(trans_loc, 1, false, light.shadow_transform);
	}
	glUniform1i(glGetUniformLocation(shader, "numberOfLights"), data.num_of_lights);
	
	// Shadow map
	if (data.num_of_lights > 0)
	{
		glActiveTexture(SHADOW_TEX);
		glBindTexture(GL_TEXTURE_2D_ARRAY, data.shadow_map);
		int shad_map_loc = glGetUniformLocation(shader, SHAD_SHADOW_MAP);
		glUniform1i(shad_map_loc, SHADOW_TEX_VAL);
	}
	
	sgsdl2_check_opengl_error("pass_light_data_to_shaders: ");
}

void sgsdl2_pass_material_data_to_shader(GLuint shader, sgsdl2_render_data data)
{
	// Pass material data
	sgsdl2_material mat = data.material;
	glUniform3f(glGetUniformLocation(shader, SHAD_MAT_DIFFUSE_COLOR), mat.diffuse_color.r, mat.diffuse_color.g, mat.diffuse_color.b);
	glUniform3f(glGetUniformLocation(shader, SHAD_MAT_SPECULAR_COLOR), mat.specular_color.r, mat.specular_color.g, mat.specular_color.b);
	glUniform1f(glGetUniformLocation(shader, SHAD_MAT_SPECULAR_EXPONENT), mat.specular_exponent);
	glUniform1f(glGetUniformLocation(shader, SHAD_MAT_SPECULAR_INTENSITY), mat.specular_intensity);
	
	// Texture
	if (glIsTexture(mat.texture))
	{
		glActiveTexture(MATERIAL_TEX);
		glBindTexture(GL_TEXTURE_2D, mat.texture);
		int matLoc = glGetUniformLocation(shader, SHAD_MAT_TEXTURE);
		glUniform1i(matLoc, MATERIAL_TEX_VAL);
		glUniform1i(glGetUniformLocation(shader, SHAD_MAT_USE_TEXTURE), 1);
	}
	else
	{
		glUniform1i(glGetUniformLocation(shader, SHAD_MAT_USE_TEXTURE), 0);
	}
	
	sgsdl2_check_opengl_error("pass_material_data_to_shaders: ");
}

void sgsdl2_perform_render(sgsdl2_geometry const * const geometry)
{
//	assert(geometry->vao && geometry->num_of_indices >= 0 && geometry->num_of_indices % 3 == 0);
	
	// Start the rendering process (same no matter what other data is present)
	glBindVertexArray(geometry->vao);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry->indices_buffer);
	
	int shader = 0;
	glGetIntegerv(GL_CURRENT_PROGRAM, &shader);
	sgsdl2_validate_program((unsigned int) shader);
	
	glDrawElements(GL_TRIANGLES, geometry->num_of_indices, GL_UNSIGNED_SHORT, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	sgsdl2_check_opengl_error("perform_render: ");
}

GLuint sgsdl2_select_shader(sgsdl2_geometry * const geometry, sgsdl2_render_profile profile)
{
	if (profile.shader_override > 0)
	{
		return profile.shader_override;
	}
	if (geometry->material->shader > 0)
	{
		return (GLuint) geometry->material->shader;
	}
	return geometry->root->default_shader;
}




//
// Shaders
//
#pragma mark Shaders

bool sgsdl2_make_shader(string const source_path, GLenum const shader_type, GLuint &handle)
{
	// Load the strings from source
	string source_content;
	if (!sgsdl2_read_file_contents(source_path, source_content))
	{
		cout << "Could not read file contents." << endl;
		return false;
	}
	
	// Convert the string into a pointer
	const char **source_content_pointer = new const char*;
	source_content_pointer[0] = source_content.c_str();
	
	// Create and compile the shader
	handle = glCreateShader(shader_type);
	glShaderSource(handle, 1, source_content_pointer, NULL);
	glCompileShader(handle);
	
	// Determine if it compiled properly
	GLint isCompiled;
	glGetShaderiv(handle, GL_COMPILE_STATUS, &isCompiled);
	if (isCompiled == GL_FALSE)
	{
		GLint log_size = 0;
		glGetShaderiv(handle, GL_INFO_LOG_LENGTH, &log_size);
		
		char infoLog[log_size];
		glGetShaderInfoLog(handle, log_size, NULL, infoLog);
		std::cout << "Shader compile error:" << endl << infoLog << endl;
		
		// Don't need the shader anymore
		glDeleteShader(handle);
		return false;
	}
	
	sgsdl2_check_opengl_error("make_shader: ");
	return true;
}

bool sgsdl2_make_shader_program(GLuint const * const shaders, int const count, GLuint &program)
{
	program = glCreateProgram();
	for (int i = 0; i < count; i++)
	{
		glAttachShader(program, shaders[i]);
	}
	
	// Set the default locations for attributes
	glBindAttribLocation(program, SHAD_LOC_VERTICES, SHAD_VERTICES);
	glBindAttribLocation(program, SHAD_LOC_NORMALS, SHAD_NORMALS);
	glBindAttribLocation(program, SHAD_LOC_COLORS, SHAD_COLORS);
	glBindAttribLocation(program, SHAD_LOC_TEX_COORDS, SHAD_TEX_COORDS);
	glBindFragDataLocation(program, 0, "finalColor");
	
	glLinkProgram(program);
	GLint isLinked = 0;
	glGetProgramiv(program, GL_LINK_STATUS, &isLinked);
	if(isLinked == GL_FALSE)
	{
		GLint maxLength = 0;
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLength);
		
		char infoLog[maxLength];
		glGetProgramInfoLog(program, maxLength, NULL, &infoLog[0]);
		std::cout << "Shader program link error:" << endl << infoLog << endl;
		
		//We don't need the program anymore.
		glDeleteProgram(program);
		//Don't leak shaders either.
		for (int i = 0; i < count; i++)
		{
			glDeleteShader(shaders[i]);
		}
		
		return false;
	}
	
	// Detatch shaders so they can be deleted if needed
	for (int i = 0; i < count; i++)
	{
		glDetachShader(program, shaders[i]);
	}
	
	sgsdl2_check_opengl_error("make_shader_program: ");
	return true;
}

bool sgsdl2_validate_program(GLuint program)
{
	glValidateProgram(program);
	GLint validateStatus = 0;
	glGetProgramiv(program, GL_VALIDATE_STATUS, &validateStatus);
	if (validateStatus == GL_FALSE)
	{
		GLint maxLength = 0;
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLength);
		
		char infoLog[maxLength];
		glGetProgramInfoLog(program, maxLength, NULL, &infoLog[0]);
		std::cout << "Shader program validate error:" << endl << infoLog << endl;
		return false;
	}
	return true;
}

void sgsdl2_delete_shader(GLuint &handle)
{
	glDeleteShader(handle);
	handle = 0;
}

void sgsdl2_delete_shader_program(GLuint &handle)
{
	glDeleteProgram(handle);
	handle = 0;
}



//
// Utility
//
#pragma mark Utility

void sgsdl2_prepare_for_3d()
{
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);
	glEnable(GL_CULL_FACE);
	glCullFace(GL_BACK);
}

bool sgsdl2_read_file_contents(string const path, string &content)
{
	ifstream file;
	file.open(path);
	
	if (file.is_open())
	{
		ifstream ifs(path);
		content.assign((std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>()));
		content += "\0";
		
//		cout << "File contents:" << endl << content << endl;

		file.close();
		return true;
	}
	return false;
}

bool sgsdl2_check_opengl_error(string prompt)
{
	GLenum glErrorNo = glGetError();
	if (glErrorNo != 0)
	{
		if (prompt == "")
		{
			cout << "GLError: " << glErrorNo << endl;
		}
		else
		{
			cout << prompt << glErrorNo << endl;
		}
		return true;
	}
	return false;
}

void checkSDLError(int line)
{
#ifdef DEBUG
	const char *error = SDL_GetError();
	if (*error != '\0')
	{
		printf("SDL Error: %s\n", error);
		if (line != -1)
			printf(" + line: %i\n", line);
		SDL_ClearError();
	}
#endif
}

void sgsdl2_print_opengl_version()
{
	int major = 0;
	int minor = 0;
	glGetIntegerv(GL_MAJOR_VERSION, &major);
	glGetIntegerv(GL_MINOR_VERSION, &minor);
//	cout << "OpenGL Version: " << major << "." << minor << endl;
	cout << "*** OpenGL Version: " << glGetString(GL_VERSION) << " ***" << endl;
	cout << "*** GLSL Version:   " << glGetString(GL_SHADING_LANGUAGE_VERSION) << " ***" << endl;
	sgsdl2_check_opengl_error("print_opengl_version :");
}

void sgsdl2_update_opengl_render(sg_drawing_surface *surface)
{
	if ( ! surface || surface->kind != SGDS_Window) return;
	
	// Get the window from the surface (assumes it is a window)
	sg_window_be *window = (sg_window_be *)surface->_data;
	SDL_GL_SwapWindow(window->window);
}

void sgsdl2_clear_opengl_window(sg_drawing_surface *surface, sg_color color)
{
	#pragma unused(surface)
	glClearColor(color.r, color.g, color.b, color.a);
	glClear(GL_COLOR_BUFFER_BIT);
	glClearColor(0, 0, 0, 1);
	glClear(GL_DEPTH_BUFFER_BIT);
}
