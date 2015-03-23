//
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

using namespace std;


// Names and locations of variables in the shaders
#define SHAD_SOLID_COLOR 	"solidColor"
#define SHAD_PROJ_MATRIX 	"proj"
#define SHAD_VIEW_MATRIX 	"view"
#define SHAD_MODEL_MATRIX 	"model"
//#define SHAD_LOC_ELEMENTS 	0	// not used
#define SHAD_LOC_VERTICES 	1
#define SHAD_LOC_COLORS 	2
#define SHAD_LOC_TEXTURES	3



//
// Scenes
#pragma mark Scenes

sgsdl2_scene* sgsdl2_make_scene()
{
	sgsdl2_scene *scene = new sgsdl2_scene();
	scene->active_camera = nullptr;
	scene->elements = vector<sgsdl2_scene_element*>();
	return scene;
}

void sgsdl2_set_active_camera(sgsdl2_scene * const scene, sgsdl2_camera * const new_active_cam)
{
	scene->active_camera = new_active_cam;
}

void sgsdl2_add_element_to_root(sgsdl2_scene * const scene, sgsdl2_scene_element * const element)
{
	scene->elements.push_back(element);
	element->root = scene;
}

void sgsdl2_remove_element_from_root(sgsdl2_scene_element * const element)
{
	// Element must belong to a scene for it to be removed
	if (element->root != nullptr)
	{
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

void sgsdl2_add_element(sgsdl2_scene * const scene, sgsdl2_scene_element * const element)
{
	sgsdl2_add_element_to_root(scene, element);
//	scene->elements.push_back(element);
//	element->root = scene;
//	sgsdl2_add_element_to_caches(element);
}

void sgsdl2_remove_element(sgsdl2_scene_element * const element)
{
	// The element must belong to a scene
	if (element->root != nullptr)
	{
		// The element is not attached to a parent element (belongs to the root set)
		if (element->parent == nullptr) {
			sgsdl2_remove_element_from_root(element);
		}
		// Element actually has a parent
		else
		{
//			sgsdl2_dettach_from_parent(element);
			for (vector<sgsdl2_scene_element*>::iterator it = element->parent->children.begin();
				 it != element->parent->children.end();
				 ++it)
			{
				if (*it == element)
				{
					element->parent->children.erase(it);
					element->parent = nullptr;
					element->root = nullptr;
				}
			}
		}
	}
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

void sgsdl2_delete_scene(sgsdl2_scene *scene)
{
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
		trans = multiplyMatrixByMatrix4f(trans, parent_trans);
	}
	return trans;
}

Matrix4f sgsdl2_calculate_proj_transform(sgsdl2_camera const * const camera, float aspect)
{
	return makeMatrix4fFromProjection(camera->field_of_view, aspect, camera->near_z, camera->far_z);
}



//
// Cameras
#pragma mark Cameras

sgsdl2_camera* sgsdl2_make_camera()
{
	return sgsdl2_make_camera({{0, 0, 10}}, {{0, 0, -1}}, {{0, 1, 0}});
}

sgsdl2_camera* sgsdl2_make_camera(Vector3f const location, Vector3f const direction, Vector3f const up)
{
	// TODO this should call the next function down
	sgsdl2_camera *camera = new sgsdl2_camera();
	camera->field_of_view = M_PI_4;
	camera->near_z = 1;
	camera->far_z = 100;
	camera->location = location;
	camera->direction = direction;
	camera->up = up;
	camera->type = sgsdl2_scene_element_type::CAMERA;
	return camera;
}



//
// Geometry
//
#pragma mark Geometry

sgsdl2_geometry* sgsdl2_make_geometry()
{
	sgsdl2_geometry *geometry = new sgsdl2_geometry();
	geometry->vertex_buffer = 0;
	geometry->color_buffer = 0;
	geometry->indices_buffer = 0;
	geometry->texcoords_buffer = 0;
	glGenVertexArrays(1, &geometry->vao);
	sgsdl2_check_opengl_error("make_geometry: ");
	
	geometry->texture = sgsdl2_make_texture();
	geometry->color = {1, 1, 1, 1};
	geometry->shader = -1;
	geometry->type = sgsdl2_scene_element_type::GEOMETRY;
	return geometry;
}

sgsdl2_geometry* sgsdl2_make_geometry(Vector3f const location, Vector3f const direction, Vector3f const up)
{
	sgsdl2_geometry *geometry = sgsdl2_make_geometry();
	geometry->location = location;
	geometry->direction = direction;
	geometry->up = up;
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
	glEnableVertexAttribArray(SHAD_LOC_TEXTURES);
	glVertexAttribPointer(SHAD_LOC_TEXTURES, 2, GL_FLOAT, false, 0, 0);
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
	sgsdl2_delete_texture(geometry->texture);
	delete geometry;
}



//
// Lights
//
#pragma mark Lights

sgsdl2_light* sgsdl2_make_light()
{
	sgsdl2_light *light = new sgsdl2_light();
	light->intensities = {{1, 1, 1}};
	light->attenuation = 1;
	light->type = sgsdl2_scene_element_type::LIGHT;
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

sgsdl2_light* sgsdl2_make_light(Vector3f const location, Vector3f const direction, Vector3f const up, Vector3f const intensities, float attenuation)
{
	sgsdl2_light *light = sgsdl2_make_light(location, direction, up);
	light->intensities = intensities;
	light->attenuation = attenuation;
	return light;
}

// Deletes a light and removes it from any scene caches
void sgsdl2_delete_light(sgsdl2_light *light)
{
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
// Rendering
//
#pragma mark Rendering

//void sgsdl2_quick_render_geometry(sgsdl2_geometry const geometry, float const * const transform)
//{
//	// TODO: complete this function
//	
//	// Check if the geometry can be rendered
//	if (!sgsdl2_can_geometry_be_rendered(geometry))
//	{
//		cout << "geometry does not have the required data to be rendered" << endl;
//		return;
//	}
//	
//	// Select the shader
//	GLuint shader_program = sgsdl2_select_shader(geometry);
//	glUseProgram(shader_program);
//	
//	// Pass it the matricies
//	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_MODEL_MATRIX), 1, false, transform);
////	glUniformMatrix4fv(glGetUniformLocation(shader_program, "view"), 1, false, view);
////	glUniformMatrix4fv(glGetUniformLocation(shader_program, "proj"), 1, false, proj);
//	sgsdl2_check_opengl_error("quick_render_geometry.uniforms: ");
//	
//	// Start the rendering process
//	glBindVertexArray(geometry.vao);
//	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry.indices_buffer);
//	glDrawElements(GL_TRIANGLES, geometry.num_of_indices, GL_FLOAT, 0);
//	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
//	glBindVertexArray(0);
//	sgsdl2_check_opengl_error("quick_render_geometry.render: ");
//	
//	glUseProgram(0);
//}

void sgsdl2_solid_render_geometry(sgsdl2_geometry const * const geometry, sg_color const color, GLuint const shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform)
{
	// Check if the geometry can be rendered
	if (!sgsdl2_can_geometry_be_rendered(geometry))
	{
		cout << "geometry does not have the required data to be rendered" << endl;
		return;
	}
	
	// Select the shader
	glUseProgram(shader_program);
	
	// Pass it the matricies
//	cout << "-- " << glGetUniformLocation(shader_program, SHAD_MODEL_MATRIX) << "," << glGetUniformLocation(shader_program, SHAD_VIEW_MATRIX) << "," << glGetUniformLocation(shader_program, SHAD_PROJ_MATRIX) << endl;
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_MODEL_MATRIX), 1, false, model_transform);
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_VIEW_MATRIX), 1, false, view_transform);
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_PROJ_MATRIX), 1, false, proj_transform);
	glUniform3f(glGetUniformLocation(shader_program, SHAD_SOLID_COLOR), color.r, color.g, color.b);
	sgsdl2_check_opengl_error("solid_render_geometry.uniforms: ");
	
	// Start the rendering process
	glBindVertexArray(geometry->vao);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry->indices_buffer);
	glDrawElements(GL_TRIANGLES, geometry->num_of_indices, GL_UNSIGNED_SHORT, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	sgsdl2_check_opengl_error("solid_render_geometry.render: ");
	
	glUseProgram(0);
}

void sgsdl2_render_geometry(sgsdl2_geometry const * const geometry, GLuint const shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform)
{
	// Check if the geometry can be rendered
	if (!sgsdl2_can_geometry_be_rendered(geometry))
	{
		cout << "geometry does not have the required data to be rendered" << endl;
		return;
	}
	
	// Select the shader
	glUseProgram(shader_program);
	
	// Pass it the matricies
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_MODEL_MATRIX), 1, false, model_transform);
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_VIEW_MATRIX), 1, false, view_transform);
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_PROJ_MATRIX), 1, false, proj_transform);
	sgsdl2_check_opengl_error("render_geometry.uniforms: ");
	
	// Start the rendering process
	glBindVertexArray(geometry->vao);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry->indices_buffer);
	glDrawElements(GL_TRIANGLES, geometry->num_of_indices, GL_UNSIGNED_SHORT, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	sgsdl2_check_opengl_error("render_geometry.render: ");
	
	glUseProgram(0);
}

void sgsdl2_texture_render_geometry(sgsdl2_geometry const * const geometry, sgsdl2_texture texture, GLuint shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform)
{
	// Check if the geometry can be rendered
	if (!sgsdl2_can_geometry_be_rendered(geometry))
	{
		cout << "geometry does not have the required data to be rendered" << endl;
		return;
	}
	
	// Select the shader
	glUseProgram(shader_program);
	
	// Pass it the matricies
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_MODEL_MATRIX), 1, false, model_transform);
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_VIEW_MATRIX), 1, false, view_transform);
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_PROJ_MATRIX), 1, false, proj_transform);
	sgsdl2_check_opengl_error("texture_render_geometry.uniforms: ");
	
	// Start the rendering process
	glBindVertexArray(geometry->vao);
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry->indices_buffer);
	glDrawElements(GL_TRIANGLES, geometry->num_of_indices, GL_UNSIGNED_SHORT, 0);
	glBindTexture(GL_TEXTURE_2D, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	sgsdl2_check_opengl_error("texture_render_geometry.render: ");
	
	glUseProgram(0);
}

void sgsdl2_render_scene(sgsdl2_scene *scene)
{
	if (scene->active_camera == nullptr)
	{
		cout << "Error@render_scene: Scene does not have an active camera, nothing will be rendered.";
		return;
	}
	
	// Calculate the view and proj matrices
	float window_aspect = scene->surface->width / scene->surface->height;
	Matrix4f view_trans = sgsdl2_calculate_view_transform(scene->active_camera);
	Matrix4f proj_trans = sgsdl2_calculate_proj_transform(scene->active_camera, window_aspect);
	
	// Pass them to all the shaders
	for (vector<GLuint>::iterator it = scene->shaders.begin();
		 it != scene->shaders.end();
		 ++it)
	{
		glUseProgram(*it);
		glUniformMatrix4fv(glGetUniformLocation(*it, SHAD_VIEW_MATRIX), 1, false, view_trans.m);
		glUniformMatrix4fv(glGetUniformLocation(*it, SHAD_PROJ_MATRIX), 1, false, proj_trans.m);
	}
	glUseProgram(0);
	
	// Iterate through the scene
	for (vector<sgsdl2_scene_element*>::iterator it = scene->elements.begin();
		 it != scene->elements.end();
		 ++it)
	{
		sgsdl2_render_element(*it);
	}
}

void sgsdl2_render_element(sgsdl2_scene_element *element)
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
		
		// Determine the right shader if not specified
		GLuint shader = sgsdl2_select_shader(geometry);
		glUseProgram(shader);
		
		// Generate and bind the model matrix
		Matrix4f model_trans = sgsdl2_calculate_model_transform(element);
		glUniformMatrix4fv(glGetUniformLocation(shader, SHAD_MODEL_MATRIX), 1, false, model_trans.m);
		
		// Pass any other needed uniforms
		if (geometry->render_solid_color)
		{
			glUniform3f(glGetUniformLocation(shader, SHAD_SOLID_COLOR), geometry->color.r, geometry->color.g, geometry->color.b);
		}
		else if (geometry->texture != nullptr)
		{
			glBindTexture(GL_TEXTURE_2D, geometry->texture->handle);
		}
		sgsdl2_check_opengl_error("render_element.uniforms: ");
		
		
		// Start the rendering process (same no matter what other data is present)
		glBindVertexArray(geometry->vao);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry->indices_buffer);
		glDrawElements(GL_TRIANGLES, geometry->num_of_indices, GL_UNSIGNED_SHORT, 0);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
		glBindVertexArray(0);
		sgsdl2_check_opengl_error("render_element.render: ");
		
		
		// Unbind the texture if needed
		if (geometry->texture != nullptr)
		{
			glBindTexture(GL_TEXTURE_2D, 0);
		}
		
		glUseProgram(0);
	}
	
	// Render children
	for (vector<sgsdl2_scene_element*>::iterator it = element->children.begin();
		 it != element->children.end();
		 ++it)
	{
		sgsdl2_render_element(*it);
	}
}

GLuint sgsdl2_select_shader(sgsdl2_geometry * const geometry)
{
	// Check for color override
	if (geometry->render_solid_color)
	{
		return geometry->root->default_solid_shader;
	}
	
	// Check if the geometry knows its shader
	if (geometry->shader == -1)
	{
		// Tex coords are present
		if (geometry->texcoords_buffer > 0)
		{
			geometry->shader = geometry->root->default_texture_shader;
		}
		// Vertex colors are present
		else if (geometry->color_buffer > 0)
		{
			geometry->shader = geometry->root->default_vertex_color_shader;
		}
		// Fallback
		else
		{
			geometry->shader = geometry->root->default_solid_shader;
		}
	}
	return geometry->shader;
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
		
		char buffer[log_size];
		glGetShaderInfoLog(handle, log_size, NULL, buffer);
		std::cout << "Shader Compile Error:" << endl << buffer << endl;
		
		// Don't need the shader anymore
		glDeleteShader(handle);
		return false;
	}
	
	return true;
}

bool sgsdl2_make_shader_program(GLuint const * const shaders, int const count, GLuint &program)
{
	program = glCreateProgram();
	for (int i = 0; i < count; i++)
	{
		glAttachShader(program, shaders[i]);
	}
	
	glLinkProgram(program);
	GLint isLinked = 0;
	glGetProgramiv(program, GL_LINK_STATUS, &isLinked);
	if(isLinked == GL_FALSE)
	{
		GLint maxLength = 0;
		glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLength);
		
		char infoLog[maxLength];
		glGetProgramInfoLog(program, maxLength, NULL, &infoLog[0]);
		
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
	cout << "OpenGL Version: " << glGetString(GL_VERSION) << endl;
	cout << "GLSL Version: " << glGetString(GL_SHADING_LANGUAGE_VERSION) << endl;
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
	glClearColor(color.r, color.g, color.b, color.a);
	glClear(GL_COLOR_BUFFER_BIT);
	glClearColor(0, 0, 0, 1);
	glClear(GL_DEPTH_BUFFER_BIT);
}
