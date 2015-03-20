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
// Geometry
//
#pragma mark Geometry

sgsdl2_geometry sgsdl2_make_geometry()
{
	sgsdl2_geometry geometry;
	geometry.vertex_buffer = 0;
	geometry.color_buffer = 0;
	geometry.indices_buffer = 0;
	geometry.texcoords_buffer = 0;
	glGenVertexArrays(1, &geometry.vao);
	sgsdl2_check_opengl_error("make_geometry: ");
	return geometry;
}

void sgsdl2_attach_vertices(sgsdl2_geometry &geometry, GLfloat const * const vertices, GLuint const count, GLint const dimensions)
{
	// Delete the previous buffer if needed
	if (geometry.vertex_buffer != 0)
	{
		glDeleteBuffers(1, &geometry.vertex_buffer);
	}
	
	glGenBuffers(1, &geometry.vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, geometry.vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLfloat) * count), vertices, GL_STATIC_DRAW);
	sgsdl2_check_opengl_error("attach_vertices@buffer_data: ");
	
	// Specify the format of the data
	glBindVertexArray(geometry.vao);
	glEnableVertexAttribArray(SHAD_LOC_VERTICES);
	glVertexAttribPointer(SHAD_LOC_VERTICES, dimensions, GL_FLOAT, false, 0, 0);
	sgsdl2_check_opengl_error("attach_vertices@data_format: ");
	
	// Unbind buffers
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

void sgsdl2_attach_indices(sgsdl2_geometry &geometry, GLushort const * const indices, GLuint const count)
{
	// Delete the previous buffer if needed
	if (geometry.indices_buffer != 0)
	{
		glDeleteBuffers(1, &geometry.indices_buffer);
		geometry.num_of_indices = 0;
	}
	
	glGenBuffers(1, &geometry.indices_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry.indices_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLushort) * count), indices, GL_STATIC_DRAW);
	geometry.num_of_indices = (GLint) count;
	sgsdl2_check_opengl_error("attach_indices: ");
	
	
	// Unbind buffers
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
}

void sgsdl2_attach_colors(sgsdl2_geometry &geometry, GLfloat const * const colors, GLuint const count, GLint const dimensions)
{
	// Delete the previous buffer if needed
	if (geometry.color_buffer != 0)
	{
		glDeleteBuffers(1, &geometry.color_buffer);
	}
	
	glGenBuffers(1, &geometry.color_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, geometry.color_buffer);
	glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLfloat) * count), colors, GL_STATIC_DRAW);
	sgsdl2_check_opengl_error("attach_colors@buffer_data: ");
	
	// Specify the format of the data
	glBindVertexArray(geometry.vao);
	glEnableVertexAttribArray(SHAD_LOC_COLORS);
	glVertexAttribPointer(SHAD_LOC_COLORS, dimensions, GL_FLOAT, false, 0, 0);
	sgsdl2_check_opengl_error("attach_colors@data_format: ");
	
	// Unbind buffers
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

void sgsdl2_attach_texcoords(sgsdl2_geometry &geometry, const GLfloat *coords, const GLuint count)
{
	// Delete the previous buffer if needed
	if (geometry.texcoords_buffer != 0)
	{
		glDeleteBuffers(1, &geometry.texcoords_buffer);
	}
	
	glGenBuffers(1, &geometry.texcoords_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, geometry.texcoords_buffer);
	glBufferData(GL_ARRAY_BUFFER, (GLsizeiptr) (sizeof(GLfloat) * count), coords, GL_STATIC_DRAW);
	sgsdl2_check_opengl_error("attach_texcoords@buffer_data: ");
	
	// Specify the format of the data
	glBindVertexArray(geometry.vao);
	glEnableVertexAttribArray(SHAD_LOC_TEXTURES);
	glVertexAttribPointer(SHAD_LOC_TEXTURES, 2, GL_FLOAT, false, 0, 0);
	sgsdl2_check_opengl_error("attach_texcoords@data_format: ");
	
	// Unbind buffers
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
}

bool sgsdl2_can_geometry_be_rendered(sgsdl2_geometry const geometry)
{
	// Geometry must have a valid vao, vertex buffer and index buffer
	return (geometry.vao != 0) && (geometry.vertex_buffer != 0) && (geometry.indices_buffer != 0);
}

void sgsdl2_delete_geometry(sgsdl2_geometry geometry)
{
	glDeleteBuffers(1, &geometry.vertex_buffer);
	glDeleteBuffers(1, &geometry.indices_buffer);
	glDeleteBuffers(1, &geometry.color_buffer);
	glDeleteBuffers(1, &geometry.texcoords_buffer);
	glDeleteVertexArrays(1, &geometry.vao);
	// TODO update with texture arrays etc.
}



//
// Textures
//
#pragma mark Textures

sgsdl2_texture sgsdl2_make_texture()
{
	sgsdl2_texture texture;
	glGenTextures(1, &texture.handle);
	return texture;
}

void sgsdl2_change_texture_wrapping(sgsdl2_texture const texture, GLint const wrapping_s, GLint const wrapping_t, sg_color const color)
{
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrapping_s);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapping_t);
	glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, &color.r);
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_wrapping: ");
}

void sgsdl2_change_texture_filtering(sgsdl2_texture const texture, GLint const min, GLint const mag)
{
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, min);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mag);
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_filtering: ");
}

void sgsdl2_generate_texture_mipmaps(sgsdl2_texture const texture)
{
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glGenerateMipmap(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
	sgsdl2_check_opengl_error("texture_image: ");
}

bool sgsdl2_attach_texture_image(sgsdl2_texture const texture, string const image_path, GLenum format)
{
	SDL_Surface *image = IMG_Load(image_path.c_str());
	if (!image)
		return false;
	
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glTexImage2D(GL_TEXTURE_2D, 0, (GLint) format, image->w, image->h, 0, format, GL_UNSIGNED_BYTE, image->pixels);
	glBindTexture(GL_TEXTURE_2D, 0);
	return !sgsdl2_check_opengl_error("texture_image: ");
}

bool sgsdl2_attach_texture_image(sgsdl2_texture const texture, const char *image_path, GLenum format)
{
	SDL_Surface *image = IMG_Load(image_path);
	if (!image)
		return false;

	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glTexImage2D(GL_TEXTURE_2D, 0, (GLint) format, image->w, image->h, 0, format, GL_UNSIGNED_BYTE, image->pixels);
	glBindTexture(GL_TEXTURE_2D, 0);
	return !sgsdl2_check_opengl_error("texture_image: ");
}

//void sgsdl2_attach_test_texture(sgsdl2_texture const texture)
//{
//	glBindTexture(GL_TEXTURE_2D, texture.handle);
//	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, image->w, image->h, 0, GL_RGBA, GL_FLOAT, image->pixels);
//	glBindTexture(GL_TEXTURE_2D, 0);
//}



//
// Rendering
//
#pragma mark Rendering

void sgsdl2_quick_render_geometry(sgsdl2_geometry const geometry, float const * const transform)
{
	// TODO: complete this function
	
	// Check if the geometry can be rendered
	if (!sgsdl2_can_geometry_be_rendered(geometry))
	{
		cout << "geometry does not have the required data to be rendered" << endl;
		return;
	}
	
	// Select the shader
	GLuint shader_program = sgsdl2_select_shader(geometry);
	glUseProgram(shader_program);
	
	// Pass it the matricies
	glUniformMatrix4fv(glGetUniformLocation(shader_program, SHAD_MODEL_MATRIX), 1, false, transform);
//	glUniformMatrix4fv(glGetUniformLocation(shader_program, "view"), 1, false, view);
//	glUniformMatrix4fv(glGetUniformLocation(shader_program, "proj"), 1, false, proj);
	sgsdl2_check_opengl_error("quick_render_geometry.uniforms: ");
	
	// Start the rendering process
	glBindVertexArray(geometry.vao);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry.indices_buffer);
	glDrawElements(GL_TRIANGLES, geometry.num_of_indices, GL_FLOAT, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	sgsdl2_check_opengl_error("quick_render_geometry.render: ");
	
	glUseProgram(0);
}

void sgsdl2_solid_render_geometry(sgsdl2_geometry const geometry, sg_color const color, GLuint const shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform)
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
	glBindVertexArray(geometry.vao);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry.indices_buffer);
	glDrawElements(GL_TRIANGLES, geometry.num_of_indices, GL_UNSIGNED_SHORT, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	sgsdl2_check_opengl_error("solid_render_geometry.render: ");
	
	glUseProgram(0);
}

void sgsdl2_render_geometry(sgsdl2_geometry const geometry, GLuint const shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform)
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
	glBindVertexArray(geometry.vao);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry.indices_buffer);
	glDrawElements(GL_TRIANGLES, geometry.num_of_indices, GL_UNSIGNED_SHORT, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	sgsdl2_check_opengl_error("render_geometry.render: ");
	
	glUseProgram(0);
}

void sgsdl2_texture_render_geometry(sgsdl2_geometry geometry, sgsdl2_texture texture, GLuint shader_program, float const * const model_transform, float const * const view_transform, float const * const proj_transform)
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
	glBindVertexArray(geometry.vao);
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry.indices_buffer);
	glDrawElements(GL_TRIANGLES, geometry.num_of_indices, GL_UNSIGNED_SHORT, 0);
	glBindTexture(GL_TEXTURE_2D, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	sgsdl2_check_opengl_error("texture_render_geometry.render: ");
	
	glUseProgram(0);
}

GLuint sgsdl2_select_shader(sgsdl2_geometry const geometry)
{
	return 0;
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
