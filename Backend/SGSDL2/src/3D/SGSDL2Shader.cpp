//
//  SGSDL2Shader.cpp
//  sgsdl2
//
//  Created by James Ferguson on 6/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Shader.h"
#include <iostream>
#include "SGSDL2Utilities.h"
#include "SGSDL2Scene.h"

using namespace std;


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
	glBindAttribLocation(program, MESH_ATTR_VERTICES, SHAD_ATTR_NAME_VERTICES);
	glBindAttribLocation(program, MESH_ATTR_NORMALS, SHAD_ATTR_NAME_NORMALS);
	glBindAttribLocation(program, MESH_ATTR_TEXCOORDS, SHAD_ATTR_NAME_TEXCOORDS);
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
		std::cout << "Shader program validate error:" << endl << infoLog;
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


void sgsdl2_bind_data_to_shader(SGuint shader, sgsdl2_shader_interface interface)
{
	auto lighting_state = interface.lighting;
	auto camera_state = interface.camera;
	auto node_state = interface.node;
	auto material_state = interface.material;
	
	glUseProgram(shader);
	
	SGint view = glGetUniformLocation(shader, SHAD_UNI_NAME_VIEW);
	SGint proj = glGetUniformLocation(shader, SHAD_UNI_NAME_PROJ);
	SGint model = glGetUniformLocation(shader, SHAD_UNI_NAME_MODEL);
	SGint normal_model = glGetUniformLocation(shader, SHAD_UNI_NAME_NORM_MODEL);
	SGint mvp = glGetUniformLocation(shader, SHAD_UNI_NAME_MVP);
	SGint camera_loc = glGetUniformLocation(shader, SHAD_UNI_NAME_CAMERA_LOC);
	SGint diffuse = glGetUniformLocation(shader, SHAD_UNI_NAME_DIFFUSE);
	SGint specular = glGetUniformLocation(shader, SHAD_UNI_NAME_SPECULAR);
	SGint specular_exponent = glGetUniformLocation(shader, SHAD_UNI_NAME_SPECULAR_EXPONENT);
	SGint specular_intensity = glGetUniformLocation(shader, SHAD_UNI_NAME_SPECULAR_INTENSITY);
	SGint diffuse_texture = glGetUniformLocation(shader, SHAD_UNI_NAME_DIFFUSE_TEXTURE);
	SGint specular_texture = glGetUniformLocation(shader, SHAD_UNI_NAME_SPECULAR_TEXTURE);
	SGint normal_map = glGetUniformLocation(shader, SHAD_UNI_NAME_NORMAL_MAP);
	SGint shadow_map = glGetUniformLocation(shader, SHAD_UNI_NAME_SHADOW_MAP);

	glUniformMatrix4fv(view, 1, false, value_ptr(camera_state.view));
	glUniformMatrix4fv(proj, 1, false, value_ptr(camera_state.proj));
	glUniformMatrix4fv(model, 1, false, value_ptr(node_state.model));
	glUniformMatrix4fv(normal_model, 1, false, value_ptr(node_state.normal_model));
	glUniformMatrix4fv(mvp, 1, false, value_ptr(interface.mvp));
	glUniform3fv(camera_loc, 1, value_ptr(camera_state.camera_loc));
	glUniform3fv(diffuse, 1, value_ptr(material_state.diffuse_color));
	glUniform3fv(specular, 1, value_ptr(material_state.specular_color));
	glUniform1f(specular_exponent, material_state.specular_exponent);
	glUniform1f(specular_intensity, material_state.specular_intensity);
	
	glActiveTexture(GL_TEXTURE0 + SHAD_TEX_INDEX_DIFFUSE);
	glBindTexture(GL_TEXTURE_2D, material_state.diffuse_texture);
	glUniform1i(diffuse_texture, SHAD_TEX_INDEX_DIFFUSE);
	
	glActiveTexture(GL_TEXTURE0 + SHAD_TEX_INDEX_SPECULAR);
	glBindTexture(GL_TEXTURE_2D, material_state.specular_texture);
	glUniform1i(specular_texture, SHAD_TEX_INDEX_SPECULAR);
	
	glActiveTexture(GL_TEXTURE0 + SHAD_TEX_INDEX_NORMAL);
	glBindTexture(GL_TEXTURE_2D, material_state.normal_map);
	glUniform1i(normal_map, SHAD_TEX_INDEX_NORMAL);
	
	glActiveTexture(GL_TEXTURE0 + SHAD_TEX_INDEX_DEPTH);
	glBindTexture(GL_TEXTURE_2D_ARRAY, lighting_state.shadow_map);
	glUniform1i(shadow_map, SHAD_TEX_INDEX_DEPTH);

	// if (interface.lighting is valid)
	// Lighting
	for (int i = 0; i < lighting_state.lighting_count; i++)
	{
		SGint light_loc = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_LOCATION(i));
		SGint light_forward = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_FORWARD(i));
		SGint depth_trans = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_TRANS(i));
		SGint intensities = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_INTENSITIES(i));
		SGint attenuation_cutoff = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_ATTENUATION(i));
		SGint ambient_coefficient = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_AMBIENT(i));
		SGint cos_outer = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_COS_OUTER(i));
		SGint cos_inner = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_COS_INNER(i));
		SGint casts_shadows = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_CASTS_SHADOWS(i));
		SGint shadow_map_level = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_SHADOW_MAP(i));
		SGint type = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_TYPE(i));
		
		sgsdl2_light_state light = lighting_state.lights[i];
		glUniform3fv(light_loc, 1, value_ptr(light.location));
		glUniform3fv(light_forward, 1, value_ptr(light.forward));
		glUniformMatrix4fv(depth_trans, 1, false, value_ptr(light.depth_transform));
		glUniform3fv(intensities, 1, value_ptr(light.intensities));
		glUniform1f(attenuation_cutoff, light.attenuation_cutoff);
		glUniform1f(ambient_coefficient, light.ambient_coefficient);
		glUniform1f(cos_outer, light.cos_inner_cone);
		glUniform1f(cos_inner, light.cos_outer_cone);
		glUniform1i(casts_shadows, light.casts_shadows);
		glUniform1i(shadow_map_level, (int) light.shadow_map_level);
		glUniform1i(type, (int) light.type);
	}
	SGint light_count = glGetUniformLocation(shader, SHAD_UNI_NAME_LIGHT_COUNT);
	glUniform1i(light_count, (int) lighting_state.lighting_count);
	
	glUseProgram(0);
	sgsdl2_check_opengl_error("bind_data_to_shader: ");
}



