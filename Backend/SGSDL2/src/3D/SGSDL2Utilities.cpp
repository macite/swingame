//
//  SGSDL2Utilities.cpp
//  sgsdl2
//
//  Created by James Ferguson on 4/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Utilities.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include "SGSDL2Types.h"
#include "SGSDL2Graphics.h"

using namespace std;

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

void sgsdl2_print_error(const char *error)
{
	cout << error << endl;
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

void sgsdl2_clear_opengl_window(sg_color color)
{
	glClearColor(color.r, color.g, color.b, color.a);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

string sgsdl2_uni_light_name(int i, const char *str)
{
	std::stringstream ss;
	ss << SHAD_UNI_NAME_LIGHT_ARRAY "[";
	ss << i;
	ss << "]." << str;
	return ss.str();
}

vec3 sgsdl2_angles_from_quat(vec4 quat)
{
	vec3 vec = vec3(0, 0, 0);
	// Code from unreal engine
	// https://github.com/EpicGames/UnrealEngine/blob/release/Engine/Source/Runtime/Core/Private/Math/UnrealMath.cpp
	
	const float SingularityTest = quat.z*quat.x-quat.w*quat.y;
	const float YawY = 2.f*(quat.w*quat.z+quat.x*quat.y);
	const float YawX = (1.f-2.f*(quat.y * quat.y + quat.z * quat.z));
	
	static const float RAD_TO_DEG = (180.f) / (float) M_PI;
	static const float DEG_TO_RAD = (float) M_PI / (180.f);
	const float SINGULARITY_THRESHOLD = 0.4999995f;
	
	if (SingularityTest < -SINGULARITY_THRESHOLD)
	{
		vec.x = 270.f;
		vec.y = atan2(YawY, YawX) * RAD_TO_DEG;
		vec.z = -vec.y - (2.f * atan2(quat.x, quat.w) * RAD_TO_DEG);
	}
	else if (SingularityTest > SINGULARITY_THRESHOLD)
	{
		vec.x = 90.f;
		vec.y = atan2(YawY, YawX);
		vec.z = vec.y - (2.f * atan2(quat.x, quat.w));
	}
	else
	{
		vec.x = asin(2.f*(SingularityTest)) * RAD_TO_DEG;
		vec.y = atan2(YawY, YawX) * RAD_TO_DEG;
		vec.z = atan2(-2.f*(quat.w*quat.x+quat.y*quat.z), 1.f-2.f*(quat.x * quat.x + quat.y * quat.y));
	}
	
	// Convert back to radians
	vec.x *= DEG_TO_RAD;
	vec.y *= DEG_TO_RAD;
	vec.z *= DEG_TO_RAD;
	return vec;
}

void sgsdl2_flatten_array(aiVector3D *vectors, unsigned int size, float *&flattened_array, unsigned int &new_size, unsigned int dimension)
{
	new_size = size * dimension;
	flattened_array = (float*) malloc(sizeof(float) * new_size);
	
	for (unsigned int i = 0; i < size; i++)
	{
		for (unsigned int j = 0; j < dimension; j++)
		{
			flattened_array[i * dimension + j] = vectors[i][j];
		}
	}
}

vec3 sgsdl2_make_vec3_color(sg_color color)
{
	vec3 result = vec3(0, 0, 0);
	result.r = color.r;
	result.b = color.b;
	result.g = color.g;
	return result;
}

sg_color sgsdl2_color(aiColor4D col)
{
	return {col.r, col.g, col.b, col.a};
}

vec3 sgsdl2_vec3(aiVector3D vec)
{
	return vec3(vec.x, vec.y, vec.z);
}

vec4 sgsdl2_vec4(aiQuaternion q)
{
	return vec4(q.x, q.y, q.z, q.w);
}

mat4 sgsdl2_mat4(aiMatrix4x4 m)
{
	mat4 new_mat = mat4();
	for (unsigned int i = 0; i < 16; i++)
	{
		value_ptr(new_mat)[i] = *m[i];
	}
	return new_mat;
}






