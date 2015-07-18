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

vec3 sgsdl2_make_vec3_color(sg_color color)
{
	vec3 result = vec3(0, 0, 0);
	result.r = color.r;
	result.b = color.b;
	result.g = color.g;
	return result;
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









