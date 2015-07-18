//
//  SGSDL2Utilities.h
//  sgsdl2
//
//  Created by James Ferguson on 4/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Utilities__
#define __sgsdl2__SGSDL2Utilities__

#include "sgInterfaces.h"
#include <string>
#include "glm/glm.hpp"

using namespace std;
using namespace glm;

vec3 sgsdl2_make_vec3_color(sg_color color);

void sgsdl2_print_opengl_version();

// Reads the entire contents of a file into content
// Returns true upon success
bool sgsdl2_read_file_contents(string const path, string &content);

bool sgsdl2_check_opengl_error(string prompt = "");

void sgsdl2_update_opengl_render(sg_drawing_surface *surface);

void sgsdl2_clear_opengl_window(sg_color color);

string sgsdl2_uni_light_name(int i, const char *str);


#endif /* defined(__sgsdl2__SGSDL2Utilities__) */
