//
//  test_graphics3d.cpp
//  sgsdl2
//
//  Created by James Ferguson on 12/03/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "test_graphics3d.h"

#ifdef __linux__
#include <SDL2/SDL.h>
#include <SDL2/SDL_net.h>
#else
#include <SDL.h>
#include <SDL_net.h>
#endif

#include "sgInterfaces.h"
#include "SGSDL2Types.h"
#include "SGSDL2Utilities.h"
#include "SGSDL2Scene.h"
#include "SGSDL2Node.h"
#include "SGSDL2Mesh.h"
#include "SGSDL2Camera.h"
#include "SGSDL2Light.h"
#include "SGSDL2Texture.h"
#include "SGSDL2Importer.h"
#include "SGSDL2Input.h"
#include <iostream>
#include <vector>
#include <cmath>

#include <assimp/quaternion.h>
//#include "assimp/quaternion.inl"

//#define SHAD_VERTEX_COLOR_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/vertexColor.vert"
//#define SHAD_VERTEX_COLOR_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/vertexColor.frag"
//#define SHAD_SOLID_COLOR_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/solidColor.vert"
//#define SHAD_SOLID_COLOR_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/solidColor.frag"
//#define SHAD_TEXTURE_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/texture.vert"
//#define SHAD_TEXTURE_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/texture.frag"
//#define SHAD_LIGHT_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/light.vert"
//#define SHAD_LIGHT_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/light.frag"

#define TEXTURE_PATH "sample.png"


using namespace std;
extern sg_interface *_sg_functions;

enum test_options
{
	BASIC = 1,
	TEXTURES = 2,
	LIGHTING = 4,
	SHADOWS = 8,
	LOADING = 16,
//	TEXT = 32,
//	NETWORK = 64,
//	GRAPHICS3D = 128
};



// Random data used for testing without loading from file.
float vertices[] = {
	1.0,	1.0,	-1.0,
	1.0,	-1.0,	-1.0,
	-1.0,	-1.0,	-1.0,
	-1.0,	1.0,	-1.0,
	1.0,	1.0,	1.0,
	1.0,	-1.0,	1.0,
	-1.0,	-1.0,	1.0,
	-1.0,	1.0,	1.0
};
float vertices_big[] = {
	10.0,	10.0,	-10.0,
	10.0,	-10.0,	-10.0,
	-10.0,	-10.0,	-10.0,
	-10.0,	10.0,	-10.0,
	10.0,	10.0,	10.0,
	10.0,	-10.0,	10.0,
	-10.0,	-10.0,	10.0,
	-10.0,	10.0,	10.0
};
float normals[] = {
	0.577f, 0.577f, -0.577f,
	0.577f, -0.577f, -0.577f,
	-0.577f, -0.577f, -0.577f,
	-0.577f, 0.577f, -0.577f,
	0.577f, 0.577f, 0.577f,
	0.577f, -0.577f, 0.577f,
	-0.577f, -0.577f, 0.577f,
	-0.577f, 0.577f, 0.577f,
};
unsigned short indices[] = {
	0, 1, 2,
	2, 3, 0,
	4, 7, 6,
	6, 5, 4,
	0, 4, 5,
	5, 1, 0,
	1, 5, 6,
	6, 2, 1,
	2, 6, 7,
	7, 3, 2,
	4, 0, 3,
	3, 7, 4
};
float colors[] = {
	0.0,	0.0,	0.0,
	0.0,	0.0,	1.0,
	0.0,	1.0,	0.0,
	0.0,	1.0,	1.0,
	1.0,	0.0,	0.0,
	1.0,	0.0,	1.0,
	1.0,	1.0,	0.0,
	1.0,	1.0,	1.0,
};
float texCoords[] = {

	1, 1,
	1, 0,
	0, 0,
	0, 1,
	0, 0,
	1, 0,
	1, 1,
	1, 0
};




void render(sgsdl2_scene *scene, int seconds)
{
	// Render for 3 seconds
	int frames = 1;
	while (frames != seconds * 60)
	{
		if (scene->active_camera)
		{
			sgsdl2_node *camera_node = scene->active_camera->parent;
			
			// Render controls
			_sg_functions->input.process_events();
			
			int forwards = 0;
			int right = 0;
			int up = 0;
			
			if (_sg_functions->input.key_pressed(SDLK_w))
			{
				forwards++;
			}
			if (_sg_functions->input.key_pressed(SDLK_s))
			{
				forwards--;
			}
			if (_sg_functions->input.key_pressed(SDLK_a))
			{
				right--;
			}
			if (_sg_functions->input.key_pressed(SDLK_d))
			{
				right++;
			}
			if (_sg_functions->input.key_pressed(SDLK_q))
			{
				up--;
			}
			if (_sg_functions->input.key_pressed(SDLK_e))
			{
				up++;
			}
			
			float speed = 0.2;
			mat4 trans = eulerAngleXYZ(camera_node->rotation.x, camera_node->rotation.y, camera_node->rotation.z);
			vec4 forwards_v = trans * vec4(0, 0, -1, 0);
			vec4 right_v = trans * vec4(1, 0, 0, 0);
			vec4 up_v = trans * vec4(0, 1, 0, 0);
			vec4 direction = forwards_v * forwards + right_v * right + up_v * up;
			direction *= speed;
			camera_node->location += vec3(direction.x, direction.y, direction.z);
		}
		
		sgsdl2_clear_opengl_window({0, 0, 0, 1});
		sgsdl2_render_scene(scene);
		sgsdl2_update_opengl_render(scene->surface);
		sgsdl2_check_opengl_error("main_loop: ");
		
		_sg_functions->utils.delay(1000 / 60);
		frames++;
	}

}

void test_graphics3d()
{
//	cout << "Select a 3D test to run" << endl;
//	print_3d_options();
//	int test_run = 0;
//	scanf("%d", &test_run);
//	if (test_run == 0) test_run = 255;
	
	// Initialize the surface
	sg_drawing_surface surface = _sg_functions->graphics.open_window("3D Graphics", 1440, 800);
	sgsdl2_print_opengl_version();
	
	// Initialize the scene
	sgsdl2_scene *scene = sgsdl2_make_scene(&surface);
//	sgsdl2_populate_scene_from_file(scene, "scenes/swords/swords.3ds");
	sgsdl2_populate_scene_from_file(scene, "scenes/batcave (OBJ)/batcave.obj");
	
	sgsdl2_node *camera_node = sgsdl2_create_new_node(scene->root_node, vec3(0, 0, 10), vec3(0, 0, 0), vec3(1, 1, 1));
	sgsdl2_camera *camera = sgsdl2_create_perspective_camera(camera_node);
	sgsdl2_set_active_camera(scene, camera);
	
	sgsdl2_node *light_node  = sgsdl2_create_new_node(camera_node, vec3(0, 0, 0), vec3(0, 0, 0), vec3(1, 1, 1));
	sgsdl2_light *light = sgsdl2_create_spot_light(light_node);
	light->attenuation_cutoff = 200;
	light->intensity = 1.3;
	
	// Render the scene
	render(scene, 0);
	sgsdl2_delete_scene(scene);
	
	
//	cout << "Starting 3D tests" << endl;
//	cout << "All tests completed" << endl;
//	if (test_run & BASIC)
//	{
//		build_basic_scene(scene);
//		render(scene);
//		sgsdl2_clear_scene(scene);
//		cout << "Completed basic test" << endl;
//	}
//
//	if (test_run & LIGHTING)
//	{
//		build_lighting_scene(scene);
//		render(scene);
//		sgsdl2_clear_scene(scene);
//		cout << "Completed lighting test" << endl;
//	}
//	
//	if (test_run & TEXTURES)
//	{
//		build_texture_scene(scene);
//		render(scene);
//		sgsdl2_clear_scene(scene);
//		cout << "Completed texture test" << endl;
//	}
//	
//	if (test_run & SHADOWS)
//	{
//		build_shadowing_scene(scene);
//		render(scene);
//		sgsdl2_clear_scene(scene);
//		cout << "Completed shadowing test" << endl;
//	}
//
//	if (test_run & LOADING)
//	{
//		load_scene(scene);
//		render(scene);
//		sgsdl2_clear_scene(scene);
//		cout << "Completed loading from file test" << endl;
//	}
}
