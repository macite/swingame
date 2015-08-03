//
//  SGSDL2Scene.cpp
//  sgsdl2
//
//  Created by James Ferguson on 29/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Scene.h"
#include <iostream>
#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>
#include "SGSDL2Node.h"
#include "SGSDL2Camera.h"
#include "SGSDL2Utilities.h"
#include "SGSDL2Light.h"
#include "SGSDL2Shader.h"
#include "SGSDL2Renderer.h"

// Paths to the default shaders
#define SHAD_SOURCE_VERT "shaders/light.vert"
#define SHAD_SOURCE_FRAG "shaders/light.frag"
#define SHAD_SOURCE_DEPTH_VERT "shaders/depth.vert"
#define SHAD_SOURCE_DEPTH_FRAG "shaders/depth.frag"

using namespace std;


sgsdl2_scene* sgsdl2_make_scene(sg_drawing_surface *surface)
{
	sgsdl2_scene *scene = new sgsdl2_scene();
	scene->surface = surface;
	sgsdl2_create_root_node(scene);
	sgsdl2_compile_default_shaders(scene);
	sgsdl2_create_default_array_texture(scene);
	
	// Generate default textures
	float white_data[] = {1, 1, 1, 1};
	glGenTextures(1, &scene->white_texture);
	glBindTexture(GL_TEXTURE_2D, scene->white_texture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA2, 1, 1, 0, GL_RGBA, GL_FLOAT, &white_data);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glBindTexture(GL_TEXTURE_2D, 0);

	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);
	glEnable(GL_CULL_FACE);
	glCullFace(GL_BACK);

	return scene;
}

void sgsdl2_compile_default_shaders(sgsdl2_scene *scene)
{
	SGuint shaders[2];

	// Lights
	sgsdl2_make_shader(SHAD_SOURCE_VERT, GL_VERTEX_SHADER, shaders[0]);
	sgsdl2_make_shader(SHAD_SOURCE_FRAG, GL_FRAGMENT_SHADER, shaders[1]);
	sgsdl2_make_shader_program(shaders, 2, scene->default_shader);

	// Depth
	sgsdl2_make_shader(SHAD_SOURCE_DEPTH_VERT, GL_VERTEX_SHADER, shaders[0]);
	sgsdl2_make_shader(SHAD_SOURCE_DEPTH_FRAG, GL_FRAGMENT_SHADER, shaders[1]);
	sgsdl2_make_shader_program(shaders, 2, scene->default_depth_shader);

}

void sgsdl2_create_default_array_texture(sgsdl2_scene *scene)
{
	int num_of_levels = MAX_LIGHTING_COUNT;
	int max_opengl_levels;
	glGetIntegerv(GL_MAX_ARRAY_TEXTURE_LAYERS, &max_opengl_levels);

	if (num_of_levels > max_opengl_levels)
	{
		cout << "Requested number of levels for array texture is higher than the maximum allowed in opengl." << endl;
		num_of_levels = max_opengl_levels;
	}

	glGenTextures(1, &scene->shadow_map_array);
	glBindTexture(GL_TEXTURE_2D_ARRAY, scene->shadow_map_array);
	glTexImage3D(GL_TEXTURE_2D_ARRAY, 0, GL_DEPTH_COMPONENT16, SHAD_MAP_SIZE, SHAD_MAP_SIZE, num_of_levels, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);

	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);

	glBindTexture(GL_TEXTURE_2D_ARRAY, 0);
	sgsdl2_check_opengl_error("create_default_array_texture: ");

	// Make sure the occupied array is set to false.
	fill_n(scene->occupied_shadow_map_levels, num_of_levels, false);
}

void sgsdl2_add_node(sgsdl2_scene *scene, sgsdl2_node *node)
{
	if (!scene->root_node)
	{
		sgsdl2_print_error(ERR_SCENE_NO_ROOT);
		return;
	}

	sgsdl2_add_node(scene->root_node, node);
}

void sgsdl2_clear_scene(sgsdl2_scene *scene)
{
	if (!scene->root_node)
	{
		sgsdl2_print_error(ERR_SCENE_NO_ROOT);
		return;
	}

	sgsdl2_remove_all_children(scene->root_node);
}

void sgsdl2_set_active_camera(sgsdl2_scene *scene, sgsdl2_camera *cam)
{
	scene->active_camera = cam;
}

void sgsdl2_render_scene(sgsdl2_scene *scene)
{
	// Render process:
	// Rebuild scene index if needed.
	// Check for active camera.
	// Calculate scene state
	// Calculate node state
	// Calculate mesh state
	// Pass all data to the shader
	// Render

	// Scene must be properly initiated
	if (!scene->root_node)
	{
		sgsdl2_print_error(ERR_SCENE_NO_ROOT);
		return;
	}

	// Scene must have an active camera set.
	if (scene->active_camera == nullptr)
	{
		sgsdl2_print_error(ERR_MISSING_CAMERA);
		return;
	}
	
	// Create a renderer for all rendering operations this frame.
	sgsdl2_renderer renderer = sgsdl2_renderer();
	renderer.scene = scene;

	// Rerender depth maps
	sgsdl2_rerender_shadow_maps(scene, &renderer);
	// Store light states in renderer
	sgsdl2_calculate_lighting_state(scene, &renderer);

	// Perform main render pass.
	sgsdl2_render_options opts;
	opts.mode = sgsdl2_shader_mode::FULL;
	sgsdl2_perform_render_pass(&renderer, scene->active_camera, opts);
}

void sgsdl2_iterate_node(sgsdl2_node *node, void (*func)(sgsdl2_node*))
{
	// Call with the current node
	(*func)(node);

	// Iterate over children
	for (unsigned long i = 0; i < node->children.size(); i++)
	{
		sgsdl2_iterate_node(node, func);
	}
}

void sgsdl2_delete_scene(sgsdl2_scene *scene)
{
	delete scene;
}
