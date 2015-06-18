//
//  test_graphics3d.cpp
//  sgsdl2
//
//  Created by James Ferguson on 12/03/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "test_graphics3d.h"

#include "SGSDL2Graphics.h"
#include "SGSDL2Graphics3D.h"
#include "sgInterfaces.h"
#include "Matrix4f.h"
//#include "../Resources/RE_CVX_hall.h"

#include <iostream>
#include <vector>
#include <cmath>

#define SHAD_VERTEX_COLOR_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/vertexColor.vert"
#define SHAD_VERTEX_COLOR_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/vertexColor.frag"
#define SHAD_SOLID_COLOR_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/solidColor.vert"
#define SHAD_SOLID_COLOR_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/solidColor.frag"
#define SHAD_TEXTURE_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/texture.vert"
#define SHAD_TEXTURE_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/texture.frag"
#define SHAD_LIGHT_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/light.vert"
#define SHAD_LIGHT_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/light.frag"

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





sgsdl2_scene_element* build_old_scene(sgsdl2_scene *scene)
{
	sgsdl2_geometry *cube1 = sgsdl2_make_geometry({{0, 0, 25}},
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_geometry *cube2 = sgsdl2_make_geometry({{0, 0, 0}},
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_camera *camera = sgsdl2_make_camera({{0, 4, -15}},
											   {{0, -4, 15}},
											   {{0, 1, 0}});
	sgsdl2_light *light = sgsdl2_make_light({{0, 0, -15}},
											{{0, 0, 15}},
											{{0, 1, 0}});
	
//	sgsdl2_texture *cat_texture = sgsdl2_make_texture();
//	sgsdl2_attach_texture_image(cat_texture, TEXTURE_PATH, GL_RGBA);
//	sgsdl2_generate_texture_mipmaps(cat_texture);
	sgsdl2_material *material1 = new sgsdl2_material();
	sgsdl2_material *material2 = new sgsdl2_material();
	
	light->intensity = 1.2f;
	light->attenuation = 0.001f;
	light->ambient_coefficient = 0.1f;
	light->light_type = sgsdl2_light_type::SPOT;
//	light->light_type = sgsdl2_light_type::DIRECTIONAL;
	light->width = 30;
	light->height = 30;
	light->cutoff = 50;
	light->cos_outer_cone = cosf(20 * M_PI / 180.0);
	
	material1->diffuse_color = {1, 0, 0, 1};
	material1->specular_color = {1, 0.8f, 0.8f, 1};
	material1->specular_exponent = 50;
	material1->specular_intensity = 1;
	material1->shader = (int) scene->default_shader;
	
	material2->diffuse_color = {0, 1, 0, 1};
	material2->specular_color = {0.5, 0.5, 0.5, 1};
	material2->specular_exponent = 25;
	material2->specular_intensity = 0.5;
	material2->shader = (int) scene->default_shader;
	
	sgsdl2_add_element_to_root(scene, cube1);
	sgsdl2_add_element_to_root(scene, cube2);
	sgsdl2_add_element_to_root(scene, camera);
	sgsdl2_add_element_to_root(scene, light);
	scene->active_camera = camera;
	
	sgsdl2_attach_vertices(cube1, vertices_big, 24, 3);
	sgsdl2_attach_vertices(cube2, vertices, 24, 3);
	sgsdl2_attach_indices(cube1, indices, 36);
	sgsdl2_attach_indices(cube2, indices, 36);
	sgsdl2_attach_normals(cube1, normals, 24, 3);
	sgsdl2_attach_normals(cube2, normals, 24, 3);
	sgsdl2_attach_colors(cube2, colors, 24, 3);
	
	cube1->material = material1;
	cube2->material = material2;
	camera->camera_type = sgsdl2_camera_type::PERSPECTIVE;
	sgsdl2_set_camera_frustum(camera, (float) M_PI / 3, (float) M_PI / 3, 1, 300);
	
	return light;
}



void print_3d_options()
{
	cout << "0: all " << endl;
//	cout << "1: basic 3d test"  << endl;
	cout << "2: textures" << endl;
	cout << "4: lighting example"  << endl;
	cout << "8: lighting with shadows"  << endl;
	cout << "16: loading geometry from file"  << endl;
//	cout << "16: input "  << endl;
//	cout << "32: text "  << endl;
//	cout << "64: network "  << endl;
//	cout << "128: graphics" << endl;
}


float randomf(float max = 1, float min = 0)
{
	return ((float) rand()) / ((float) RAND_MAX) * max + min;
}


sgsdl2_camera* add_default_camera(sgsdl2_scene *scene)
{
	sgsdl2_camera *camera = sgsdl2_make_camera({{0, 5, 10}},
											   {{0, -2, -10}},
											   {{0, 1, 0}});
	camera->camera_type = sgsdl2_camera_type::PERSPECTIVE;
	sgsdl2_set_camera_frustum(camera, (float) M_PI / 3, (float) M_PI / 3, 1, 100);
	sgsdl2_add_element_to_root(scene, camera);
	scene->active_camera = camera;
	return camera;
}


sgsdl2_light* add_default_light(sgsdl2_scene *scene)
{
	sgsdl2_light *light = sgsdl2_make_light({{0, 0, 10}},
											{{0, 0, -10}},
											{{0, 1, 0}});
	light->intensity = 1.2f;
	light->attenuation = 0.001f;
	light->ambient_coefficient = 0.2;
	light->light_type = sgsdl2_light_type::DIRECTIONAL;
	light->width = 30;
	light->height = 30;
	light->cutoff = 100;
	light->cos_outer_cone = cosf(30 * M_PI / 180);
	light->shadow_type = sgsdl2_shadowing_type::DYNAMIC;
	sgsdl2_add_element_to_root(scene, light);
	
	return light;
}


// Generates n random solid color materials. materials must be pre allocated to store n materials.
void generate_random_flat_materials(sgsdl2_material ***materials, int n)
{
	sgsdl2_material *mat;
	for (int i = 0; i < n; i++)
	{
		mat = new sgsdl2_material;
		// Each value is random from 0 to 1
		mat->diffuse_color = {randomf(), randomf(), randomf(), 1};
		// Always white
		mat->specular_color = {1, 1, 1, 1};
		// Random from 1 to 50
		mat->specular_exponent = randomf(50, 1);
		// Random from 0 to 1
		mat->specular_intensity = randomf();
		// Assigning a shader of zero will default to the default shader for the scene
		mat->shader = 0;
		(*materials)[i] = mat;
	}
}


sgsdl2_geometry* make_cube(Vector3f location)
{
	sgsdl2_geometry *cube = sgsdl2_make_geometry(location,
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_material **array = (sgsdl2_material**) malloc(sizeof(sgsdl2_material*));
	generate_random_flat_materials(&array, 1);
	cube->material = array[0];
	sgsdl2_attach_vertices(cube, vertices, 24, 3);
	sgsdl2_attach_indices(cube, indices, 36);
	sgsdl2_attach_normals(cube, normals, 24, 3);
	return cube;
}


sgsdl2_geometry* make_big_cube(Vector3f location)
{
	sgsdl2_geometry *cube = sgsdl2_make_geometry(location,
												 {{0, 0, -1}},
												 {{0, 1, 0}});
	sgsdl2_material **array = (sgsdl2_material**) malloc(sizeof(sgsdl2_material*));
	generate_random_flat_materials(&array, 1);
	cube->material = array[0];
	sgsdl2_attach_vertices(cube, vertices_big, 24, 3);
	sgsdl2_attach_indices(cube, indices, 36);
	sgsdl2_attach_normals(cube, normals, 24, 3);
	return cube;
}


void add_geometry_from_file_to_scene(sgsdl2_scene *scene, const char *file_name)
{
	int count = 0;
	sgsdl2_geometry **objects;
	sgsdl2_create_geometry_objects_from_file(file_name, &objects, &count);
	
	// Create materials for the geometry
	sgsdl2_material **materials = (sgsdl2_material**) malloc(sizeof(sgsdl2_material*) * count);
	generate_random_flat_materials(&materials, count);
	for (int i = 0; i < count; i++)
	{
		objects[i]->material = materials[i];
		sgsdl2_add_element_to_root(scene, objects[i]);
	}
}


void build_basic_scene(sgsdl2_scene *scene)
{
	add_default_camera(scene);
	sgsdl2_add_element_to_root(scene, make_cube({{0, 0, 0}}));
}

void build_lighting_scene(sgsdl2_scene *scene)
{
	add_default_camera(scene);
	add_default_light(scene);
	sgsdl2_add_element_to_root(scene, make_cube({{0, 0, 0}}));
	sgsdl2_add_element_to_root(scene, make_cube({{3, 0, 0}}));
	sgsdl2_add_element_to_root(scene, make_cube({{-3, 0, 0}}));
}

void build_texture_scene(sgsdl2_scene *scene)
{
	add_default_camera(scene);
	add_default_light(scene);
	
	sgsdl2_texture *cat_texture = sgsdl2_make_texture();
	sgsdl2_attach_texture_image(cat_texture, TEXTURE_PATH, GL_RGBA);
	sgsdl2_generate_texture_mipmaps(cat_texture);
	
	sgsdl2_geometry* textured_cube = make_big_cube({{0, 0, -25}});
	textured_cube->material->texture = cat_texture->handle;
	sgsdl2_attach_texcoords(textured_cube, texCoords, 16);
	sgsdl2_add_element_to_root(scene, textured_cube);
}

void build_shadowing_scene(sgsdl2_scene *scene)
{
	add_default_camera(scene);
	add_default_light(scene);
	sgsdl2_add_element_to_root(scene, make_cube({{0, 0, 0}}));
	sgsdl2_add_element_to_root(scene, make_big_cube({{0, 0, -25}}));
}

void load_scene(sgsdl2_scene *scene)
{
	add_default_camera(scene);
	add_default_light(scene);
	add_geometry_from_file_to_scene(scene, "3-shape-scene-normals-with-backing.obj");
}

void render(sgsdl2_scene *scene, int num_of_frames = 5 * 60)
{
	for (int frame_num = 0; frame_num < num_of_frames; frame_num++)
	{
		sgsdl2_clear_opengl_window(scene->surface, {0, 0, 0, 1});
		sgsdl2_render_scene(scene);
		sgsdl2_check_opengl_error();
		sgsdl2_update_opengl_render(scene->surface);
		
		_sg_functions->utils.delay(1000 / 60);
	}
}

void test_graphics3d()
{
	cout << "Select a 3D test to run" << endl;
	print_3d_options();
	int test_run = 0;
	scanf("%d", &test_run);
	if (test_run == 0) test_run = 255;
	
	// Initialize the surface
	sg_drawing_surface surface = _sg_functions->graphics.open_window("3D Graphics", 800, 600);
	sgsdl2_print_opengl_version();
	
	// Initialize the scene
	// These could be moved into one command (ie load scene from bundle)
	sgsdl2_scene *scene = sgsdl2_make_scene();
	scene->surface = &surface;
	sgsdl2_compile_default_shaders(scene);
	sgsdl2_prepare_for_3d();
	
	
	cout << "Starting 3D tests" << endl;
//	if (test_run & BASIC)
//	{
//		build_basic_scene(scene);
//		render(scene);
//		sgsdl2_clear_scene(scene);
//		cout << "Completed basic test" << endl;
//	}

	if (test_run & LIGHTING)
	{
		build_lighting_scene(scene);
		render(scene);
		sgsdl2_clear_scene(scene);
		cout << "Completed lighting test" << endl;
	}
	
	if (test_run & TEXTURES)
	{
		build_texture_scene(scene);
		render(scene);
		sgsdl2_clear_scene(scene);
		cout << "Completed texture test" << endl;
	}
	
	if (test_run & SHADOWS)
	{
		build_shadowing_scene(scene);
		render(scene);
		sgsdl2_clear_scene(scene);
		cout << "Completed shadowing test" << endl;
	}

	if (test_run & LOADING)
	{
		load_scene(scene);
		render(scene);
		sgsdl2_clear_scene(scene);
		cout << "Completed loading from file test" << endl;
	}
	
	sgsdl2_delete_scene(scene);
	cout << "All tests completed" << endl;
}
