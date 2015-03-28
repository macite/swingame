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

#include <iostream>
#include <vector>

#define SHAD_VERTEX_COLOR_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/vertexColorVert.glsl"
#define SHAD_VERTEX_COLOR_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/vertexColorFrag.glsl"
#define SHAD_SOLID_COLOR_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/solidColorVert.glsl"
#define SHAD_SOLID_COLOR_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/solidColorFrag.glsl"
#define SHAD_TEXTURE_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/textureVert.glsl"
#define SHAD_TEXTURE_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/textureFrag.glsl"
#define SHAD_LIGHT_VERT_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/lightVert.glsl"
#define SHAD_LIGHT_FRAG_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/SGSDL2/src/shaders/lightFrag.glsl"

#define TEXTURE_PATH "/Users/jamesferguson/Documents/Coding/SwingameForked/Backend/Test/Resources/sample.png"


using namespace std;
extern sg_interface *_sg_functions;


GLuint program;
struct ShaderPrograms
{
	vector<GLuint> shaders;
	GLuint solidColor, vertexColor, texture, lights;
};

ShaderPrograms compiledShaders;

//float vertices[] = {
//	-1.0f,-1.0f,-1.0f,
//	1.0f,-1.0f,-1.0f,
//	-1.0f,-1.0f, 1.0f,
//	1.0f,-1.0f,-1.0f,
//	1.0f,-1.0f, 1.0f,
//	-1.0f,-1.0f, 1.0f,
//	-1.0f, 1.0f,-1.0f,
//	-1.0f, 1.0f, 1.0f,
//	1.0f, 1.0f,-1.0f,
//	1.0f, 1.0f,-1.0f,
//	-1.0f, 1.0f, 1.0f,
//	1.0f, 1.0f, 1.0f,
//	-1.0f,-1.0f, 1.0f,
//	1.0f,-1.0f, 1.0f,
//	-1.0f, 1.0f, 1.0f,
//	1.0f,-1.0f, 1.0f,
//	1.0f, 1.0f, 1.0f,
//	-1.0f, 1.0f, 1.0f,
//	-1.0f,-1.0f,-1.0f,
//	-1.0f, 1.0f,-1.0f,
//	1.0f,-1.0f,-1.0f,
//	1.0f,-1.0f,-1.0f,
//	-1.0f, 1.0f,-1.0f,
//	1.0f, 1.0f,-1.0f,
//	-1.0f,-1.0f, 1.0f,
//	-1.0f, 1.0f,-1.0f,
//	-1.0f,-1.0f,-1.0f,
//	-1.0f,-1.0f, 1.0f,
//	-1.0f, 1.0f, 1.0f,
//	-1.0f, 1.0f,-1.0f,
//	1.0f,-1.0f, 1.0f,
//	1.0f,-1.0f,-1.0f,
//	1.0f, 1.0f,-1.0f,
//	1.0f,-1.0f, 1.0f,
//	1.0f, 1.0f,-1.0f,
//	1.0f, 1.0f, 1.0f
//};
//
//float texCoords[] = {
//	0.0f, 0.0f,
//	1.0f, 0.0f,
//	0.0f, 1.0f,
//	1.0f, 0.0f,
//	1.0f, 1.0f,
//	0.0f, 1.0f,
//	0.0f, 0.0f,
//	0.0f, 1.0f,
//	1.0f, 0.0f,
//	1.0f, 0.0f,
//	0.0f, 1.0f,
//	1.0f, 1.0f,
//	1.0f, 0.0f,
//	0.0f, 0.0f,
//	1.0f, 1.0f,
//	0.0f, 0.0f,
//	0.0f, 1.0f,
//	1.0f, 1.0f,
//	0.0f, 0.0f,
//	0.0f, 1.0f,
//	1.0f, 0.0f,
//	1.0f, 0.0f,
//	0.0f, 1.0f,
//	1.0f, 1.0f,
//	0.0f, 1.0f,
//	1.0f, 0.0f,
//	0.0f, 0.0f,
//	0.0f, 1.0f,
//	1.0f, 1.0f,
//	1.0f, 0.0f,
//	1.0f, 1.0f,
//	1.0f, 0.0f,
//	0.0f, 0.0f,
//	1.0f, 1.0f,
//	0.0f, 0.0f,
//	0.0f, 1.0f
//};
//
//float normals[] = {
//	0.0f, -1.0f, 0.0f,
//	0.0f, -1.0f, 0.0f,
//	0.0f, -1.0f, 0.0f,
//	0.0f, -1.0f, 0.0f,
//	0.0f, -1.0f, 0.0f,
//	0.0f, -1.0f, 0.0f,
//	0.0f, 1.0f, 0.0f,
//	0.0f, 1.0f, 0.0f,
//	0.0f, 1.0f, 0.0f,
//	0.0f, 1.0f, 0.0f,
//	0.0f, 1.0f, 0.0f,
//	0.0f, 1.0f, 0.0f,
//	0.0f, 0.0f, 1.0f,
//	0.0f, 0.0f, 1.0f,
//	0.0f, 0.0f, 1.0f,
//	0.0f, 0.0f, 1.0f,
//	0.0f, 0.0f, 1.0f,
//	0.0f, 0.0f, 1.0f,
//	0.0f, 0.0f, -1.0f,
//	0.0f, 0.0f, -1.0f,
//	0.0f, 0.0f, -1.0f,
//	0.0f, 0.0f, -1.0f,
//	0.0f, 0.0f, -1.0f,
//	0.0f, 0.0f, -1.0f,
//	-1.0f, 0.0f, 0.0f,
//	-1.0f, 0.0f, 0.0f,
//	-1.0f, 0.0f, 0.0f,
//	-1.0f, 0.0f, 0.0f,
//	-1.0f, 0.0f, 0.0f,
//	-1.0f, 0.0f, 0.0f,
//	1.0f, 0.0f, 0.0f,
//	1.0f, 0.0f, 0.0f,
//	1.0f, 0.0f, 0.0f,
//	1.0f, 0.0f, 0.0f,
//	1.0f, 0.0f, 0.0f,
//	1.0f, 0.0f, 0.0f
//};
//
//unsigned short indices[] = {
//	
//}

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
	0, 0,
	1, 0,
	1, 1,
	1, 0,
	1, 1,
	1, 0,
	0, 0,
	0, 1
};


void compile_default_shaders(sgsdl2_scene *scene)
{
	compiledShaders.shaders.resize(8, 0);
	
	// Solid color
	sgsdl2_make_shader(SHAD_SOLID_COLOR_VERT_PATH, GL_VERTEX_SHADER, compiledShaders.shaders[0]);
	sgsdl2_make_shader(SHAD_SOLID_COLOR_FRAG_PATH, GL_FRAGMENT_SHADER, compiledShaders.shaders[1]);
	sgsdl2_make_shader_program(&compiledShaders.shaders[0], 2, compiledShaders.solidColor);
	
	// Vertex color
	sgsdl2_make_shader(SHAD_VERTEX_COLOR_VERT_PATH, GL_VERTEX_SHADER, compiledShaders.shaders[2]);
	sgsdl2_make_shader(SHAD_VERTEX_COLOR_FRAG_PATH, GL_FRAGMENT_SHADER, compiledShaders.shaders[3]);
	sgsdl2_make_shader_program(&compiledShaders.shaders[2], 2, compiledShaders.vertexColor);
	
	// Texture
	sgsdl2_make_shader(SHAD_TEXTURE_VERT_PATH, GL_VERTEX_SHADER, compiledShaders.shaders[4]);
	sgsdl2_make_shader(SHAD_TEXTURE_FRAG_PATH, GL_FRAGMENT_SHADER, compiledShaders.shaders[5]);
	sgsdl2_make_shader_program(&compiledShaders.shaders[4], 2, compiledShaders.texture);
	
	// Lights
	sgsdl2_make_shader(SHAD_LIGHT_VERT_PATH, GL_VERTEX_SHADER, compiledShaders.shaders[6]);
	sgsdl2_make_shader(SHAD_LIGHT_FRAG_PATH, GL_FRAGMENT_SHADER, compiledShaders.shaders[7]);
	sgsdl2_make_shader_program(&compiledShaders.shaders[6], 2, compiledShaders.lights);
	
//	scene->shaders.push_back(compiledShaders.solidColor);
//	scene->shaders.push_back(compiledShaders.texture);
//	scene->shaders.push_back(compiledShaders.vertexColor);
	scene->default_solid_shader = compiledShaders.solidColor;
	scene->default_texture_shader = compiledShaders.texture;
	scene->default_vertex_color_shader = compiledShaders.vertexColor;
}

void clean_up_shaders()
{
	sgsdl2_delete_shader_program(compiledShaders.solidColor);
	sgsdl2_delete_shader_program(compiledShaders.vertexColor);
	
	for (unsigned int i = 0; i < compiledShaders.shaders.size(); i++)
	{
		sgsdl2_delete_shader(compiledShaders.shaders[i]);
	}
}

sgsdl2_scene_element* build_scene(sgsdl2_scene *scene)
{
	compile_default_shaders(scene);
	
	sgsdl2_geometry *cube1 = sgsdl2_make_geometry({{0, 0, 0}},
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_geometry *cube2 = sgsdl2_make_geometry({{5, 0, 0}},
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_geometry *cube3 = sgsdl2_make_geometry({{-5, 0, 0}},
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_camera *camera = sgsdl2_make_camera({{0, 5, -15}},
											   {{0, -0.5f, 1}},
											   {{0, 1, 0}});
	sgsdl2_light *light = sgsdl2_make_light({{0, 5, 0}},
											{{0, 0, -1}},
											{{0, 1, 0}});

	sgsdl2_texture *cat_texture = sgsdl2_make_texture();
	sgsdl2_attach_texture_image(cat_texture, TEXTURE_PATH, GL_RGBA);
	sgsdl2_generate_texture_mipmaps(cat_texture);
	
	sgsdl2_material *material1 = new sgsdl2_material();
	sgsdl2_material *material2 = new sgsdl2_material();
	sgsdl2_material *material3 = new sgsdl2_material();
	
	light->intensity = 0.5;
	light->attenuation = 0.01f;
	light->ambientCoefficient = 0.3f;
	
	material1->diffuse_color = {1, 0 ,0, 1};
	material1->specular_color = {1, 0.8f, 0.8f, 1};
	material1->specular_exponent = 50;
	material1->specular_intensity = 1;
//	material1->shader = SHADER_DEFAULT_SOLID;
	material1->shader = (int) compiledShaders.lights;
	
	material2->diffuse_color = {1, 1, 1, 1};
	material2->specular_color = {0.5, 0.5, 0.5, 1};
	material2->specular_exponent = 25;
	material2->specular_intensity = 0.5;
//	material2->shader = SHADER_DEFAULT_VERTEX_COLOR;
	material2->shader = (int) compiledShaders.lights;
	
	material3->diffuse_color = {1, 0, 0, 1};
	material3->specular_color = {1, 1, 1, 1};
	material3->specular_exponent = 30;
	material3->specular_intensity = 0.1f;
	material3->texture = cat_texture->handle;
//	material3->shader = SHADER_DEFAULT_TEXTURE;
	material3->shader = (int) compiledShaders.lights;
	
	sgsdl2_add_element_to_root(scene, cube1);
	sgsdl2_add_element_to_root(scene, cube2);
	sgsdl2_add_element_to_root(scene, cube3);
	sgsdl2_add_element_to_root(scene, camera);
	sgsdl2_add_element_to_root(scene, light);
	scene->active_camera = camera;
	
	sgsdl2_attach_vertices(cube1, vertices, 24, 3);
	sgsdl2_attach_vertices(cube2, vertices, 24, 3);
	sgsdl2_attach_vertices(cube3, vertices, 24, 3);
	
	sgsdl2_attach_indices(cube1, indices, 36);
	sgsdl2_attach_indices(cube2, indices, 36);
	sgsdl2_attach_indices(cube3, indices, 36);
	
	sgsdl2_attach_normals(cube1, normals, 24, 3);
	sgsdl2_attach_normals(cube2, normals, 24, 3);
	sgsdl2_attach_normals(cube3, normals, 24, 3);
	
	sgsdl2_attach_colors(cube2, colors, 24, 3);
	sgsdl2_attach_texcoords(cube3, texCoords, 24);
	
	cube1->material = material1;
	cube2->material = material2;
	cube3->material = material3;
	
	return light;
}

void test_graphics3d()
{
	cout << "Drawing 3D on screen" << endl;
	
	sg_drawing_surface surface = _sg_functions->graphics.open_window("3D Graphics", 800, 600);
	sgsdl2_print_opengl_version();
	
	// These could be moved into one command (ie load scene from bundle)
	sgsdl2_scene *scene = sgsdl2_make_scene();
	scene->surface = &surface;
	sgsdl2_prepare_for_3d();
	sgsdl2_scene_element *rotate_element = build_scene(scene);
	
	float radians = 0;
	float rotateSpeed = (float) (M_PI * 0.01);
	
	rotate_element->location = {{15 * sinf(radians), 0, 15 * cosf(radians)}};
	rotate_element->direction = negateVector3f(rotate_element->location);
	
	while (radians < 20 * M_PI)
	{
		sgsdl2_clear_opengl_window(&surface, {1, 1, 1, 1});
		sgsdl2_render_scene(scene);
		sgsdl2_check_opengl_error();
		sgsdl2_update_opengl_render(&surface);
		
		radians += rotateSpeed;
		rotate_element->location = {{15 * sinf(radians), 0, 15 * cosf(radians)}};
//		rotate_element->direction = negateVector3f(main_camera->location);
		_sg_functions->utils.delay(1000 / 60);
	}
	
	sgsdl2_delete_scene(scene);
	clean_up_shaders();
	cout << "End 3D test" << endl;
}
