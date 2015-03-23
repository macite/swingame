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

#define SHAD_VERTEX_COLOR_VERT_PATH "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/src/shaders/vertexColorVert.glsl"
#define SHAD_VERTEX_COLOR_FRAG_PATH "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/src/shaders/vertexColorFrag.glsl"
#define SHAD_SOLID_COLOR_VERT_PATH "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/src/shaders/solidColorVert.glsl"
#define SHAD_SOLID_COLOR_FRAG_PATH "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/src/shaders/solidColorFrag.glsl"
#define SHAD_TEXTURE_VERT_PATH "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/src/shaders/textureVert.glsl"
#define SHAD_TEXTURE_FRAG_PATH "/Users/jamesferguson/Documents/Coding/Swingame/Backend/SGSDL2/src/shaders/textureFrag.glsl"

#define TEXTURE_PATH "/Users/jamesferguson/Documents/Coding/Swingame/Backend/Test/Resources/sample.png"


using namespace std;
extern sg_interface *_sg_functions;


GLuint program;
struct ShaderPrograms
{
	vector<GLuint> shaders;
	GLuint solidColor, vertexColor, texture;
};

ShaderPrograms compiledShaders;

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
	compiledShaders.shaders.resize(6, 0);
	
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
	
	scene->shaders.push_back(compiledShaders.solidColor);
	scene->shaders.push_back(compiledShaders.texture);
	scene->shaders.push_back(compiledShaders.vertexColor);
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

sgsdl2_camera* build_scene(sgsdl2_scene *scene)
{
	sgsdl2_geometry *cube1 = sgsdl2_make_geometry({{0, 0, 0}},
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_geometry *cube2 = sgsdl2_make_geometry({{5, 0, 0}},
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_geometry *cube3 = sgsdl2_make_geometry({{-5, 0, 0}},
												  {{0, 0, -1}},
												  {{0, 1, 0}});
	sgsdl2_camera *camera = sgsdl2_make_camera({{0, 5, 15}},
											   {{0, -0.7, -1}},
											   {{0, 1, 0}});
	sgsdl2_texture *cat_texture = sgsdl2_make_texture();
	
	sgsdl2_add_element_to_root(scene, cube1);
	sgsdl2_add_element_to_root(scene, cube2);
	sgsdl2_add_element_to_root(scene, cube3);
	sgsdl2_add_element_to_root(scene, camera);
	scene->active_camera = camera;
	
	sgsdl2_attach_vertices(cube1, vertices, 24, 3);
	sgsdl2_attach_vertices(cube2, vertices, 24, 3);
	sgsdl2_attach_vertices(cube3, vertices, 24, 3);
	
	sgsdl2_attach_indices(cube1, indices, 36);
	sgsdl2_attach_indices(cube2, indices, 36);
	sgsdl2_attach_indices(cube3, indices, 36);
	
	sgsdl2_attach_colors(cube2, colors, 24, 3);
	
	sgsdl2_attach_texture_image(cat_texture, TEXTURE_PATH, GL_RGBA);
	sgsdl2_generate_texture_mipmaps(cat_texture);
	cube3->texture = cat_texture;
	sgsdl2_attach_texcoords(cube3, texCoords, 24);
	
	cube1->color = {1, 0, 0, 1};
	cube1->render_solid_color = true;
	
//	sgsdl2_attach_texcoords(cube1, texCoords, 24);
//	sgsdl2_attach_colors(cube2, colors, 24, 3);
//	sgsdl2_attach_colors(cube2, colors, 24, 3);
	
	compile_default_shaders(scene);
	return camera;
}

void draw_test(sg_drawing_surface *surface)
{
//	if ( ! surface || ! surface->_data ) return;
//	cout << "Starting to draw a cube" << endl;
//
//	float radians = 0;
//	float rotateSpeed = (float) (M_PI * 0.01);
//	
//	sgsdl2_scene *scene = build_scene();
//	
//	Matrix4f modelCentreMat = makeMatrix4fIdentity();
//	Matrix4f modelLeftMat = makeMatrix4fFromReverseLookAt(3, 0, 0, 0, 0, 0, 0, 1, 0);
//	Matrix4f modelRightMat = makeMatrix4fFromReverseLookAt(-3, 0, 0, 0, 0, 0, 0, 1, 0);
//	Matrix4f viewMat = makeMatrix4fFromLookAt(0, 0, 10, 0, 0, 0, 0, 1, 0);
//	Matrix4f projMat = makeMatrix4fFromProjection(45, 1, 1, 100);
//	
//	sgsdl2_prepare_for_3d();
//	compile_default_shaders();
//	
//	sgsdl2_geometry geometry = sgsdl2_make_geometry();
//	sgsdl2_attach_vertices(geometry, &vertices[0], 24, 3);
//	sgsdl2_attach_indices(geometry, &indices[0], 36);
//	sgsdl2_attach_colors(geometry, &colors[0], 24, 3);
//	sgsdl2_attach_texcoords(geometry, &texCoords[0], 16);
//	
//	sgsdl2_texture texture = sgsdl2_make_texture();
//	sgsdl2_change_texture_filtering(texture);
//	sgsdl2_change_texture_wrapping(texture);
//	sgsdl2_generate_texture_mipmaps(texture);
//	sgsdl2_attach_texture_image(texture, TEXTURE_PATH, GL_RGBA);
//	
//	while (radians < 10 * M_PI)
//	{
//		sgsdl2_clear_opengl_window(surface, {0, 0, 0, 1});
//
//		sgsdl2_render_geometry(geometry, compiledShaders.vertexColor, modelCentreMat.m, viewMat.m, projMat.m);
//		sgsdl2_solid_render_geometry(geometry, {1, 0, 0, 1}, compiledShaders.solidColor, modelLeftMat.m, viewMat.m, projMat.m);;
//		sgsdl2_texture_render_geometry(geometry, texture, compiledShaders.texture, modelRightMat.m, viewMat.m, projMat.m);
//		
//		sgsdl2_check_opengl_error();
//		sgsdl2_update_opengl_render(surface);
//		
//		radians += rotateSpeed;
//		viewMat = makeMatrix4fFromLookAt(10 * sinf(radians), 5, 10 * cosf(radians), 0, 0, 0, 0, 1, 0);
//		_sg_functions->utils.delay(1000 / 60);
//	}
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
	sgsdl2_camera *main_camera = build_scene(scene);
	
	float radians = 0;
	float rotateSpeed = (float) (M_PI * 0.01);
	
	main_camera->location = {{15 * sinf(radians), 5, 15 * cosf(radians)}};
	main_camera->direction = negateVector3f(main_camera->location);
	
	while (radians < 10 * M_PI)
	{
		sgsdl2_clear_opengl_window(&surface, {1, 1, 1, 1});
		sgsdl2_render_scene(scene);
		sgsdl2_check_opengl_error();
		sgsdl2_update_opengl_render(&surface);
		
		radians += rotateSpeed;
//		main_camera->field_of_view = (float) M_PI_4 + (float) (M_PI / 18 * sinf(radians));
		main_camera->location = {{15 * sinf(radians), 5, 15 * cosf(radians)}};
		main_camera->direction = negateVector3f(main_camera->location);
		_sg_functions->utils.delay(1000 / 60);
	}
	
	sgsdl2_delete_scene(scene);
	clean_up_shaders();
	cout << "End 3D test" << endl;
}
