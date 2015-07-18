//
//  SGSDL2Renderer.cpp
//  sgsdl2
//
//  Created by James Ferguson on 16/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Renderer.h"
#include "SGSDL2Camera.h"
#include "SGSDL2Shader.h"
#include "SGSDL2Scene.h"
#include "SGSDL2Light.h"
#include "SGSDL2Utilities.h"
#include "SGSDL2Node.h"


void sgsdl2_rerender_shadow_maps(sgsdl2_scene *scene, sgsdl2_renderer *renderer)
{
	// Generates a framebuffer to store render pass results.
	GLuint framebuffer = 0;
	glGenFramebuffers(1, &framebuffer);
	glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
	glDrawBuffer(GL_NONE); // No color buffer is drawn to.
	glViewport(0, 0, SHAD_MAP_SIZE, SHAD_MAP_SIZE);
	
	for (unsigned long i = 0; i < scene->lights.size(); i++)
	{
		sgsdl2_light *light = scene->lights[i];
		sgsdl2_node *light_node = light->parent;
		
		// Check if the light generates shadows
		if (light->shadow_type != sgsdl2_shadowing_type::NONE)
		{
			// Allocate a position for the shadow map if needed
			if (light->shadow_map_level == -1)
			{
				sgsdl2_allocate_shadow_map_location(light_node);
			}
			
			// Bind the texture to the framebuffer
			glFramebufferTextureLayer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, scene->shadow_map_array, 0, light->shadow_map_level);
			
			// Check the framebuffer is valid
			SGenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
			if (status == GL_FRAMEBUFFER_COMPLETE)
			{
				sgsdl2_camera *light_camera = sgsdl2_create_temporary_camera(light_node, sgsdl2_camera_type::ORTHOGONAL);
				sgsdl2_set_camera_frustum(light, light_camera);
				
				// TODO this may not be needed.
				glClear(GL_DEPTH_BUFFER_BIT);
				sgsdl2_render_options opts;
				opts.mode = sgsdl2_shader_mode::DEPTH;
				sgsdl2_perform_render_pass(renderer, light_camera, opts);
				
				sgsdl2_delete_camera(light_camera);
			}
		}
	}
	
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	glDrawBuffer(GL_BACK);
	glViewport(0, 0, scene->surface->width, scene->surface->height);
	glDeleteFramebuffers(1, &framebuffer);
	
	sgsdl2_check_opengl_error("recalculate_lights: ");
}

void sgsdl2_perform_render_pass(sgsdl2_renderer *renderer, sgsdl2_camera *camera, sgsdl2_render_options opts)
{
	renderer->opts = opts;
	sgsdl2_calculate_camera_state(camera, renderer);
	sgsdl2_render_nodes(renderer->scene->root_node, renderer);
	
	sgsdl2_check_opengl_error("render_pass: ");
}

void sgsdl2_render_nodes(sgsdl2_node *node, sgsdl2_renderer *renderer)
{
	sgsdl2_render_node(node, renderer);
	for (unsigned long i = 0; i < node->children.size(); i++)
	{
		sgsdl2_render_nodes(node->children[i], renderer);
	}
}

void sgsdl2_render_node(sgsdl2_node *node, sgsdl2_renderer *renderer)
{
	// Make sure the node belongs to a scene
	if (!node->root)
	{
		// TODO emit warning
		return;
	}
	
	// Check if there is geometry to render
	if (!node->mesh)
	{
		// Do not need to emit a warning here as it is legal to call this function with a node that does not have a mesh.
		return;
	}
	
	// Node must be active
	if (!node->mesh->is_active
		// Must be visible if it is a full render
		|| (renderer->opts.mode == sgsdl2_shader_mode::FULL && !node->mesh->is_visible)
		// Must cast shadows if it is a shadow pass.
		|| (renderer->opts.mode == sgsdl2_shader_mode::DEPTH && !node->mesh->casts_shadows))
		
	{
		// Mesh should not be rendered
		return;
	}
	
	sgsdl2_calculate_node_state(node, renderer);
	sgsdl2_render_mesh(node->mesh, renderer);
}

void sgsdl2_render_mesh(sgsdl2_mesh *mesh, sgsdl2_renderer *renderer)
{
	if (!mesh->material)
	{
		// TODO emit warning
		return;
	}
	
	sgsdl2_calculate_material_state(mesh->material, renderer);
	sgsdl2_complete_shader_interface(renderer->interface);
	SGuint shader = mesh->material->shader;
	
	// Pass all the data to the shader
	sgsdl2_bind_data_to_shader(shader, renderer->interface);
	
	// Perform the render
	glUseProgram(shader);
	glBindVertexArray(mesh->vao);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh->indices_handle);
	
	// Validate shader
	sgsdl2_validate_program(shader);
	
	glDrawElements(GL_TRIANGLES, mesh->indices_count, GL_UNSIGNED_SHORT, 0);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindVertexArray(0);
	glUseProgram(0);
	sgsdl2_check_opengl_error("render_mesh: ");
}

void sgsdl2_calculate_lighting_state(sgsdl2_scene *scene, sgsdl2_renderer *renderer)
{
	sgsdl2_lighting_state result;
	for (unsigned int i = 0; i < scene->lights.size(); i++)
	{
		// Attempt to validate the shadow map
		// TODO this could be tested. maybe there is a better way.
		if (scene->lights[i]->shadow_map_level >= 0)
		{
			sgsdl2_light *light = scene->lights[i];
			sgsdl2_node *light_node = light->parent;
			result.lights[i].location = light_node->location;
			result.lights[i].forward = vec3(sgsdl2_get_global_transform(light_node) * vec4(0, 0, -1, 0));
			// The transform is normalized between [0-1]
			result.lights[i].depth_transform = sgsdl2_normalize_shadow_transform(sgsdl2_get_shadow_transform(light));
			result.lights[i].intensities = sgsdl2_make_vec3_color(light->color) * light->intensity;
			result.lights[i].attenuation_cutoff = light->attenuation_cutoff;
			result.lights[i].ambient_coefficient = light->ambient_coefficient;
			result.lights[i].cos_inner_cone = light->cos_inner_cone;
			result.lights[i].cos_outer_cone = light->cos_outer_cone;
			result.lights[i].casts_shadows = light->shadow_type != sgsdl2_shadowing_type::NONE;
			result.lights[i].shadow_map_level = (SGuint) light->shadow_map_level; // shadow map level should never be negative at this stage.
			result.lights[i].type = static_cast<SGuint>(light->type);
		}
	}
	result.lighting_count = (int) scene->lights.size();
	result.shadow_map = scene->shadow_map_array;
	
	renderer->interface.lighting = result;
}

void sgsdl2_calculate_camera_state(sgsdl2_camera *camera, sgsdl2_renderer *renderer)
{
	sgsdl2_camera_state state;
	state.camera_loc = sgsdl2_get_global_location(camera->parent);
	state.proj = sgsdl2_get_proj_transform(camera);
	state.view = sgsdl2_get_view_transform(camera->parent);
	renderer->interface.camera = state;
}

void sgsdl2_calculate_node_state(sgsdl2_node *node, sgsdl2_renderer *renderer)
{
	sgsdl2_node_state result;
	result.model = sgsdl2_get_global_transform(node);
	result.normal_model = transpose(inverse(result.model));
	renderer->interface.node = result;
}

void sgsdl2_calculate_material_state(sgsdl2_material *mat, sgsdl2_renderer *renderer)
{
	sgsdl2_material_state result;
	result.diffuse_color = sgsdl2_make_vec3_color(mat->diffuse_color);
	result.diffuse_texture = (glIsTexture(mat->diffuse_texture)) ? mat->diffuse_texture : renderer->scene->white_texture;
	result.specular_color = sgsdl2_make_vec3_color(mat->specular_color);
	result.specular_exponent = mat->specular_exponent;
	result.specular_texture = (glIsTexture(mat->specular_texture)) ? mat->specular_texture : renderer->scene->white_texture;
	result.specular_intensity = mat->specular_intensity;
	result.normal_map = (glIsTexture(mat->normal_map)) ? mat->normal_map : renderer->scene->white_texture;
	renderer->interface.material = result;
}

void sgsdl2_complete_shader_interface(sgsdl2_shader_interface &interface)
{
	interface.mvp = interface.camera.proj * interface.camera.proj * interface.node.model;
}
