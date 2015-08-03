//
//  SGSDL2Light.cpp
//  sgsdl2
//
//  Created by James Ferguson on 6/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Light.h"
#include "SGSDL2Node.h"
#include "SGSDL2Camera.h"
#include "SGSDL2Utilities.h"


sgsdl2_light* sgsdl2_create_light(sgsdl2_node *parent)
{
	// Parent must belong to a scene
	if (!parent->root)
	{
		sgsdl2_print_error(ERR_NODE_NO_ROOT);
		return nullptr;
	}
	
	if (parent->light)
	{
		// TODO emit warning
		return nullptr;
	}
	
	sgsdl2_light *light = new sgsdl2_light();
	light->parent = parent;
	parent->light = light;
	parent->root->lights.push_back(light);
	return light;
}

sgsdl2_light* sgsdl2_create_spot_light(sgsdl2_node *parent)
{
	sgsdl2_light *light = sgsdl2_create_light(parent);
	light->type = sgsdl2_light_type::SPOT;
	light->color = {1, 1, 1, 1};
	light->intensity = 1;
	light->ambient_coefficient = 0.1f;
	light->attenuation_cutoff = 100;
	light->radius = 1;
	light->shadow_type = sgsdl2_shadowing_type::DYNAMIC;
	light->cos_inner_cone = cosf((float) M_PI * (10.0f / 180.0f));
	light->cos_outer_cone = cosf((float) M_PI * (45.0f / 180.0f));
	
	// Set the directional light options even though they aren't used.
	light->width = 400;
	light->height = 400;
	
	return light;
}

void sgsdl2_set_camera_frustum(sgsdl2_light *light, sgsdl2_camera *camera)
{
	if (light->type == sgsdl2_light_type::DIRECTIONAL)
	{
		camera->type = sgsdl2_camera_type::ORTHOGONAL;
		camera->left = -light->width / 2;
		camera->right = light->width / 2;
		camera->top = light->height / 2;
		camera->bottom = -light->height / 2;
	}
	else if (light->type == sgsdl2_light_type::SPOT)
	{
		camera->type = sgsdl2_camera_type::PERSPECTIVE;
		float dist = light->radius * tanf(acosf(light->cos_outer_cone));
		sgsdl2_set_proj_dist(camera, dist);
	}
	camera->near = light->radius;
	camera->far = light->attenuation_cutoff;
}

mat4 sgsdl2_get_proj_transform(sgsdl2_light *light)
{
	mat4 trans;
	if (light->type == sgsdl2_light_type::DIRECTIONAL)
	{
		trans = ortho(-light->width / 2 ,
					  light->width / 2,
					  light->height / 2,
					  -light->height / 2,
					  light->radius,
					  light->attenuation_cutoff);
	}
	else
	{
		float dist = light->radius * tanf(acosf(light->cos_outer_cone));
		trans = frustum(-dist, dist, dist, -dist, light->radius, light->attenuation_cutoff);
	}

	return trans;
}

mat4 sgsdl2_normalize_shadow_transform(mat4 trans)
{
	return mat4(0.5f, 0.0f, 0.0f, 0.0f,
				0.0f, 0.5f, 0.0f, 0.0f,
				0.0f, 0.0f, 0.5f, 0.0f,
				0.5f, 0.5f, 0.5f, 1.0f) * trans;
}

mat4 sgsdl2_get_shadow_transform(sgsdl2_light *light)
{
	return sgsdl2_get_proj_transform(light) * sgsdl2_get_view_transform(light->parent);
}

void sgsdl2_allocate_shadow_map_location(sgsdl2_node *node)
{
	// Node must have an assigned light
	if (!node->light)
	{
		sgsdl2_print_error(ERR_MISSING_LIGHT);
		return;
	}
	
	// TODO macro name should be replaced with the value from the scene when implemented
	for (int i = 0; i < MAX_LIGHTING_COUNT; i++)
	{
		if (!node->root->occupied_shadow_map_levels[i])
		{
			node->light->shadow_map_level = i;
			node->root->occupied_shadow_map_levels[i] = true;
			break;
		}
	}
}













