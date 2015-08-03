//
//  SGSDL2Camera.cpp
//  sgsdl2
//
//  Created by James Ferguson on 30/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#include "SGSDL2Camera.h"
#include "SGSDL2Utilities.h"


sgsdl2_camera* sgsdl2_create_camera(sgsdl2_node *parent, sgsdl2_camera_type camera_type)
{
	// Parent must belong to a scene
	if (!parent->root)
	{
		sgsdl2_print_error(ERR_NODE_NO_ROOT);
		return nullptr;
	}
	
	if (parent->camera)
	{
		sgsdl2_print_error(ERR_CAMERA_ALREADY_EXISTS);
		return nullptr;
	}
	
	sgsdl2_camera *result = new sgsdl2_camera();
	result->parent = parent;
	parent->camera = result;
	result->type = camera_type;
	return result;
}

sgsdl2_camera* sgsdl2_create_perspective_camera(sgsdl2_node *parent)
{
	sgsdl2_camera *cam = sgsdl2_create_camera(parent, sgsdl2_camera_type::PERSPECTIVE);
	cam->near = 1;
	cam->far = 100;
	sgsdl2_set_proj_dist(cam, 100);
	return cam;
}

sgsdl2_camera* sgsdl2_create_temporary_camera(sgsdl2_node *parent, sgsdl2_camera_type camera_type)
{
	// Parent must belong to a scene
	if (!parent->root)
	{
		sgsdl2_print_error(ERR_NODE_NO_ROOT);
		return nullptr;
	}
	
	sgsdl2_camera *result = new sgsdl2_camera();
	result->parent = parent;
	result->type = camera_type;
	return result;
}

void sgsdl2_set_proj_dist(sgsdl2_camera *cam, float dist)
{
	cam->left = -dist;
	cam->right = dist;
	cam->top = dist;
	cam->bottom = -dist;
}

mat4 sgsdl2_get_proj_transform(sgsdl2_camera *cam)
{
	if (cam->type == sgsdl2_camera_type::ORTHOGONAL)
	{
		return ortho(cam->left, cam->right, cam->bottom, cam->top, cam->near, cam->far);
	}
	else
	{
		// When the camera is projective, the distances are measured on the back side of the frustum
		//            *  -
		//           /|  |
		//          / | dist
		//         /  |  |
		// cam--> *---*  -
		return frustum(cam->left * cam->near / cam->far,
					   cam->right * cam->near / cam->far,
					   cam->bottom * cam->near / cam->far,
					   cam->top * cam->near / cam->far,
					   cam->near, cam->far);
	}
}

bool sgsdl2_is_active_camera(sgsdl2_camera *cam)
{
	return cam->parent && cam->parent->root && cam->parent->root->active_camera == cam;
}

void sgsdl2_delete_camera(sgsdl2_camera *camera)
{
	delete camera;
}

