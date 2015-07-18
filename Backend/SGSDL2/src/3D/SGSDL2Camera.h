//
//  SGSDL2Camera.h
//  sgsdl2
//
//  Created by James Ferguson on 30/06/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Camera__
#define __sgsdl2__SGSDL2Camera__

#include <stdio.h>
#include "SGSDL2Types.h"


sgsdl2_camera* sgsdl2_create_camera(sgsdl2_node *parent, sgsdl2_camera_type camera_type = sgsdl2_camera_type::PERSPECTIVE);

// A temporary camera does not officially belong in the scene.
// It is used for one off render passes.
sgsdl2_camera* sgsdl2_create_temporary_camera(sgsdl2_node *parent, sgsdl2_camera_type camera_type);

void sgsdl2_set_proj_dist(sgsdl2_camera *cam, float dist);

mat4 sgsdl2_get_proj_transform(sgsdl2_camera *cam);

bool sgsdl2_is_active_camera(sgsdl2_node *cam_node);

void sgsdl2_delete_camera(sgsdl2_camera *camera);

#endif /* defined(__sgsdl2__SGSDL2Camera__) */
