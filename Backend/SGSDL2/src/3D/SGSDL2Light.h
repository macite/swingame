//
//  SGSDL2Light.h
//  sgsdl2
//
//  Created by James Ferguson on 6/07/2015.
//  Copyright (c) 2015 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Light__
#define __sgsdl2__SGSDL2Light__

#include "SGSDL2Types.h"


sgsdl2_light* sgsdl2_create_light(sgsdl2_node *parent);

sgsdl2_light* sgsdl2_create_spot_light(sgsdl2_node *parent);

// Mimics the view of a light onto a camera.
void sgsdl2_set_camera_frustum(sgsdl2_light *light, sgsdl2_camera *camera);

mat4 sgsdl2_get_proj_transform(sgsdl2_light *light);

mat4 sgsdl2_normalize_shadow_transform(mat4 trans);

mat4 sgsdl2_get_shadow_transform(sgsdl2_light *light);

void sgsdl2_allocate_shadow_map_location(sgsdl2_node *node);

#endif /* defined(__sgsdl2__SGSDL2Light__) */
