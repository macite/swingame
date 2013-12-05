//
//  SGSDL2Graphics.h
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Graphics__
#define __sgsdl2__SGSDL2Graphics__

#include <iostream>

#include "sgBackendTypes.h"
#include "sgInterfaces.h"
#include "SDL.h"


void sgsdl2_load_graphics_fns(sg_interface *functions);
void sgsdl2_load_image_fns(sg_interface *functions);
void sgsdl2_finalise_graphics();


#endif /* defined(__sgsdl2__SGSDL2Graphics__) */
