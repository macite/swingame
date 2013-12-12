//
//  SGDL2Utils.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "SGSDL2Utils.h"

#ifdef __linux__
#include <SDL2/SDL.h>
#else
#include <SDL.h>
#endif

void sgsdl2_delay(unsigned int ms)
{
    SDL_Delay(ms);
}

unsigned int sgsdl2_get_ticks()
{
    return SDL_GetTicks();
}

void sgsdl2_load_util_fns(sg_interface *functions)
{
    functions->utils.delay = &sgsdl2_delay;
    functions->utils.get_ticks = &sgsdl2_get_ticks;
}
