//
//  SGSDL2Core.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "SGSDL2Core.h"

#include "sgInterfaces.h"
#include "sgBackendUtils.h"

#include "SGSDL2Graphics.h"
#include "SGSDL2Utils.h"

using namespace std;

void init_sgsdk2()
{
    clear_error();
    
    if ( -1 == SDL_Init( SDL_INIT_EVERYTHING ) )
    {
        // fatal error so...
        // no other functions can now be called
        clear_functions();
        
        set_error_state(SDL_GetError());
        return;
    }

    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 0);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 0);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
    
    SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL,  1);
}

extern "C"
{
    
sg_interface * sg_load()
{
    clear_functions();
    
    _functions.init = &init_sgsdk2;
    
    sgsdl2_load_graphics_fns(&_functions);
    sgsdl2_load_util_fns(&_functions);
    
    return &_functions;
}

}