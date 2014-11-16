//
//  SGSDL2Core.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifdef __linux__
#include <SDL2/SDL.h>
#else
#include <SDL.h>
#endif

#include "SGSDL2Core.h"

#include "sgInterfaces.h"
#include "sgBackendUtils.h"

#include "SGSDL2Graphics.h"
#include "SGSDL2Audio.h"
#include "SGSDL2Utils.h"
#include "SGSDL2Input.h"
#include "SGSDL2Text.h"
#include "SGSDL2Network.h"


using namespace std;

void sgsdk_setup_displays();

void init_sgsdl2()
{
    static bool done_init = false;
    if ( done_init ) return;
    done_init = true;
    
    clear_error();
    
    if ( -1 == SDL_Init( SDL_INIT_EVERYTHING ) )
    {
        // fatal error so...
        // no other functions can now be called
        clear_functions();
        
        set_error_state(SDL_GetError());
        return;
    }

    SDL_SetHint(SDL_HINT_RENDER_DRIVER, "opengl");
    
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 0);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 0);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE,    8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,  8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,   8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE,  8);
    
    SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL,  1);
    
    sgsdl2_init_audio();
    sgsdl2_init_text();
    
    sgsdk_setup_displays();
}

void sgsdk2_setup_display(int idx, sg_display &disp)
{
    SDL_DisplayMode mode;
    SDL_Rect rect;
    
    disp.name = SDL_GetDisplayName(idx);
    
    SDL_GetCurrentDisplayMode(idx, &mode);
    disp.width = mode.w;
    disp.height = mode.h;
    disp.format = mode.format;
    disp.refresh_rate = mode.refresh_rate;
    
    SDL_GetDisplayBounds(idx, &rect);
    disp.x = rect.x;
    disp.y = rect.y;
    
    disp.num_modes = 0;
    disp.modes = NULL;
    int max_modes = SDL_GetNumDisplayModes(idx);
    if ( max_modes < 1 ) return;
    bool add;
    
    for (int i = 0; i < max_modes; i++)
    {
        SDL_GetDisplayMode(idx, i, &mode);
        add = true;
        
        for ( unsigned int m = 0; m < disp.num_modes; m++)
        {
            if ( disp.modes[m].width == mode.w && disp.modes[m].height == mode.h )
            {
                add = false;
                break;
            }
        }
        
        if ( add )
        {
            disp.num_modes++;
            sg_mode * new_modes = (sg_mode*) realloc(disp.modes, disp.num_modes * sizeof(sg_mode));
            if ( new_modes == NULL )
            {
                set_error_state("Out of memory loading video modes.");
                free(disp.modes);
                disp.modes = NULL;
                disp.num_modes = 0;
                return; //TODO: add error!
            }
            disp.modes = new_modes;
            disp.modes[disp.num_modes - 1].width = mode.w;
            disp.modes[disp.num_modes - 1].height = mode.h;
            disp.modes[disp.num_modes - 1].format = mode.format;
            disp.modes[disp.num_modes - 1].refresh_rate = mode.refresh_rate;
        }
    }
}

void sgsdk_setup_displays()
{
    int num_displays = SDL_GetNumVideoDisplays();
    // 0 = zero displays
    // less than 0 = use SDL_GetError() for failure
    if (num_displays <= 0) {
      exit(-1);
    }
    _sgsdk_system_data.num_displays = static_cast<unsigned int>(num_displays);
    _sgsdk_system_data.displays = (sg_display *)malloc(sizeof(sg_display) * _sgsdk_system_data.num_displays);
    
    for (unsigned int i = 0; i < _sgsdk_system_data.num_displays; i++)
    {
        sgsdk2_setup_display(static_cast<int>(i), _sgsdk_system_data.displays[i]);
    }
}

sg_system_data * sgsdl2_read_system_data()
{
    return &_sgsdk_system_data;
}

void sgsdl2_finalise()
{
    sgsdl2_finalise_graphics();

    for (unsigned int i = 0; i < _sgsdk_system_data.num_displays; i++)
    {
        free(_sgsdk_system_data.displays[i].modes);
    }
    
    free(_sgsdk_system_data.displays);
    
    sgsdl2_finalize_text();

    SDL_Quit();
}

extern "C"
{
    
sg_interface * sg_load(sg_input_callbacks callbacks)
{
    clear_functions();
    
    _functions.input_callbacks = callbacks;
    
    _functions.init = &init_sgsdl2;
    _functions.read_system_data = &sgsdl2_read_system_data;
    _functions.finalise = &sgsdl2_finalise;
    
    sgsdl2_load_audio_fns(&_functions);
    sgsdl2_load_graphics_fns(&_functions);
    sgsdl2_load_image_fns(&_functions);
    sgsdl2_load_input_fns(&_functions);
    sgsdl2_load_text_fns(&_functions);
    sgsdl2_load_util_fns(&_functions);
    sgsdl2_load_network_fns(&_functions);
    
    return &_functions;
}

}
