//
//  SGSDK2Audio.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 28/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "SGSDL2Audio.h"
#include "SDL.h"
#include "SDL_Mixer.h"
#include "sgBackendUtils.h"

void sgsdl2_init_audio()
{
    if ( Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 1024 ) < 0 )
    {
        set_error_state("Unable to load audio. Mix_OpenAudio failed.");
        return;
    }
    Mix_AllocateChannels(50);
}

void sgsdl2_load_audio_fns(sg_interface *functions)
{
    
}