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
}

void sgsdl2_open_audio()
{
    if ( Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 1024 ) < 0 )
    {
        set_error_state("Unable to load audio. Mix_OpenAudio failed.");
        return;
    }
    
    Mix_AllocateChannels(64);
}

void sgsdl2_close_audio()
{
    Mix_CloseAudio();
}

sg_sound_data sgsdl2_load_sound_effect(const char * filename, sg_sound_kind kind)
{
    sg_sound_data result = { SGSD_UNKNOWN, NULL } ;
    
    result.kind = kind;
    
    switch (kind)
    {
        case SGSD_SOUND_EFFECT:
        {
            result.data = Mix_LoadWAV(filename);
            break;
        }
        default:
            break;
    }
    
    return result;
}

void sgsdl2_load_audio_fns(sg_interface *functions)
{
    functions->audio.open_audio = & sgsdl2_open_audio;
    functions->audio.close_audio = & sgsdl2_close_audio;
    functions->audio.load_sound_effect = & sgsdl2_load_sound_effect;
}