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

Mix_Chunk * _sgsdl2_sound_channels[64];

void sgsdl2_init_audio()
{
    Mix_Init(MIX_INIT_OGG);
}

void sgsdl2_open_audio()
{
    if ( Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 4096 ) < 0 )
    {
        set_error_state("Unable to load audio. Mix_OpenAudio failed.");
        return;
    }
    int audio_rate;
    Uint16 audio_format;
    int audio_channels;

    Mix_QuerySpec(&audio_rate, &audio_format, &audio_channels);
    printf("Opened audio at %d Hz %d bit %s\n", audio_rate,
           (audio_format&0xFF),
           (audio_channels > 2) ? "surround" :
           (audio_channels > 1) ? "stereo" : "mono");

    
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
        case SGSD_MUSIC:
        {
            result.data = Mix_LoadMUS(filename);
            break;
        }
        default:
            break;
    }
    
    return result;
}

void sgsdl2_play_sound_effect(sg_sound_data * sound, int loops, float volume)
{
    if ( (!sound) || (!sound->data) ) return;
    
    switch (sound->kind)
    {
        case SGSD_SOUND_EFFECT:
        {
            Mix_Chunk *effect = (Mix_Chunk*) sound->data;
            int channel = Mix_PlayChannel( -1, effect, loops);
            if (channel >= 0 && channel < 64)
            {
                Mix_Volume(channel, (int)(volume * 128));
                _sgsdl2_sound_channels[channel] = effect;   // record which channel is playing the effect
            }
            break;
        }
        case SGSD_MUSIC:
        {
            Mix_PlayMusic((Mix_Music *)sound->data, loops);
            Mix_VolumeMusic((int)volume * 128);
            break;
        }
        default:
            break;
    }
}

void sgsdl2_load_audio_fns(sg_interface *functions)
{
    functions->audio.open_audio = & sgsdl2_open_audio;
    functions->audio.close_audio = & sgsdl2_close_audio;
    functions->audio.load_sound_effect = & sgsdl2_load_sound_effect;
    functions->audio.play_sound_effect = & sgsdl2_play_sound_effect;
}

