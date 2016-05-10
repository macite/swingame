//
//  SGSDK2Audio.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 28/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifdef __linux__
#include <SDL2/SDL.h>
#include <SDL2/SDL_mixer.h>
#else
#include <SDL.h>
#include <SDL_mixer.h>
#endif

#include "SGSDL2Audio.h"
#include "sgBackendUtils.h"
#include "SGSDL2Core.h"

#define SG_MAX_CHANNELS 64

static Mix_Chunk * _sgsdl2_sound_channels[SG_MAX_CHANNELS];
static sg_sound_data * _current_music;


void sgsdl2_init_audio()
{
    Mix_Init(~0);
}

void sgsdl2_open_audio()
{
    internal_sgsdl2_init();
    if ( Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 4096 ) < 0 )
    {
        set_error_state("Unable to load audio. Mix_OpenAudio failed.");
        return;
    }

    Uint16 format;
    Mix_QuerySpec(&_sgsdk_system_data.audio_specs.audio_rate, &format, &_sgsdk_system_data.audio_specs.audio_channels);
    _sgsdk_system_data.audio_specs.times_opened++;
    _sgsdk_system_data.audio_specs.audio_format = format;

    Mix_AllocateChannels(SG_MAX_CHANNELS);
}

void sgsdl2_close_audio()
{
    Mix_CloseAudio();
    _sgsdk_system_data.audio_specs.times_opened--;
    if ( 0 == _sgsdk_system_data.audio_specs.times_opened )
    {
        sg_audiospec empty = { 0, 0, 0, 0 };
        _sgsdk_system_data.audio_specs = empty;
    }
}

int sgsdl2_get_channel(sg_sound_data *sound)
{
    if ( (!sound) || (!sound->_data) ) return -1;

    for (int i = 0; i < SG_MAX_CHANNELS; i++)
    {
        if ( _sgsdl2_sound_channels[i] == sound->_data && Mix_Playing(i) )
        {
            return i;
        }
    }
    return -1;
}



sg_sound_data sgsdl2_load_sound_data(const char * filename, sg_sound_kind kind)
{
    internal_sgsdl2_init();
    sg_sound_data result = { SGSD_UNKNOWN, NULL } ;

    result.kind = kind;

    switch (kind)
    {
        case SGSD_SOUND_EFFECT:
        {
            result._data = Mix_LoadWAV(filename);
            break;
        }
        case SGSD_MUSIC:
        {
            result._data = Mix_LoadMUS(filename);
            break;
        }

        case SGSD_UNKNOWN:
            break;
    }

    return result;
}

void sgsdl2_close_sound_data(sg_sound_data * sound )
{
    if ( (!sound) || (!sound->_data) ) return;

    switch (sound->kind)
    {
        case SGSD_MUSIC:
            Mix_FreeMusic(static_cast<Mix_Music *>(sound->_data));
            break;

        case SGSD_SOUND_EFFECT:
            if (_current_music == sound)
            {
                _current_music = NULL;
            }
            Mix_FreeChunk(static_cast<Mix_Chunk *>(sound->_data));
            break;

        case SGSD_UNKNOWN:
            break;
    }

    sound->kind = SGSD_UNKNOWN;
    sound->_data = NULL;
}

void sgsdl2_play_sound(sg_sound_data * sound, int loops, float volume)
{
    if ( (!sound) || (!sound->_data) ) return;

    switch (sound->kind)
    {
        case SGSD_SOUND_EFFECT:
        {
            Mix_Chunk *effect = static_cast<Mix_Chunk *>(sound->_data);
            int channel = Mix_PlayChannel( -1, effect, loops);
            if (channel >= 0 && channel < SG_MAX_CHANNELS)
            {
                Mix_Volume(channel, static_cast<int>(volume * MIX_MAX_VOLUME));
                _sgsdl2_sound_channels[channel] = effect;   // record which channel is playing the effect
            }
            break;
        }
        case SGSD_MUSIC:
        {
            Mix_PlayMusic(static_cast<Mix_Music *>(sound->_data), loops);
            Mix_VolumeMusic(static_cast<int>(MIX_MAX_VOLUME * volume));
            _current_music = sound;
            break;
        }
        case SGSD_UNKNOWN:
            break;
    }
}

float sgsdl2_sound_playing(sg_sound_data * sound)
{
    if ( ! sound ) {
        return 0.0f;
    }

    switch (sound->kind)
    {
        case SGSD_SOUND_EFFECT:
        {
            int idx = sgsdl2_get_channel(sound);
            return ( idx >= 0 && idx < SG_MAX_CHANNELS ? 1.0f : 0.0f );
        }
        case SGSD_MUSIC:
        {
            if ( _current_music == sound && Mix_PlayingMusic() ) return 1.0f;
            break;
        }

        case SGSD_UNKNOWN:
            break;
    }

    return 0.0f;
}

void sgsdl2_fade_in(sg_sound_data *sound, int loops, int ms)
{
    if ( !sound ) return;

    switch (sound->kind)
    {
        case SGSD_SOUND_EFFECT:
        {
            int channel;
            channel = Mix_FadeInChannel(-1, static_cast<Mix_Chunk *>(sound->_data), loops, ms);
            if ( channel >= 0 && channel < SG_MAX_CHANNELS )
            {
                _sgsdl2_sound_channels[channel] = static_cast<Mix_Chunk *>(sound->_data);
            }
            break;
        }

        case SGSD_MUSIC:
        {
            Mix_FadeInMusic(static_cast<Mix_Music *>(sound->_data), loops, ms);
            _current_music = sound;
            break;
        }

        case SGSD_UNKNOWN:
            break;
    }
}

void sgsdl2_fade_out(sg_sound_data *sound, int ms)
{
    if ( !sound ) return;

    switch (sound->kind)
    {
        case SGSD_SOUND_EFFECT:
        {
            int channel = sgsdl2_get_channel(sound);
            Mix_FadeOutChannel(channel, ms);
            break;
        }

        case SGSD_MUSIC:
        {
            if ( _current_music == sound )
            {
                Mix_FadeOutMusic(ms);
                _current_music = NULL;
            }
            break;
        }

        case SGSD_UNKNOWN:
            break;
    }
}

void sgsdl2_fade_all_sound_effects_out(int ms)
{
    internal_sgsdl2_init();
    Mix_FadeOutChannel(-1, ms);
}

void sgsdl2_fade_music_out(int ms)
{
    internal_sgsdl2_init();
    Mix_FadeOutMusic(ms);
    _current_music = NULL;
}

#include <iostream>

void sgsdl2_set_music_vol(float vol)
{
    internal_sgsdl2_init();
    Mix_VolumeMusic( static_cast<int>(MIX_MAX_VOLUME * vol) );
}

float sgsdl2_music_vol()
{
    internal_sgsdl2_init();
    return Mix_VolumeMusic(-1) / static_cast<float>(MIX_MAX_VOLUME);
}

float sgsdl2_sound_volume(sg_sound_data *sound)
{
    if ( ! sound ) return 0.0f;

    switch (sound->kind)
    {
        case SGSD_MUSIC:
            if ( _current_music == sound ) return sgsdl2_music_vol();
            break;
        case SGSD_SOUND_EFFECT:
            return Mix_VolumeChunk(static_cast<Mix_Chunk *>(sound->_data), -1) / static_cast<float>(MIX_MAX_VOLUME);
        case SGSD_UNKNOWN:
            break;
    }

    return 0.0f;
}

void sgsdl2_set_sound_volume(sg_sound_data *sound, float vol)
{
    if ( !sound ) return;

    switch (sound->kind)
    {
        case SGSD_MUSIC:
            if ( _current_music == sound )
                sgsdl2_set_music_vol(vol);
            break;

        case SGSD_SOUND_EFFECT:
            Mix_VolumeChunk(static_cast<Mix_Chunk *>(sound->_data), static_cast<int>(vol * MIX_MAX_VOLUME));
            break;

        case SGSD_UNKNOWN:
            break;
    }
}

void sgsdl2_pause_music()
{
    internal_sgsdl2_init();
    Mix_PauseMusic();
}

void sgsdl2_resume_music()
{
    internal_sgsdl2_init();
    if ( Mix_PausedMusic() )
    {
        Mix_ResumeMusic();
    }
}

void sgsdl2_stop_music()
{
    internal_sgsdl2_init();
    Mix_HaltMusic();
}

void sgsdl2_stop_sound(sg_sound_data *sound)
{
    if ( ! sound ) return;

    switch (sound->kind)
    {
        case SGSD_MUSIC:
            if ( _current_music == sound ) sgsdl2_stop_music();
            break;

        case SGSD_SOUND_EFFECT:
        {
            for (int i = 0; i < SG_MAX_CHANNELS; i++)
            {
                if ( _sgsdl2_sound_channels[i] == sound->_data )
                {
                    Mix_HaltChannel(i);
                }
            }
            break;
        }

        case SGSD_UNKNOWN:
            break;
    }
}

float sgsdl2_music_playing()
{
    internal_sgsdl2_init();
    if ( Mix_PlayingMusic() ) {
        return -1.0f;
    }
    else
    {
        return 0.0f;
    }
}

sg_sound_data * sgsdl2_current_music()
{
    return _current_music;
}



void sgsdl2_load_audio_fns(sg_interface *functions)
{
    _current_music = NULL;

    functions->audio.open_audio = & sgsdl2_open_audio;
    functions->audio.close_audio = & sgsdl2_close_audio;
    functions->audio.load_sound_data = & sgsdl2_load_sound_data;
    functions->audio.play_sound = & sgsdl2_play_sound;
    functions->audio.close_sound_data = & sgsdl2_close_sound_data;
    functions->audio.sound_playing = &sgsdl2_sound_playing;
    functions->audio.fade_in = &sgsdl2_fade_in;
    functions->audio.fade_out = &sgsdl2_fade_out;
    functions->audio.fade_music_out = &sgsdl2_fade_music_out;
    functions->audio.fade_all_sound_effects_out = &sgsdl2_fade_all_sound_effects_out;
    functions->audio.set_music_vol = &sgsdl2_set_music_vol;
    functions->audio.music_vol = &sgsdl2_music_vol;
    functions->audio.sound_volume = &sgsdl2_sound_volume;
    functions->audio.set_sound_volume = &sgsdl2_set_sound_volume;
    functions->audio.pause_music =  & sgsdl2_pause_music;
    functions->audio.resume_music = & sgsdl2_resume_music;
    functions->audio.stop_music =   & sgsdl2_stop_music;
    functions->audio.stop_sound =   & sgsdl2_stop_sound;
    functions->audio.music_playing = & sgsdl2_music_playing;
    functions->audio.current_music = & sgsdl2_current_music;
}
