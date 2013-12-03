//
//  test_audio.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 2/12/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "test_audio.h"
#include "sgInterfaces.h"
#include <iostream>

using namespace std;

extern sg_interface * _sg_functions;
extern sg_system_data _sgsdk_system_data;

void test_audio()
{
    _sg_functions->audio.open_audio();
    
    printf("Opened audio at %d Hz %d bit %s\n", _sgsdk_system_data.audio_specs.audio_rate,
           (_sgsdk_system_data.audio_specs.audio_format&0xFF),
           (_sgsdk_system_data.audio_specs.audio_channels > 2) ? "surround" :
           (_sgsdk_system_data.audio_specs.audio_channels > 1) ? "stereo" : "mono");

    
    cout << "Audio open ... " << (_sg_functions->has_error ? "FAILED" : "SUCCESS") << endl;
    _sg_functions->utils.delay(500);
    
    sg_sound_data sound = _sg_functions->audio.load_sound_data("error.wav", SGSD_SOUND_EFFECT);
    
    cout << "Loaded sound effect WAV ... " << ( sound.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound(&sound, 0, 1.0f);
    
    _sg_functions->utils.delay(2000);

    sg_sound_data sound_ogg = _sg_functions->audio.load_sound_data("test.ogg", SGSD_SOUND_EFFECT);
    
    cout << "Loaded sound effect OGG ... " << ( sound_ogg.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound(&sound_ogg, 0, 1.0f);

    _sg_functions->utils.delay(2000);

    
    sg_sound_data sound_flac = _sg_functions->audio.load_sound_data("30248__streety__sword7.flac", SGSD_SOUND_EFFECT);
    
    cout << "Loaded sound effect FLAC ... " << ( sound_ogg.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound(&sound_flac, 0, 1.0f);
    
    _sg_functions->utils.delay(2000);
    
    sg_sound_data sound_mp3 = _sg_functions->audio.load_sound_data("280.mp3", SGSD_MUSIC);
    
    cout << "Loaded music MP3 ... " << ( sound_mp3.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound(&sound_mp3, 0, 1.0f);
    
    _sg_functions->utils.delay(4000);
    
    sg_sound_data sound_mod = _sg_functions->audio.load_sound_data("energize_my_mind.mod", SGSD_MUSIC);
    
    cout << "Loaded music MOD ... " << ( sound_mod.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound(&sound_mod, 0, 1.0f);
    
    _sg_functions->utils.delay(3000);
    
    sg_sound_data sound_midi = _sg_functions->audio.load_sound_data("superman_1.mid", SGSD_MUSIC);
    
    cout << "Loaded music MIDI ... " << ( sound_midi.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound(&sound_midi, 0, 1.0f);
    
    _sg_functions->utils.delay(5000);
    
    _sg_functions->audio.close_sound_data(&sound);
    _sg_functions->audio.close_sound_data(&sound_flac);
    _sg_functions->audio.close_sound_data(&sound_midi);
    _sg_functions->audio.close_sound_data(&sound_mod);
    _sg_functions->audio.close_sound_data(&sound_mp3);
    _sg_functions->audio.close_sound_data(&sound_ogg);
    
    _sg_functions->audio.close_audio();
}