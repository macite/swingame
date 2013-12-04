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
    _sg_functions->audio.resume_music();

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
    
    _sg_functions->utils.delay(1000);
    _sg_functions->audio.set_music_vol(0.25f);
    _sg_functions->utils.delay(1500);
    _sg_functions->audio.set_sound_volume(&sound_mod, 0.75f);
    _sg_functions->utils.delay(1500);

    cout << "Testing pause" << endl;
    _sg_functions->audio.pause_music();
    _sg_functions->utils.delay(1000);
    cout << "Testing resume" << endl;
    _sg_functions->audio.resume_music();
    _sg_functions->utils.delay(1000);
    cout << "Testing stop" << endl;
    _sg_functions->audio.stop_music();
    _sg_functions->utils.delay(1000);
    
    
    sg_sound_data sound_midi = _sg_functions->audio.load_sound_data("superman_1.mid", SGSD_MUSIC);
    
    cout << "Loaded music MIDI ... " << ( sound_midi.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound(&sound_midi, 0, 1.0f);
    
    _sg_functions->utils.delay(5000);
    
    cout << "Midi is " << (_sg_functions->audio.sound_playing(&sound_midi) > 0 ? "Playing" : "Not Playing") << endl;
    cout << "Wav is " << (_sg_functions->audio.sound_playing(&sound) > 0 ? "Playing" : "Not Playing") << endl;
    
    
    cout << "Fade in MOD" << endl;
    _sg_functions->audio.fade_in(&sound_mod, 0, 2000);
    
    _sg_functions->utils.delay(3000);
    
    cout << "Fade out MOD" << endl;
    _sg_functions->audio.fade_out(&sound_mod, 2000);
    
    _sg_functions->utils.delay(3000);

    cout << "Fade in OGG" << endl;
    _sg_functions->audio.fade_in(&sound_ogg, 0, 200);
    
    _sg_functions->utils.delay(200);
    
    cout << "Fade out OGG" << endl;
    _sg_functions->audio.fade_out(&sound_ogg, 200);
    
    _sg_functions->utils.delay(1500);

    _sg_functions->audio.play_sound(&sound_ogg, 0, 1.0f);
    cout << "OGG volume is " << _sg_functions->audio.sound_volume(&sound_ogg) << endl;
    _sg_functions->audio.set_sound_volume(&sound_ogg, 0.25f);
    cout << "OGG volume is " << _sg_functions->audio.sound_volume(&sound_ogg) << endl;
    
    _sg_functions->utils.delay(1500);
    
    cout << "Play OGG lots" << endl;
    for (int i = 0; i < 100; i++)
    {
        _sg_functions->audio.play_sound(&sound_ogg, 0, 1 - 0.01f * i );
        _sg_functions->utils.delay(50);
    }
    cout << "Stop OGG!" << endl;
     _sg_functions->audio.stop_sound(&sound_ogg);
    
    
    _sg_functions->audio.close_sound_data(&sound);
    _sg_functions->audio.close_sound_data(&sound_flac);
    _sg_functions->audio.close_sound_data(&sound_midi);
    _sg_functions->audio.close_sound_data(&sound_mod);
    _sg_functions->audio.close_sound_data(&sound_mp3);
    _sg_functions->audio.close_sound_data(&sound_ogg);
    
    _sg_functions->audio.close_audio();
}




