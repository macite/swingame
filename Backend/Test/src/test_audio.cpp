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

void test_audio()
{
    _sg_functions->audio.open_audio();
    
    cout << "Audio open ... " << (_sg_functions->has_error ? "FAILED" : "SUCCESS") << endl;
    _sg_functions->utils.delay(500);
    
    sg_sound_data sound = _sg_functions->audio.load_sound_effect("error.wav", SGSD_SOUND_EFFECT);
    
    cout << "Loaded sound effect ... " << ( sound.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound_effect(&sound, 0, 1.0f);
    
    _sg_functions->utils.delay(2000);

    sg_sound_data sound_ogg = _sg_functions->audio.load_sound_effect("test.ogg", SGSD_SOUND_EFFECT);
    
    cout << "Loaded sound effect OGG ... " << ( sound_ogg.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound_effect(&sound_ogg, 0, 1.0f);

    _sg_functions->utils.delay(2000);

    
    sg_sound_data sound_flac = _sg_functions->audio.load_sound_effect("30248__streety__sword7.flac", SGSD_SOUND_EFFECT);
    
    cout << "Loaded sound effect FLAC ... " << ( sound_ogg.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound_effect(&sound_flac, 0, 1.0f);
    
    _sg_functions->utils.delay(2000);
    
    sg_sound_data sound_mp3 = _sg_functions->audio.load_sound_effect("280.mp3", SGSD_MUSIC);
    
    cout << "Loaded music MP3 ... " << ( sound_mp3.data ? "SUCCESS" : "FAILED") << endl;
    
    _sg_functions->audio.play_sound_effect(&sound_mp3, 0, 1.0f);
    
    _sg_functions->utils.delay(2000);
    
    _sg_functions->audio.close_audio();
}