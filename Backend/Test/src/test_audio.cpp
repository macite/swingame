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
    
    sg_sound_data sound = _sg_functions->audio.load_sound_effect("test.ogg", SGSD_SOUND_EFFECT);
    
    cout << "Loaded sound effect ... " << (sound.data ? "FAILED" : "SUCCESS") << endl;
    
    
    _sg_functions->audio.close_audio();
}