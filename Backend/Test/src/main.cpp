//
//  main.cpp
//  SGSDL2Test
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include <iostream>

#include "sgInterfaces.h"

using namespace std;

sg_interface * _sg_functions = NULL;

//
// Test the core functions, can the driver be loaded
// and initialised?
//
bool test_core_functions()
{
    cout << "Testing Core Functions!" << endl;
    
    cout << "Calling load_sg..." << endl;
    _sg_functions = sg_load();
    
    if ( !_sg_functions )
    {
        cout << "Failed to load functions!" << endl;
        return false;
    }
    
    cout << "Calling init..." << endl;
    _sg_functions->init();
    
    return false == _sg_functions->has_error;
}

void test_colors(sg_drawing_surface *window)
{
    cout << "Testing Colors - R,G,B,W,Ralpha" << endl;
    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 0.0, 0.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);

    _sg_functions->graphics.clear_drawing_surface(window, {0.0, 1.0, 0.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);

    _sg_functions->graphics.clear_drawing_surface(window, {0.0, 0.0, 1.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);

    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 1.0, 1.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(2000);

    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 0.0, 0.0, 0.2});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(1500);
}

bool test_basic_drawing()
{
    cout << "Testing Basic Drawing!" << endl;
    
    sg_drawing_surface window;
    window = _sg_functions->graphics.open_window("Test Basic Drawing", 800, 600);
    
    test_colors(&window);
    
    _sg_functions->graphics.close_drawing_surface(&window);
    
    return false == _sg_functions->has_error;
}

bool test_window_operations()
{
    cout << "Testing Window Operations!" << endl;

    sg_drawing_surface w1, w2;
    w1 = _sg_functions->graphics.open_window("Window 1", 800, 600);
    w2 = _sg_functions->graphics.open_window("Window 2", 300, 300);
    
    if ( w1.width != 800 ) cout << " >> Error with w1 width! " << w1.width << endl;
    if ( w2.width != 300 ) cout << " >> Error with w2 width! " << w2.width << endl;
    
    if ( w1.height != 600 ) cout << " >> Error with w1 height! " << w1.height << endl;
    if ( w2.height != 300 ) cout << " >> Error with w2 height! " << w2.height << endl;
    
    _sg_functions->utils.delay(500);
    
    _sg_functions->graphics.close_drawing_surface(&w1);
    _sg_functions->graphics.close_drawing_surface(&w2);

    return true;
}

int main(int argc, const char * argv[])
{
    cout << "Starting driver backend test" << endl;
    
    if ( ! test_core_functions() )
    {
        cout << "Core functions failed with error: " << endl;
        cout << _sg_functions->current_error << endl;
        return -1;
    }

    if ( ! test_basic_drawing() )
    {
        cout << "Basic drawing failed with error: " << endl;
        cout << _sg_functions->current_error << endl;
        return -1;
    }
    
    test_window_operations();
    
    cout << "Success" << endl;
    return 0;
}

