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
    _sg_functions = load_sg();
    
    if ( !_sg_functions )
    {
        cout << "Failed to load functions!" << endl;
        return false;
    }
    
    cout << "Calling init..." << endl;
    _sg_functions->init();
    
    return false == _sg_functions->has_error;
}

bool test_basic_drawing()
{
    cout << "Testing Basic Drawing!" << endl;
    
    sg_drawing_surface window;
    window = _sg_functions->graphics.open_window("Test Basic Drawing", 800, 600);
    
    _sg_functions->utils.delay(2000);
    
    _sg_functions->graphics.close_drawing_surface(&window);
    
    return false == _sg_functions->has_error;
}

bool test_window_operations()
{
    cout << "Testing Window Operations!" << endl;

    sg_drawing_surface w1, w2;
    w1 = _sg_functions->graphics.open_window("Window 1", 800, 600);
    w2 = _sg_functions->graphics.open_window("Window 2", 300, 300);
    
    _sg_functions->utils.delay(2000);
    
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

