//
//  test_input.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 11/12/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "test_input.h"

#include <iostream>

using namespace std;

#include "sgInterfaces.h"

extern sg_interface * _sg_functions;


//
// Callback functions
//

void _callback_do_quit()
{
    cout << "Do Quit" << endl;
    exit(0);
}

void _callback_handle_key_down(int key_char)
{
    cout << "Key Down " << key_char << endl;
}

void _callback_handle_key_up(int key_char)
{
    cout << "Key Up " << key_char << endl;
}

void _callback_handle_mouse_up(int mouse_button)
{
    cout << "Mouse up " << mouse_button << endl;
}

void _callback_handle_mouse_down(int mouse_button)
{
    cout << "Mouse down " << mouse_button << endl;
}

sg_input_callbacks get_input_callbacks()
{
    sg_input_callbacks callbacks;
    
    callbacks.do_quit = &_callback_do_quit;
    callbacks.handle_key_down = & _callback_handle_key_down;
    callbacks.handle_key_up = & _callback_handle_key_up;
    callbacks.handle_mouse_up = &_callback_handle_mouse_up;
    callbacks.handle_mouse_down = &_callback_handle_mouse_down;

    return callbacks;
}

//
// Input handling tests
//

void test_events(sg_drawing_surface * window_arr, int sz)
{
	cout << "Processing Events" << endl;
    
    for (int i = 0; i < 300; i++)
    {
        _sg_functions->input.process_events();
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
    
    cout << "Ended Events" << endl;
}



void test_input(sg_drawing_surface * window_arr, int sz)
{
    test_events(window_arr, sz);
}