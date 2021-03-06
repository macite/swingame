//
//  test_input.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 11/12/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "test_input.h"

#include <iostream>
#include <cstdlib>

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

void _callback_handle_mouse_wheel(int x, int y)
{
    cout << "Wheel scroll " << x << "," << y << endl;
}


void _callback_handle_input_text(char* text)
{
  cout << "Text entered: " << text << endl; 
}

void _callback_handle_window_move(pointer w, int x, int y)
{
    cout << "Window Moved to " << x << " " << y << endl;
}

void _callback_handle_window_resize(pointer wnd, int w, int h)
{
    cout << "Window Size Now " << w << " " << h << endl;
}


sg_input_callbacks get_input_callbacks()
{
    sg_input_callbacks callbacks;
    
    callbacks.do_quit = &_callback_do_quit;
    callbacks.handle_key_down = & _callback_handle_key_down;
    callbacks.handle_key_up = & _callback_handle_key_up;
    callbacks.handle_mouse_up = &_callback_handle_mouse_up;
    callbacks.handle_mouse_down = &_callback_handle_mouse_down;
    callbacks.handle_mouse_wheel = &_callback_handle_mouse_wheel;
    callbacks.handle_window_move = &_callback_handle_window_move;
    callbacks.handle_window_resize = &_callback_handle_window_resize;

    callbacks.handle_input_text = &_callback_handle_input_text; 

    return callbacks;
}

//
// Input handling tests
//

void test_events(sg_drawing_surface * window_arr, int sz)
{
	cout << "Processing Events" << endl;
    
    unsigned int mouse_button;
    int x, y;
    
    for (int i = 0; i < 300; i++)
    {
        _sg_functions->input.process_events();
        
        mouse_button = _sg_functions->input.mouse_state(&x, &y);
        cout << "Mouse       at " << x << "," << y << "( " << mouse_button << " )" << endl;

        mouse_button = _sg_functions->input.mouse_relative_state(&x, &y);
        cout << "Mouse relative " << x << "," << y << "( " << mouse_button << " )" << endl;
        
        if ( i == 100 )
        {
            _sg_functions->input.mouse_cursor_state(0);
            cout << " - Hide mouse" << endl;
        }
        if ( i == 200 )
        {
            _sg_functions->input.mouse_cursor_state(1);
            cout << " - Show mouse" << endl;
        }
        
        pointer focus = _sg_functions->input.focus_window();
        
        for (int w = 0; w < sz; w++)
        {
            if ( focus == window_arr[w]._data )
            {
                int wx, wy;
                _sg_functions->input.window_position(&window_arr[w], &wx, &wy);
                cout << "Focus on " << window_arr[w].width << "x" << window_arr[w].height << " @" << wx << ":" << wy <<  "\n";
            }
            
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
    
    cout << "Ended Events" << endl;
}

void test_key_press()
{
    cout << "Testing key press" << endl;
    cout << " - Press the 'a' key" << endl;
    
    while ( ! _sg_functions->input.key_pressed('a') )
    {
        _sg_functions->input.process_events();
    }
    _sg_functions->utils.delay(1000);
}


void test_unicode_input() 
{
  cout << "Testing unicode input" << endl; 
  cout << " - enter some unicode " << endl; 
  _sg_functions->input.start_unicode_text_input(0,0,200,200); 

  _sg_functions->input.process_events();
  while ( ! _sg_functions->input.key_pressed('a') )
  {
      _sg_functions->input.process_events();
  }
  _sg_functions->input.stop_unicode_text_input(); 
}

void test_input(sg_drawing_surface * window_arr, int sz)
{
    test_events(window_arr, sz);
    test_key_press();
    test_unicode_input(); 
}




