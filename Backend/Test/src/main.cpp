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
    cout << "Testing Colors - R,G,B,W" << endl;
    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 0.0, 0.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);

    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);

    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 1.0, 1.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(1000);
    
    _sg_functions->graphics.clear_drawing_surface(window, {0.0, 1.0, 0.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);
    
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);


    _sg_functions->graphics.clear_drawing_surface(window, {0.0, 0.0, 1.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);
    
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);

    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 1.0, 1.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);
}

color random_color()
{
    return {    rand() / (float)RAND_MAX,
                rand() / (float)RAND_MAX,
                rand() / (float)RAND_MAX,
                rand() / (float)RAND_MAX
            };
}

void test_rects(sg_drawing_surface *window_arr, int sz)
{
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
    }
   
    for (int i = 0; i < 300; i++)
    {
        float data[] = {    rand() / (float)RAND_MAX * 800,
                            rand() / (float)RAND_MAX * 600,
                            rand() / (float)RAND_MAX * 100,
                            rand() / (float)RAND_MAX * 100
                        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.draw_aabb_rect(&window_arr[w], random_color(), data, 4 );
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }

    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
    }
    
    for (int i = 0; i < 300; i++)
    {
        float data[] = {    rand() / (float)RAND_MAX * 800,
            rand() / (float)RAND_MAX * 600,
            rand() / (float)RAND_MAX * 100,
            rand() / (float)RAND_MAX * 100
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.fill_aabb_rect(&window_arr[w], random_color(), data, 4 );
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
}

void test_triangles(sg_drawing_surface *window_arr, int sz)
{
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
    }
    
    for (int i = 0; i < 300; i++)
    {
        float data[] = {    rand() / (float)RAND_MAX * 800,
            rand() / (float)RAND_MAX * 600,
            rand() / (float)RAND_MAX * 800,
            rand() / (float)RAND_MAX * 600,
            rand() / (float)RAND_MAX * 800,
            rand() / (float)RAND_MAX * 600
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.draw_triangle(&window_arr[w], random_color(), data, 6 );
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
    
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
    }
    
    for (int i = 0; i < 300; i++)
    {
        float data[] = {    rand() / (float)RAND_MAX * 800,
            rand() / (float)RAND_MAX * 600,
            rand() / (float)RAND_MAX * 800,
            rand() / (float)RAND_MAX * 600,
            rand() / (float)RAND_MAX * 800,
            rand() / (float)RAND_MAX * 600
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.fill_triangle(&window_arr[w], random_color(), data, 6 );
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
}

void test_pixels(sg_drawing_surface *window_arr, int sz)
{
    color clr = { 0.0, 0.0, 0.0, 1.0 };
    
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
        _sg_functions->graphics.refresh_window(&window_arr[w]);
    }
    
    for (int i = 0; i < 300; i++)
    {
        for (int w = 0; w < sz; w++)
        {
            for (int j = 0; j < 100; j++)
            {
                float data[] = {
                    rand() / (float)RAND_MAX * 800,
                    rand() / (float)RAND_MAX * 600
                };

                _sg_functions->graphics.draw_pixel(&window_arr[w], clr, data, 2 );
            }
            
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
}



bool test_basic_drawing()
{
    cout << "Testing Basic Drawing!" << endl;
    
    sg_drawing_surface window;
    window = _sg_functions->graphics.open_window("Test Basic Drawing", 800, 600);
    
    test_colors(&window);
    test_pixels( &window, 1);
    test_rects( &window, 1);
    test_triangles( &window, 1);
    
    _sg_functions->graphics.close_drawing_surface(&window);
    
    return false == _sg_functions->has_error;
}

bool test_window_operations()
{
    cout << "Testing Window Operations!" << endl;

    sg_drawing_surface w[2];
    w[0] = _sg_functions->graphics.open_window("Window 1", 800, 600);
    w[1] = _sg_functions->graphics.open_window("Window 2", 300, 300);
    
    if ( w[0].width != 800 ) cout << " >> Error with w[0] width! " << w[0].width << endl;
    if ( w[1].width != 300 ) cout << " >> Error with w[1] width! " << w[1].width << endl;
    
    if ( w[0].height != 600 ) cout << " >> Error with w[0] height! " << w[0].height << endl;
    if ( w[1].height != 300 ) cout << " >> Error with w[1] height! " << w[1].height << endl;
    
    test_rects(w, 2);
    test_triangles(w, 2);
    test_pixels(w, 2);
    
    _sg_functions->graphics.close_drawing_surface(&w[0]);
    _sg_functions->graphics.close_drawing_surface(&w[1]);

    return true;
}

#include "test_draw_point.h"

int main(int argc, const char * argv[])
{
//    test_draw_point();
//    return 0;
    
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
        //cout << _sg_functions->current_error << endl;
        return -1;
    }
    
    test_window_operations();
    
    cout << "Success" << endl;
    return 0;
}

