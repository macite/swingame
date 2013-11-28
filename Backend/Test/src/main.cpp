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
    _sg_functions->utils.delay(200);
    
    color clr = _sg_functions->graphics.read_pixel(window, 10, 10);

    cout << " Color at 10,10 is RGBA " << clr.r << "," << clr.g << "," << clr.b << "," << clr.a << endl;
    cout << "Should be 1, 0, 0, 1" << endl;
    
    clr = _sg_functions->graphics.read_pixel(window, -10, -10);
    cout << " Color at -10,-10 is RGBA " << clr.r << "," << clr.g << "," << clr.b << "," << clr.a << endl;
    cout << "Should be 0, 0, 0, 0" << endl;

    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(200);

    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 1.0, 1.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(500);
    
    clr = _sg_functions->graphics.read_pixel(window, 10, 10);
    
    cout << " Color at 10,10 is RGBA " << clr.r << "," << clr.g << "," << clr.b << "," << clr.a << endl;
    cout << "Should be 1, 1, 1, 1" << endl;
    
    _sg_functions->graphics.clear_drawing_surface(window, {0.0, 1.0, 0.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(200);
    
    clr = _sg_functions->graphics.read_pixel(window, 10, 10);
    
    cout << " Color at 10,10 is RGBA " << clr.r << "," << clr.g << "," << clr.b << "," << clr.a << endl;
    cout << "Should be 0, 1, 0, 1" << endl;
    
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(200);


    _sg_functions->graphics.clear_drawing_surface(window, {0.0, 0.0, 1.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(200);
    
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(200);

    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 1.0, 1.0, 1.0});
    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(200);
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
        float data[] = {0.0f, 599.0f, 799.0f, 599.0f, 400.0f, 1.0f};
        _sg_functions->graphics.draw_triangle(&window_arr[w], {1.0,0,0,1.0}, data, 6 );
        _sg_functions->graphics.refresh_window(&window_arr[w]);

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
    
    for (int w = 0; w < sz; w++)
    {
        int sz = window_arr[w].width * window_arr[w].height;
        int pixels[sz];
        
        _sg_functions->graphics.to_pixels(&window_arr[w], pixels, sz);
        
        int count = 0;
        for (int x = 0; x < window_arr[w].width; x++)
        {
            for (int y = 0; y < window_arr[w].height; y++)
            {
                if  ( pixels[x + y * window_arr[w].width] == 0xffffffff ) count++;
            }
        }
        cout << "Window " << w << " has " << count << " white pixels" << endl;
        
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
        _sg_functions->graphics.refresh_window(&window_arr[w]);
    }

}


void test_circles(sg_drawing_surface *window_arr, int sz)
{
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
    }
    
    for (int i = 0; i < 300; i++)
    {
        float data[] = {    rand() / (float)RAND_MAX * 800,
            rand() / (float)RAND_MAX * 600,
            rand() / (float)RAND_MAX * 100
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.draw_circle(&window_arr[w], random_color(), data, 3 );
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
            rand() / (float)RAND_MAX * 100
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.fill_circle(&window_arr[w], random_color(), data, 3 );
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
}

void test_ellipses(sg_drawing_surface *window_arr, int sz)
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
            _sg_functions->graphics.draw_ellipse(&window_arr[w], random_color(), data, 4 );
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
            _sg_functions->graphics.fill_ellipse(&window_arr[w], random_color(), data, 4 );
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
}

void test_lines(sg_drawing_surface *window_arr, int sz)
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
            rand() / (float)RAND_MAX * 600
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.draw_line(&window_arr[w], random_color(), data, 4 );
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
}

void test_clip(sg_drawing_surface *window_arr, int sz)
{
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {0.0, 0.0, 1.0, 1.0});
        
        float data[] = {    0.0f,
                            0.0f,
                            window_arr[w].width * 0.9f,
                            window_arr[w].height * 0.9f };
        
        for (int c = 0; c < 8; c++)
        {
            data[2] = window_arr[w].width * (0.9 - c * 0.1);
            data[3] = window_arr[w].height * (0.9 - c * 0.1);
            
            _sg_functions->graphics.set_clip_rect(&window_arr[w], {0.0f}, data, 4);
            _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0f - c * 0.1f, 0.0, 0.0, 1.0});
        }
        _sg_functions->graphics.refresh_window(&window_arr[w]);
        
        data[2] = window_arr[w].width * 0.4;
        data[3] = window_arr[w].height * 0.4;
        _sg_functions->graphics.set_clip_rect(&window_arr[w], {0.0f}, data, 4);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {0.0, 1.0f, 0.0, 1.0f});
    }
    
    _sg_functions->utils.delay(500);
    
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_clip_rect(&window_arr[w]);
//        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {0.0, 1.0f, 0.0, 1.0f});
        _sg_functions->graphics.refresh_window(&window_arr[w]);
//        _sg_functions->utils.delay(100);
    }

}




bool test_basic_drawing()
{
    cout << "Testing Basic Drawing!" << endl;
    
    sg_drawing_surface window;
    window = _sg_functions->graphics.open_window("Test Basic Drawing", 800, 600);
    
    _sg_functions->graphics.show_fullscreen(&window, true);
    
    test_colors(&window);
    test_clip( &window, 1);
    test_pixels( &window, 1);
    test_rects( &window, 1);
    
    _sg_functions->graphics.show_fullscreen(&window, false);
    
    test_triangles( &window, 1);
    test_circles( &window, 1);
    
    _sg_functions->graphics.resize(&window, 320, 240);
    
    test_ellipses( &window, 1);
    test_lines( &window, 1);
    
    _sg_functions->graphics.close_drawing_surface(&window);
    
    return false == _sg_functions->has_error;
}

bool test_window_operations()
{
    cout << "Testing Window Operations!" << endl;

    sg_drawing_surface w[2];
    w[0] = _sg_functions->graphics.open_window("Window 1", 800, 600);
    w[1] = _sg_functions->graphics.open_window("Window 2", 300, 300);
    
    _sg_functions->graphics.show_border(&w[0], false);
    
    if ( w[0].width != 800 ) cout << " >> Error with w[0] width! " << w[0].width << endl;
    if ( w[1].width != 300 ) cout << " >> Error with w[1] width! " << w[1].width << endl;
    
    if ( w[0].height != 600 ) cout << " >> Error with w[0] height! " << w[0].height << endl;
    if ( w[1].height != 300 ) cout << " >> Error with w[1] height! " << w[1].height << endl;
    
    test_clip(w, 2);
    test_pixels(w, 2);
    test_rects(w, 2);
    test_triangles(w, 2);
    test_circles(w, 2);
    test_ellipses(w, 2);
    test_lines(w, 2);
    
    _sg_functions->graphics.close_drawing_surface(&w[0]);
    _sg_functions->graphics.close_drawing_surface(&w[1]);

    return true;
}

void output_system_details()
{
    sg_system_data *data = _sg_functions->read_system_data();
    
    cout << "Display count: " << data->num_displays << endl;
    
    for (int i = 0; i < data->num_displays; i++)
    {
        cout << "  -> Display[" << i << "] = " << data->displays[i].name << endl;
        cout << "     " << data->displays[i].width << " x " << data->displays[i].height << " at " << data->displays[i].x << "," << data->displays[i].y << endl;
        cout << "     Modes:" << endl;
        for (int m = 0; m < data->displays[i].num_modes; m++)
        {
            cout << "           " << data->displays[i].modes[m].width << " x " << data->displays[i].modes[m].height << endl;
        }
    }
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
    
    output_system_details();
    return 0;

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

