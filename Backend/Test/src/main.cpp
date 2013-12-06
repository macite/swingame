//
//  main.cpp
//  SGSDL2Test
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include <iostream>

#include "sgInterfaces.h"
#include "test_audio.h"

#define SHAPE_COUNT 60

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
   
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
    }
    
    for (int i = 0; i < SHAPE_COUNT; i++)
    {
        float x, y;
        
        x = rand() / (float)RAND_MAX * 800;
        y = rand() / (float)RAND_MAX * 600;
        
        float data[] = {
            x, y,
            x + 100, y,
            x, y + 100,
            x + 100, y + 100
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.draw_rect(&window_arr[w], random_color(), data, 8 );
            _sg_functions->graphics.refresh_window(&window_arr[w]);
        }
    }
    
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
    }
    
    for (int i = 0; i < SHAPE_COUNT; i++)
    {
        float x = rand() / (float)RAND_MAX * 800;
        float y = rand() / (float)RAND_MAX * 600;
        
        float data[] = {
            x, y,
            x + 100, y,
            x, y + 100,
            x + 100, y + 100
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.fill_rect(&window_arr[w], random_color(), data, 8 );
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    for (int i = 0; i < SHAPE_COUNT; i++)
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
    
    _sg_functions->utils.delay(2000);
    
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_clip_rect(&window_arr[w]);
//        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {0.0, 1.0f, 0.0, 1.0f});
//        _sg_functions->graphics.refresh_window(&window_arr[w]);
//        _sg_functions->utils.delay(100);
    }

}

bool test_positions(sg_drawing_surface *window_arr, int sz)
{
    for (int i = 0; i < sz; i++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1.0, 1.0, 1.0, 1.0});
        
        float data[] = {0.0f, 0.0f, 50.0f, 50.0f};
        float data1[] = {window_arr[i].width - 50.0f, 0.0f, 50.0f, 50.0f};
        float data2[] = {0.0f, window_arr[i].height - 50.0f, 50.0f, 50.0f};
        float data3[] = {window_arr[i].width - 50.0f, window_arr[i].height - 50.0f, 50.0f, 50.0f};
        
        _sg_functions->graphics.fill_aabb_rect(&window_arr[i], {1.0f, 0.0f, 0.0f, 1.0f}, data,  4);
        _sg_functions->graphics.fill_aabb_rect(&window_arr[i], {0.0f, 1.0f, 0.0f, 1.0f}, data1,  4);
        _sg_functions->graphics.fill_aabb_rect(&window_arr[i], {0.0f, 0.0f, 1.0f, 1.0f}, data2,  4);
        _sg_functions->graphics.fill_aabb_rect(&window_arr[i], {0.0f, 0.0f, 0.0f, 1.0f}, data3,  4);
        
        _sg_functions->graphics.refresh_window(&window_arr[i]);
    }

    _sg_functions->utils.delay(2000);
    
    return true;
}

bool test_alpha(sg_drawing_surface *window_arr, int sz)
{
    for (int i = 0; i < sz; i++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1.0, 1.0, 1.0, 1.0});
        
        float data[] = {0.0f, 0.0f, window_arr[i].width / 11.0f, window_arr[i].height * 1.0f};

        for (int j = 0; j < 11; j++)
        {
            data[0] = j * data[2];
            _sg_functions->graphics.fill_ellipse(&window_arr[i], {1.0f, 0.0f, 0.0f, 0.1f * j}, data,  4);
        }
        
        _sg_functions->graphics.refresh_window(&window_arr[i]);
    }
    
    _sg_functions->utils.delay(2000);
    
    return true;
}

void test_resize(sg_drawing_surface * window_arr, int sz)
{
    for (int i = 0; i < sz; i++)
    {
        int w, h;
        w = window_arr[i].width;
        h = window_arr[i].height;
        
        _sg_functions->graphics.resize(&window_arr[i], 320, 240);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(500);
        
        _sg_functions->graphics.resize(&window_arr[i], 640, 480);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(500);

        _sg_functions->graphics.resize(&window_arr[i], 800, 600);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(500);
        
        _sg_functions->graphics.resize(&window_arr[i], 1024, 768);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(1000);
        
        _sg_functions->graphics.resize(&window_arr[i], w, h);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1.0f, 1.0f, 1.0f, 1.0f});
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(500);
    }

}


sg_drawing_surface img, img2;

bool test_basic_drawing()
{
    cout << "Testing Basic Drawing!" << endl;
    
    sg_drawing_surface window;
    window = _sg_functions->graphics.open_window("Test Basic Drawing", 800, 600);

    img = _sg_functions->image.load_bitmap("on_med.png", &window);
    
    _sg_functions->image.draw_bitmap( &img, &window, 0, 0);
    _sg_functions->graphics.refresh_window(&window);
    _sg_functions->utils.delay(3000);
    
    test_colors(&window);
    test_positions(&window, 1);
    test_alpha(&window, 1);

    test_clip( &window, 1);
    test_pixels( &window, 1);
    
    _sg_functions->graphics.show_fullscreen(&window, true);

    test_rects( &window, 1);
    
    _sg_functions->graphics.show_fullscreen(&window, false);
    
    test_triangles( &window, 1);
    test_circles( &window, 1);
    
    test_resize(&window, 1);
    
    test_ellipses( &window, 1);
    test_lines( &window, 1);
    
    img2 = _sg_functions->image.create_bitmap("testbmp1", 50, 100);
    _sg_functions->graphics.clear_drawing_surface(&img2, {1.0f, 0.0f, 0.0f, 1.0f});
    _sg_functions->image.draw_bitmap(&img2, &window, 50, 50);
    _sg_functions->graphics.refresh_window(&window);
    _sg_functions->utils.delay(3000);

    _sg_functions->graphics.clear_drawing_surface(&img2, {1.0f, 0.0f, 1.0f, 0.2f});
    
    int sz = 50 * 100;
    int pixels[ sz ];
    _sg_functions->graphics.to_pixels(&img2, pixels, sz);
    
    for (int i = 0; i < sz; i++)
    {
        cout << std::hex << pixels[i] << " ";
        if (i % 50 == 0) {
            cout << endl;
        }
    }
    
    _sg_functions->image.draw_bitmap(&img, &window, 100, 100);
    _sg_functions->image.draw_bitmap(&img2, &window, 100, 100);
    _sg_functions->graphics.refresh_window(&window);
    _sg_functions->utils.delay(3000);

    
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
    
    _sg_functions->image.draw_bitmap( &img, &w[0], 0, 0);
    _sg_functions->image.draw_bitmap( &img2, &w[0], 50, 50);
    _sg_functions->graphics.refresh_window(&w[0]);

    _sg_functions->image.draw_bitmap( &img, &w[1], 0, 0);
    _sg_functions->image.draw_bitmap( &img2, &w[1], 50, 50);
    _sg_functions->graphics.refresh_window(&w[1]);

    _sg_functions->utils.delay(3000);

    
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
    
    cout << "Time is " << _sg_functions->utils.get_ticks() << endl;
}

#include "test_draw_point.h"

int * leak()
{
    int *p = (int*)malloc(sizeof(int) * 1024);
    int a = 0;
    for (int i = 0; i < 1024; i++)
    {
        *(p+i) = i;
        a += *(p+i);
        cout << a << endl;
    }
    
    return p;
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
    
    output_system_details();
    
    if ( ! test_basic_drawing() )
    {
        cout << "Basic drawing failed with error: " << endl;
        //cout << _sg_functions->current_error << endl;
        return -1;
    }
    
    test_window_operations();
    
    _sg_functions->graphics.close_drawing_surface(&img);
    _sg_functions->graphics.close_drawing_surface(&img2);
    
    test_audio();
    
    _sg_functions->finalise();
    cout << "Success" << endl;
    
    return 0;
}

