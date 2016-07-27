//
//  main.cpp
//  SGSDL2Test
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include <iostream>
#include <cstdlib>
#include <climits>
#include <cstdio>
#include <cmath>
#include <ctime>

#include "sgInterfaces.h"
#include "test_audio.h"
#include "test_input.h"
#include "test_text.h"
#include "test_network.h"

#define SHAPE_COUNT 60

using namespace std;

sg_interface * _sg_functions = NULL;

// images drawn in tests
sg_drawing_surface img, img2;
sg_drawing_surface bmp;

enum test_options 
{ 
    BASIC_DRAWING = 1,
    WINDOW_OPERATIONS = 2,  
    BITMAP_DRAWING = 4, 
    AUDIO = 8,
    INPUT = 16,
    TEXT = 32,
    NETWORK = 64
}; 

enum test_drawing_options 
{
    TEST_COLORS = 1, 
    TEST_READ_PIXELS = 2, 
    TEST_POSITIONS = 4, 
    TEST_ALPHA = 8, 
    TEST_CLIP = 16, 
    TEST_PIXELS = 32, 
    TEST_SHAPES = 64, 
    TEST_RESIZE = 128, 
    TEST_LINES = 256,
    TEST_BITMAPS = 512, 
    TEST_INPUT = 1024,
    TEST_FULLSCREEN = 2048,
    TEST_DIRECT_PIXSPEED = 4096,
    TEST_BITMAP_PIXSPEED = 8192
};

void print_options() 
{
    cout << "0: all " << endl; 
    cout << "1: basic drawing functions"  << endl; 
    cout << "2: window operations"  << endl; 
    cout << "4: bitmap drawing"  << endl; 
    cout << "8: audio "  << endl; 
    cout << "16: input "  << endl; 
    cout << "32: text "  << endl;
    cout << "64: network "  << endl;
}

void print_drawing_options() 
{
    cout << "0: all " << endl; 
    cout << "1: colors"  << endl; 
    cout << "2: read pixels"  << endl; 
    cout << "4: test positions"  << endl; 
    cout << "8: test alpha"  << endl; 
    cout << "16: test clip"  << endl; 
    cout << "32 test pixels"  << endl; 
    cout << "64: test shapes"  << endl; 
    cout << "128: test resize "  << endl; 
    cout << "256: test lines "  << endl; 
    cout << "512: test bitmaps "  << endl; 
    cout << "1024: test input "  << endl; 
    cout << "2048: test fullscreen "  << endl; 
    cout << "4096: test direct pixel drawing speed "  << endl; 
    cout << "8192: test bitmap pixel drawing speed "  << endl; 
}


//
// Test the core functions, can the driver be loaded
// and initialised?
//
bool test_core_functions()
{    
    cout << "Testing Core Functions!" << endl;
    
    cout << "Calling load_sg..." << endl;
    _sg_functions = sg_load(get_input_callbacks());
    
    if ( !_sg_functions )
    {
        cout << "Failed to load functions!" << endl;
        return false;
    }
    
    cout << "Calling init..." << endl;
    _sg_functions->init();
    
    return false == _sg_functions->has_error;
}

//
// The _bmp_wnd is a hack to allow the drawing test functions to be called with a
// bitmap surface rather than a window. In these cases "refreshing" the bitmap would
// have no effect. In these cases the _bmp_wnd is set to an open window that the bitmap
// can be drawn to, and this window is then refreshed.
//
sg_drawing_surface *_bmp_wnd = NULL;

void refresh_or_draw(sg_drawing_surface *surf)
{
    // if we are refreshing a window... do normal processing
    if (surf->kind == SGDS_Window)
    {
      _sg_functions->input.process_events(); 
      if (_sg_functions->input.window_close_requested(surf))
      { 
        exit(0); 
      }

      _sg_functions->graphics.refresh_window(surf);
    }
    else
    {
        // the test has asked to "refresh" a bitmap - use the _bmp_wnd to show the new
        // state of surf
        float src_data[] = {0, 0, static_cast<float>(surf->width), static_cast<float>(surf->height)};
        float dst_data[] = {10, 10, 0, 0, 0, 1, 1};

        _sg_functions->image.draw_bitmap(surf, _bmp_wnd, src_data, 4, dst_data, 7, SG_FLIP_NONE);
        _sg_functions->graphics.refresh_window(_bmp_wnd);
    }
}


void test_colors(sg_drawing_surface *window_arr, int sz)
{
    cout << "Testing Colors" << endl;
    
    for (int w = 0; w < sz; w++)
    {
        cout << " - Clearning the surface to..." << endl;
        cout << "   - RED" << endl;
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 0.0, 0.0, 1.0});
        refresh_or_draw(&window_arr[w]);
        _sg_functions->utils.delay(200);
        
        cout << "   - WHITE" << endl;
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
        refresh_or_draw(&window_arr[w]);
        _sg_functions->utils.delay(500);
        
        cout << "   - GREEN" << endl;
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {0.0, 1.0, 0.0, 1.0});
        refresh_or_draw(&window_arr[w]);
        _sg_functions->utils.delay(200);
       
        cout << "   - BLUE" << endl;
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {0.0, 0.0, 1.0, 1.0});
        refresh_or_draw(&window_arr[w]);
        _sg_functions->utils.delay(200);
     
        cout << "   - WHITE" << endl;
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
        refresh_or_draw(&window_arr[w]);
        _sg_functions->utils.delay(200);
    }
}

void test_read_pixels(sg_drawing_surface *window)
{
    cout << "Testing Reading of Pixel data" << endl;
    
    
    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 0.0, 0.0, 1.0});
    sg_color clr = _sg_functions->graphics.read_pixel(window, 10, 10);
    
    cout << " - Color at  10,10  is RGBA " << clr.r << "," << clr.g << "," << clr.b << "," << clr.a << endl;
    cout << "   -                  Match 1,0,0,1" << endl;
    
    clr = _sg_functions->graphics.read_pixel(window, -10, -10);
    cout << " - Color at -10,-10 is RGBA " << clr.r << "," << clr.g << "," << clr.b << "," << clr.a << endl;
    cout << "                      Match 0,0,0,0" << endl;
    
    _sg_functions->graphics.clear_drawing_surface(window, {1.0, 1.0, 1.0, 1.0});
    refresh_or_draw(window);
    clr = _sg_functions->graphics.read_pixel(window, 10, 10);
    
    cout << " - Color at  10,10  is RGBA " << clr.r << "," << clr.g << "," << clr.b << "," << clr.a << endl;
    cout << "   -                  Match 1,1,1,1" << endl;
}


sg_color random_color()
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
            refresh_or_draw(&window_arr[w]);
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
            refresh_or_draw(&window_arr[w]);
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
            refresh_or_draw(&window_arr[w]);
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
            refresh_or_draw(&window_arr[w]);
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
        refresh_or_draw(&window_arr[w]);

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
            refresh_or_draw(&window_arr[w]);
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
            refresh_or_draw(&window_arr[w]);
        }
    }
}

void test_pixels(sg_drawing_surface *window_arr, int sz)
{
    _sg_functions->input.process_events();
    sg_color clr = { 0.0, 0.0, 0.0, 1.0 };
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
        refresh_or_draw(&window_arr[w]);
        _sg_functions->input.process_events();
    }
    
    _sg_functions->input.process_events();
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
            
            _sg_functions->input.process_events();
            refresh_or_draw(&window_arr[w]);
        }
    }
    
    _sg_functions->input.process_events();
    for (int w = 0; w < sz; w++)
    {
        sg_drawing_surface *wnd = &window_arr[w];
        int sz = window_arr[w].width * window_arr[w].height;
        int pixels[sz];
        
        _sg_functions->graphics.to_pixels(wnd, pixels, sz);
        
        int count = 0;
        for (int x = 0; x < window_arr[w].width; x++)
        {
            for (int y = 0; y < window_arr[w].height; y++)
            {
                if  ( pixels[x + y * window_arr[w].width] == 0xffffffff ) count++;
            }
            _sg_functions->input.process_events();
        }
        cout << "Window " << w << " has " << count << " white pixels" << endl;
        
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
        refresh_or_draw(&window_arr[w]);
        _sg_functions->input.process_events();
    }
}


void test_direct_pixel_speed(sg_drawing_surface *window_arr, int sz)
{
    _sg_functions->input.process_events();
    for (int cnt = 0; cnt < 5; cnt++)
    {
        for (int w = 0; w < sz; w++)
        {
            float data[] = {0,0};

            int start_time = _sg_functions->utils.get_ticks();
            for (int y = 0; y < 600; y++)
            {
                for (int x = 0; x < 800; x++)
                {
                    data[0] = x;
                    data[1] = y;
                    _sg_functions->graphics.draw_pixel(&window_arr[w], {1.0,0,0,1.0}, data, 2 );
                }
            }

            int end_time = _sg_functions->utils.get_ticks();

            _sg_functions->input.process_events();
            refresh_or_draw(&window_arr[w]);

            cout << "Direct pixel drawing time for window " << w
                 << " was " << end_time - start_time << "ms" << endl;
        }
    } 
    cout << endl;
}

void test_bitmap_pixel_speed(sg_drawing_surface *window_arr, int sz)
{
	_sg_functions->input.process_events();
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0, 1.0, 1.0, 1.0});
        refresh_or_draw(&window_arr[w]);
    _sg_functions->input.process_events();
    }
    
    _sg_functions->input.process_events();
    sg_drawing_surface bitmap = _sg_functions->image.create_bitmap(800, 600);
    for (int cnt = 0; cnt < 5; cnt++)
    {
        for (int w = 0; w < sz; w++)
        {
            float data[2];
            int start_time = _sg_functions->utils.get_ticks();

            _sg_functions->graphics.clear_drawing_surface( &bitmap, {1,1,1,1} );

            for (int y = 0; y < 600; y++)
            {
                for (int x = 0; x < 800; x++)
                {
                    data[0] = x;
                    data[1] = y;
                    _sg_functions->graphics.draw_pixel(&bitmap, {0,0,1,1}, data, 2 );
                }
            }

            int mid_time = _sg_functions->utils.get_ticks();

            float srcDat[] = { 0, 0, 0, 0, 0, 1, 1 };
            float dstDat[] = { 0, 0, 0, 0 };

            _sg_functions->image.draw_bitmap(&bitmap, &window_arr[w],srcDat,7,dstDat,4,SG_FLIP_NONE);

            int end_time = _sg_functions->utils.get_ticks();

            _sg_functions->input.process_events();
            refresh_or_draw(&window_arr[w]);

            cout << "Pixel drawing time to bitmap was " << mid_time - start_time << "ms" << endl;
            cout << "Drawing time from bitmap to window " << w  << " was "
                 << end_time - mid_time << "ms" << endl;
        }
    }
    _sg_functions->graphics.close_drawing_surface(&bitmap);
    cout << endl;
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
            refresh_or_draw(&window_arr[w]);
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
            refresh_or_draw(&window_arr[w]);
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
            refresh_or_draw(&window_arr[w]);
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
            refresh_or_draw(&window_arr[w]);
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
            rand() / (float)RAND_MAX * 600,
            1 + rand() / (float)RAND_MAX * 30
        };
        
        for (int w = 0; w < sz; w++)
        {
            _sg_functions->graphics.draw_line(&window_arr[w], random_color(), data, 5 );
            refresh_or_draw(&window_arr[w]);
        }
    }
}

void test_clip(sg_drawing_surface *window_arr, int sz)
{
    cout << "Testing Clipping" << endl;
    
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
            
            _sg_functions->graphics.set_clip_rect(&window_arr[w], data, 4);
            _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {1.0f - c * 0.1f, 0.0, 0.0, 1.0});
        }
        refresh_or_draw(&window_arr[w]);
        
        data[2] = window_arr[w].width * 0.4;
        data[3] = window_arr[w].height * 0.4;
        _sg_functions->graphics.set_clip_rect(&window_arr[w], data, 4);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[w], {0.0, 1.0f, 0.0, 1.0f});
    }
    
    _sg_functions->utils.delay(3000);
    
    for (int w = 0; w < sz; w++)
    {
        _sg_functions->graphics.clear_clip_rect(&window_arr[w]);
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
        
        refresh_or_draw(&window_arr[i]);
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
        
        refresh_or_draw(&window_arr[i]);
    }
    
    _sg_functions->utils.delay(2000);
    
    return true;
}

void test_resize(sg_drawing_surface * window_arr, int sz)
{
    cout << "Testing resize" << endl;
    for (int i = 0; i < sz; i++)
    {
        int w, h;
        w = window_arr[i].width;
        h = window_arr[i].height;
        
        _sg_functions->graphics.resize(&window_arr[i], 320, 240);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(1000);
        
        _sg_functions->graphics.resize(&window_arr[i], 640, 480);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(1000);

        _sg_functions->graphics.resize(&window_arr[i], 800, 600);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(1000);
        
        _sg_functions->graphics.resize(&window_arr[i], 1024, 768);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], random_color());
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(1000);
        
        _sg_functions->graphics.resize(&window_arr[i], w, h);
        _sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1.0f, 1.0f, 1.0f, 1.0f});
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(1000);
    }

}

void test_bitmaps(sg_drawing_surface * window_arr, int sz)
{
    float src_data[] = {0, 0, static_cast<float>(img.width), static_cast<float>(img.height)};
    float bmp_src_data[] = {0, 0, static_cast<float>(bmp.width), static_cast<float>(bmp.height)};
    float dst_data[] = {0, 0, 0, 0, 0, 1, 1};
    
	//Draw at TOP LEFT (shows that the scaling of the draw coordinates works)
	for (int i = 0; i < sz; i++)
	{
		_sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1, 1, 1, 1});

        _sg_functions->image.draw_bitmap( &bmp, &window_arr[i], bmp_src_data, 4, dst_data, 7, SG_FLIP_NONE);
        _sg_functions->graphics.refresh_window(&window_arr[i]);
        _sg_functions->utils.delay(5000);
        
		_sg_functions->image.draw_bitmap( &img, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
		_sg_functions->graphics.refresh_window(&window_arr[i]);
		_sg_functions->utils.delay(1000);
	}
	
    dst_data[0] = 300.0f;
    dst_data[1] = 300.0f;
    
	//Test rotation (should rotate around centre)
	for (int u = 0; u <= 360; u += 4)
	{
		for (int i = 0; i < sz; i++)
		{
			//use sin
			_sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1, 1, 1, 1});
            
            dst_data[2] = static_cast<float>(u);
            
		    _sg_functions->image.draw_bitmap( &img, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->image.draw_bitmap( &img2, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->graphics.refresh_window(&window_arr[i]);	
		}
    }
    _sg_functions->utils.delay(200);

    dst_data[0] = 150.0f;
    dst_data[1] = 150.0f;
    dst_data[2] = 0;
    
	//Test scale
	for (int u = 0; u <= 90; u++) 
	{
		for (int i = 0; i < sz; i++)
		{
			//use sin
			_sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1, 1, 1, 1});

            dst_data[5] = u/40.0;
            dst_data[6] = u/40.0;

		    _sg_functions->image.draw_bitmap( &img, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->image.draw_bitmap( &img2, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->graphics.refresh_window(&window_arr[i]);
		}
    }
    
	//Test rotate and scale (should rotate around centre)
	for (int u = 0; u <= 360; u += 2) 
	{
		for (int i = 0; i < sz; i++)
		{
            dst_data[2] = static_cast<float>(u * 2);
            dst_data[5] = static_cast<float>(u / 360.0);
            dst_data[6] = static_cast<float>(u / 360.0);

			//use sin
			_sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1, 1, 1, 1});
		    _sg_functions->image.draw_bitmap( &img, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->image.draw_bitmap( &img2, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->graphics.refresh_window(&window_arr[i]);
		}
    }

    //Test rotate and scale (should rotate around centre)
	for (int u = 0; u <= 720; u += 2)
	{
		for (int i = 0; i < sz; i++)
		{
            dst_data[2] = static_cast<float>(u * 2);
            dst_data[5] = static_cast<float>(u / 360.0);
            dst_data[6] = static_cast<float>((720 - u) / 360.0);
            
			//use sin
			_sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1, 1, 1, 1});
		    _sg_functions->image.draw_bitmap( &img, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->image.draw_bitmap( &img2, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->graphics.refresh_window(&window_arr[i]);
		}
    }

    //Test rotate and scale (should rotate around centre)
	for (int u = 0; u <= 720; u += 2)
	{
		for (int i = 0; i < sz; i++)
		{
            dst_data[2] = static_cast<float>(u * 2);
            dst_data[5] = static_cast<float>(u / 360.0);
            dst_data[6] = static_cast<float>((720 - u) / 360.0);
            
			//use sin
			_sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1, 1, 1, 1});
		    _sg_functions->image.draw_bitmap( &img, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->image.draw_bitmap( &img2, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->graphics.refresh_window(&window_arr[i]);
		}
    }
    
    dst_data[2] = 0;
    dst_data[5] = 1;
    dst_data[6] = 1;
    
	//Test flip
	for (int u = 0; u < 240; u++)
	{
		for (int i = 0; i < sz; i++)
		{
			//use sin
			_sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1, 1, 1, 1});
		    _sg_functions->image.draw_bitmap( &img, &window_arr[i], src_data, 4, dst_data, 7, (sg_renderer_flip)((u/30) % 4));
			_sg_functions->image.draw_bitmap( &img2, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->graphics.refresh_window(&window_arr[i]);
		}
    }
	
	//Test pretty
//	double scale;
	for (int u = 0; u <= 720; u += 4)
	{
		for (int i = 0; i < sz; i++)
		{
//			scale = sin(u/90.0) + 0.5;
//			scale = scale > 0.2 ? scale : 0.2;
			         
            dst_data[2] = u;
            dst_data[3] = 100 + sin(u/60.0)*100;
            dst_data[4] = -u;
            
			//use sin
			_sg_functions->graphics.clear_drawing_surface(&window_arr[i], {1, 1, 1, 1});
		    _sg_functions->image.draw_bitmap( &img, &window_arr[i], src_data, 4, dst_data, 7, SG_FLIP_NONE);
			_sg_functions->graphics.refresh_window(&window_arr[i]);	
		}
    }
    
    _sg_functions->utils.delay(300);
    _sg_functions->input.process_events();
}

bool test_draw_bitmap_without_window()
{
    sg_drawing_surface window;

    cout << "Creating bitmap" << endl;
    
    bmp = _sg_functions->image.create_bitmap(100, 100);

    cout << "Drawing to bitmap" << endl;
    
    _sg_functions->graphics.clear_drawing_surface(&bmp, {1.0f, 0.0f, 0.0f, 1.0f});

    float data_t[] = {0.0f, 99.0f, 99.0f, 99.0f, 50.0f, 1.0f};
    _sg_functions->graphics.fill_triangle(&bmp, {1.0,1.0,1.0,1.0}, data_t, 6 );
    
    float data[] = {0.0f, 20.0f, 80.0f, 20.0f};
    _sg_functions->graphics.fill_aabb_rect(&bmp, {0.0f, 1.0f, 0.0f, 1.0f}, data, 4);
    
    data[1] = 40.0f;
    data[2] = 60.0f;
    _sg_functions->graphics.fill_aabb_rect(&bmp, {0.0f, 0.0f, 1.0f, 1.0f}, data, 4);

    data[1] = 60.0f;
    data[2] = 40.0f;
    _sg_functions->graphics.fill_aabb_rect(&bmp, {1.0f, 0.0f, 1.0f, 1.0f}, data, 4);

    data[1] = 80.0f;
    data[2] = 20.0f;
    _sg_functions->graphics.fill_aabb_rect(&bmp, {1.0f, 1.0f, 0.0f, 1.0f}, data, 4);

    cout << "Saving bitmap" << endl;
    
#ifdef WINDOWS
    _sg_functions->graphics.save_png(&bmp, "c:\\Users\\acain\\Desktop\\test1.png");
#else
    _sg_functions->graphics.save_png(&bmp, "/Users/acain/Desktop/test1.png");
#endif
    float src_data[] = {0, 0, static_cast<float>(bmp.width), static_cast<float>(bmp.height)};
    float dst_data[] = {0, 0, 0, 0, 0, 1, 1};
    
    window = _sg_functions->graphics.open_window("Test Bitmap Drawing", 600, 600);
    _sg_functions->graphics.clear_drawing_surface(&window, {0.0f, 0.0f, 0.0f, 1.0f});
    _sg_functions->image.draw_bitmap( &bmp, &window, src_data, 4, dst_data, 7, SG_FLIP_NONE);
    _sg_functions->graphics.refresh_window(&window);
    
    _sg_functions->input.process_events();
    _sg_functions->utils.delay(2000);
    
#ifdef WINDOWS
    _sg_functions->graphics.save_png(&window, "c:\\Users\\acain\\Desktop\\test2.png");
#else
    _sg_functions->graphics.save_png(&window, "/Users/acain/Desktop/test2.png");
#endif    
    sg_color clr = _sg_functions->graphics.read_pixel(&bmp, 5, 5);
    cout << "Bmp Color is : " << clr.r << ":" << clr.g << ":" << clr.b << ":" << clr.a << endl;

    clr = _sg_functions->graphics.read_pixel(&window, 0, 0);
    cout << "Wnd Color is : " << clr.r << ":" << clr.g << ":" << clr.b << ":" << clr.a << endl;

    
    _sg_functions->graphics.close_drawing_surface(&window);
    
#ifdef WINDOWS
    _sg_functions->graphics.save_png(&bmp, "c:\\Users\\acain\\Desktop\\test3.png");
#else    
    _sg_functions->graphics.save_png(&bmp, "/Users/acain/Desktop/test3.png");
#endif
    
    window = _sg_functions->graphics.open_window("Draw in new window!", 600, 600);

    _sg_functions->input.process_events();
    _sg_functions->graphics.clear_drawing_surface(&window, {0.0f, 0.0f, 0.0f, 1.0f});
    _sg_functions->image.draw_bitmap( &bmp, &window, src_data, 4, dst_data, 7, SG_FLIP_NONE);
    _sg_functions->graphics.refresh_window(&window);
    _sg_functions->utils.delay(2000);
    
    _sg_functions->graphics.close_drawing_surface(&window);

    return true;
}

bool test_basic_drawing(int drawing_test_run)
{
    cout << "Testing Basic Drawing!" << endl;
    cout << drawing_test_run << endl; 
    
    sg_drawing_surface window;
    window = _sg_functions->graphics.open_window("Test Basic Drawing", 800, 600);

    // Create the images
    img = _sg_functions->image.load_bitmap("whac-a-mole-background.png");
    img2 = _sg_functions->image.create_bitmap(100, 50);
    _sg_functions->graphics.clear_drawing_surface(&img2, {1.0f, 0.0f, 0.0f, 1.0f});

    
    if (drawing_test_run & TEST_COLORS) 
    {
        test_colors(&window, 1);
    }

    if (drawing_test_run & TEST_PIXELS) 
    {
        test_read_pixels(&window);
    }

    if (drawing_test_run & TEST_POSITIONS) 
    {
        test_positions(&window, 1);
    }

    if (drawing_test_run & TEST_ALPHA) 
    {
        test_alpha(&window, 1);
    }

    if (drawing_test_run & TEST_CLIP) 
    {
        test_clip( &window, 1);
    }
    
    if (drawing_test_run & TEST_PIXELS) 
    {
        test_pixels( &window, 1);
    }
    
    if (drawing_test_run & TEST_FULLSCREEN) 
    {
        _sg_functions->graphics.show_fullscreen(&window, true);

        test_rects( &window, 1);

        _sg_functions->graphics.show_fullscreen(&window, false);
    }
    
    if (drawing_test_run & TEST_SHAPES) 
    {
        test_triangles( &window, 1);
    }

    if (drawing_test_run & TEST_SHAPES) 
    {
        test_circles( &window, 1);
    }
    
    if (drawing_test_run & TEST_RESIZE) 
    {
        test_resize(&window, 1);
    }
    
    if (drawing_test_run & TEST_SHAPES) 
    {
        test_ellipses( &window, 1);
    }

    if (drawing_test_run & TEST_LINES) 
    {
        test_lines( &window, 1);
    }

    if (drawing_test_run & TEST_BITMAPS)
    {
        // Test the bitmaps - changing img2 in between
        test_bitmaps( &window, 1);
        _sg_functions->graphics.clear_drawing_surface(&img2, {1.0f, 0.0f, 1.0f, 0.2f});
        _sg_functions->graphics.clear_drawing_surface(&window, {1.0f, 1.0f, 1.0f, 1.0f});
        test_bitmaps( &window, 1);
    }
    
    if (drawing_test_run & TEST_INPUT) 
    {
        test_input(&window, 1);
    }

    _sg_functions->graphics.close_drawing_surface(&window);
    
    return false == _sg_functions->has_error;
}

bool test_window_operations()
{
    cout << "Testing Window Operations!" << endl;
	_sg_functions->input.process_events();
	
    sg_drawing_surface w[2];
    w[0] = _sg_functions->graphics.open_window("Window 1", 800, 600);
    w[1] = _sg_functions->graphics.open_window("Window 2", 300, 300);

    _sg_functions->input.move_window(&w[1], 0, 0);


    _sg_functions->graphics.show_border(&w[0], false);

    if ( w[0].width != 800 ) cout << " >> Error with w[0] width! " << w[0].width << endl;
    if ( w[1].width != 300 ) cout << " >> Error with w[1] width! " << w[1].width << endl;

    if ( w[0].height != 600 ) cout << " >> Error with w[0] height! " << w[0].height << endl;
    if ( w[1].height != 300 ) cout << " >> Error with w[1] height! " << w[1].height << endl;

    test_colors(w, 2);
    test_clip(w, 2);
    test_pixels(w, 2);
    test_rects(w, 2);
    test_triangles(w, 2);
    test_circles(w, 2);
    test_ellipses(w, 2);
    test_lines(w, 2);
    test_bitmaps(w, 2);

    test_input(w, 2);

    _sg_functions->graphics.close_drawing_surface(&w[0]);
    _sg_functions->graphics.close_drawing_surface(&w[1]);

    return true;
}

bool test_bitmap_dest_drawing()
{
    cout << "Testing Drawing to Bitmap!" << endl;
    
    sg_drawing_surface window;
    window = _sg_functions->graphics.open_window("Drawing to Bitmap", 800, 600);
    _bmp_wnd = &window;
    
    sg_drawing_surface bmp = _sg_functions->image.create_bitmap(640, 480);
    
    float src_data[] = {0, 0, static_cast<float>(img.width), static_cast<float>(img.height)};
    float dst_data[] = {0, 0, 0, 0, 0, 1, 1};
    
    _sg_functions->image.draw_bitmap( &img, &bmp, src_data, 4, dst_data, 7, SG_FLIP_NONE);
    
    refresh_or_draw(&bmp);
    _sg_functions->utils.delay(3000);
    
    test_colors(&bmp, 1);
    test_positions(&bmp, 1);
    test_alpha(&bmp, 1);
    
    test_clip( &bmp, 1);
    test_pixels( &bmp, 1);
    
    test_rects( &bmp, 1);
    
    test_triangles( &bmp, 1);
    test_circles( &bmp, 1);
    
    test_ellipses( &bmp, 1);
    test_lines( &bmp, 1);

    dst_data[0] = 50;
    dst_data[1] = 50;
    
    src_data[2] = img2.width;
    src_data[3] = img2.height;
    
    
    _sg_functions->image.draw_bitmap( &img, &bmp, src_data, 4, dst_data, 7, SG_FLIP_NONE);
    refresh_or_draw(&bmp);
    _sg_functions->utils.delay(3000);
    
    _sg_functions->graphics.close_drawing_surface(&bmp);
    _sg_functions->graphics.close_drawing_surface(&window);
    _bmp_wnd = NULL;
    
    return false == _sg_functions->has_error;
}

void test_bitmap_loading_saving()
{
    sg_drawing_surface lines = _sg_functions->image.load_bitmap("Lines.png");
    int sz = lines.width * lines.height;
    int pixels[sz];
    
    for (int i = 0; i < sz; i++) pixels[i] = 0;
    
    _sg_functions->graphics.to_pixels(&lines, pixels, sz);
    
    for (int i = 0; i < sz; i++)
    {
        cout << std::hex << pixels[i] << std::dec << " ";
        if ((i+1) % lines.width == 0)
        {
            cout << endl;
        }
    }
    
    sg_color clr = _sg_functions->graphics.read_pixel(&lines, 0, 0);
    cout << "Lines color is : " << clr.r << ":" << clr.g << ":" << clr.b << ":" << clr.a << endl;
    
    _sg_functions->graphics.save_png(&lines, "/Users/acain/Desktop/test.png");
    
    _sg_functions->graphics.close_drawing_surface(&lines);
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

void test_fullscreen()
{
    cout << "Calling load_sg..." << endl;
    _sg_functions = sg_load(get_input_callbacks());
    
    if ( !_sg_functions )
    {
        cout << "Failed to load functions!" << endl;
        return;
    }
    
    cout << "Calling init..." << endl;
    _sg_functions->init();

    sg_drawing_surface window;
    window = _sg_functions->graphics.open_window("Test Basic Drawing", 800, 600);
    
    _sg_functions->graphics.show_fullscreen(&window, false);
    
    _sg_functions->graphics.clear_drawing_surface(&window, {1.0f, 1.0f, 1.0f, 1.0f});
    _sg_functions->graphics.refresh_window(&window);
    _sg_functions->input.process_events();
    _sg_functions->utils.delay(2000);
    
    _sg_functions->graphics.show_fullscreen(&window, true);
    
    //        test_rects( &window, 1);
    _sg_functions->graphics.clear_drawing_surface(&window, {1.0f, 1.0f, 1.0f, 1.0f});
    _sg_functions->graphics.refresh_window(&window);
    _sg_functions->input.process_events();
    _sg_functions->utils.delay(2000);
    
    _sg_functions->graphics.show_fullscreen(&window, false);
    
    _sg_functions->graphics.clear_drawing_surface(&window, {1.0f, 0.0f, 0.0f, 1.0f});
    _sg_functions->graphics.refresh_window(&window);
    _sg_functions->input.process_events();
    _sg_functions->utils.delay(2000);
    
    _sg_functions->graphics.close_drawing_surface(&window);
}

int main(int argc, const char * argv[])
{
    srand ((unsigned int)time(NULL));
    cout << "Starting driver backend test" << endl;

    if ( ! test_core_functions() )
    {
        cout << "Core functions failed with error: " << endl;
        cout << _sg_functions->current_error << endl;
        return -1;
    }

    test_bitmap_loading_saving();

    cout << " Which tests do you want to run? " << endl; 
    print_options(); 

    int test_run = 0; 
    int test_drawing_run = INT_MAX; 
    scanf("%d", &test_run);

    if (test_run == 0) 
    {
      test_run |= 255; 
    }
    else if (test_run & BASIC_DRAWING) 
    {
      cout << "Which drawing functions would you like to run? " << endl; 
      print_drawing_options(); 
      scanf("%d", &test_drawing_run); 
    }

    output_system_details();

    if (test_run & BASIC_DRAWING && ! test_draw_bitmap_without_window() )
    {
        cout << "Drawing to bitmap without window failed..." << endl;
        return -1;
    }

    if (test_run & BASIC_DRAWING && ! test_basic_drawing(test_drawing_run) )
    {
        cout << "Basic drawing failed with error: " << endl;
        //cout << _sg_functions->current_error << endl;
        return -1;
    }
    
    _sg_functions->input.process_events();
    

    if (test_run & WINDOW_OPERATIONS) 
    {
        test_window_operations();
    }
    
    if (test_run & BITMAP_DRAWING) 
    {
        test_bitmap_dest_drawing();
        test_bitmap_loading_saving();
    }
    
    _sg_functions->graphics.close_drawing_surface(&img);
    _sg_functions->graphics.close_drawing_surface(&img2);
    
    if (test_run & AUDIO) 
    {
      test_audio();
    }

    if (test_run & INPUT)
    {
        sg_drawing_surface w[1];
        w[0] = _sg_functions->graphics.open_window("Window 1", 800, 600);
        test_input(w, 1);
    }

    if  (test_run & TEXT) 
    {
        test_text();
    }

    if (test_run & NETWORK)
    {
        test_network();
    }
    
    _sg_functions->finalise();
    cout << "Success" << endl;
    
    return 0;
}

