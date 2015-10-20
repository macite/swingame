//
//  test_text.h
//  sgsdl2
//
//  Created by James Armstrong on 11/12/2013.
//

#include "test_text.h"

#include <iostream>

using namespace std;

#include "sgInterfaces.h"

extern sg_interface * _sg_functions;

void test_draw_text(sg_font_data *font, float x, float y, sg_font_style style, sg_drawing_surface *window)
{
    _sg_functions->text.set_font_style(font, style);
    int font_style = _sg_functions->text.get_font_style(font); 
    cout << " The font style is: " << font_style <<  endl; 

    cout << "Drawing text on screen" << endl;
    _sg_functions->text.draw_text(window, font, x + 400, y, "AWESOME!", {0.0, 1.0, 0.0, 1.0});

    sg_drawing_surface bitmap = _sg_functions->image.create_bitmap(400, 400);
    _sg_functions->graphics.clear_drawing_surface(&bitmap, {0.0, 0.0, 0.0, 0.0});
    _sg_functions->text.draw_text(&bitmap, font, 0, 0, "AWESOME!", {0.0, 1.0, 0.0, 1.0});

    float src_data[] = {0, 0, 400, 400};
    float dst_data[] = {x, y, 0, 0, 0, 1, 1};
    _sg_functions->image.draw_bitmap(&bitmap, window, src_data, 4, dst_data, 7, SG_FLIP_NONE);

    _sg_functions->graphics.refresh_window(window);
    _sg_functions->utils.delay(1000);

    _sg_functions->graphics.close_drawing_surface(&bitmap);
}

void test_basic_text(sg_drawing_surface *window)
{
    cout <<  "Testing text " << endl;
    sg_font_data font =  _sg_functions->text.load_font("BATTLEST.TTF", 54);
    
    cout << " Loaded " << font._data << endl;
    
    int line_skip = _sg_functions->text.text_line_skip(&font);
    cout << " The line skip is: " << line_skip << endl;
    
    int w;
    int h;
    int text_size = _sg_functions->text.text_size(&font,(char*)"Hello World", &w, &h);
    cout << "Result for text size is : " << text_size << " with a width of: " << w << " and a height of: " << h << endl;
    
    cout << "Testing drawing of different font styles" << endl;
    test_draw_text(&font, 0, 100, SG_FONT_STYLE_NORMAL, window);
    test_draw_text(&font, 0, 200, SG_FONT_STYLE_BOLD, window);
    test_draw_text(&font, 0, 300, SG_FONT_STYLE_ITALIC, window);
    test_draw_text(&font, 0, 400, SG_FONT_STYLE_UNDERLINE, window);
    test_draw_text(&font, 0, 500, SG_FONT_STYLE_STRIKETHROUGH, window);
    
    cout << "Closing same font twice." << endl;
    _sg_functions->text.close_font(&font);
    _sg_functions->text.close_font(&font);
}

void test_memory_of_draw_text(sg_drawing_surface *window)
{
    sg_font_data font =  _sg_functions->text.load_font("BATTLEST.TTF", 16);
    
    cout << "Hit q key to quit" << endl;
    
    char line[256] = "";
    int i = 0;
    
    while ( ! (_sg_functions->input.key_pressed( (int)'q' ) || _sg_functions->input.window_close_requested(window)) )
    {
        i++;
        
        _sg_functions->input.process_events();
        
        sprintf(line, "Iteration %d", i);

        _sg_functions->graphics.clear_drawing_surface(window, {1.0, 1.0, 1.0, 1.0});
        _sg_functions->text.draw_text(window, &font, 10, 10, line, {0.0, 0.0, 0.0, 1.0});
        
        _sg_functions->graphics.refresh_window(window);
    }
}

void test_text()
{
    sg_drawing_surface window = _sg_functions->graphics.open_window("Text", 800, 600);

    test_basic_text(&window);
    
    test_memory_of_draw_text(&window);

    cout << "Closing text window" << endl;
    _sg_functions->graphics.close_drawing_surface(&window);
}

