#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

int main()
{
    open_audio();
    open_graphics_window("Hello World", 800, 600);
    load_default_colors();
    
    do
    {
        process_events();
        
        clear_screen_to(ColorWhite);
        
        draw_framerate_with_simple_font(0,0);
        
        refresh_screen();
    } while ( ! window_close_requested() );
    
    close_audio();
    
    release_all_resources();
    return 0;
}
