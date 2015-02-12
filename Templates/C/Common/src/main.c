#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

int main()
{
    open_graphics_window("Hello World", 800, 600);
    show_swin_game_splash_screen();
    
    do
    {
        process_events();
        
        clear_screen_to(ColorWhite);
        
        draw_framerate_with_simple_font(0,0);
        
        refresh_screen_restrict_fps(60);
    } while ( ! window_close_requested() );
    
    return 0;
}
