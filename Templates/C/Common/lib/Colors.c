#include "Colors.h"
#include "Graphics.h"

color ColorBlue     = 0xff0000ff;
color ColorGreen    = 0xff00ff00;
color ColorRed      = 0xffff0000;
color ColorWhite    = 0xffffffff;
color ColorBlack    = 0xff000000;
color ColorYellow   = 0xffffff00;
color ColorPink     = 0xffff1493;
color ColorTurquoise= 0xffced1ff;
color ColorGrey     = 0xff808080;
color ColorMagenta  = 0xffff00ff;
color ColorTransparent = 0x0;
color ColorLightGrey   = 0xffc8c8c8;

color color_blue     = 0xff0000ff;
color color_green    = 0xff00ff00;
color color_red      = 0xffff0000;
color color_white    = 0xffffffff;
color color_black    = 0xff000000;
color color_yellow   = 0xffffff00;
color color_pink     = 0xffff1493;
color color_turquoise= 0xffced1ff;
color color_grey     = 0xff808080;
color color_magenta  = 0xffff00ff;
color color_transparent = 0x0;
color color_light_grey   = 0xffc8c8c8;


void load_default_colors()
{
    ColorBlue =         rgbacolor(0, 0, 255, 255);
    ColorGreen =        rgbacolor(0, 255, 0, 255);
    ColorRed =          rgbacolor(255, 0, 0, 255);
    ColorWhite =        rgbacolor(255, 255, 255, 255);
    ColorBlack =        rgbacolor(0, 0, 0, 255);
    ColorYellow =       rgbacolor(255, 255, 0, 255);
    ColorPink =         rgbacolor(255, 20, 147, 255);
    ColorTurquoise =    rgbacolor(0, 206, 209, 255);
    ColorGrey =         rgbacolor(128, 128, 128, 255);
    ColorMagenta =      rgbacolor(255, 0, 255, 255);
    ColorTransparent =  rgbacolor(0, 0, 0, 0);
    ColorLightGrey =    rgbacolor(200, 200, 200, 255);
    
    color_blue         = rgbacolor(0, 0, 255, 255);
    color_green        = rgbacolor(0, 255, 0, 255);
    color_red          = rgbacolor(255, 0, 0, 255);
    color_white        = rgbacolor(255, 255, 255, 255);
    color_black        = rgbacolor(0, 0, 0, 255);
    color_yellow       = rgbacolor(255, 255, 0, 255);
    color_pink         = rgbacolor(255, 20, 147, 255);
    color_turquoise    = rgbacolor(0, 206, 209, 255);
    color_grey         = rgbacolor(128, 128, 128, 255);
    color_magenta      = rgbacolor(255, 0, 255, 255);
    color_transparent  = rgbacolor(0, 0, 0, 0);
    color_light_grey   = rgbacolor(200, 200, 200, 255);
}
