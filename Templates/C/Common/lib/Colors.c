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

color COLOR_BLUE         = 0xff0000ff;
color COLOR_GREEN        = 0xff00ff00;
color COLOR_RED          = 0xffff0000;
color COLOR_WHITE        = 0xffffffff;
color COLOR_BLACK        = 0xff000000;
color COLOR_YELLOW       = 0xffffff00;
color COLOR_PINK         = 0xffff1493;
color COLOR_TURQUOISE    = 0xffced1ff;
color COLOR_GREY         = 0xff808080;
color COLOR_MAGENTA      = 0xffff00ff;
color COLOR_TRANSPARENT  = 0x0;
color COLOR_LIGHTGREY    = 0xffc8c8c8;

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
    color_blue         = COLOR_BLUE         =  ColorBlue =         rgbacolor(0, 0, 255, 255);
    color_green        = COLOR_GREEN        =  ColorGreen =        rgbacolor(0, 255, 0, 255);
    color_red          = COLOR_RED          =  ColorRed =          rgbacolor(255, 0, 0, 255);
    color_white        = COLOR_WHITE        =  ColorWhite =        rgbacolor(255, 255, 255, 255);
    color_black        = COLOR_BLACK        =  ColorBlack =        rgbacolor(0, 0, 0, 255);
    color_yellow       = COLOR_YELLOW       =  ColorYellow =       rgbacolor(255, 255, 0, 255);
    color_pink         = COLOR_PINK         =  ColorPink =         rgbacolor(255, 20, 147, 255);
    color_turquoise    = COLOR_TURQUOISE    =  ColorTurquoise =    rgbacolor(0, 206, 209, 255);
    color_grey         = COLOR_GREY         =  ColorGrey =         rgbacolor(128, 128, 128, 255);
    color_magenta      = COLOR_MAGENTA      =  ColorMagenta =      rgbacolor(255, 0, 255, 255);
    color_transparent  = COLOR_TRANSPARENT  =  ColorTransparent =  rgbacolor(0, 0, 0, 0);
    color_light_grey   = COLOR_LIGHTGREY    =  ColorLightGrey =    rgbacolor(200, 200, 200, 255);
}
