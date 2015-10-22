//
//  SGSDL2Text.h
//  sgsdl2
//
//  Created by James Armstrong on 11/12/2013.
//

#include <iostream>

#ifdef __linux__
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL2_gfxPrimitives.h>
#else
#include <SDL.h>
#include <SDL_ttf.h>
#include <SDL2_gfxPrimitives.h>
#endif

#include "SGSDL2Text.h"
#include "SGSDL2Graphics.h"
#include "sgBackendTypes.h"

void sgsdl2_init_text()
{
    if (TTF_Init() == -1)
    {
        std::cout << "Text loading is broken." << std::endl;
        exit(-1);
    }
}

void sgsdl2_finalize_text()
{
    TTF_Quit();
}

sg_font_data sgsdl2_load_font(const char * filename, int font_size)
{
    sg_font_data font;
    font.kind = SGFT_TTF;
    font._data = TTF_OpenFont(filename, font_size);
    return font;
}

void sgsdl2_close_font(sg_font_data* font)
{
    if (font && font->_data) 
    {
        TTF_CloseFont((TTF_Font*)font->_data);
        font->kind = SGFT_UNKNOWN; 
        font->_data = NULL; 
    }
}

int sgsdl2_text_line_skip(sg_font_data* font) 
{ 
    if (font && font->_data)
        return TTF_FontLineSkip((TTF_Font*)font->_data);
    else
        return 0;
}

int sgsdl2_text_size(sg_font_data* font, char* text, int* w, int* h) 
{
    if (font && font->_data)
        return TTF_SizeText((TTF_Font*)font->_data, text, w, h); 
    else
        return 0;
}

void sgsdl2_set_font_style(sg_font_data* font,int style) 
{
    if (font && font->_data)
        TTF_SetFontStyle((TTF_Font*)font->_data, style); 
}

int sgsdl2_get_font_style(sg_font_data* font) 
{
    if (font && font->_data)
        return TTF_GetFontStyle((TTF_Font*)font->_data); 
    else
        return 0;
}

void _sgsdl2_draw_bitmap_text( sg_drawing_surface * surface,
                               float x, float y,
                               const char * text,
                               sg_color clr )
{
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        stringRGBA(renderer,
                   static_cast<Sint16>(x),
                   static_cast<Sint16>(y),
                   text,
                   static_cast<Uint8>(clr.r * 255),
                   static_cast<Uint8>(clr.g * 255),
                   static_cast<Uint8>(clr.b * 255),
                   static_cast<Uint8>(clr.a * 255) );
        _sgsdl2_complete_render(surface, i);
    }
        
}
    
void sgsdl2_draw_text(
        sg_drawing_surface * surface, 
        sg_font_data* font, 
        float x, float y, 
        const char * text, 
        sg_color clr)
{
    if (!font) // draw bitmap based text -- no font
    {
        _sgsdl2_draw_bitmap_text(surface, x, y, text, clr);
        return;
    }
    if (! (font && font->_data)) return; // error with font
    
    SDL_Surface * text_surface = NULL;
    SDL_Texture * text_texture = NULL;

    SDL_Color sdl_color;
    sdl_color.r = static_cast<Uint8>(clr.r * 255);
    sdl_color.g = static_cast<Uint8>(clr.g * 255);
    sdl_color.b = static_cast<Uint8>(clr.b * 255);
    sdl_color.a = static_cast<Uint8>(clr.a * 255);
    
    text_surface = TTF_RenderText_Blended((TTF_Font*)font->_data, text, sdl_color);

    if (text_surface == NULL)
    {
        // fail
    }
    else
    {
        unsigned int count = _sgsdl2_renderer_count(surface);

        for (unsigned int i = 0; i < count; i++)
        {
            SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
            text_texture = SDL_CreateTextureFromSurface(renderer, text_surface);
            if (text_texture == NULL)
            {
                // fail
            }
            else
            {
                SDL_Rect rect;
                rect.x = (int)x;
                rect.y = (int)y;
                rect.w = text_surface->w;
                rect.h = text_surface->h;

                SDL_RenderCopy(renderer, text_texture, NULL, &rect);

                _sgsdl2_complete_render(surface, i);
                
                SDL_DestroyTexture(text_texture);
            }
        }
        SDL_FreeSurface(text_surface);
    }
}

void sgsdl2_load_text_fns(sg_interface *functions)
{
    functions->text.load_font = &sgsdl2_load_font;
    functions->text.close_font = &sgsdl2_close_font;
    functions->text.text_line_skip = &sgsdl2_text_line_skip; 
    functions->text.text_size = &sgsdl2_text_size; 
    functions->text.get_font_style = &sgsdl2_get_font_style; 
    functions->text.set_font_style = &sgsdl2_set_font_style; 
    functions->text.draw_text = &sgsdl2_draw_text;
}
