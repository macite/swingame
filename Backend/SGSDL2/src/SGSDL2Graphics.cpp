//
//  SGSDL2Graphics.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "SGSDL2Graphics.h"

#include "SDL.h"
#include "SDL2_gfxPrimitives.h"
#include "sgBackendUtils.h"

typedef struct sg_window_be
{
    SDL_Window *    window;
    SDL_Renderer *  renderer;
} sg_window_be;


sg_drawing_surface sgsdl2_open_window(const char *title, int width, int height)
{
    sg_drawing_surface  result = { SGDS_Unknown, NULL };

    sg_window_be *      window_be;
    
    window_be = (sg_window_be *) malloc(sizeof(sg_window_be));
    
    if ( ! window_be )
    {
        set_error_state("Unable to open window: Out of memory");
        return result;
    }

    window_be->window = SDL_CreateWindow(title,
                                         SDL_WINDOWPOS_CENTERED,
                                         SDL_WINDOWPOS_CENTERED,
                                         width,
                                         height,
                                         SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN );
    
    if ( ! window_be->window)
    {
        set_error_state(SDL_GetError());
        return result;
    }
    
    window_be->renderer = SDL_CreateRenderer(window_be->window,
                                             -1,
                                             SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC );
    
    SDL_SetRenderDrawColor(window_be->renderer, 120, 120, 120, 255);
    SDL_RenderClear(window_be->renderer);
    SDL_RenderPresent(window_be->renderer);
    
    result.kind = SGDS_Window;
    
    result.width = width;
    result.height = height;

    result._data = window_be;

    return result;
}

void sgsdl2_close_window(sg_drawing_surface *window)
{
    sg_window_be * window_be;
    window_be = (sg_window_be *)window->_data;
    
    if ( window_be )
    {
        SDL_DestroyRenderer(window_be->renderer);
        SDL_DestroyWindow(window_be->window);
        
        window_be->renderer = NULL;
        window_be->window = NULL;
        free(window_be);
        
        window->_data = NULL;
    }
}

void sgsdl2_close_drawing_surface(sg_drawing_surface *surface)
{
    if ( ! surface )
    {
        set_error_state("No surface provided to close_drawing_surface");
        return;
    }
    
    switch (surface->kind)
    {
        case SGDS_Window:
            sgsdl2_close_window(surface);
            break;
            
        default:
            break;
    }
}

void sgsdl2_set_renderer_color(sg_window_be *window_be, color clr)
{
    if ( window_be && window_be->renderer )
    {
        SDL_SetRenderDrawColor(window_be->renderer, clr.r * 255, clr.g * 255, clr.b * 255, clr.a * 255);
    }
}

void sgsdl2_clear_window(sg_drawing_surface *window, color clr)
{
    sg_window_be * window_be;
    window_be = (sg_window_be *)window->_data;
    
    if ( window_be )
    {
        sgsdl2_set_renderer_color(window_be, clr);
        SDL_RenderClear(window_be->renderer);
    }
}

void sgsdl2_clear_drawing_surface(sg_drawing_surface *surface, color clr)
{
    if ( ! surface ) return;
    
    switch (surface->kind)
    {
        case SGDS_Window:
            sgsdl2_clear_window(surface, clr);
            break;
            
        default:
            break;
    }
}

void sgsdl2_refresh_window(sg_drawing_surface *window)
{
    if ( (! window) || window->kind != SGDS_Window ) return;
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)window->_data;
    
    if ( window_be )
    {
        SDL_RenderPresent(window_be->renderer);
    }
}


//
// Surface ops
//

Uint32 _to_gfx_color(color clr)
{
    byte r, g, b, a;
    
    r = 255 * clr.r;
    g = 255 * clr.g;
    b = 255 * clr.b;
    a = 255 * clr.a;
    
    return (r << 24) | (g << 16) | (b << 8) | a;
}



//
//  Rectangles
//

void sgsdl2_draw_aabb_rect(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface ) return;
    if ( data_sz != 4 ) return;
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    if ( window_be )
    {
        SDL_Rect rect = { (int)data[0], (int)data[1], (int)data[2], (int)data[3] };
        sgsdl2_set_renderer_color(window_be, clr);
        
        SDL_RenderDrawRect(window_be->renderer, &rect);
    }
}

void sgsdl2_fill_aabb_rect(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface ) return;
    if ( data_sz != 4 ) return;
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    if ( window_be )
    {
        SDL_Rect rect = { (int)data[0], (int)data[1], (int)data[2], (int)data[3] };
        sgsdl2_set_renderer_color(window_be, clr);
        
        SDL_RenderFillRect(window_be->renderer, &rect);
    }
}


//
//  Triangles
//

void sgsdl2_draw_triangle(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 6) return;

    // 6 values = 3 points
    int x1 = (int)data[0], y1 = (int)data[1];
    int x2 = (int)data[2], y2 = (int)data[3];
    int x3 = (int)data[4], y3 = (int)data[4];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
            sgsdl2_set_renderer_color(window_be, clr);
            SDL_RenderDrawLine(window_be->renderer, x1, y1, x2, y2);
            SDL_RenderDrawLine(window_be->renderer, x2, y2, x3, y3);
            SDL_RenderDrawLine(window_be->renderer, x3, y3, x1, y1);
            break;
            
        default:
            break;
    }
}

void sgsdl2_fill_triangle(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 6) return;
    
    // 6 values = 3 points
    float x1 = data[0], y1 = data[1];
    float x2 = data[2], y2 = data[3];
    float x3 = data[4], y3 = data[4];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
            filledTrigonColor(window_be->renderer,
                              x1, y1,
                              x2, y2,
                              x3, y3,
                              _to_gfx_color(clr)
                              );
            break;
            
        default:
            break;
    }
    
}

//
// Pixel
//

void sgsdl2_draw_pixel(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 2) return;
    
    // 2 values = 1 point
    int x1 = (int)data[0], y1 = (int)data[1];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
        {
            SDL_Rect rect = { x1, y1, 1, 1 };
            
            sgsdl2_set_renderer_color(window_be, clr);
            
            SDL_RenderFillRect(window_be->renderer, &rect);

// For some reason the following does not work :(
// when multisample is 1, but without multisample 1
// double buffer causes flicker
//
//            sgsdl2_set_renderer_color(window_be, clr);
//            SDL_RenderDrawPoint(window_be->renderer, x1, y1);
            break;
        }
        default:
            break;
    }
    
}

void sgsdl2_load_graphics_fns(sg_interface * functions)
{
    functions->graphics.open_window = &sgsdl2_open_window;
    functions->graphics.close_drawing_surface = &sgsdl2_close_drawing_surface;
    functions->graphics.refresh_window = &sgsdl2_refresh_window;
    functions->graphics.clear_drawing_surface = &sgsdl2_clear_drawing_surface;
    functions->graphics.draw_aabb_rect = &sgsdl2_draw_aabb_rect;
    functions->graphics.fill_aabb_rect = &sgsdl2_fill_aabb_rect;
    functions->graphics.draw_triangle = &sgsdl2_draw_triangle;
    functions->graphics.fill_triangle = &sgsdl2_fill_triangle;
    functions->graphics.draw_pixel = &sgsdl2_draw_pixel;
}

