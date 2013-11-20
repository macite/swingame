//
//  SGSDL2Graphics.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "SGSDL2Graphics.h"

#include "SDL.h"
#include "sgBackendUtils.h"

typedef struct sg_window_be
{
    SDL_Window *    window;
    SDL_Renderer *  renderer;
} sg_window_be;


sg_drawing_surface sgsdl2_open_window(const char *title, int width, int height)
{
    sg_drawing_surface  result = { SGDS_Unknown, NULL};

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
                                         SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_ALLOW_HIGHDPI );
    
    if ( ! window_be->window)
    {
        set_error_state(SDL_GetError());
        return result;
    }
    
    window_be->renderer = SDL_CreateRenderer(window_be->window,
                                             -1,
                                             SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    
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

void sgsdl2_load_graphics_fns(sg_interface * functions)
{
    functions->graphics.open_window = &sgsdl2_open_window;
    functions->graphics.close_drawing_surface = &sgsdl2_close_drawing_surface;
    functions->graphics.refresh_window = &sgsdl2_refresh_window;
    functions->graphics.clear_drawing_surface = &sgsdl2_clear_drawing_surface;

}

