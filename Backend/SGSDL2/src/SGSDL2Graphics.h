//
//  SGSDL2Graphics.h
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__SGSDL2Graphics__
#define __sgsdl2__SGSDL2Graphics__

#ifdef __linux__
#include <SDL2/SDL.h>
#else
#include <SDL.h>
#endif

#include "sgBackendTypes.h"
#include "sgInterfaces.h"


void sgsdl2_load_graphics_fns(sg_interface *functions);
void sgsdl2_load_image_fns(sg_interface *functions);
void sgsdl2_finalise_graphics();

unsigned int _sgsdl2_renderer_count(sg_drawing_surface *surface);
SDL_Renderer * _sgsdl2_prepared_renderer(sg_drawing_surface* surface, unsigned int idx);
void _sgsdl2_complete_render(sg_drawing_surface* surface, unsigned int idx);

typedef struct sg_window_be
{
    SDL_Window *    window;
    SDL_Renderer *  renderer;
    SDL_Texture *   backing;
    bool            clipped;
    SDL_Rect        clip;
    unsigned int    idx;
    
    // Event data store
    sg_window_data  event_data;
    sg_drawing_surface *surface;
} sg_window_be;

typedef struct sg_bitmap_be
{
    // 1 texture per open window
    SDL_Texture **  texture;
    SDL_Surface *   surface;
    bool            clipped;
    SDL_Rect        clip;
    
    bool            drawable; // can be drawn on
} sg_bitmap_be;


sg_window_be *_sgsdl2_get_window_with_id(unsigned int window_id);
sg_window_be *_sgsdl2_get_window_with_pointer(pointer p);


#endif /* defined(__sgsdl2__SGSDL2Graphics__) */
