//
//  SGSDL2Graphics.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include <limits.h>
#include <iostream>

#ifdef __linux__
#include <SDL2/SDL.h>
#include <SDL2/SDL2_gfxPrimitives.h>
#include <SDL2/SDL_image.h>
#else
#include <SDL.h>
#include <SDL2_gfxPrimitives.h>
#include <SDL_image.h>
#endif

#include "SGSDL2Graphics.h"
#include "sgBackendUtils.h"


static sg_window_be ** _sgsdl2_open_windows = NULL;
static unsigned int _sgsdl2_num_open_windows = 0;

static sg_bitmap_be ** _sgsdl2_open_bitmaps = NULL;
static unsigned int _sgsdl2_num_open_bitmaps = 0;

//
// Misc
//
sg_window_be *_sgsdl2_get_window_with_id(unsigned int window_id)
{
    SDL_Window *window = SDL_GetWindowFromID(window_id);
    
    for (unsigned int i = 0; i < _sgsdl2_num_open_windows; i++)
    {
        if (window == _sgsdl2_open_windows[i]->window)
        {
            return _sgsdl2_open_windows[i];
        }
    }
    
    return NULL;
}


//--------------------------------------------------------------------------------------
//
// Functions to work with renderer targets - switching targets etc
//
//--------------------------------------------------------------------------------------

void _sgsdl2_restore_default_render_target(sg_window_be *window_be, sg_bitmap_be *from_bmp)
{
    SDL_SetRenderTarget(window_be->renderer, window_be->backing);
    SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
    if ( window_be->clipped )
    {
        SDL_RenderSetClipRect(window_be->renderer, &window_be->clip);
    }
    else if ( from_bmp && from_bmp->clipped )
    {
        SDL_RenderSetClipRect(window_be->renderer, NULL);
    }
}

void _sgsdl2_restore_default_render_target(unsigned int window_idx, sg_bitmap_be *from_bmp)
{
    _sgsdl2_restore_default_render_target(_sgsdl2_open_windows[window_idx], from_bmp);
}

void _sgsdl2_set_renderer_target(unsigned int window_idx, sg_bitmap_be *target)
{
    sg_window_be * window_be = _sgsdl2_open_windows[window_idx];
    SDL_SetRenderTarget(window_be->renderer, target->texture[window_idx]);
    SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
    if ( target->clipped )
    {
        SDL_RenderSetClipRect(window_be->renderer, &target->clip);
    }
}

void _sgsdl2_make_drawable(sg_bitmap_be *bitmap)
{
    // recreate all textures with target access
    
    int access, w, h;
    
    for (unsigned int i = 0; i < _sgsdl2_num_open_windows; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_open_windows[i]->renderer;
        
        SDL_Texture *orig_tex = bitmap->texture[i];
        
        SDL_QueryTexture(orig_tex, NULL, &access, &w, &h);
        
        if ( access == SDL_TEXTUREACCESS_TARGET ) continue; // already target
        
        // Create new texture
        SDL_Texture *tex = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, w, h);
        bitmap->texture[i] = tex;
        
        // Draw onto new texture
        SDL_SetRenderTarget(renderer, tex);
        SDL_RenderCopy(renderer, orig_tex, NULL, NULL);
        
        // Destroy old
        SDL_DestroyTexture(orig_tex);
        
        _sgsdl2_restore_default_render_target(_sgsdl2_open_windows[i], bitmap);
    }
    
    // Remove surface
    SDL_FreeSurface(bitmap->surface);
    bitmap->surface = NULL;
    
    // Set drawable
    bitmap->drawable = true;
}


//--------------------------------------------------------------------------------------
//
// Window and Bitmap store functions
//
//--------------------------------------------------------------------------------------

static bool _sgsdl2_has_initial_window = false;
static sg_window_be * _sgsdl2_initial_window = NULL;

// The initial window is a hidden window that is always "open"
// This allows drawing without the user having to open a window initially.
void _sgsdl2_create_initial_window()
{
    _sgsdl2_has_initial_window = true;
    _sgsdl2_initial_window = (sg_window_be *) malloc(sizeof(sg_window_be));
    _sgsdl2_initial_window->window = SDL_CreateWindow("",
                                         0, 0, 1, 1,
                                         SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN | SDL_WINDOW_BORDERLESS); //TODO: Log SDL issue re drawing using hidden windows
    
    if ( ! _sgsdl2_initial_window->window )
    {
        set_error_state(SDL_GetError());
        exit(-1);
    }
    
    _sgsdl2_initial_window->renderer = SDL_CreateRenderer(_sgsdl2_initial_window->window,
                                                         -1,
                                                         SDL_RENDERER_ACCELERATED | SDL_RENDERER_TARGETTEXTURE );
    
    SDL_SetRenderDrawBlendMode(_sgsdl2_initial_window->renderer, SDL_BLENDMODE_BLEND);
    
//    std::cout << "Initial Renderer is " << _sgsdl2_initial_window->renderer << std::endl;
    
    // The user cannot draw onto this window!
    _sgsdl2_initial_window->backing = NULL;
    
    _sgsdl2_initial_window->close_requested = false;
    _sgsdl2_initial_window->has_focus = false;
    _sgsdl2_initial_window->mouse_over = false;
    _sgsdl2_initial_window->shown = false;
    
    _sgsdl2_initial_window->clipped = false;
    _sgsdl2_initial_window->clip = {0,0,0,0};
    
    // Add the first window
    if (_sgsdl2_open_windows)
    {
        // error windows exist!
        exit(-1);
    }
    _sgsdl2_open_windows = (sg_window_be**)malloc(sizeof(sg_window_be *));
    _sgsdl2_open_windows[0] = _sgsdl2_initial_window;
    _sgsdl2_initial_window->idx = 0;
    _sgsdl2_num_open_windows = 1;
}

SDL_Texture* _sgsdl2_copy_texture(SDL_Texture *src_tex, SDL_Renderer *src_renderer, SDL_Renderer *dest_renderer) 
{	
	// Read from initial window
	void *pixels;
	int w, h;

	SDL_QueryTexture(src_tex, NULL, NULL, &w, &h);
	pixels = malloc(static_cast<size_t>(4 * w * h));

	SDL_SetRenderTarget(src_renderer, src_tex);
	SDL_RenderReadPixels(src_renderer, NULL, SDL_PIXELFORMAT_RGBA8888, pixels, 4 * w);

	SDL_Texture *tex = SDL_CreateTexture(dest_renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, w, h);
	SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);

	SDL_UpdateTexture(tex, NULL, pixels, 4 * w);
	free(pixels);

	// Restore default target
	SDL_SetRenderTarget(src_renderer, NULL);
	
	return tex;
}

void _sgsdl2_create_texture_for_bitmap_window(sg_bitmap_be *current_bmp, unsigned int src_window_idx, unsigned int dest_window_idx) 
{
	sg_window_be *window = _sgsdl2_open_windows[dest_window_idx];
	
	// if the surface exists, use that to create the new bitmap... otherwise extract from texture
	if (current_bmp->surface)
	{
		current_bmp->texture[dest_window_idx] = SDL_CreateTextureFromSurface(window->renderer, current_bmp->surface );
	}
	else
	{
		SDL_Texture *src_tex = current_bmp->texture[src_window_idx];
		current_bmp->texture[dest_window_idx] = _sgsdl2_copy_texture(src_tex, 
				_sgsdl2_open_windows[src_window_idx]->renderer, window->renderer);
	}	
}

//
// Add a window to the array of windows, and update all textures so
// they can be drawn to this window
//
void _sgsdl2_add_window(sg_window_be * window)
{
    if ( ! _sgsdl2_has_initial_window ) _sgsdl2_create_initial_window();
    
    // expand array
    _sgsdl2_num_open_windows++;
    
    sg_window_be ** windows = NULL;
    windows = (sg_window_be**)realloc(_sgsdl2_open_windows, sizeof(sg_window_be*) * _sgsdl2_num_open_windows);
    if ( windows == NULL )
    {
        // out of memory exception!
        exit(-1);
    }
    _sgsdl2_open_windows = windows;
    
    unsigned int idx = _sgsdl2_num_open_windows - 1; // get idx for later use
    windows[idx] = window;
    window->idx = idx;
    
    // create textures for new window
    
    SDL_Texture ** textures = NULL;
    sg_bitmap_be *current_bmp;
    
    for (unsigned int i = 0; i < _sgsdl2_num_open_bitmaps; i++)
    {
        current_bmp = _sgsdl2_open_bitmaps[i];
        
        // expand texture array in bitmap
        textures = (SDL_Texture**)realloc(current_bmp->texture, sizeof(SDL_Texture*) * _sgsdl2_num_open_windows);
        if ( !textures ) exit (-1); // out of memory

        current_bmp->texture = textures;
		
		_sgsdl2_create_texture_for_bitmap_window(current_bmp, 0, idx);
    }
}

void _sgsdl2_remove_window(sg_window_be * window_be)
{
    unsigned int idx = window_be->idx;
    if ( idx >= _sgsdl2_num_open_windows || _sgsdl2_open_windows[idx] != window_be)
    {
        std::cout << "error in window close - incorrect idx" << std::endl;
        exit(-1);
    }
    
    // Remove all of the textures for this window
    for (unsigned int bmp_idx = 0; bmp_idx < _sgsdl2_num_open_bitmaps; bmp_idx++)
    {
        // Delete the relevant texture
        SDL_DestroyTexture(_sgsdl2_open_bitmaps[bmp_idx]->texture[idx]);
        
        // shuffle left from idx
        for (unsigned int i = idx; i < _sgsdl2_num_open_windows - 1; i++)
        {
            _sgsdl2_open_bitmaps[bmp_idx]->texture[i] = _sgsdl2_open_bitmaps[bmp_idx]->texture[i + 1];
        }
        
        // Change size of array
        _sgsdl2_open_bitmaps[bmp_idx]->texture = (SDL_Texture**)realloc(_sgsdl2_open_bitmaps[bmp_idx]->texture, sizeof(sg_bitmap_be *) * _sgsdl2_num_open_windows - 1);
    }
    
    // Shuffle all windows left from idx
    for (unsigned int i = idx; i < _sgsdl2_num_open_windows - 1; i++)
    {
        _sgsdl2_open_windows[i] = _sgsdl2_open_windows[i + 1];
        _sgsdl2_open_windows[i]->idx--; // adjust index
        if ( i != _sgsdl2_open_windows[i]->idx ) {
            std::cout << "error in window close!" << std::endl;
            exit(-1);
        }
    }
    // resize windows array
    _sgsdl2_num_open_windows--;
//    std::cout << "windows open " << _sgsdl2_num_open_windows << std::endl;
    if (_sgsdl2_num_open_windows > 0)
    {
        sg_window_be ** temp = (sg_window_be **)realloc(_sgsdl2_open_windows, sizeof(sg_window_be*) * _sgsdl2_num_open_windows);
        if (!temp) 
        {
          exit(-1);
        }
        else
        {
          _sgsdl2_open_windows = temp;
        }
    }
    else
    {
        free(_sgsdl2_open_windows);
        _sgsdl2_open_windows = NULL;
    }

}

void _sgsdl2_destroy_window(sg_window_be *window_be)
{
    _sgsdl2_remove_window(window_be);
    
    SDL_DestroyRenderer(window_be->renderer);
    SDL_DestroyWindow(window_be->window);
    if (window_be->backing)
    {
        SDL_DestroyTexture(window_be->backing);
    }
    
    window_be->idx = UINT_MAX;
    window_be->renderer = NULL;
    window_be->window = NULL;
    window_be->backing = NULL;
    
    free(window_be);
}

//
// Add the bitmap to the array to allow it to be added to future windows opened
//
void _sgsdl2_add_bitmap(sg_bitmap_be *bmp)
{
    _sgsdl2_num_open_bitmaps++;
    sg_bitmap_be **new_arr = (sg_bitmap_be **)realloc(_sgsdl2_open_bitmaps, sizeof(sg_bitmap_be *) * _sgsdl2_num_open_bitmaps);
    
    if (!new_arr) exit(-1); // out of memory
    
    _sgsdl2_open_bitmaps = new_arr;
    new_arr[_sgsdl2_num_open_bitmaps-1] = bmp;
}

//
// Remove the bitmap...
//
void _sgsdl2_remove_bitmap(sg_bitmap_be *bitmap_be)
{
    unsigned int idx = 0;
    for (idx = 0; idx < _sgsdl2_num_open_bitmaps; idx++)
    {
        if ( _sgsdl2_open_bitmaps[idx] == bitmap_be ) break;
    }
    
    if ( idx >= _sgsdl2_num_open_bitmaps )
    {
        //unable to locate bitmap being closed!
        exit(-1);
    }
    
    // Shuffle bitmaps left
    for (unsigned int i = idx; i < _sgsdl2_num_open_bitmaps -1; i++)
    {
        _sgsdl2_open_bitmaps[i] = _sgsdl2_open_bitmaps[i + 1];
    }
    
    // Remove the bitmap
    _sgsdl2_num_open_bitmaps--;
    if (_sgsdl2_num_open_bitmaps > 0)
    {
        sg_bitmap_be ** temp = (sg_bitmap_be**)realloc(_sgsdl2_open_bitmaps, sizeof(sg_bitmap_be *) * _sgsdl2_num_open_bitmaps);

        if (!temp)
        {
          exit(-1);
        }
        else
        {
          _sgsdl2_open_bitmaps = temp;
        }
    }
    else
    {
        free(_sgsdl2_open_bitmaps);
        _sgsdl2_open_bitmaps = NULL;
    }
    
    if ( _sgsdl2_num_open_bitmaps > 0 && ! _sgsdl2_num_open_bitmaps )
    {
        // Error reducing memory allocation?
        exit(-1);
    }
}

void _sgsdl2_destroy_bitmap(sg_bitmap_be *bitmap_be)
{
    _sgsdl2_remove_bitmap(bitmap_be);
    
    for (unsigned int bmp_idx = 0; bmp_idx < _sgsdl2_num_open_windows; bmp_idx++)
    {
        SDL_DestroyTexture(bitmap_be->texture[bmp_idx]);
        bitmap_be->texture[bmp_idx] = NULL;
    }
    free(bitmap_be->texture);
    
    if (bitmap_be->surface)
    {
        SDL_FreeSurface(bitmap_be->surface);
    }

    bitmap_be->surface = NULL;
    bitmap_be->texture = NULL;
    
    free(bitmap_be);
}


//--------------------------------------------------------------------------------------
//
// Window functions
//
//--------------------------------------------------------------------------------------


bool _sgsdl2_open_window(const char *title, int width, int height, unsigned int options, sg_window_be *window_be)
{
    window_be->window = SDL_CreateWindow(title,
                                         SDL_WINDOWPOS_CENTERED,
                                         SDL_WINDOWPOS_CENTERED,
                                         width,
                                         height,
                                         options | SDL_WINDOW_OPENGL);
    
    if ( ! window_be->window )
    {
        set_error_state(SDL_GetError());
        return false;
    }
    
    window_be->renderer = SDL_CreateRenderer(window_be->window,
                                             -1,
                                             SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_TARGETTEXTURE );
    
   //std::cout << "Renderer is " << window_be->renderer << std::endl;
    
    SDL_SetRenderDrawColor(window_be->renderer, 120, 120, 120, 255);
    SDL_RenderClear(window_be->renderer);
    SDL_RenderPresent(window_be->renderer);
    SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
    
    window_be->backing = SDL_CreateTexture(window_be->renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, width, height);
    
    SDL_SetRenderTarget(window_be->renderer, window_be->backing);
    SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
    SDL_RenderClear(window_be->renderer);
	
	_sgsdl2_add_window(window_be);
    
    return true;
}

sg_drawing_surface sgsdl2_open_window(const char *title, int width, int height) 
{
    sg_drawing_surface  result = { SGDS_Unknown, 0, 0, NULL };

    sg_window_be *      window_be;
    
    window_be = (sg_window_be *) malloc(sizeof(sg_window_be));
    
    if ( ! window_be )
    {
        set_error_state("Unable to open window: Out of memory");
        return result;
    }
	
	if ( ! _sgsdl2_open_window(title, width, height, SDL_WINDOW_SHOWN, window_be) )
    {
        free ( window_be );
        return result;
    }
	
	result._data = window_be;
    
    window_be->clipped = false;
    window_be->clip = {0,0,0,0};
    
    window_be->close_requested = false;
    window_be->has_focus = false;
    window_be->mouse_over = false;
    window_be->shown = true;
    
    result.kind = SGDS_Window;
    
    result.width = width;
    result.height = height;
	
	return result;
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
            _sgsdl2_destroy_window( (sg_window_be*) surface->_data);
            break;
        
        case SGDS_Bitmap:
            _sgsdl2_destroy_bitmap( (sg_bitmap_be*) surface->_data);
            break;

        case SGDS_Unknown:
            break;
    }
    
    surface->kind = SGDS_Unknown;
    surface->_data = NULL;
}

void _sgsdl2_do_clear(SDL_Renderer *renderer, sg_color clr)
{
    SDL_SetRenderDrawColor(renderer, 
        static_cast<Uint8>(clr.r * 255), 
        static_cast<Uint8>(clr.g * 255), 
        static_cast<Uint8>(clr.b * 255), 
        static_cast<Uint8>(clr.a * 255));
    SDL_RenderClear(renderer);
}

void _sgsdl2_clear_window(sg_drawing_surface *window, sg_color clr)
{
    sg_window_be * window_be;
    window_be = (sg_window_be *)window->_data;
    
    if ( window_be )
    {
        _sgsdl2_do_clear(window_be->renderer, clr);
		
		//ATI cards are lazy, won't draw the clear screen until you actually draw something else on top of it
		SDL_Rect rect = { 0, 0, 1, 1 }; 
		SDL_RenderFillRect(window_be->renderer, &rect);
    }
}

void _sgsdl2_clear_bitmap(sg_drawing_surface *bitmap, sg_color clr)
{
    sg_bitmap_be * bitmap_be = (sg_bitmap_be *)bitmap->_data;
    
    if ( bitmap_be )
    {
        if ( ! bitmap_be->drawable ) _sgsdl2_make_drawable( bitmap_be );
        
        for (unsigned int i = 0; i < _sgsdl2_num_open_windows; i++)
        {
            sg_window_be *window = _sgsdl2_open_windows[i];
            SDL_Renderer *renderer = window->renderer;
            
            _sgsdl2_set_renderer_target(i, bitmap_be);
            
            _sgsdl2_do_clear(renderer, clr);
            
            _sgsdl2_restore_default_render_target(window, bitmap_be);
        }
    }
}

void sgsdl2_clear_drawing_surface(sg_drawing_surface *surface, sg_color clr)
{
    if ( ! surface ) return;
    
    switch (surface->kind)
    {
        case SGDS_Window:
            _sgsdl2_clear_window(surface, clr);
            break;
        
        case SGDS_Bitmap:
            _sgsdl2_clear_bitmap(surface, clr);
            break;
            
        case SGDS_Unknown:
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
        SDL_SetRenderTarget(window_be->renderer, NULL);
        
        SDL_RenderCopy(window_be->renderer, window_be->backing, NULL, NULL);
        SDL_RenderPresent(window_be->renderer);
        _sgsdl2_restore_default_render_target(window_be, NULL);
    }
}

//
// Renderer functions - switch between bmp and window
//

SDL_Renderer * _sgsdl2_prepared_renderer(sg_drawing_surface *surface, unsigned int idx)
{
    switch (surface->kind)
    {
        case SGDS_Window:
            return ((sg_window_be *)surface->_data)->renderer;
            
        case SGDS_Bitmap:
        {
            sg_bitmap_be *bitmap_be = (sg_bitmap_be *)surface->_data;
            if ( ! bitmap_be->drawable ) _sgsdl2_make_drawable( bitmap_be );
            _sgsdl2_set_renderer_target(idx, bitmap_be);
            
            if (idx < _sgsdl2_num_open_windows)
                return _sgsdl2_open_windows[idx]->renderer;
            else return NULL;
        }
        
        case SGDS_Unknown:
            return NULL;
    }
}

void _sgsdl2_complete_render(sg_drawing_surface *surface, unsigned int idx)
{
    switch (surface->kind)
    {
        case SGDS_Window:
            break;
        case SGDS_Bitmap:
            if (idx < _sgsdl2_num_open_windows)
                _sgsdl2_restore_default_render_target(_sgsdl2_open_windows[idx], (sg_bitmap_be *)surface->_data);
            break;
        case SGDS_Unknown:
            break;
    }
}

unsigned int _sgsdl2_renderer_count(sg_drawing_surface *surface)
{
    switch (surface->kind)
    {
        case SGDS_Window:
            return 1;
        case SGDS_Bitmap:
            return _sgsdl2_num_open_windows;
        case SGDS_Unknown:
            return 0;
    }
}


//
//  Rectangles
//

void sgsdl2_draw_aabb_rect(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( (! surface) || (! surface->_data) ) return;
    if ( data_sz != 4 ) return;
    
    SDL_Rect rect = { (int)data[0], (int)data[1], (int)data[2], (int)data[3] };
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        SDL_SetRenderDrawColor(renderer, 
            static_cast<Uint8>(clr.r * 255), 
            static_cast<Uint8>(clr.g * 255), 
            static_cast<Uint8>(clr.b * 255), 
            static_cast<Uint8>(clr.a * 255));
        
        SDL_RenderDrawRect(renderer, &rect);
        
        _sgsdl2_complete_render(surface, i);
    }
}

void sgsdl2_fill_aabb_rect(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( (! surface) || (! surface->_data)  ) return;
    if ( data_sz != 4 ) return;
    SDL_Rect rect = { (int)data[0], (int)data[1], (int)data[2], (int)data[3] };
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        SDL_SetRenderDrawColor(renderer, 
            static_cast<Uint8>(clr.r * 255), 
            static_cast<Uint8>(clr.g * 255), 
            static_cast<Uint8>(clr.b * 255), 
            static_cast<Uint8>(clr.a * 255));

        SDL_RenderFillRect(renderer, &rect);
        
        _sgsdl2_complete_render(surface, i);
    }
}


// Rectangle points are...
//
//   0 ..... 1
//   .       .
//   .       .
//   .       .
//   2 ..... 3
//
void sgsdl2_draw_rect(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( (! surface) || ! surface->_data ) return;
    if ( data_sz != 8 ) return;
    
    // 8 values = 4 points
    int x1 = (int)data[0], y1 = (int)data[1];
    int x2 = (int)data[2], y2 = (int)data[3];
    int x3 = (int)data[4], y3 = (int)data[5];
    int x4 = (int)data[6], y4 = (int)data[7];
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        SDL_SetRenderDrawColor(renderer, 
            static_cast<Uint8>(clr.r * 255), 
            static_cast<Uint8>(clr.g * 255), 
            static_cast<Uint8>(clr.b * 255), 
            static_cast<Uint8>(clr.a * 255));

        SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
        SDL_RenderDrawLine(renderer, x1, y1, x3, y3);
        SDL_RenderDrawLine(renderer, x4, y4, x2, y2);
        SDL_RenderDrawLine(renderer, x4, y4, x3, y3);
        
        _sgsdl2_complete_render(surface, i);
    }
}

// Rectangle points are...
//
//   0 ..... 1
//   .       .
//   .       .
//   .       .
//   2 ..... 3
//
void sgsdl2_fill_rect(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface ) return;
    if ( data_sz != 8 ) return;
    
    // 8 values = 4 points
    Sint16 x[4], y[4];
    
    x[0] = static_cast<Sint16>(data[0]);
    x[1] = static_cast<Sint16>(data[2]);
    x[2] = static_cast<Sint16>(data[6]);    // Swap last 2 for SDL_gfx order
    x[3] = static_cast<Sint16>(data[4]);

    y[0] = static_cast<Sint16>(data[1]);
    y[1] = static_cast<Sint16>(data[3]);
    y[2] = static_cast<Sint16>(data[7]);    // Swap last 2 for SDL_gfx order
    y[3] = static_cast<Sint16>(data[5]);
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        Uint8 a = (Uint8)(clr.a * 255);
        filledPolygonRGBA(renderer, x, y, 4, (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a);
        
        if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
        {
            SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
        }
        
        _sgsdl2_complete_render(surface, i);
    }
}



//
//  Triangles
//

void sgsdl2_draw_triangle(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 6) return;

    // 6 values = 3 points
    int x1 = (int)data[0], y1 = (int)data[1];
    int x2 = (int)data[2], y2 = (int)data[3];
    int x3 = (int)data[4], y3 = (int)data[5];
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        SDL_SetRenderDrawColor(renderer, 
            static_cast<Uint8>(clr.r * 255), 
            static_cast<Uint8>(clr.g * 255), 
            static_cast<Uint8>(clr.b * 255), 
            static_cast<Uint8>(clr.a * 255));
        
        SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
        SDL_RenderDrawLine(renderer, x2, y2, x3, y3);
        SDL_RenderDrawLine(renderer, x3, y3, x1, y1);
        
        _sgsdl2_complete_render(surface, i);
    }
}

void sgsdl2_fill_triangle(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 6) return;
    
    // 6 values = 3 points
    float x1 = data[0], y1 = data[1];
    float x2 = data[2], y2 = data[3];
    float x3 = data[4], y3 = data[5];
    
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        Uint8 a = static_cast<Uint8>(clr.a * 255);
        filledTrigonRGBA(renderer,
                         static_cast<Sint16>(x1), static_cast<Sint16>(y1),
                         static_cast<Sint16>(x2), static_cast<Sint16>(y2),
                         static_cast<Sint16>(x3), static_cast<Sint16>(y3),
                         static_cast<Uint8>(clr.r * 255), 
                         static_cast<Uint8>(clr.g * 255), 
                         static_cast<Uint8>(clr.b * 255), 
                         a
                         );
        
        if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
        {
            SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
        }
        
        _sgsdl2_complete_render(surface, i);
    }
}

//
//  Ellipse
//

void sgsdl2_draw_ellipse(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 4) return;
    
    // 4 values = 1 point w + h
    int x1 = (int)data[0], y1 = (int)data[1];
    int w = (int)data[2], h = (int)data[3];
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        Uint8 a = (Uint8)(clr.a * 255);
        ellipseRGBA( renderer,
                    static_cast<Sint16>(x1 + w / 2), 
                    static_cast<Sint16>(y1 + h / 2),
                    static_cast<Sint16>(w / 2), 
                    static_cast<Sint16>(h / 2),
                    static_cast<Uint8>(clr.r * 255), 
                    static_cast<Uint8>(clr.g * 255), 
                    static_cast<Uint8>(clr.b * 255), a);
        
        if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
        {
            SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
        }
        
        _sgsdl2_complete_render(surface, i);
    }
}

void sgsdl2_fill_ellipse(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 4) return;
    
    // 4 values = 1 point w + h
    int x1 = (int)data[0], y1 = (int)data[1];
    int w = (int)data[2], h = (int)data[3];
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        Uint8 a = (Uint8)(clr.a * 255);
        filledEllipseRGBA(renderer,
                          static_cast<Sint16>(x1 + w / 2), 
                          static_cast<Sint16>(y1 + h / 2),
                          static_cast<Sint16>(w / 2), 
                          static_cast<Sint16>(h / 2),
                          static_cast<Uint8>(clr.r * 255), 
                          static_cast<Uint8>(clr.g * 255), 
                          static_cast<Uint8>(clr.b * 255), a);
        
        if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
        {
            SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
        }
        
        _sgsdl2_complete_render(surface, i);
    }
}


//
// Pixel
//

void sgsdl2_draw_pixel(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 2) return;
    
    // 2 values = 1 point
    int x1 = (int)data[0], y1 = (int)data[1];
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        SDL_SetRenderDrawColor(renderer, 
            static_cast<Uint8>(clr.r * 255), 
            static_cast<Uint8>(clr.g * 255), 
            static_cast<Uint8>(clr.b * 255), 
            static_cast<Uint8>(clr.a * 255));
        
        // The following works with multisampling on... use if we
        // want multisampling... otherwise use the following
        //
        //            SDL_Rect rect = { x1, y1, 1, 1 };
        //            SDL_RenderFillRect(window_be->renderer, &rect);
        
        // For some reason the following does not work :(
        // when multisample is 1, but without multisample 1
        // double buffer causes flicker
        //
        SDL_RenderDrawPoint(renderer, x1, y1);
        
        _sgsdl2_complete_render(surface, i);
    }
    
}


sg_color sgsdl2_read_pixel(sg_drawing_surface *surface, int x, int y)
{
    sg_color result = {0,0,0,0};
    unsigned int clr = 0;
    SDL_Rect rect = {x,y, 1, 1};
    
    if ( ! surface || ! surface->_data ) return result;

    SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, 0);
    SDL_RenderReadPixels(renderer,
                         &rect,
                         SDL_PIXELFORMAT_RGBA8888,
                         &clr,
                         4 * surface->width );
    result.a = (clr & 0x000000ff) / 255.0f;
    result.r = ((clr & 0xff000000) >> 24) / 255.0f;
    result.g = ((clr & 0x00ff0000) >> 16) / 255.0f;
    result.b = ((clr & 0x0000ff00) >> 8) / 255.0f;
    
    _sgsdl2_complete_render(surface, 0);

    return result;
}


//
// Circles
//

void sgsdl2_draw_circle(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 3) return;
    
    // 3 values = 1 point + radius
    int x1 = (int)data[0], y1 = (int)data[1];
    int r = (int)data[2];
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        Uint8 a = (Uint8)(clr.a * 255);

        circleRGBA(renderer,
                   (Sint16)    x1,
                   (Sint16)    y1,
                   (Sint16)    r,
                   (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a );
        
        if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
        {
            SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
        }
        
        _sgsdl2_complete_render(surface, i);
    }
}

void sgsdl2_fill_circle(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 3) return;
    
    // 3 values = 1 point + radius
    int x1 = (int)data[0], y1 = (int)data[1];
    int r = (int)data[2];
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        Uint8 a = (Uint8)(clr.a * 255);
        
        filledCircleRGBA( renderer,
                         (Sint16)    x1,
                         (Sint16)    y1,
                         (Sint16)    r,
                         (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a );
        
        if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
        {
            SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
        }
        
        _sgsdl2_complete_render(surface, i);
    }
}


//
// Lines
//

void sgsdl2_draw_line(sg_drawing_surface *surface, sg_color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 4) return;
    
    // 4 values = 2 points
    int x1 = (int)data[0], y1 = (int)data[1];
    int x2 = (int)data[2], y2 = (int)data[3];
    
    unsigned int count = _sgsdl2_renderer_count(surface);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
        SDL_SetRenderDrawColor(renderer, 
            static_cast<Uint8>(clr.r * 255), 
            static_cast<Uint8>(clr.g * 255), 
            static_cast<Uint8>(clr.b * 255), 
            static_cast<Uint8>(clr.a * 255));
        
        SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
        
        _sgsdl2_complete_render(surface, i);
    }
}


//
// Clipping
//

void sgsdl2_set_clip_rect(sg_drawing_surface *surface, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 4) return;
    
    // 4 values = 1 point w + h
    int x1 = (int)data[0], y1 = (int)data[1];
    int w = (int)data[2], h = (int)data[3];
    
    switch (surface->kind) {
        case SGDS_Window:
        {
            sg_window_be * window_be;
            window_be = (sg_window_be *)surface->_data;

            window_be->clipped = true;
#ifndef WINDOWS
            //HACK: Current hack to fix SDL clip rect error
            window_be->clip = { x1, surface->height - y1 - h, w, h };
#else
            window_be->clip = { x1, y1, w, h };
#endif

            //Should be: window_be->clip = { x1, y1, w, h };
            SDL_RenderSetClipRect(window_be->renderer, &window_be->clip);
            break;
        }
        case SGDS_Bitmap:
        {
            sg_bitmap_be * bitmap_be;
            bitmap_be = (sg_bitmap_be *)surface->_data;
            
            bitmap_be->clipped = true;
            //HACK: Current hack to fix SDL clip rect error
            bitmap_be->clip = { x1, surface->height - h + y1, w, h };
            break;
        }

        case SGDS_Unknown:
            break;
    }
}

void sgsdl2_clear_clip_rect(sg_drawing_surface *surface)
{
    switch (surface->kind)
    {
        case SGDS_Window:
        {
            sg_window_be * window_be;
            window_be = (sg_window_be *)surface->_data;
            
            window_be->clipped = false;
            window_be->clip = { 0, 0, surface->width, surface->height };
            SDL_RenderSetClipRect(window_be->renderer, NULL);
            SDL_RenderPresent(window_be->renderer);
            break;
        }
        case SGDS_Bitmap:
        {
            sg_bitmap_be * bitmap_be;
            bitmap_be = (sg_bitmap_be *)surface->_data;
            
            bitmap_be->clipped = false;
            bitmap_be->clip = { 0, 0, surface->width, surface->height };
            
            unsigned int count = _sgsdl2_renderer_count(surface);
            
            for (unsigned int i = 0; i < count; i++)
            {
                SDL_Renderer *renderer = _sgsdl2_prepared_renderer(surface, i);
                SDL_RenderPresent(renderer);
                _sgsdl2_complete_render(surface, i);
            }

            break;
        }
      
        case SGDS_Unknown:
            break;
    }
}

Uint32 _get_pixel(SDL_Surface *surface, int x, int y)
{
    Uint8 *p;
    
    if(!surface->pixels) return 0;
    
    p = (Uint8 *)surface->pixels
    + y * surface->pitch
    + x * surface->format->BytesPerPixel;
    
    if(x < 0 || y < 0 || x >= surface->w || y >= surface->h) return 0;
    
    switch(surface->format->BytesPerPixel) {
        case 1:
            return *p;
        case 2:
            return *(Uint16 *)p;
        case 3:
            if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
                return static_cast<Uint32>(p[0] << 16 | p[1] << 8 | p[2]);
            }
            else return static_cast<Uint32>(p[0] | p[1] << 8 | p[2] << 16);
        case 4:
            return *(Uint32 *)p;
        default:
            return 0;
    }
}

//
// To Pixels
//
void sgsdl2_to_pixels(sg_drawing_surface *surface, int *pixels, int sz)
{
    if ( ! surface || ! surface->_data || surface->width * surface->height != sz) return;

    SDL_Rect rect = {0, 0, surface->width, surface->height};
    
    switch (surface->kind)
    {
        case SGDS_Window:
        {
            sg_window_be * window_be;
            window_be = (sg_window_be *)surface->_data;
            
            SDL_RenderReadPixels(window_be->renderer, &rect, SDL_PIXELFORMAT_RGBA8888, pixels, surface->width * 4);
            break;
        }
            
        case SGDS_Bitmap:
        {
            sg_bitmap_be * bitmap_be = (sg_bitmap_be *)surface->_data;
            
            if (bitmap_be->drawable)
            {
                _sgsdl2_set_renderer_target(0, bitmap_be);
                SDL_RenderReadPixels(_sgsdl2_open_windows[0]->renderer, &rect, SDL_PIXELFORMAT_RGBA8888, pixels, surface->width * 4);
            }
            else
            {
                // cannot set as target - use surface
                for (int y = 0; y < surface->height; y++)
                {
                    for(int x = 0; x < surface->width; x++)
                    {
                        pixels[y * surface->width + x] = (int)_get_pixel(bitmap_be->surface, x, y);
                    }
                }
            }
            break;
        }

        case SGDS_Unknown:
            break;
    }
}


//
// Window change functions...
//

void sgsdl2_show_border(sg_drawing_surface *surface, int border)
{
    if ( ! surface || ! surface->_data ) return;
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind)
    {
        case SGDS_Window:
        {
#ifdef WINDOWS
			//There's a Windows-specific bug where turning the border off after 
			//  showing the Window causes a small amount of display corruption.
			//  The window is not really borderless either, the border just becomes white.
			//As a workaround, a new window will be opened, the current backing texture
			//  copied to it, and the current window closed.
			
			// get backing texture .. copy before free
			// window
			// renderer
			// ... free before end
			SDL_Texture *backing = window_be->backing;
			SDL_Window *window = window_be->window;
			SDL_Renderer *renderer = window_be->renderer;
			
			//Replace window_be with a new window, renderer and backing texture
			if ( ! _sgsdl2_open_window(SDL_GetWindowTitle(window_be->window), surface->width,
					surface->height, SDL_WINDOW_BORDERLESS | SDL_WINDOW_SHOWN, window_be) )
            {
                std::cout << "Error switching to borderless window - switching windows failed" << std::endl;
                exit(-1);
            }
			
			//Replace the new window's backing texture with the old one, making everything seamless
			SDL_DestroyTexture(window_be->backing);
			window_be->backing = _sgsdl2_copy_texture(backing, renderer, window_be->renderer);
			
			//Same location
			int x, y;
			SDL_GetWindowPosition(window, &x, &y);
			SDL_SetWindowPosition(window_be->window, x, y);
			
			//Destroy old window
			SDL_DestroyTexture(backing);
			SDL_DestroyRenderer(renderer);
			SDL_DestroyWindow(window);
			
#else
            SDL_SetWindowBordered(window_be->window, border ? SDL_TRUE : SDL_FALSE);			
#endif
            break;
        }

        case SGDS_Bitmap:
          break;

        case SGDS_Unknown:
          break;
    }
}

void sgsdl2_show_fullscreen(sg_drawing_surface *surface, int fullscreen)
{
    if ( ! surface || ! surface->_data ) return;
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind)
    {
        case SGDS_Window:
        {
            SDL_SetWindowFullscreen(window_be->window, fullscreen ? SDL_WINDOW_FULLSCREEN : 0);
            break;
        }

        case SGDS_Bitmap:
          break;

        case SGDS_Unknown:
          break;
    }
}

void sgsdl2_resize(sg_drawing_surface *surface, int width, int height)
{
    if ( ! surface || ! surface->_data ) return;
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind)
    {
        case SGDS_Window:
        {
            SDL_Rect dst = {0, 0, surface->width, surface->height};

            // Get old backing texture
            SDL_Texture * old = window_be->backing;
            
            // Set renderer to draw onto window
            SDL_SetRenderTarget(window_be->renderer, NULL);
            
            // Change window size
            SDL_SetWindowSize(window_be->window, width, height);
            surface->width = width;
            surface->height = height;
            
            // Clear new window surface
            SDL_RenderSetClipRect(window_be->renderer, NULL);
            SDL_SetRenderDrawColor(window_be->renderer, 120, 120, 120, 255);
            SDL_RenderClear(window_be->renderer);
            
            // Create new backing
            window_be->backing = SDL_CreateTexture(window_be->renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, width, height);
            
            // Copy across old display data
            SDL_SetRenderTarget(window_be->renderer, window_be->backing);
            SDL_RenderClear(window_be->renderer);
            SDL_RenderCopy(window_be->renderer, old, NULL, &dst);
            SDL_RenderPresent(window_be->renderer);

            // Restore clipping
            if ( window_be->clipped )
            {
                SDL_RenderSetClipRect(window_be->renderer, &window_be->clip);
            }
            
            // Delete old backing texture
            SDL_DestroyTexture(old);
            
            break;
        }
         
        case SGDS_Bitmap:
            break;

        case SGDS_Unknown:
            break;
    }
}


//
// Load
//

void sgsdl2_load_graphics_fns(sg_interface * functions)
{
    functions->graphics.open_window = &sgsdl2_open_window;
    functions->graphics.close_drawing_surface = &sgsdl2_close_drawing_surface;
    functions->graphics.refresh_window = &sgsdl2_refresh_window;
    functions->graphics.clear_drawing_surface = &sgsdl2_clear_drawing_surface;
    functions->graphics.draw_rect = &sgsdl2_draw_rect;
    functions->graphics.fill_rect = &sgsdl2_fill_rect;
    functions->graphics.draw_aabb_rect = &sgsdl2_draw_aabb_rect;
    functions->graphics.fill_aabb_rect = &sgsdl2_fill_aabb_rect;
    functions->graphics.draw_triangle = &sgsdl2_draw_triangle;
    functions->graphics.fill_triangle = &sgsdl2_fill_triangle;
    functions->graphics.draw_circle = &sgsdl2_draw_circle;
    functions->graphics.fill_circle = &sgsdl2_fill_circle;
    functions->graphics.draw_ellipse = &sgsdl2_draw_ellipse;
    functions->graphics.fill_ellipse = &sgsdl2_fill_ellipse;
    functions->graphics.draw_pixel = &sgsdl2_draw_pixel;
    functions->graphics.read_pixel = &sgsdl2_read_pixel;
    functions->graphics.draw_line = &sgsdl2_draw_line;
    functions->graphics.set_clip_rect = &sgsdl2_set_clip_rect;
    functions->graphics.clear_clip_rect = &sgsdl2_clear_clip_rect;
    functions->graphics.to_pixels = &sgsdl2_to_pixels;
    functions->graphics.show_border = &sgsdl2_show_border;
    functions->graphics.show_fullscreen = &sgsdl2_show_fullscreen;
    functions->graphics.resize = &sgsdl2_resize;
}



//--------------------------------------------------------------------------------------
//
// Images
//
//--------------------------------------------------------------------------------------


sg_drawing_surface sgsdl2_create_bitmap(int width, int height)
{
    if ( ! _sgsdl2_has_initial_window ) _sgsdl2_create_initial_window();
    
    sg_drawing_surface result = { SGDS_Unknown, 0, 0, NULL };
    
    result.kind = SGDS_Bitmap;
    
    sg_bitmap_be *data = (sg_bitmap_be *)malloc(sizeof(sg_bitmap_be));
    
    result._data = data;
    result.width = width;
    result.height = height;

    data->clipped = false;
    data->clip = {0, 0, width, height};
    data->drawable = true;
    data->surface = NULL;
    data->texture = (SDL_Texture**)malloc(sizeof(SDL_Texture*) * _sgsdl2_num_open_windows);
    
    for (unsigned int i = 0; i < _sgsdl2_num_open_windows; i++)
    {
        data->texture[i] = SDL_CreateTexture(_sgsdl2_open_windows[i]->renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, width, height);
        
        SDL_SetTextureBlendMode(data->texture[i], SDL_BLENDMODE_BLEND);
        
        _sgsdl2_set_renderer_target(i, data);
        SDL_SetRenderDrawColor(_sgsdl2_open_windows[i]->renderer, 255, 255, 255, 255);
        SDL_RenderClear(_sgsdl2_open_windows[i]->renderer);
        _sgsdl2_restore_default_render_target(i, data);
    }
    
    _sgsdl2_add_bitmap(data);
    return result;
}

sg_drawing_surface sgsdl2_load_bitmap(const char * filename)
{
    if ( ! _sgsdl2_has_initial_window ) _sgsdl2_create_initial_window();
    
    sg_drawing_surface result = { SGDS_Unknown, 0, 0, NULL };
    
    SDL_Surface *surface;
    
    surface = IMG_Load(filename);
    
    if ( ! surface ) {
        return result;
    }
    
    sg_bitmap_be *data = (sg_bitmap_be *)malloc(sizeof(sg_bitmap_be));
    
    result._data = data;
    
    // Allocate space for one texture per window
    data->texture = (SDL_Texture**)malloc(sizeof(SDL_Texture*) * _sgsdl2_num_open_windows);
    
    for (unsigned int i = 0; i < _sgsdl2_num_open_windows; i++)
    {
        // Create a texture for each window
        data->texture[i] = SDL_CreateTextureFromSurface(_sgsdl2_open_windows[i]->renderer, surface);
    }
    
    data->surface = surface;
    data->drawable = false;
    data->clipped = false;
    data->clip = {0,0,0,0};
    
    result.kind = SGDS_Bitmap;
    result.width = surface->w;
    result.height = surface->h;
    
    _sgsdl2_add_bitmap(data);
    
    return result;
}

//x, y is the position to draw the bitmap to. As bitmaps scale around their centre, (x, y) is the top-left of the bitmap IF and ONLY IF scale = 1.
//Angle is in degrees, 0 being right way up
//Centre is the point to rotate around, relative to the bitmap centre (therefore (0,0) would rotate around the centre point)
void sgsdl2_draw_bitmap( sg_drawing_surface * src, sg_drawing_surface * dst, float * src_data, int src_data_sz, float * dst_data, int dst_data_sz, sg_renderer_flip flip )
{
    if ( ! src || ! dst || src->kind != SGDS_Bitmap )
		return;
    
    if ( dst_data_sz != 7 )
        return;
    
    // dst_data must be 7 values
    float x         = dst_data[0];
    float y         = dst_data[1];
    float angle     = dst_data[2];
    float centre_x  = dst_data[3];
    float centre_y  = dst_data[4];
    float scale_x   = dst_data[5];
    float scale_y   = dst_data[6];
    
    if ( src_data_sz != 4)
        return;
    
    // src_data must be
    float src_x     = src_data[0];
    float src_y     = src_data[1];
    float src_w     = src_data[2];
    float src_h     = src_data[3];
    
    // Other locals
    SDL_Texture *srcT;
    
	// Create destination rect from scale values
    SDL_Rect dst_rect = {
            (int)(x - (src_w * scale_x / 2.0) + src_w/2.0),
            (int)(y - (src_h * scale_y / 2.0) + src_h/2.0), //fix the drawing position as scaling broke it
            (int)(src_w * scale_x),
            (int)(src_h * scale_y)
        }; //scale bitmap

    SDL_Rect src_rect = {(int)src_x, (int)src_y, (int)src_w, (int)src_h};

    // check if any size is 0... and return if nothing is to be drawn
    if ( dst_rect.w * dst_rect.h * src_rect.w * src_rect.h == 0) return;
    
	// Adjust centre to be relative to the bitmap centre rather than top-left
	centre_x = (centre_x * scale_x) + dst_rect.w / 2.0f;
	centre_y = (centre_y * scale_y) + dst_rect.h / 2.0f;
	
    unsigned int count = _sgsdl2_renderer_count(dst);
    
    for (unsigned int i = 0; i < count; i++)
    {
        SDL_Renderer *renderer = _sgsdl2_prepared_renderer(dst, i);
        
        // if its a window, dont use the renderer index to get the texture
        if (dst->kind == SGDS_Window)
        {
            unsigned int idx = ((sg_window_be *)dst->_data)->idx;
            
            srcT = ((sg_bitmap_be *)src->_data)->texture[ idx ];
        }
        else
            srcT = ((sg_bitmap_be *)src->_data)->texture[ i ];
        
		//Convert parameters to format SDL_RenderCopyEx expects
		SDL_Point centre = {(int)centre_x, (int)centre_y};
		SDL_RendererFlip sdl_flip = (SDL_RendererFlip) ((flip == SG_FLIP_BOTH) ? (SDL_FLIP_HORIZONTAL | SDL_FLIP_VERTICAL) : flip); //SDL does not have a FLIP_BOTH
		
		//Render
        SDL_RenderCopyEx(renderer, srcT, &src_rect, &dst_rect, angle, &centre, sdl_flip);
        
        _sgsdl2_complete_render(dst, i);
    }
}

void sgsdl2_load_image_fns(sg_interface *functions)
{
    functions->image.create_bitmap = &sgsdl2_create_bitmap;
    functions->image.load_bitmap = &sgsdl2_load_bitmap;
    functions->image.draw_bitmap = & sgsdl2_draw_bitmap;
}

void sgsdl2_finalise_graphics()
{
    // Close all bitmaps
    for (unsigned int i = 0; i < _sgsdl2_num_open_bitmaps; i++)
    {
        _sgsdl2_destroy_bitmap(_sgsdl2_open_bitmaps[i]);
    }
    
    // Close all windows 
    for (unsigned int i = 0; i < _sgsdl2_num_open_windows; i++)
    {
        _sgsdl2_destroy_window(_sgsdl2_open_windows[i]);
    }
}

