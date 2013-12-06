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
    SDL_Texture *   backing;
    bool            clipped;
    SDL_Rect        clip;
    int             idx;
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


sg_window_be ** _sgsdl2_open_windows = NULL;
int _sgsdl2_num_open_windows = 0;

sg_bitmap_be ** _sgsdl2_open_bitmaps = NULL;
int _sgsdl2_num_open_bitmaps = 0;


//--------------------------------------------------------------------------------------
//
// Functions to work with renderer targets - switching targets etc
//
//--------------------------------------------------------------------------------------

void _sgsdl2_restore_default_render_target(sg_window_be *window_be)
{
    SDL_SetRenderTarget(window_be->renderer, window_be->backing);
    SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
    if ( window_be->clipped )
    {
        SDL_RenderSetClipRect(window_be->renderer, &window_be->clip);
    }
}

void _sgsdl2_restore_default_render_target(int window_idx)
{
    _sgsdl2_restore_default_render_target(_sgsdl2_open_windows[window_idx]);
}

void _sgsdl2_set_renderer_target(int window_idx, sg_bitmap_be *target)
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
    
    for (int i = 0; i < _sgsdl2_num_open_windows; i++)
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
        
        _sgsdl2_restore_default_render_target(_sgsdl2_open_windows[i]);
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

bool _sgsdl2_has_initial_window = false;
sg_window_be * _sgsdl2_initial_window = NULL;

// The initial window is a hidden window that is always "open"
// This allows drawing without the user having to open a window initially.
void _sgsdl2_create_initial_window()
{
    _sgsdl2_has_initial_window = true;
    _sgsdl2_initial_window = (sg_window_be *) malloc(sizeof(sg_window_be));
    _sgsdl2_initial_window->window = SDL_CreateWindow("",
                                         0, 0, 1, 1,
                                         SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN ); //TODO: Log SDL issue re drawing using hidden windows
    
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
    
    // You cannot draw onto this window!
    _sgsdl2_initial_window->backing = NULL;
    
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
    
    int idx = _sgsdl2_num_open_windows - 1; // get idx for later use
    windows[idx] = window;
    window->idx = idx;
    
    // create textures for new window
    
    SDL_Texture ** textures = NULL;
    sg_bitmap_be *current_bmp;
    
    for (int i = 0; i < _sgsdl2_num_open_bitmaps; i++)
    {
        current_bmp = _sgsdl2_open_bitmaps[i];
        
        // expand texture array in bitmap
        textures = (SDL_Texture**)realloc(current_bmp->texture, sizeof(SDL_Texture*) * _sgsdl2_num_open_windows);
        if ( !textures ) exit (-1); // out of memory

        current_bmp->texture = textures;
        
        // if the surface exists, use that to create the new bitmap... otherwise extract from texture
        if (current_bmp->surface)
        {
            current_bmp->texture[idx] = SDL_CreateTextureFromSurface(window->renderer, current_bmp->surface );
        }
        else
        {
            // Read from initial window
            void *pixels;
            int w, h;
            
            SDL_Texture *orig_tex = current_bmp->texture[0];
            
            SDL_QueryTexture(orig_tex, NULL, NULL, &w, &h);
            pixels = malloc(4 * w * h);
            
            SDL_Renderer *orig_renderer = _sgsdl2_open_windows[0]->renderer;
            SDL_SetRenderTarget(orig_renderer, orig_tex);
            SDL_RenderReadPixels(orig_renderer, NULL, SDL_PIXELFORMAT_RGBA8888, pixels, 4 * w);
            
            
            SDL_Texture *tex = SDL_CreateTexture(window->renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, w, h);
            SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
            
            SDL_UpdateTexture(tex, NULL, pixels, 4 * w);
            free(pixels);
            
            current_bmp->texture[idx] = tex;
            
            // Restore default target
            SDL_SetRenderTarget(orig_renderer, NULL);
        }
    }
}

void _sgsdl2_remove_window(sg_window_be * window_be)
{
    int idx = window_be->idx;
    if ( idx < 0 || idx >= _sgsdl2_num_open_windows || _sgsdl2_open_windows[idx] != window_be)
    {
        std::cout << "error in window close - incorrect idx" << std::endl;
        exit(-1);
    }
    
    // Remove all of the textures for this window
    for (int bmp_idx = 0; bmp_idx < _sgsdl2_num_open_bitmaps; bmp_idx++)
    {
        // Delete the relevant texture
        SDL_DestroyTexture(_sgsdl2_open_bitmaps[bmp_idx]->texture[idx]);
        
        // shuffle left from idx
        for (int i = idx; i < _sgsdl2_num_open_windows - 1; i++)
        {
            _sgsdl2_open_bitmaps[bmp_idx]->texture[i] = _sgsdl2_open_bitmaps[bmp_idx]->texture[i + 1];
        }
        
        // Change size of array
        _sgsdl2_open_bitmaps[bmp_idx]->texture = (SDL_Texture**)realloc(_sgsdl2_open_bitmaps[bmp_idx]->texture, sizeof(sg_bitmap_be *) * _sgsdl2_num_open_windows - 1);
    }
    
    // Shuffle all windows left from idx
    for (int i = idx; i < _sgsdl2_num_open_windows - 1; i++)
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
        _sgsdl2_open_windows = (sg_window_be **)realloc(_sgsdl2_open_windows, sizeof(sg_window_be*) * _sgsdl2_num_open_windows);
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
    
    window_be->idx = -1;
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
    int idx = -1;
    for (idx = 0; idx < _sgsdl2_num_open_bitmaps; idx++)
    {
        if ( _sgsdl2_open_bitmaps[idx] == bitmap_be ) break;
    }
    
    if ( idx < 0 || idx >= _sgsdl2_num_open_bitmaps )
    {
        //unable to locate bitmap being closed!
        exit(-1);
    }
    
    // Shuffle bitmaps left
    for (int i = idx; i < _sgsdl2_num_open_bitmaps -1; i++)
    {
        _sgsdl2_open_bitmaps[i] = _sgsdl2_open_bitmaps[i + 1];
    }
    
    // Remove the bitmap
    _sgsdl2_num_open_bitmaps--;
    if (_sgsdl2_num_open_bitmaps > 0)
    {
        _sgsdl2_open_bitmaps = (sg_bitmap_be**)realloc(_sgsdl2_open_bitmaps, sizeof(sg_bitmap_be *) * _sgsdl2_num_open_bitmaps);
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
    
    for (int bmp_idx = 0; bmp_idx < _sgsdl2_num_open_windows; bmp_idx++)
    {
        SDL_DestroyTexture(bitmap_be->texture[bmp_idx]);
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
    
    if ( ! window_be->window )
    {
        set_error_state(SDL_GetError());
        free ( window_be );
        return result;
    }
    
    result._data = window_be;
    
    window_be->renderer = SDL_CreateRenderer(window_be->window,
                                             -1,
                                             SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_TARGETTEXTURE );
    
//    std::cout << "Renderer is " << window_be->renderer << std::endl;
    
    SDL_SetRenderDrawColor(window_be->renderer, 120, 120, 120, 255);
    SDL_RenderClear(window_be->renderer);
    SDL_RenderPresent(window_be->renderer);
    SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
    
    window_be->backing = SDL_CreateTexture(window_be->renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, width, height);
    
    SDL_SetRenderTarget(window_be->renderer, window_be->backing);
    SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
    SDL_RenderClear(window_be->renderer);
    
    window_be->clipped = false;
    window_be->clip = {0,0,0,0};
    
    result.kind = SGDS_Window;
    
    result.width = width;
    result.height = height;
    
    _sgsdl2_add_window(window_be);

    return result;
}

void _sgsdl2_close_window(sg_drawing_surface *window)
{
    // window assumed to be ok - private
    sg_window_be * window_be;
    window_be = (sg_window_be *)window->_data;
    
    if ( window_be )
    {
        _sgsdl2_destroy_window(window_be);
    }
}

void _sgsdl2_close_bitmap(sg_drawing_surface *bitmap)
{
    // bitmap assumed to be ok - private
    sg_bitmap_be *bitmap_be = (sg_bitmap_be *)bitmap->_data;
    
    if (bitmap_be)
    {
        _sgsdl2_destroy_bitmap(bitmap_be);
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
            _sgsdl2_close_window(surface);
            break;
        
        case SGDS_Bitmap:
            _sgsdl2_close_bitmap(surface);
            break;
            
        default:
            break;
    }
    
    surface->kind = SGDS_Unknown;
    surface->_data = NULL;
}

void sgsdl2_set_renderer_color(sg_window_be *window_be, color clr)
{
    if ( window_be && window_be->renderer )
    {
        SDL_SetRenderDrawColor(window_be->renderer, clr.r * 255, clr.g * 255, clr.b * 255, clr.a * 255);
    }
}

void _sgsdl2_do_clear(SDL_Renderer *renderer, color clr)
{
    SDL_SetRenderDrawColor(renderer, clr.r * 255, clr.g * 255, clr.b * 255, clr.a * 255);
    SDL_RenderClear(renderer);
}

void _sgsdl2_clear_window(sg_drawing_surface *window, color clr)
{
    sg_window_be * window_be;
    window_be = (sg_window_be *)window->_data;
    
    if ( window_be )
    {
        _sgsdl2_do_clear(window_be->renderer, clr);
    }
}

void _sgsdl2_clear_bitmap(sg_drawing_surface *bitmap, color clr)
{
    sg_bitmap_be * bitmap_be = (sg_bitmap_be *)bitmap->_data;
    
    if ( bitmap_be )
    {
        if ( ! bitmap_be->drawable ) _sgsdl2_make_drawable( bitmap_be );
        
        for (int i = 0; i < _sgsdl2_num_open_windows; i++)
        {
            sg_window_be *window = _sgsdl2_open_windows[i];
            SDL_Renderer *renderer = window->renderer;
            
            _sgsdl2_set_renderer_target(i, bitmap_be);
            
            _sgsdl2_do_clear(renderer, clr);
            
            _sgsdl2_restore_default_render_target(window);
        }
    }
}

void sgsdl2_clear_drawing_surface(sg_drawing_surface *surface, color clr)
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
        SDL_SetRenderTarget(window_be->renderer, NULL);
        
        SDL_RenderCopy(window_be->renderer, window_be->backing, NULL, NULL);
        SDL_RenderPresent(window_be->renderer);
        _sgsdl2_restore_default_render_target(window_be);
    }
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


// Rectangle points are...
//
//   0 ..... 1
//   .       .
//   .       .
//   .       .
//   2 ..... 3
//
void sgsdl2_draw_rect(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface ) return;
    if ( data_sz != 8 ) return;
    
    // 8 values = 4 points
    int x1 = (int)data[0], y1 = (int)data[1];
    int x2 = (int)data[2], y2 = (int)data[3];
    int x3 = (int)data[4], y3 = (int)data[5];
    int x4 = (int)data[6], y4 = (int)data[7];

    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    if ( window_be )
    {
        sgsdl2_set_renderer_color(window_be, clr);
        
        SDL_RenderDrawLine(window_be->renderer, x1, y1, x2, y2);
        SDL_RenderDrawLine(window_be->renderer, x1, y1, x3, y3);
        SDL_RenderDrawLine(window_be->renderer, x4, y4, x2, y2);
        SDL_RenderDrawLine(window_be->renderer, x4, y4, x3, y3);
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
void sgsdl2_fill_rect(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface ) return;
    if ( data_sz != 8 ) return;
    
    // 8 values = 4 points
    Sint16 x[4], y[4];
    
    x[0] = (int)data[0];
    x[1] = (int)data[2];
    x[2] = (int)data[6];    // Swap last 2 for SDL_gfx order
    x[3] = (int)data[4];

    y[0] = (int)data[1];
    y[1] = (int)data[3];
    y[2] = (int)data[7];    // Swap last 2 for SDL_gfx order
    y[3] = (int)data[5];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    if ( window_be )
    {
        Uint8 a = (Uint8)(clr.a * 255);
        filledPolygonRGBA(window_be->renderer, x, y, 4, (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a);
        
        if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
        {
            SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
        }

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
    int x3 = (int)data[4], y3 = (int)data[5];
    
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
    float x3 = data[4], y3 = data[5];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
        {
            Uint8 a = (Uint8)(clr.a * 255);
            filledTrigonRGBA(window_be->renderer,
                              x1, y1,
                              x2, y2,
                              x3, y3,
                              (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a
                              );
            if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
            {
                SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
            }
            break;
        }
        default:
            break;
    }
    
}

//
//  Ellipse
//

void sgsdl2_draw_ellipse(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 4) return;
    
    // 4 values = 1 point w + h
    int x1 = (int)data[0], y1 = (int)data[1];
    int w = (int)data[2], h = (int)data[3];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
        {
            Uint8 a = (Uint8)(clr.a * 255);
            ellipseRGBA(window_be->renderer, (Sint16)x1, (Sint16)y1, (Sint16)(w / 2), (Sint16)(h / 2), (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a);

            if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
            {
                SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
            }
            break;
        }
        default:
            break;
    }
}

void sgsdl2_fill_ellipse(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 4) return;
    
    // 4 values = 1 point w + h
    int x1 = (int)data[0], y1 = (int)data[1];
    int w = (int)data[2], h = (int)data[3];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
        {
            Uint8 a = (Uint8)(clr.a * 255);
            
            filledEllipseRGBA(window_be->renderer, (Sint16)x1 + w / 2, (Sint16)y1 + h / 2, (Sint16)(w / 2), (Sint16)(h / 2), (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a);

            if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
            {
                SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
            }

            break;
        }
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
// The following works with multisampling on... use if we
// want multisampling... otherwise use the following
//
//            SDL_Rect rect = { x1, y1, 1, 1 };
//            sgsdl2_set_renderer_color(window_be, clr);
//            SDL_RenderFillRect(window_be->renderer, &rect);

// For some reason the following does not work :(
// when multisample is 1, but without multisample 1
// double buffer causes flicker
//
            sgsdl2_set_renderer_color(window_be, clr);
            SDL_RenderDrawPoint(window_be->renderer, x1, y1);
            break;
        }
        default:
            break;
    }
    
}


color sgsdl2_read_pixel(sg_drawing_surface *surface, int x, int y)
{
    color result = {0,0,0,0};
    int clr = 0;
    SDL_Rect rect = {x,y, 1, 1};

    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) 
    {
        case SGDS_Window:
        {
            SDL_RenderReadPixels(   window_be->renderer,
                                    &rect,
                                    SDL_PIXELFORMAT_RGBA8888,
                                    &clr, 
                                    4 * surface->width );
            result.a = (clr & 0x000000ff) / 255.0f;
            result.r = ((clr & 0xff000000) >> 24) / 255.0f;
            result.g = ((clr & 0x00ff0000) >> 16) / 255.0f;
            result.b = ((clr & 0x0000ff00) >> 8) / 255.0f;
            break;
        }
        default:
            break;
    }

    return result;
}


//
// Circles
//

void sgsdl2_draw_circle(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 3) return;
    
    // 3 values = 1 point + radius
    int x1 = (int)data[0], y1 = (int)data[1];
    int r = (int)data[2];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
        {
            Uint8 a = (Uint8)(clr.a * 255);
            
            circleRGBA(             window_be->renderer,
                        (Sint16)    x1,
                        (Sint16)    y1,
                        (Sint16)    r,
                                    (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a );
            if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
            {
                SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
            }
            break;
        }
        default:
            break;
    }
}

void sgsdl2_fill_circle(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 3) return;
    
    // 3 values = 1 point + radius
    int x1 = (int)data[0], y1 = (int)data[1];
    int r = (int)data[2];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
        {
            Uint8 a = (Uint8)(clr.a * 255);
            
            filledCircleRGBA(            window_be->renderer,
                              (Sint16)    x1,
                              (Sint16)    y1,
                              (Sint16)    r,
                              (Uint8)(clr.r * 255), (Uint8)(clr.g * 255), (Uint8)(clr.b * 255), a );

            if ( a == 255 ) // SDL_Gfx changes renderer state ... undo change here
            {
                SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
            }

            break;
        }
        default:
            break;
    }
    
}


//
// Lines
//

void sgsdl2_draw_line(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 4) return;
    
    // 4 values = 2 points
    int x1 = (int)data[0], y1 = (int)data[1];
    int x2 = (int)data[2], y2 = (int)data[3];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
            sgsdl2_set_renderer_color(window_be, clr);
            SDL_RenderDrawLine(window_be->renderer, x1, y1, x2, y2);
            break;
            
        default:
            break;
    }
}


//
// Clipping
//

void sgsdl2_set_clip_rect(sg_drawing_surface *surface, color clr, float *data, int data_sz)
{
    if ( ! surface || ! surface->_data || data_sz != 4) return;
    
    // 4 values = 1 point w + h
    int x1 = (int)data[0], y1 = (int)data[1];
    int w = (int)data[2], h = (int)data[3];
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind) {
        case SGDS_Window:
        {
            window_be->clipped = true;
            //HACK: Current hack to fix SDL clip rect error
            window_be->clip = { x1, surface->height - h + y1, w, h };
            //Should be: window_be->clip = { x1, y1, w, h };
            SDL_RenderSetClipRect(window_be->renderer, &window_be->clip);
            break;
        }
            
        default:
            break;
    }
}

void sgsdl2_clear_clip_rect(sg_drawing_surface *surface)
{
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind)
    {
        case SGDS_Window:
        {
            window_be->clipped = false;
            window_be->clip = { 0, 0, surface->width, surface->height };
            SDL_RenderSetClipRect(window_be->renderer, NULL);
            SDL_RenderPresent(window_be->renderer);
            break;
        }
            
        default:
            break;
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
            
            _sgsdl2_set_renderer_target(0, bitmap_be);
            SDL_RenderReadPixels(_sgsdl2_open_windows[0]->renderer, &rect, SDL_PIXELFORMAT_RGBA8888, pixels, surface->width * 4);
            break;
        }
            
        default:
            break;
    }
}


//
// Window change functions...
//

void sgsdl2_show_border(sg_drawing_surface *surface, bool border)
{
    if ( ! surface || ! surface->_data ) return;
    
    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind)
    {
        case SGDS_Window:
        {
            SDL_SetWindowBordered(window_be->window, border ? SDL_TRUE : SDL_FALSE);
            break;
        }
            
        default:
            break;
    }
}

void sgsdl2_show_fullscreen(sg_drawing_surface *surface, bool fullscreen)
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
            
        default:
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
            
            SDL_SetRenderTarget(window_be->renderer, window_be->backing);
            SDL_RenderClear(window_be->renderer);
            
            SDL_RenderCopy(window_be->renderer, old, NULL, &dst);
            
            // Restore clipping
            if ( window_be->clipped )
            {
                SDL_RenderSetClipRect(window_be->renderer, &window_be->clip);
            }
            
            // Delete old backing texture
            SDL_DestroyTexture(old);
            
            break;
        }
            
        default:
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

#include "SDL_image.h"


sg_drawing_surface sgsdl2_create_bitmap(const char * title, int width, int height)
{
    if ( ! _sgsdl2_has_initial_window ) _sgsdl2_create_initial_window();
    
    sg_drawing_surface result = { SGDS_Unknown, NULL };
    
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
    
    for (int i = 0; i < _sgsdl2_num_open_windows; i++)
    {
        data->texture[i] = SDL_CreateTexture(_sgsdl2_open_windows[i]->renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, width, height);
        
        SDL_SetTextureBlendMode(data->texture[i], SDL_BLENDMODE_BLEND);
        
        _sgsdl2_set_renderer_target(i, data);
        SDL_SetRenderDrawColor(_sgsdl2_open_windows[i]->renderer, 255, 255, 255, 255);
        SDL_RenderClear(_sgsdl2_open_windows[i]->renderer);
        _sgsdl2_restore_default_render_target(i);
    }
    
    _sgsdl2_add_bitmap(data);
    return result;
}

sg_drawing_surface sgsdl2_load_bitmap(const char * filename, sg_drawing_surface *wind)
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
    
    sg_window_be * window = (sg_window_be *) wind->_data;
    
    // Allocate space for one texture per window
    data->texture = (SDL_Texture**)malloc(sizeof(SDL_Texture*) * _sgsdl2_num_open_windows);
    
    for (int i = 0; i < _sgsdl2_num_open_windows; i++)
    {
        // Create a texture for each window
        data->texture[i] = SDL_CreateTextureFromSurface(window->renderer, surface);
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

void sgsdl2_draw_bitmap(sg_drawing_surface * src, sg_drawing_surface * dst, int x, int y )
{
    //assume dst = window for now...
    
    SDL_Renderer *renderer = ((sg_window_be *)dst->_data)->renderer;
    int idx = ((sg_window_be *)dst->_data)->idx;
    SDL_Texture *srcT = ((sg_bitmap_be *)src->_data)->texture[idx];
    
    int w, h;
    SDL_QueryTexture(srcT, NULL, NULL, &w, &h);
    
    SDL_Rect dstrect = { x, y, w, h};
    SDL_RenderCopy(renderer, srcT, NULL, &dstrect);
}

void sgsdl2_load_image_fns(sg_interface *functions)
{
    functions->image.create_bitmap = &sgsdl2_create_bitmap;
    functions->image.load_bitmap = &sgsdl2_load_bitmap;
    functions->image.draw_bitmap = & sgsdl2_draw_bitmap;
}

void sgsdl2_finalise_graphics()
{
    // Close all bitmaps - in reverse order
    for (int i = _sgsdl2_num_open_bitmaps - 1; i >= 0; i--)
    {
        _sgsdl2_destroy_bitmap(_sgsdl2_open_bitmaps[i]);
    }
    
    // Close all windows - in reverse order
    for (int i = _sgsdl2_num_open_windows - 1; i >= 0; i--)
    {
        _sgsdl2_destroy_window(_sgsdl2_open_windows[i]);
    }
}

