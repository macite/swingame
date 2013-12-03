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
    
    result._data = window_be;
    
    if ( ! window_be->window)
    {
        set_error_state(SDL_GetError());
        return result;
    }
    
    window_be->renderer = SDL_CreateRenderer(window_be->window,
                                             -1,
                                             SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_TARGETTEXTURE );
    
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
        SDL_DestroyTexture(window_be->backing);
        
        
        window_be->renderer = NULL;
        window_be->window = NULL;
        window_be->backing = NULL;
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
        SDL_SetRenderTarget(window_be->renderer, NULL);
        
        SDL_RenderCopy(window_be->renderer, window_be->backing, NULL, NULL);
        SDL_RenderPresent(window_be->renderer);
        SDL_SetRenderTarget(window_be->renderer, window_be->backing);
        SDL_SetRenderDrawBlendMode(window_be->renderer, SDL_BLENDMODE_BLEND);
        if ( window_be->clipped )
        {
            SDL_RenderSetClipRect(window_be->renderer, &window_be->clip);
        }
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
            
            circleRGBA(            window_be->renderer,
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

    sg_window_be * window_be;
    window_be = (sg_window_be *)surface->_data;
    
    switch (surface->kind)
    {
        case SGDS_Window:
        {
            SDL_Rect rect = {0, 0, surface->width, surface->height};
            SDL_RenderReadPixels(window_be->renderer, &rect, SDL_PIXELFORMAT_RGBA8888, pixels, surface->width * 4);
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

