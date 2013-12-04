//
//  sgInterfaces.h
//  sgsdl2
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_sgInterfaces_h
#define sgsdl2_sgInterfaces_h

#ifndef __cplusplus
#include <stdbool.h>
#endif

#ifdef __cplusplus

#include "sgBackendTypes.h"

extern "C" {
#endif
    
    typedef unsigned char byte;
    typedef enum sg_interface_features
    {
        SGV_NONE = 0x00,
        SGV_INIT = 0x01
    } sg_interface_version;
    
    typedef struct color
    {
        float r, g, b, a;
    } color;
    
    typedef void (sg_empty_procedure)( void );
    
    typedef void (sg_drawing_surface_proc)( sg_drawing_surface * );
    typedef void (sg_single_uint32param_proc)( unsigned int ms );
    typedef void (sg_drawing_surface_clr_proc)( sg_drawing_surface *surface, color clr );
    typedef void (sg_drawing_proc)( sg_drawing_surface *surface, color clr, float *data, int data_sz );
    typedef void (sg_surface_bool_proc)( sg_drawing_surface *surface, bool value );
    typedef void (sg_to_pixel_array_proc)( sg_drawing_surface *surface, int *pixels, int sz );
    
    typedef void (sg_surface_size_proc)( sg_drawing_surface *surface, int width, int height );

    typedef sg_drawing_surface  (sg_new_surface_fn)(const char *title, int width, int height);

    typedef color (sg_surface_color_fn)( sg_drawing_surface *surface, int x, int y );
    
    typedef sg_system_data * (sg_system_data_fn)();

    
    //
    // Sound related function pointers
    //
    
    // Load functions
    typedef sg_sound_data (sg_sound_load_fn)(const char *filename, sg_sound_kind kind);
    
    // Play functions
    typedef void (sg_play_sound_fn)(sg_sound_data *sound, int loops, float volume);
    
    // Sound data procedure
    typedef void (sg_sound_data_proc)(sg_sound_data *sound);

    // Sound data float function
    typedef float (sg_sound_float_fn)(sg_sound_data *sound);
    
    // Fade in procedure
    typedef void (sg_sound_fade_proc)(sg_sound_data *sound, int loops, int ms);

    // Fade out procedure
    typedef void (sg_sound_fade_out_proc)(sg_sound_data *sound, int ms);

    typedef void (sg_sound_float_proc)(sg_sound_data *sound, float val);
    typedef void (sg_intp_proc)( int ms );
    typedef void (sg_floatp_proc)( float val );
    
    typedef float (sg_float_fn)( );
    
    typedef float (sg_float_soundp_fn)( sg_sound_data *sound );
    
    
    //
    // Utility related
    //
    typedef unsigned int  (uint_fn)();
    
    
    //
    // Utility relation functions
    //
    // - delay = Function to delay by a specified number of milliseconds.
    //
    typedef struct sg_utils_interface
    {
        sg_single_uint32param_proc *    delay;
        uint_fn *   get_ticks;
        
    } sg_utils_interface;
    
    
    typedef struct sg_audio_interface
    {
        sg_empty_procedure      *    open_audio;
        sg_empty_procedure      *    close_audio;
        
        sg_sound_load_fn        *    load_sound_data;
        sg_sound_data_proc      *    close_sound_data;
        
        sg_play_sound_fn        *    play_sound;
        
        sg_sound_float_fn       *    sound_playing;
        
        sg_sound_fade_proc      *    fade_in;
        sg_sound_fade_out_proc  *    fade_out;
        
        sg_intp_proc        *   fade_music_out;
        sg_intp_proc        *   fade_all_sound_effects_out;
        
        sg_floatp_proc      *   set_music_vol;
        
        sg_float_fn         *   music_vol;
        
        sg_float_soundp_fn  *   sound_volume;
        sg_sound_float_proc *   set_sound_volume;
        
        sg_empty_procedure  *   pause_music;
        sg_empty_procedure  *   resume_music;
        sg_empty_procedure  *   stop_music;
        
        sg_sound_data_proc  *   stop_sound;
        
    } sg_audio_interface;
    
    //
    // Graphics related functions.
    //
    // - open_window = Open a new window and return its details.
    // - close_drawing_surface = Close a previously open drawing surface (window/bitmap/etc)
    // - refresh_window = Present window to user
    // - clear_drawing_surface = Clear the screen or bitmap
    // - draw_aabb_rect = an axis aligned bounding box
    //
    typedef struct sg_graphics_interface
    {
        sg_new_surface_fn       * open_window;
        
        sg_drawing_surface_proc * close_drawing_surface;
        sg_drawing_surface_proc * refresh_window;
        
        sg_drawing_surface_clr_proc * clear_drawing_surface;
        
        sg_drawing_proc * draw_aabb_rect;
        sg_drawing_proc * fill_aabb_rect;

        sg_drawing_proc * draw_rect;
        sg_drawing_proc * fill_rect;

        
        sg_drawing_proc * draw_triangle;
        sg_drawing_proc * fill_triangle;

        sg_drawing_proc * draw_circle;
        sg_drawing_proc * fill_circle;

        sg_drawing_proc * draw_ellipse;
        sg_drawing_proc * fill_ellipse;
        
        sg_drawing_proc * draw_pixel;
        sg_drawing_proc * draw_line;

        sg_surface_color_fn * read_pixel;
        
        sg_drawing_proc * set_clip_rect;
        sg_drawing_surface_proc * clear_clip_rect;
        
        sg_to_pixel_array_proc * to_pixels;
        
        sg_surface_bool_proc * show_border;
        
        sg_surface_bool_proc * show_fullscreen;
        
        sg_surface_size_proc * resize;

    } sg_graphics_interface;
    
    
    //
    // All sg functions.
    //
    // - has_error (data) = is there currently an error
    // - current_error (data) = A pointer to the current error message. This error
    //                          is managed by the driver and must not be freed by
    //                          the driver's user.
    // - graphics (data) = Functions associated with windows and drawing
    // - utils (data) = Functions associated with utilities
    //
    // - init = The init procedure is called when the SwinGame starts and should
    //          be used to initialise the underlying system.
    //
    typedef struct sg_interface
    {
        bool has_error;
        const char *    current_error;

        //
        // function pointers by functionality
        //
        sg_audio_interface audio;
        sg_graphics_interface graphics;
        sg_utils_interface utils;
        
        sg_system_data_fn *read_system_data;
        
        sg_empty_procedure *init;
    } sg_interface;

    
    //
    // All sg backends need to implement a load function that can be
    // called to load the function pointers for the frontend to use.
    //
    sg_interface * sg_load();

    
#ifdef __cplusplus
}
#endif

#endif
