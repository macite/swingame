//
//  sgInterfaces.h
//  sgsdl2
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_sgInterfaces_h
#define sgsdl2_sgInterfaces_h

//// Align structs to four byte boundaries
//#pragma pack(push, 8)

#include "sgBackendTypes.h"

#if defined(__WIN64__) || defined(WINNT) || defined(__WINNT) || defined(WIN64) || defined(_WIN64) || defined(__WIN64) || defined(WIN32) || defined(_WIN32) || defined(__WIN32) && !defined(__CYGWIN__)
#define WINDOWS
#endif

#ifdef __cplusplus
extern "C" {
#endif

    typedef struct sg_color
    {
        float r, g, b, a;
    } sg_color;

    typedef void (sg_empty_procedure)( void );

    typedef char * pchar;

    typedef pchar (sg_charp_fn)();
    typedef void (sg_rectangle_dimensions_proc)(int x, int y, int w, int h);

    typedef void (sg_charp_proc)(char* text);
    typedef void (sg_drawing_surface_proc)( sg_drawing_surface * );
    typedef int  (sg_drawing_surface_bool_fn)(sg_drawing_surface *);
    typedef void (sg_single_uint32param_proc)( unsigned int ms );
    typedef void (sg_drawing_surface_clr_proc)( sg_drawing_surface *surface, sg_color clr );
    typedef void (sg_drawing_proc)( sg_drawing_surface *surface, sg_color clr, float *data, int data_sz );
    typedef void (sg_clip_proc)( sg_drawing_surface *surface, float *data, int data_sz );
    typedef void (sg_surface_bool_proc)( sg_drawing_surface *surface, int value );
    typedef void (sg_to_pixel_array_proc)( sg_drawing_surface *surface, int *pixels, int sz );

    typedef void (sg_surface_size_proc)( sg_drawing_surface *surface, int width, int height );

    typedef sg_drawing_surface  (sg_new_surface_fn)(const char *title, int width, int height);
    typedef sg_drawing_surface  (sg_create_surface_fn)(int width, int height);

    typedef sg_color (sg_surface_color_fn)( sg_drawing_surface *surface, int x, int y );

    typedef sg_system_data * psg_system_data;
    typedef psg_system_data (sg_system_data_fn)();

    typedef int  (sg_drawing_surface_string_bool_fn)(sg_drawing_surface *, const char *); // (surface, string) -> bool

    //
    // Text related function pointers
    //
    typedef sg_font_data (sg_font_load_fn)(const char *filename, int font_size);
    typedef void (sg_font_data_proc)(sg_font_data* font);
    typedef int  (sg_font_int_fn)(sg_font_data* font);
    typedef void  (sg_font_int_proc)(sg_font_data* font,int style);
    typedef int  (sg_font_size_fn)(sg_font_data* font, char* text, int* w, int* h);
    typedef void (sg_draw_text_proc)(sg_drawing_surface *surface, sg_font_data* font, float x, float y, const char *text, sg_color clr);

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
    typedef int   (sg_int_intp_fn)( int val );

    typedef float (sg_float_soundp_fn)( sg_sound_data *sound );

    typedef sg_sound_data * (sg_sound_fn)();

    //
    // Utility related
    //
    typedef unsigned int  (sg_uint_fn)();

    //
    // Image related
    //
    typedef sg_drawing_surface  (sg_load_surface_fn)(const char *title);

    typedef void (sg_drawing_surface_surface_proc)( sg_drawing_surface * src, sg_drawing_surface * dst, float * src_data, int src_data_sz, float * dst_data, int dst_data_sz, sg_renderer_flip flip );

    //
    // Input related
    //
    typedef unsigned int (sg_mouse_state_fn)(int *x, int *y);
    typedef void (sg_window_pos_fn)(sg_drawing_surface *surface, int *x, int *y);
    typedef void (sg_surface_xy_proc)(sg_drawing_surface *surface, int x, int y);
    typedef pointer (pointer_fn)();
    typedef void (sg_window_xy_proc)(pointer window, int x, int y);
    typedef sg_window_data (sg_window_data_fn)(sg_drawing_surface *surface);

    //
    // Network related
    //
    typedef sg_network_connection (sg_create_network_fn)(const char *host, unsigned short port);
    typedef sg_network_connection (sg_create_udp_network_fn)(unsigned short port);
    typedef int (sg_network_data_fn)(sg_network_connection *connection, char *buffer, int size);
    typedef void (sg_connection_fn)(sg_network_connection *connection);
    typedef unsigned int (sg_connection_uint_fn)(sg_network_connection *connection);
    typedef sg_network_connection (sg_accept_connection_fn)(sg_network_connection *connection);
    typedef int (sg_udp_send_fn)(sg_network_connection *con, const char *host, unsigned short port, const char *buffer, int size);
    typedef void (sg_read_udp_fn)(sg_network_connection *con, unsigned int *host, unsigned short *port, char *buffer, unsigned int *size);

    //
    // Utility relation functions
    //
    // - delay = Function to delay by a specified number of milliseconds.
    //
    typedef struct sg_utils_interface
    {
        sg_single_uint32param_proc *    delay;
        sg_uint_fn *   get_ticks;
    } sg_utils_interface;


    //
    // Image related functions
    //
    // - creating and loading bitmaps
    // - drawing bitmaps
    //
    typedef struct sg_image_interface
    {
        sg_create_surface_fn * create_bitmap;

        sg_load_surface_fn * load_bitmap;

        sg_drawing_surface_surface_proc * draw_bitmap;
    } sg_image_interface;

    //
    // Audio related functions
    //
    // - loading and playing sound effects and music
    //
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

        sg_float_fn         * music_playing;
        sg_sound_fn         * current_music;

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

        sg_clip_proc * set_clip_rect;
        sg_drawing_surface_proc * clear_clip_rect;

        sg_to_pixel_array_proc * to_pixels;

        sg_surface_bool_proc * show_border;

        sg_surface_bool_proc * show_fullscreen;

        sg_surface_size_proc * resize;

        sg_drawing_surface_string_bool_fn * save_png;

    } sg_graphics_interface;


    //
    // Input callback functions
    //
    typedef struct sg_input_callbacks
    {
        sg_empty_procedure * do_quit;

        sg_intp_proc * handle_key_down;
        sg_intp_proc * handle_key_up;

        sg_intp_proc * handle_mouse_up;
        sg_intp_proc * handle_mouse_down;

        sg_charp_proc * handle_input_text;

        sg_window_xy_proc * handle_window_resize;
        sg_window_xy_proc *handle_window_move;
    } sg_input_callbacks;

    //
    // Input functions
    //
    typedef struct sg_input_interface
    {
        sg_empty_procedure * process_events;
        sg_drawing_surface_bool_fn * window_close_requested;
        sg_int_intp_fn * key_pressed;
        sg_mouse_state_fn * mouse_state;
        sg_mouse_state_fn * mouse_relative_state;
        sg_int_intp_fn * mouse_cursor_state;
        sg_surface_xy_proc * warp_mouse;

        sg_rectangle_dimensions_proc * start_unicode_text_input;
        sg_empty_procedure * stop_unicode_text_input;

        pointer_fn * focus_window;
        sg_window_pos_fn * window_position;

        sg_window_data_fn * get_window_event_data;
        sg_surface_xy_proc * move_window;
    } sg_input_interface;

    //
    // Text functions
    //

    enum sg_font_style
    {
        SG_FONT_STYLE_NORMAL = 0,
        SG_FONT_STYLE_BOLD = 1,
        SG_FONT_STYLE_ITALIC = 2,
        SG_FONT_STYLE_UNDERLINE = 4,
        SG_FONT_STYLE_STRIKETHROUGH = 8
    };

    typedef struct sg_text_interface
    {
        sg_font_load_fn * load_font;
        sg_font_data_proc * close_font;
        sg_font_int_fn * text_line_skip;
        sg_font_size_fn * text_size;
        sg_font_int_fn * get_font_style;
        sg_font_int_proc * set_font_style;
        sg_draw_text_proc * draw_text;
    } sg_text_interface;

    typedef struct sg_network_interface
    {
        sg_create_network_fn * open_tcp_connection;
        sg_create_udp_network_fn * open_udp_connection;
        sg_network_data_fn * read_bytes;
        sg_network_data_fn * send_bytes;
        sg_connection_fn * close_connection;
        sg_connection_uint_fn * network_address;
        sg_connection_uint_fn * network_port;
        sg_accept_connection_fn * accept_new_connection;
        sg_uint_fn * network_has_data;
        sg_connection_uint_fn * connection_has_data;
        sg_udp_send_fn * send_udp_message;
        sg_read_udp_fn * read_udp_message;
    } sg_network_interface;

    //
    // Web related
    //

    typedef enum sg_http_method
    {
        HTTP_GET,
        HTTP_POST,
        HTTP_PUT,
        HTTP_DELETE
    } sg_http_method;

    typedef struct sg_http_request
    {
        sg_http_method request_type;
        const char *url;
        unsigned short port;
        const char *body;
    } sg_http_request;

    typedef struct sg_http_response
    {
        unsigned short status;
        unsigned int size;
        char *data;
    } sg_http_response;

    typedef sg_http_response (sg_http_request_fn) (sg_http_request request);
    typedef void (sg_free_response_fn) (sg_http_response * response);

    typedef struct sg_web_interface
    {
        sg_http_request_fn * http_request;
        sg_free_response_fn * free_response;
    } sg_web_interface;

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
        int                 has_error;
        const char *        current_error;

        sg_empty_procedure  *init;
        sg_empty_procedure  *finalise;

        //
        // function pointers by functionality
        //
        sg_audio_interface      audio;
        sg_graphics_interface   graphics;
        sg_image_interface      image;
        sg_input_interface      input;
        sg_text_interface       text;
        sg_utils_interface      utils;
        sg_network_interface    network;
        sg_web_interface        web;

        //
        // callback
        //
        sg_input_callbacks      input_callbacks;

        //
        // system data
        //
        sg_system_data_fn   *read_system_data;
    } sg_interface;


    //
    // All sg backends need to implement a load function that can be
    // called to load the function pointers for the frontend to use.
    //
    #ifdef WINDOWS
      #ifdef BUILDING_DLL
        #define DLL_DECL_SPEC __declspec(dllexport)
      #else
        #define DLL_DECL_SPEC __declspec(dllimport)
      #endif
      sg_interface * DLL_DECL_SPEC sg_load(sg_input_callbacks callbacks);
    #else
      sg_interface * sg_load(sg_input_callbacks callbacks);
    #endif

#ifdef __cplusplus
}
#endif

// Stop aligning structs to one byte boundaries
//#pragma pack(pop)

#endif
