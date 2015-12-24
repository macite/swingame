//
//  sgBackendTypes.h
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_sgBackendTypes_h
#define sgsdl2_sgBackendTypes_h

// Align structs to one byte boundaries
//#pragma pack(push, 8)

#ifdef __cplusplus
extern "C" {
#endif

//
// A list of the available kinds of drawing surface.
// Drivers must support drawing onto these.
//
typedef enum sg_drawing_surface_kind
{
    SGDS_Unknown = 0,   // Unknown, so do not draw onto this!
    SGDS_Window = 1,    // A window
    SGDS_Bitmap = 2     // A surface, bitmap, or texture
} sg_drawing_surface_kind;

//
// A drawing surface is something the user can draw onto.
// The driver is then required to provide the ability to
// perform the requested drawing actions on the different
// kinds of drawing surface.
//
typedef struct sg_drawing_surface
{
    sg_drawing_surface_kind kind;
    int width;
    int height;
    
    // private data used by the backend
    void * _data;
} sg_drawing_surface;

typedef enum sg_renderer_flip
{
	SG_FLIP_NONE = 0,
	SG_FLIP_HORIZONTAL = 1,
	SG_FLIP_VERTICAL = 2,
	SG_FLIP_BOTH = 3
} sg_renderer_flip;

typedef struct sg_mode
{
    unsigned int format;
    int width, height, refresh_rate;
} sg_mode;

typedef struct sg_display
{
    const char *    name;
    int             x, y;
    int             width, height, refresh_rate;
    unsigned int    format;
    unsigned int    num_modes;
    sg_mode *       modes;
    
    // private data used by the backend
    void * _data;
} sg_display;

typedef struct sg_audiospec
{
    int audio_rate;
    int audio_format;
    int audio_channels;
    int times_opened;

} sg_audiospec;

typedef struct sg_system_data
{
    unsigned int    num_displays;
    sg_display    * displays;
    sg_audiospec    audio_specs;
    
} sg_system_data;

typedef enum sg_font_kind
{
  SGFT_UNKNOWN = 0,
  SGFT_TTF = 1
} sg_font_kind;

typedef struct sg_font_data
{
  sg_font_kind kind;

  // private data used by backend
  void * _data;
} sg_font_data;

typedef enum sg_sound_kind
{
    SGSD_UNKNOWN = 0,
    SGSD_SOUND_EFFECT = 1,
    SGSD_MUSIC = 2
} sg_sound_kind;

//
// Sound data is an audio chunk the user can play.
//
typedef struct sg_sound_data
{
    sg_sound_kind kind;
    
    // private data used by backend
    void * _data;
} sg_sound_data;

typedef enum sg_connection_kind
{
    SGCK_UNKNOWN = 0,
    SGCK_TCP = 1,
    SGCK_UDP = 2
} sg_connection_kind;

typedef struct sg_network_connection
{
    sg_connection_kind kind;
    
    // private data used by the backend
    void * _socket;
} sg_network_connection;

    
typedef void *pointer;

typedef struct sg_window_data
{
    int close_requested;
    int has_focus;
    int mouse_over;
    int shown;
} sg_window_data;
    
#ifdef __cplusplus
}
#endif
    
// Stop aligning structs to one byte boundaries
//#pragma pack(pop)

#endif
