//
//  sgBackendTypes.h
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_sgBackendTypes_h
#define sgsdl2_sgBackendTypes_h

//
// A list of the available kinds of drawing surface.
// Drivers must support drawing onto these.
//
enum sg_drawing_surface_kind
{
    SGDS_Unknown,   // Unknown, so do not draw onto this!
    SGDS_Window,    // A window
    SGDS_Bitmap     // A surface, bitmap, or texture
};

//
// A drawing surface is something the user can draw onto.
// The driver is then required to provide the ability to
// perform the requested drawing actions on the different
// kinds of drawing surface.
//
typedef struct sg_drawing_surface
{
    sg_drawing_surface_kind kind;
    
    // private data used by the backend
    void * _data;
} sg_drawing_surface;

#endif
