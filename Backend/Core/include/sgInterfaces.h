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
    
    typedef enum sg_interface_features
    {
        SGV_NONE = 0x00,
        SGV_INIT = 0x01
    } sg_interface_version;
    
    typedef void (sg_empty_procedure)(void);
    typedef sg_drawing_surface (sg_new_surface_fn)(const char *title, int width, int height);
    typedef void (sg_drawing_surface_proc)(sg_drawing_surface *);
    typedef void (sg_single_uint32param_proc)(unsigned int ms);
    
    
    typedef struct sg_utils_interface
    {
        //
        // Function to delay by a specified number of milliseconds.
        //
        sg_single_uint32param_proc *delay;
        
    } sg_utils_interface;
    
    typedef struct sg_graphics_interface
    {
        //
        // Open a new window and return its details.
        //
        sg_new_surface_fn *open_window;
        
        //
        // Close a previously open drawing surface
        //
        sg_drawing_surface_proc *close_drawing_surface;
        
    } sg_graphics_interface;
    
    typedef struct sg_interface
    {
        //
        // This defines the version of the structure to allow
        // future expansion. This should be checked before
        // the structure is initialised.
        //
        int version;
        
        //
        // features stores the features supported by the
        // loaded interface.
        //
        sg_interface_features features;
        
        //
        // is there currently an error
        //
        bool has_error;
        
        //
        // A pointer to the current error message. This error
        // is managed by the driver and must not be freed by
        // the driver's user.
        //
        const char *    current_error;
        
        //
        // The init procedure is called when the SwinGame starts
        // and should be used to initialise the underlying system.
        //
        sg_empty_procedure *init;
        
        //
        // function pointers by functionality
        //
        sg_graphics_interface graphics;
        sg_utils_interface utils;
        
    } sg_interface;

    
    //
    // All sg backends need to implement a load function that can be
    // called to load the function pointers for the frontend to use.
    //
    sg_interface * load_sg();

    
#ifdef __cplusplus
}
#endif

#endif
