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

extern "C" {
#endif
    
    typedef void (empty_procedure)(void);
    
    typedef struct sg_interface
    {
        //
        // This defines the version of the structure to allow
        // future expansion. This should be checked before
        // the structure is initialised.
        //
        int version;
        
        //
        // The init procedure is called when the SwinGame starts
        // and should be used to initialise the underlying system.
        //
        empty_procedure *init;
        
    } sg_interface;

    
    //
    // All sg backends need to implement a load function that can be
    // called to load
    //
    bool load(sg_interface *functions);

    
#ifdef __cplusplus
}
#endif

#endif
