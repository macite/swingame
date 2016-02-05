//
//  sgBackendUtils.h
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifndef __sgsdl2__sgBackendUtils__
#define __sgsdl2__sgBackendUtils__

#include "sgInterfaces.h"

#ifdef __cplusplus
extern "C" {
#endif

//
// Default storage for a backend's function pointers.
// This will be loaded in load_sg and a pointer returned
// to the caller.
//
extern sg_interface _functions;


//
// Storage for the system data
//
extern sg_system_data _sgsdk_system_data;

//
// Clear all function pointers by initialising
// all to NULL. Can be used on startup and on
// fatal errors.
//
void clear_functions();

//
// Set the current error state of the driver.
//
void set_error_state(const char *error);

//
// Clear the current error.
//
void clear_error();


#ifdef __cplusplus
}
#endif


#endif /* defined(__sgsdl2__sgBackendUtils__) */
