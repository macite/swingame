//
//  sgBackendUtils.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 20/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "sgBackendUtils.h"
#include <iostream>
#include <cstring>

// Storage for the sg functions
sg_interface _functions;

// Storage for the system data
sg_system_data _sgsdk_system_data;


// Set all functions to NULL to initialise/clear structure
void clear_functions()
{
    memset(&_functions, 0, sizeof(_functions));
}

// Set the current error.
void set_error_state(const char *error)
{
    _functions.has_error = true;
    _functions.current_error = error;
}

// Clear the error state and its message
void clear_error()
{
    _functions.has_error = false;
    _functions.current_error = NULL; // message is not owned dont free
}

