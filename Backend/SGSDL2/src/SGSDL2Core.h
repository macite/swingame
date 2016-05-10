//
//  SGSDL2Core.h
//  sgsdl2
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#ifndef sgsdl2_SGSDL2Core_h
#define sgsdl2_SGSDL2Core_h


//
// _fatal_error indicates that the driver has failed
// to load correctly and that no other op
//
extern bool _fatal_error;

void set_error_state();

void internal_sgsdl2_init();


#endif /* defined(__sgsdl2__SGSDL2Core__) */
