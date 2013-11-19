//
//  main.cpp
//  SGSDL2Test
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include <iostream>

#include "sgInterfaces.h"

int main(int argc, const char * argv[])
{
    sg_interface functions;
    
    functions.version = 100;
    
    // insert code here...
    std::cout << "Hello, World!\n";
    
    std::cout << load(&functions) << std::endl;
    
    functions.init();
    
    return 0;
}

