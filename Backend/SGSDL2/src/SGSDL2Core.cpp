//
//  SGSDL2Core.cpp
//  sgsdl2
//
//  Created by Andrew Cain on 19/11/2013.
//  Copyright (c) 2013 Andrew Cain. All rights reserved.
//

#include "SGSDL2Core.h"

#include "sgInterfaces.h"

using namespace std;

void init_sgsdk2()
{
    cout << "init" << endl;
}


extern "C"
{
    
bool load(sg_interface *functions)
{
    cout << "load" << endl;
    
    functions->init = &init_sgsdk2;
    
    return true;
}

}