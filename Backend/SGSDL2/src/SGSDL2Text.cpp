//
//  SGSDL2Text.h
//  sgsdl2
//
//  Created by James Armstrong on 11/12/2013.
//

#include <SDL.h>
#include <SDL_ttf.h>
#include <iostream>

#include "SGSDL2Text.h"

void sgsdl2_init_text()
{
  if (TTF_Init() == -1)
  {
    std::cout << "Text loading is broken." << std::endl;
    exit(-1);
  }
}

void sgsdl2_finalize_text()
{
  TTF_Quit();
}

void sgsdl2_load_text_fns(sg_interface *functions)
{

}
