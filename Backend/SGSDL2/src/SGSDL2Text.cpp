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
#include "sgBackendTypes.h"

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

sg_font_data sgsdl2_load_font(const char * filename, int font_size)
{
  sg_font_data font;
  font.kind = SGFT_TTF;
  font.data = TTF_OpenFont(filename, font_size);
  return font;
}

void sgsdl2_close_font(sg_font_data font)
{
  TTF_CloseFont((TTF_Font*)font.data);
}

void sgsdl2_load_text_fns(sg_interface *functions)
{
  functions->text.load_font = &sgsdl2_load_font;
  functions->text.close_font = &sgsdl2_close_font;
}
