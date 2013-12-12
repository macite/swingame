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
  font._data = TTF_OpenFont(filename, font_size);
  return font;
}



void sgsdl2_close_font(sg_font_data* font)
{
  if (font->_data) 
  {
    TTF_CloseFont((TTF_Font*)font->_data);
    font->kind = SGFT_UNKNOWN; 
    font->_data = NULL; 
  }
}

int sgsdl2_text_line_skip(sg_font_data* font) 
{ 
  return TTF_FontLineSkip((TTF_Font*)font->_data);
}

int sgsdl2_text_size(sg_font_data* font, char* text, int* w, int* h) 
{
  return TTF_SizeText((TTF_Font*)font->_data, text, w, h); 
}


void sgsdl2_set_font_style(sg_font_data* font,int style) 
{
  TTF_SetFontStyle((TTF_Font*)font->_data, style); 
}

int sgsdl2_get_font_style(sg_font_data* font) 
{
  return TTF_GetFontStyle((TTF_Font*)font->_data); 
}

void sgsdl2_load_text_fns(sg_interface *functions)
{
  functions->text.load_font = &sgsdl2_load_font;
  functions->text.close_font = &sgsdl2_close_font;
  functions->text.text_line_skip = &sgsdl2_text_line_skip; 
  functions->text.text_size = &sgsdl2_text_size; 
  functions->text.get_font_style = &sgsdl2_get_font_style; 
  functions->text.set_font_style = &sgsdl2_set_font_style; 
}
