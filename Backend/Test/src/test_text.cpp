//
//  test_text.h
//  sgsdl2
//
//  Created by James Armstrong on 11/12/2013.
//

#include "test_text.h"

#include <iostream>

using namespace std;

#include "sgInterfaces.h"

extern sg_interface * _sg_functions;

void test_text()
{
  cout <<  "Testing text " << endl; 
  sg_font_data font =  _sg_functions->text.load_font("BATTLEST.TTF",14); 

  int line_skip = _sg_functions->text.text_line_skip(&font); 
  cout << " The line skip is: " << line_skip << endl; 

  int w; 
  int h; 
  int text_size = _sg_functions->text.text_size(&font,(char*)"Hello World", &w, &h); 
  cout << "Result for text size is : " << text_size << " with a width of: " << w << " and a height of: " << h << endl; 



  _sg_functions->text.set_font_style(&font, SG_FONT_STYLE_ITALIC); 
  int font_style = _sg_functions->text.get_font_style(&font); 
  cout << " The font style is: " << font_style <<  endl; 

  _sg_functions->text.close_font(&font); 
  _sg_functions->text.close_font(&font); 

}
