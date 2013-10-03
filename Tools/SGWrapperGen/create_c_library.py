# #!/usr/bin/env python
# # encoding: utf-8
# """
# create_c_lib.py
# 
# Created by Andrew Cain on 2009-06-02.
# Copyright (c) 2009 Swinburne. All rights reserved.
# """
# 

import generated_folders

import logging
import sys
import os
from lang_c import create_c_code_for_file, write_c_code_files
from sg import parser_runner

def create_swingame_h():
  c_generated_lib = '../../Generated/C/lib/'
  c_common_template = '../../Templates/C/Common/lib/'
  swingame_h_path = c_generated_lib+'SwinGame.h'

  if os.path.isfile(swingame_h_path):
    os.remove(swingame_h_path)
  f = open(c_generated_lib+'SwinGame.h',"w")
  f.write( "#ifndef SWINGAME\n" )
  f.write("#define SWINGAME\n" )
  
  for filename in os.listdir(c_generated_lib):
    if(filename.find('SGSDK') == -1) and (filename.find('.h') >=0):
      f.write("""#include "%s" \n""" % filename)
  
  for filename in os.listdir(c_common_template):
    if(filename.find('SGSDK') == -1) and (filename.find('.h') >=0):
      f.write("""#include "%s" \n""" % filename)

  f.write("#endif")
  f.close()  


def main():
  logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
  
  parser_runner.parse_all_units()
  parser_runner.visit_all_units(create_c_code_for_file)
  parser_runner.visit_all_units(write_c_code_files)
  create_swingame_h()
if __name__ == '__main__':
  main()

