#!/usr/bin/env python
# encoding: utf-8
"""
create_pascal_lib.py

This script creates the code for the SwinGame.pas unit used in the Pascal template.

Steps include:
1: Parsing swingame source files
2: 

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import generated_folders

import logging
import sys

from lang_pas_unit import create_pas_code_for_file, write_pas_unit_code
from lang_c import create_c_code_for_file
from sg import parser_runner

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    parser_runner.parse_all_units()
    parser_runner.visit_all_units(create_pas_code_for_file)
    write_pas_unit_code()

if __name__ == '__main__':
    main()
