#!/usr/bin/env python
# encoding: utf-8
"""
check_pas_parameters.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

_errors = 0

def param_visitor(param, last, other):
    '''Check that all types in the types list are passed by reference...'''
    
    if param.modifier in ['const', 'var', 'out', 'result']: return    
    if param.being_updated:
        print ' -------- NOTE --------'
        print param.name, 'is updating data passed in.'
        print param.file_line_details
        print ' ----------------------'
        return
    if param.data_type.is_struct or param.data_type.is_array:
        print ' ******* ERROR *********'
        print param.name
        print param.file_line_details
        print ' ***********************'
        global _errors
        _errors += 1

def method_visitor(method, other):
    print 'checking method   %s' % method.name
    method.visit_params(param_visitor, other)

def property_visitor(prop, other):
    print 'checking property %s' % prop.name
    
    if prop.getter != None:
        method_visitor(prop.getter, None)
    if prop.setter != None:
        method_visitor(prop.setter, None)


def visitor(the_file, other):
    for element in the_file.members:
        element.visit_properties(property_visitor, None)
        element.visit_methods(method_visitor, None)
        


def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    parser_runner.run_for_all_units(visitor)
    
    print '%s Errors' % _errors

if __name__ == '__main__':
    main()
