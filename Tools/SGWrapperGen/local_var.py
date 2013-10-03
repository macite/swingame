#!/usr/bin/env python
# encoding: utf-8
"""
local_var.py

Created by Andrew Cain on 2011-01-05.
Copyright (c) 2011 Swinburne University. All rights reserved.
"""

import sys
import os

from sg.sg_parameter import SGParameter

class LocalVar(object):
    
    def __init__(self):
        self.name               = ''
        self.is_returned        = False
        self.is_length_param    = False
        self.maps_result        = False
        self.pass_through       = False
        self.has_field          = False
        self.has_length_param   = False
        self.maps_to_temp       = False
        
        self.local_for          = None
        self.data_type          = None
        self.modifier           = None
        self.field_name         = ''
        self.length_param       = None
        
    def setup_for_result(self, var_type):
        self.name               = 'result'
        self.maps_result        = True
        self.pass_through       = True
        self.modifier           = 'result'        
        self.data_type          = var_type
        
    def setup_for_parameter(self, for_parameter, lang_key):
        self.name = for_parameter.name + '_temp'
        
        if not for_parameter.maps_result:
            for_parameter.alias(lang_key).maps_to_temp = True
        else:
            self.is_returned = True
        
        self.local_for          = for_parameter
        self.data_type          = for_parameter.data_type
        self.modifier           = for_parameter.modifier
        self.maps_result        = for_parameter.maps_result
        self.pass_through       = for_parameter.pass_through
        self.length_param       = for_parameter.length_param
        self.has_length_param   = for_parameter.has_length_param
        
        if for_parameter.data_type.is_struct and for_parameter.data_type.wraps_array:
            self.has_field = True
            self.field_name = for_parameter.data_type.fields[0].name
    
    def setup_for_length(self, for_parameter):
        self.name = for_parameter.local_var_name() + '_len'
        self.is_length_param = True
        self.local_for = for_param
        self.data_type = find_or_add_type('Longint')
        self.modifier = for_param.modifier
        self.length_of = for_param
        
        for_param.length_param = local_var
        for_param.has_length_param = True

        to_method.local_vars.append(local_var)
        to_method.args.append(local_var.name)
        
    
    param_name = property(lambda self: self.local_for.name, None, None, 'The name of the property this local variable maps.')
    
    def __getattr__(self, attr):
        return getattr(self.local_for, attr)


def main():
    pass


if __name__ == '__main__':
    main()

