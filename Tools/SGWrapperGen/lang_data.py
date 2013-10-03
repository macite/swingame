#!/usr/bin/env python
# encoding: utf-8
"""
lang_data.py

Created by Andrew Cain on 2011-01-05.
Copyright (c) 2011 Swinburne University. All rights reserved.
"""

import sys
import os


class LangBasicData(object):
    
    def __init__(self, sg_member):
        self.sg_member      = sg_member
        self.signature      = ''
        self.code           = ''
        
    
    def __getattr__(self, attr):
        return getattr(self.sg_member, attr)

class LangMethodData(LangBasicData):
    
    def __init__(self, sg_method):
        LangBasicData.__init__(self, sg_method)
        self.params         = list(sg_method.params)
        self.args           = list(sg_method.args)
        self.local_vars     = list()
        self.return_type    = sg_method.return_type
        self.length_call    = sg_method.length_call
        self.field_name     = sg_method.field_name
        
    
    is_function = property (lambda self: self.return_type != None,
        None, None, 'The method is a function'
        )

class LangParamData(LangBasicData):
    
    def __init__(self, sg_param):
        LangBasicData.__init__(self, sg_param)
        self.maps_to_temp  = sg_param.maps_to_temp
    

    