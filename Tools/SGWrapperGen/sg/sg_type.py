#!/usr/bin/env python
# encoding: utf-8
"""
SGType.py

Created by Andrew Cain on 2009-05-29.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging

from sg_metadata_container import SGMetaDataContainer
from sg_cache import logger

class SGType(SGMetaDataContainer):
    """Represents a data type."""
    
    def __init__(self, name):
        """Initialise the type, setting its name"""
        SGMetaDataContainer.__init__(self, ['fields',
            'class','uname', 'dimensions','nested_type', 'related_type',
            'pointer_wrapper','no_free_pointer_wrapper', 'data_wrapper', 'values', 'is_pointer',
            'struct', 'enum', 'array_wrapper', 'fixed_array_wrapper', 'type', 'via_pointer', 'sameas'])
        self.name = name
        self.set_tag('fields', [])
        self.dimensions = None
        self.related_type = None
        self.nested_type = None
        self.pointer_wrapper = False
        self.no_free_pointer_wrapper = False
        self.data_wrapper = False
        self.array_wrapper = False
        self.fixed_array_wrapper = False
        self.values = None
        self.is_pointer = False
        self.is_class = False
        self.is_type = False
        self.is_struct = False
        self.is_procedure = False
        self.method = None
        self.same_as = None
        self.via_pointer = False # is the type accessed via a pointer (i.e. C# cant get to it :) )
    
    def __str__(self):
        '''String rep'''
        return self.name
    
    def __repr__(self):
        return self.name
    
    def clone(self, other):
        '''Clones all but name'''
        self.set_tag('fields',other.fields)
        self.dimensions = other.dimensions
        self.is_pointer = other.is_pointer
        self.via_pointer = other.via_pointer
        self.related_type = other
        self.is_struct = other.is_struct
    
    fields = property(lambda self: self['fields'].other, None, None, "The fields for the type.")
    
    dimensions = property(lambda self: self['dimensions'].other, 
        lambda self, value: self.set_tag('dimensions', value), 
        None, 'The dimensions of this if it is an array')
    
    is_pointer = property(lambda self: self['is_pointer'].other, 
        lambda self, value: self.set_tag('is_pointer', value), 
        None, 'The type is a pointer to its related type')
    
    via_pointer = property(lambda self: self['via_pointer'].other, 
        lambda self, value: self.set_tag('via_pointer', value),
        None, 'The type is accessed via a Pointer and may be hidden from other languages.')
    
    is_class = property(lambda self: self['class'].other, 
        lambda self, value: self.set_tag('class', value), 
        None, 'The type is class')
        
    is_type = property(lambda self: self['type'].other, 
        lambda self, value: self.set_tag('type', value), 
        None, 'The type is a type mapping')
    
    is_struct = property(lambda self: self['struct'].other, 
        lambda self, value: self.set_tag('struct', value), 
        None, 'The type is struct')
    
    nested_type = property(lambda self: self['nested_type'].other, 
        lambda self, value: self.set_tag('nested_type', value), 
        None, 'The other type contained within this type eg. array of ...')
    
    related_type = property(lambda self: self['related_type'].other, 
        lambda self, value: self.set_tag('related_type', value), 
        None, 'The other type this one relates to')
    
    same_as = property(lambda self: self['sameas'].other, 
        lambda self, value: self.set_tag('sameas', value), 
        None, 'The type is the same as this other type...')
    
    pointer_wrapper = property(lambda self: self['pointer_wrapper'].other, 
        lambda self, value: self.set_tag('pointer_wrapper', value), 
        None, 'This type wraps a pointer')
    
    no_free_pointer_wrapper = property(lambda self: self['no_free_pointer_wrapper'].other, 
        lambda self, value: self.set_tag('no_free_pointer_wrapper', value), 
        None, 'This type wraps a pointer that has no free procedure')
    
    data_wrapper = property(lambda self: self['data_wrapper'].other, 
        lambda self, value: self.set_tag('data_wrapper', value), 
        None, 'This type wraps a pointer')
    
    array_wrapper = property(lambda self: self['array_wrapper'].other, 
        lambda self, value: self.set_tag('array_wrapper', value), 
        None, 'This type wraps a variable length array')
    
    fixed_array_wrapper = property(lambda self: self['fixed_array_wrapper'].other, 
        lambda self, value: self.set_tag('fixed_array_wrapper', value), 
        None, 'This type wraps a fixed length array')
    
    wraps_array = property( lambda self: self.array_wrapper or self.fixed_array_wrapper, 
        None, None, 'Is a variable or fixed length array wrapper')
    
    
    values = property(lambda self: self['values'].other, 
        lambda self, value: self.set_tag('values', value), 
        None, 'The values for a enumeration')
        
    camel_name = property(lambda self: self.name.lower()[0] + self.name[1:],
        None, None, 'The camelCase name.' )
    
    is_array = property(lambda self: self.dimensions != None, None, None, 'Indicates that this type is an array')
    is_enum = property(lambda self: self.values != None, None, None, 'Indicates that this type is an enumeration')

def main():
  pass

if __name__ == '__main__':
  main()

