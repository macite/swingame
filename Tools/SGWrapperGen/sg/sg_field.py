#!/usr/bin/env python
# encoding: utf-8
"""
SGField.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

from sg_metadata_container import SGMetaDataContainer

class SGField(SGMetaDataContainer):
    """Represents a Field, defining a getter and setter method"""
    
    def __init__(self, name):
        """initialises the Field setting its name."""
        
        SGMetaDataContainer.__init__(self, ['type'])
        
        self.name = name
        self.data_type = 'unknown'
    
    data_type = property(lambda self: self['type'].other, lambda self,type: self.set_tag('type', type), None, "The data type of the field.")
    
    def __str__(self):
        '''Return a string representation of the fielf'''
        return '%s %s' % (self.data_type, self.name)
    
    def __repr__(self):
        '''Return a string representation of the fielf'''
        return '%s %s' % (self.data_type, self.name)
        
    pascalName = property(lambda self: self.name.upper()[0] + self.name[1:], None, None, 'The name in PascalCase')
    

from sg_method import SGMethod

def test_field_creation():
    """tests the creation of a basic field"""
    my_field = SGField("name")
    
    assert my_field.name == "name"

def test_field_with_type():
    """tests creating a field with a type"""
    my_field = SGField("name")
    
    my_field.data_type = "String"
    
    assert my_field.name == "name"
    assert my_field.data_type == "String"

if __name__ == '__main__':
    import nose
    nose.run()
