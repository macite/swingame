#!/usr/bin/env python
# encoding: utf-8
"""
SGProperty.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

from sg_cache import logger
from sg_metadata_container import SGMetaDataContainer 

class SGProperty(SGMetaDataContainer):
    """Represents a property, defining a getter and setter method"""
    
    def __init__(self, name):
        """
        initialises the property setting its name. 
        
        getter and setter are set to none
        """
        SGMetaDataContainer.__init__(self, ['getter','setter','data_type','class'])
        
        self.name = name
        self.getter = None
        self.setter = None
        self.data_type = None
        self.in_class = None
        self.is_static = False
    
    def set_getter(self, method):
        """sets the getter method of the property"""
        self.set_tag('getter', method)
        if method == None: return;
        if self.data_type == None:
            self.data_type = method.return_type;
        elif self.data_type != method.return_type:
            logger.error('Inconsistent types in property %s: %s is not %s',
                self.name, self.data_type, method.return_type)
    
    def set_setter(self, method):
        """sets the setter method of the property"""
        self.set_tag('setter', method)
        if method == None: return;
        if len(method.params) != 1:
            logger.error('Model error: setter method %s of property %s does '+
                ' not have exactly 1 parameter', method.name, self.name)
        if self.data_type == None:
            self.data_type = method.params[0].data_type;
        elif self.data_type != method.params[0].data_type:
            logger.error('Inconsistent types in property %s: %s is not %s',
                self.name, self.data_type, method.params[0].data_type)
    
    data_type = property(lambda self: self['data_type'].other, 
        lambda self,data_type: self.set_tag('data_type', data_type), 
        None, 'The data type of the property.')
    
    getter = property(lambda self: self['getter'].other, 
        set_getter, 
        None, 'The getter for the property.')
    
    setter = property(lambda self: self['setter'].other, 
        set_setter, 
        None, 'The setter for the property.')
    
    name = property(lambda self: self['name'].other, 
        lambda self,name: self.set_tag('name', name), 
        None, 'The name of the property.')
        
    in_class = property(lambda self: self['class'].other, 
        lambda self,value: self.set_tag('class', value), 
        None, 'The class containing the property')
    
    def __str__(self):
        return '%s %s.%s' % (self.data_type, str(self.in_class), self.name)

def test_property_creation():
    """tests the creation of a basic property"""
    my_property = SGProperty("name")
    
    assert my_property.name == "name"
    assert my_property.getter == None
    assert my_property.setter == None
    assert my_property.data_type == None

def test_setting_setter():
    """tests the adding of a setter"""
    from SGMethod import SGMethod
    
    my_property = SGProperty("name")
    my_setter = SGMethod("setName")
    my_setter.create_parameter('test')
    
    my_property.set_setter(my_setter)
    
    assert my_property.name == "name"
    assert my_property.getter == None
    assert my_property.setter == my_setter

def test_setting_getter():
    """tests the adding of a getter"""
    from SGMethod import SGMethod
    
    my_property = SGProperty("name")
    my_getter = SGMethod("name")
    
    my_property.set_getter(my_getter)
    
    assert my_property.name == "name"
    assert my_property.getter == my_getter
    assert my_property.setter == None

if __name__ == '__main__':
    import nose
    nose.run()
