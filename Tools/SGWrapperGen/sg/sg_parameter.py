#!/usr/bin/env python
# encoding: utf-8
"""
SGParameter.py

Created by Andrew Cain on 2009-05-21.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

from sg_metadata_container import SGMetaDataContainer

class SGParameter(SGMetaDataContainer):
    """Represents a parameter to a method"""
    
    def __init__(self, name, data_type = None):
        """initialise the parameter with a name, sets type to None"""
        SGMetaDataContainer.__init__(self, ['type','modifier','maps_result','related_params'])
        self.name = name
        self.data_type = data_type
        self.modifier = None
        #post parse values
        self.maps_result = False
        self.maps_to_temp = False
        self.is_length_param = False
        self.has_length_param = False   # does this have a length param? (var arrays)
        self.length_idx = -1            # which param is the length param for this?
        self.length_of = None           # the SGParameter this represents the length of...
        self.length_param = None           # the SGParameter this represents the length of...
        self.local_for = None           # Used by local variables to link to their associated parameter
        self.has_field = False          # Used to check if a parameter/arg has a field (i.e. array wrapper)
        self.is_returned = False        # Does this variable actually contain the return value (must be maps_result)
        self.pass_through = False       # This parameter exists just to pass the result out after out parameters...
        self.being_updated = False       # This parameter is updated (so ignore reporting errors on arrays when parameters are checked) 
        self.related_params = None
    
    # def set_as_output(self):
    #     """marks this as an output parameter"""
    #     self.set_tag('output')
    # 
    # def is_output(self):
    #     """returns True if the parameter is an output parameter"""
    #     #print self.tags.keys()
    #     return 'output' in self.tags.keys()
    # 
    # def set_as_array(self):
    #     """marks this as an array parameter"""
    #     self.set_tag('array')
    # 
    def is_array(self):
        """returns True if the parameter is an array parameter"""
        #print self.tags.keys()
        return self.data_type.is_array
    
    def __str__(self):
        return '%s%s %s'% ( 
            self.modifier + ' ' if self.modifier != None else '', 
            self.data_type, 
            self.name)
    
    def __repr__(self):
        return self.name
    
    def arg_name(self, lang_key):
        alias = self.alias(lang_key)
        
        return '%s%s' % (
            alias.name,
            '_temp' if alias.maps_to_temp else ''
            )
    
    data_type = property(lambda self: self['type'].other, lambda self,the_type: self.set_tag('type', the_type), None, "The data type of the parameter.")
    modifier = property(lambda self: self['modifier'].other, lambda self,value: self.set_tag('modifier', value), None, "The modifier of the parameter.")
    maps_result = property(lambda self: self['maps_result'].other, 
        lambda self,value: self.set_tag('maps_result', value), 
        None, "The parameter wraps the result of a function.")
    
    def add_related_params(self, lst):
        # print 'Added related parameters to', self.name, ' related = ', lst
        self.related_params = lst
    
    def local_var_name(self):
        """returns the local variable name that this would be copied to if needed, otherwise the parameter name"""
        
        if self.maps_result:
            return 'result'
        elif self.data_type.array_wrapper:
            return self.name + '_temp'
        else:
            return self.name
            
    def clone(self):
        """ Returns a copy of the Parameter."""
        result = SGParameter(self.name)
        result.data_type = self.data_type
        result.modifier = self.modifier
        
        #post parse values
        result.maps_result = self.maps_result
        result.maps_to_temp = self.maps_to_temp
        result.is_length_param = self.is_length_param
        result.has_length_param = self.has_length_param
        result.length_idx = self.length_idx
        result.length_of = self.length_of
        result.length_param = self.length_param
        result.local_for = self.local_for
        result.has_field = self.has_field
        result.is_returned = self.is_returned
        result.pass_through = self.pass_through
        result.updated = self.being_updated
        result.related_params = self.related_params
        
        return result
#
# Test methods
#

def test_parameter_creation():
    """tests basic creation of a parameter"""
    my_param = SGParameter("test")
    
    assert my_param.name == "test"
    assert len(my_param.tags) == 3 #type,name,modifier
    assert my_param.data_type == None

def test_output_param():
    """tests the creation of an output parameter"""
    my_param = SGParameter("Test")
    assert False == my_param.is_output()
    
    my_param.set_as_output()
    assert my_param.is_output()

def test_array_param():
    """tests the creation of an array parameter"""
    my_param = SGParameter("Test")
    assert False == my_param.is_array()
    
    my_param.set_as_array()
    assert my_param.is_array()

def test_type_param():
    """tests the setting of a parameters type"""
    my_param = SGParameter("Test")
    assert None == my_param.data_type
    
    my_param.data_type = "SoundEffect"
    assert "SoundEffect" == my_param.data_type

if __name__ == '__main__':
    import nose
    nose.run()