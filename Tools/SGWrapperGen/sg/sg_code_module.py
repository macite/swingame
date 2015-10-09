#!/usr/bin/env python
# encoding: utf-8
"""
sgcodemodule.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging

from sg_method import SGMethod
from sg_property import SGProperty
from sg_field import SGField
from sg_metadata_container import SGMetaDataContainer
from sg_cache import find_or_add_type
from sg_parameter import SGParameter

logger = logging.getLogger("SGWrapperGen")

class SGCodeModule(SGMetaDataContainer):
    """Represents a class in the comment meta language."""
    
    def __init__(self, name):
        """Initialise the class, setting its name"""
        super(SGCodeModule,self).__init__(['static','module_kind',
            'data_wrapper','pointer_wrapper', 'no_free_pointer_wrapper','type_name', 'doc_types', 
            'values', 'array_wrapper', 'fixed_array_wrapper', 'via_pointer', 'sameas'])
        self.name = name
        self.type_name = name
        self.methods = dict()
        self.operators = dict()
        self.properties = dict()
        self.fields = dict()
        self.field_list = list()
        self.values = list()
        self.is_static = False
        self.module_kind = 'unknown'
        self.is_pointer_wrapper = False
        self.is_no_free_pointer_wrapper = False;
        self.is_data_wrapper = False
        self.is_array_wrapper = False
        self.is_fixed_array_wrapper = False
        self.via_pointer = False
        self.data_type = None
        self.doc_type = None
        self.same_as = None
    
    def to_keyed_dict(self, doc_transform = None, type_visitor = None, array_idx_sep = ', ', param_visitor = None, map_data_value = None):
        """Export a keyed dictionary of the class for template matching"""
        
        import wrapper_helper
        
        result = dict()
        result['name'] = self.name
        result['name_lower'] = wrapper_helper.lower_name(self.name)
        result['camel_name'] = self.name.lower()[0] + self.name[1:]
        result['doc'] = doc_transform(self.doc) if doc_transform != None else self.doc
        result['static'] = 'static ' if self.is_static else ''
        result['sealed'] = 'sealed ' if self.is_module else ''
        
        if self.wraps_array:
            # add accessor methods
            main_type = self.data_type.related_type
            dim = main_type.dimensions
            data_type = find_or_add_type('longint')
            idx_type = type_visitor(data_type) if type_visitor != None else 'int'
            
            result['element.type'] = type_visitor(main_type.nested_type)
            
            if param_visitor == None:
                result['element.idx.params'] =  ', '.join([idx_type + ' idx%s' % i for i,d in enumerate(dim)])
            else:
                result['element.idx.params'] =  ''.join([param_visitor(SGParameter('idx%s' % i, data_type), i + 1 == len(dim)) for i,d in enumerate(dim)])
            
            result['element.idx.expr'] = array_idx_sep.join(['idx%s' % i for i,d in enumerate(dim)])
            
            result['element.access'] = map_data_value('return_val', main_type.nested_type, 'data[%(element.idx.expr)s]' % result)
            result['element.value'] = map_data_value('arg_val', main_type.nested_type, 'value')
        
        return result
    
    def add_member(self, member):
        """Add a member (method, property) to the class"""
        if isinstance(member, SGMethod):
            logger.info(' Code Modul: Adding method %s.%s(%s)', self.name, member.name, member.param_string())
            if self.is_static: member.is_static = True
            if member.is_operator:
                self.operators[member.signature] = member
            else:
                self.methods[member.signature] = member
        elif isinstance(member, SGProperty):
            logger.info(' Code Modul: Adding property %s.%s', self.name, member.name)
            self.properties[member.name] = member
        elif isinstance(member, SGField):
            logger.info(' Code Modul: Adding field %s.%s', self.name, member.name)
            self.fields[member.name] = member
            self.field_list.append(member)
        else:
            logger.error('Model Error: Unknown member type')
            assert False
    
    def del_member(self, member):
        if isinstance(member, SGMethod):
            del(self.methods[member.name])
        else:
            raise Exception, "Unknown member type"
    
    def find_method(self, name):
        m = lambda method, other : method if method.uname == name and other == None else other
        result = self.visit_methods(m, None)
        
        if result != None: return result
        
        logger.error('  CODE MODUL: Error finding method %s in class %s', name, self.name)
        assert False
    
    def number_of_methods_named(self, name):
        result = 0
        for method in self.methods.values():
            if method.name == name:
                result += 1
                
        return result
    
    def set_tag(self, title, other = None):
        if title == 'class':
            self.module_kind = 'class'
            super(SGCodeModule,self).set_tag('name', other)
            super(SGCodeModule,self).set_tag('uname', other)
        elif title == 'type':
            # print 'type ', self
            self.module_kind = 'type'
            super(SGCodeModule,self).set_tag('name', other)
        elif title == 'module':
            self.module_kind = 'module'
            super(SGCodeModule,self).set_tag('name', other)
        elif title == 'header':
            self.module_kind = 'header'
            self['in_file'].other.has_body = False
            super(SGCodeModule,self).set_tag('name', other)
        elif title == 'struct':
            self.module_kind = 'struct'
            super(SGCodeModule,self).set_tag('name', other)
        elif title == 'enum':
            self.module_kind = 'enum'
            super(SGCodeModule,self).set_tag('name', other)
        else:
            super(SGCodeModule,self).set_tag(title, other)
    
    is_static = property(lambda self: self['static'].other, 
        lambda self,value: self.set_tag('static', value), 
        None, 'Is the class static (no instance members).')
    
    is_pointer_wrapper = property(lambda self: self['pointer_wrapper'].other, 
        lambda self,value: self.set_tag('pointer_wrapper', value), 
        None, 'Does the class wrap a pointer in SwinGame')
    
    is_data_wrapper = property(lambda self: self['data_wrapper'].other, 
        lambda self,value: self.set_tag('data_wrapper', value), 
        None, 'Does the class wrap data from SwinGame')
    
    is_array_wrapper = property(lambda self: self['array_wrapper'].other, 
        lambda self,value: self.set_tag('array_wrapper', value), 
        None, 'Does the class wrap a variable length array of data from SwinGame')

    same_as = property(lambda self: self['sameas'].other, 
        lambda self, value: self.set_tag('sameas', value), 
        None, 'The type is the same as this other type...')
    
    is_fixed_array_wrapper = property(lambda self: self['fixed_array_wrapper'].other, 
        lambda self,value: self.set_tag('fixed_array_wrapper', value), 
        None, 'Does the class wrap a fixed length array of data from SwinGame')
    
    module_kind = property(lambda self: self['module_kind'].other, 
        lambda self,value: self.set_tag('module_kind', value), 
        None, 'The kind of code module this represents (class,module,library).')
    
    type_name = property(lambda self: self['type_name'].other, 
        lambda self,value: self.set_tag('type_name', value), 
        None, 'The name of the type in Pascal.')
    
    values = property(lambda self: self['values'].other, 
        lambda self,value: self.set_tag('values', value), 
        None, 'The values of an enum member.')
    
    doc_types = property(lambda self: self['doc_types'].other, 
        lambda self,value: self.set_tag('doc_types', value), 
        None, 'The types that should be printed alongside the documentation of this module.')
    
    is_class = property(lambda self: self.module_kind == 'class', 
        None, None, 'Is the module a class?')
    
    is_type = property(lambda self: self.module_kind == 'type', 
        None, None, 'Is the module a type?')
    
    is_library = property(lambda self: self.module_kind == 'library', 
        None, None, 'Is the module a library?')
    
    is_module = property(lambda self: self.module_kind == 'module', 
        None, None, 'Is the module a module?')
        
    is_header = property(lambda self: self.module_kind == 'header', 
        None, None, 'Is the module a header?')
    
    is_enum = property(lambda self: self.module_kind == 'enum', 
        None, None, 'Is the module a enum?')
    
    is_struct = property(lambda self: self.module_kind == 'struct', 
        None, None, 'Is the module a structure?')
    
    wraps_array = property( lambda self: self.is_array_wrapper or self.is_fixed_array_wrapper, 
        None, None, 'Is a variable or fixed length array wrapper')
    
    def setup_from(self, the_type):
        self.doc = the_type.doc
        fields = the_type.fields
        for field in fields:
            self.add_member(field)
            
        self.data_type = the_type
        
        if the_type.is_class:
            self.module_kind = 'class'
            self.name = the_type['class'].other
            self.uname = self.name
            self.is_pointer_wrapper = the_type.pointer_wrapper
            self.is_no_free_pointer_wrapper = the_type.no_free_pointer_wrapper
            self.is_data_wrapper = the_type.data_wrapper
            self.is_array_wrapper = the_type.array_wrapper
            self.is_fixed_array_wrapper = the_type.fixed_array_wrapper
        elif the_type.is_struct:
            self.module_kind = 'struct'
            self.name = the_type['struct'].other
            self.uname = self.name
            self.via_pointer = the_type.via_pointer
            self.is_data_wrapper = the_type.data_wrapper
            self.is_array_wrapper = the_type.array_wrapper
            self.is_fixed_array_wrapper = the_type.fixed_array_wrapper
            self.same_as = the_type.same_as
        elif the_type.is_enum:
            self.module_kind = 'enum'
            self.name = the_type['enum'].other
            self.uname = self.name
            self.values = the_type.values
            self.via_pointer = the_type.via_pointer
        elif the_type.is_type:
            self.module_kind = 'type'
            self.name = the_type['type'].other
            self.uname = self.name
            self.via_pointer = the_type.via_pointer            
            self.is_pointer_wrapper = the_type.pointer_wrapper
            self.is_data_wrapper = the_type.data_wrapper
            self.is_array_wrapper = the_type.array_wrapper
            self.is_fixed_array_wrapper = the_type.fixed_array_wrapper
        else:
            logger.warning('Code Modul: Unknown type kind for %s', self.name)
    
    def __str__(self):
        '''returns a string representation of the class'''
        name = 'struct ' + self.name if self.is_struct else 'class ' + self.name
        name = 'static ' + name if self.is_static else name
        
        #return '%s\n%s' % (self.doc, name)
        return name
    
    def get_field(self, name):
        if name in self.fields:
            return self.fields[name]
        return None
    
    def visit_methods(self, visitor, other):
        logger.debug('Code Modul: Visiting method of %s' % self.name)
        
        #
        # Sort based on name, doc_idx, then number of parameters....
        #
        
        for method in sorted(self.methods.values(), key=lambda mthd: mthd.name + str(mthd.doc_idx) + str(len(mthd.params))) :
            # method = self.methods[key]
            logger.debug('Code Modul: Visiting method %s' % method.uname)
            # print method.uname
            other = visitor(method, other)
        return other
    
    def visit_operators(self, visitor, other):
        logger.debug('Code Modul: Visiting operator of %s' % self.name)
        
        keys = self.operators.keys()
        keys.sort()
        
        for key in keys :
            operator = self.operators[key]
            logger.debug('Code Modul: Visiting operator %s' % operator.name)
            other = visitor(operator, other)
        return other
    
    def visit_fields(self, visitor, other):
        #DO NOT sort these... they map in order to the native code
        for field in self.field_list:
            other = visitor(field, field == self.field_list[-1], other)
        return other
    
    def visit_properties(self, visitor, other):
        keys = self.properties.keys()
        keys.sort()
        
        for key in keys:
            prop = self.properties[key]
            other = visitor(prop, other)
        return other

def test_class_creation():
    """test basic class creation"""
    my_class = SGCodeModule("Hello")
    
    assert my_class.name == "Hello"
    assert len(my_class.methods) == 0

def test_add_method():
    """test adding a method to a class"""
    my_class = SGCodeModule("Hello")
    my_method = SGMethod("test")
    
    my_class.add_member(my_method)
    
    assert len(my_class.methods) == 1

def test_add_property():
    """test adding a property to the class"""
    my_class = SGCodeModule("Hello")
    my_property = SGProperty("test")
    
    my_class.add_member(my_property)
    
    assert len(my_class.properties) == 1

def test_add_field():
    """test adding a field to the class"""
    my_class = SGCodeModule("Hello")
    my_field = SGField("test")
    
    my_class.add_member(my_field)
    
    assert len(my_class.fields) == 1

# @raises(Exception)
# def test_add_unkown_member():
#     """test adding some unknown member type to the class, expects to fail"""
#     my_class = SGCodeModule("Hello")
#     my_class.add_member("Hello")

def test_static_class():
    """tests the creation of a static class"""
    my_class = SGCodeModule("Hello")
    assert False == my_class.is_static
    
    my_class.is_static = True
    assert my_class.is_static

def test_struct():
    """tests the creation of a struct"""
    my_class = SGCodeModule("Hello")
    assert False == my_class.is_struct()
    
    my_class.set_as_struct()
    assert my_class.is_struct()

# def test_has_pointer():
#     """tests the creation of a class used to wrap a pointer"""
#     my_class = SGCodeModule("SoundEffect")
#     assert False == my_class.has_pointer()
#     
#     my_class.set_has_pointer()
#     assert my_class.has_pointer()
# 
if __name__ == '__main__':
    import nose
    from nose.tools import raises 
    
    nose.run()
    