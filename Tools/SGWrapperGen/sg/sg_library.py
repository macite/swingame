#!/usr/bin/env python
# encoding: utf-8
"""
SGLibrary.py

Created by Andrew Cain on 2009-05-22.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

from sg_cache import find_or_add_file, logger
from sg_code_module import SGCodeModule

class SGLibrary(SGCodeModule):
    """Represents the SwinGame SDK library."""
    
    def __init__(self):
        """Initialise the library"""
        super(SGLibrary,self).__init__('sgLibrary')
        self.is_static = True
        self.module_kind = "library"
        self.version = 0
        self.in_file = find_or_add_file('SGSDK')
    
    def add_member(self, member):
        """Add a method to the library"""
        from sg_method import SGMethod
        
        if isinstance(member, SGMethod):
            member.is_external = True
            member.name = 'sg_%s_%s' % (member.in_file.name, member.name)
            member.uname = 'sg_%s_%s' % (member.in_file.name, member.uname)
            member.in_file = find_or_add_file('SGSDK')
            super(SGLibrary,self).add_member(member)
        else:
            raise Exception, "Unknown member type"
    
    def find_method(self, uname, file_name):
        for key, method in self.methods.items():
            if method.uname == 'sg_%s_%s' % (file_name, uname): 
                logger.debug('Library   : Found match for %s', method.uname)
                return method
        return None
    
    def check_methods(self):
        #for all methods in the library
        for key, method in self.methods.items():
            #check the methods arguments
            method.check_arguments()
            for caller in method.called_by:
                #check that the methods that call the library
                caller.check_arguments()

def test_library_creation():
    """test basic library creation"""
    my_library = SGLibrary()
    
    assert len(my_library.methods) == 0

def test_add_method():
    """test adding a method to a library"""
    from sg_method import SGMethod
    my_library = SGLibrary()
    my_method = SGMethod('test')
    
    my_library.add_member(my_method)
    
    print my_library.methods
    
    assert len(my_library.methods) == 1
    assert my_method == my_library.methods[("test", ())]

# @raises(Exception)
# def test_add_unkown_member():
#     """test adding some unknown member type to the library, expects to fail"""
#     my_library = SGLibrary("Hello")
#     my_library.add_method("Hello")
        
if __name__ == '__main__':
    import nose
    from nose.tools import raises 

    nose.run()
