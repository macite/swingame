#!/usr/bin/env python
# encoding: utf-8
'''Reads in a pascal source code file and outputs an abstract
syntax tree, with attached meta tags.

Meta tag syntax:
> /// @tag [parameters] 

parameters= parameter[, parameters]
parameter= <value>|([parameters])
pas_parser.py

Created by Andrew Cain on 2009-05-25.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
'''
    
import logging
import sys

from sg import parser_runner
from sg.sg_cache import logger
from sg.print_writer import PrintWriter
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

DEBUG = False

def property_visitor(element, other): #last, other):
    print '\tpublic %s%s %s'%(
        'static ' if element.is_static else '',
        element.data_type,
        element.name
        )
    print '\t{'
    print '\t',
    method_visitor(element.getter, other)
    print '\t',
    method_visitor(element.setter, other)
    print '\t}'
    #if last: print
    return other

def field_visitor(element, last, other):
    print '\tprivate %s %s;'%(
        element.data_type,
        element.name
        )
    if last: print
    return other

def param_visitor(element, last, other):
    print '%s%s %s%s'%(
        '' if element.modifier == None else element.modifier + ' ',
        element.data_type,
        element.name, 
        ',' if not last else ''), 
    return other
    
def arg_visitor(arg, last, other):
    print '%s%s'%(
        arg.name if isinstance(arg, SGParameter) else arg,
        ',' if not last else ''), 
    return other

def method_visitor(element, other):
    if element == None: 
        print '\t NONE...'
        return
    
    print '\t<@%s> %s%s%s %s(' % (
        hex(id(element)),
        'static ' if element.is_static else '',
        'extern ' if element.is_external else '',
        element.return_type if element.return_type != None else 'void' , 
        element.uname),
    
    element.visit_params(param_visitor, other)
    
    print ')' 
    print '\t\t-> in_file\t', element.in_file
    print '\t\t-> calls',
    
    if element.method_called == None:
        print '???'
        return
    
    m = element.method_called
    print '\t<@%s> %s%s%s %s(' % (
        hex(id(m)),
        'static ' if m.is_static else '',
        'extern ' if m.is_external else '',
        m.return_type if m.return_type != None else 'void' , 
        m.name),
    
    m.visit_params(param_visitor, other)
    
    print ') from ', m.in_file
    
    print '\t\t-> args\t\t', 
    element.visit_args(arg_visitor, other)
    print '\n'
    return other

def visitor(the_file, other):
    for element in the_file.members:
        if element.is_static:
            print 'static',
        print 'class %s' % element.name
        print '{'
        element.visit_fields(field_visitor, None)
        element.visit_properties(property_visitor, None)
        element.visit_methods(method_visitor, None)
        print '}'

def main():
    logging.basicConfig(level=logging.WARNING,
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        stream=sys.stdout)
    
    # tokeniser = SGPasTokeniser()
    # tokeniser.tokenise('../../CoreSDK/src/SGSDK_Audio.pas')
    # tok = tokeniser.next_token()
    # while tok[1] != 'implementation':
    #     print tok
    #     if tok[0] == 'attribute':
    #         tok = tokeniser.next_token()
    #         print tok
    #         if tok[1] == 'param':
    #             tok = tokeniser.next_token()
    #             print tok
    #             print tokeniser.read_to_eol()
    #         elif tok[1] == 'note':
    #             print tokeniser.read_to_eol()
    #     tok = tokeniser.next_token()
    # 
    # parser = SGPasParser()
    # 
    # files = [
    #         find_or_add_file('SGSDK_Audio', 'Audio', '../../CoreSDK/src/SGSDK_Audio.pas'),
    #         find_or_add_file('SGSDK_Core', 'Core', '../../CoreSDK/src/SGSDK_Core.pas')
    #     ]
    # 
    # for a_file in files:
    #     parser.parse(a_file)
    # 
    # find_or_add_class('lib').check_methods()
    # 
    # for key,each_class in all_classes().items():
    #     each_class.visit(visitor)
    parser_runner.run_for_all_units(visitor)

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass

    main()
