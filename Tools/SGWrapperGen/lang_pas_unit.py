#!/usr/bin/env python
# encoding: utf-8
"""
lang_pas.py

Created by Andrew Cain on 2011-01-19.
Copyright (c) 2011 Swinburne University. All rights reserved.
"""

import sys
import os
import logging
from datetime import datetime

from lang_data import LangMethodData, LangParamData, LangBasicData
from local_var import LocalVar

from sg import parser_runner, wrapper_helper
from sg.sg_cache import logger, find_or_add_file
from sg.print_writer import PrintWriter
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

import pas_lib
import lang_helper
import lang_c


_names = []
__my_writer = None

# ============
# = Visitors =
# ============

def doc_transform(the_docs):
    docLines = the_docs.splitlines(True)
    return ''.join([line if line == docLines[0] else '//' + line for line in docLines])

def type_visitor(the_type):
    '''switch types for the c SwinGame code (code facing the user)'''
    assert the_type.name == pas_lib._type_switcher[the_type.name.lower()]
    return the_type.name
    
    #return pas_lib._type_switcher[the_type.name.lower()]

def pas_standard_param_visitor(the_param, last):
    '''Just return the parameter with standard Pascal syntax, without changing types, names or modifiers.'''
    return '%s%s: %s%s' % (
        the_param.modifier + ' ' if the_param.modifier != None else '',
        the_param.name, 
        the_param.data_type.name, 
        '; ' if not last else ''
        )

# =========================
# = Code Creation Methods =
# =========================

def _do_create_pas_signature(method, the_file):
    '''Create the pascal signature for the passed in method and store it in its lang_data.
       This is used on all methods other than those in the SGSDK library and is used for
       documentation creation.'''
    method_alias = method.alias('pas')
    
    method_data = method_alias.to_keyed_dict(pas_standard_param_visitor, lang_key='pas', doc_transform=doc_transform)
    method_data["module_name"] = "sg" + the_file.name
    
    if method_alias.is_function:
        method_alias.signature = '// %(doc)s\nfunction %(name)s(%(params)s): %(return_type)s; overload;\n' % method_data
        method_alias.code = pas_lib.unit_function_pas % method_data
    else:
        method_alias.signature = '// %(doc)s\nprocedure %(name)s(%(params)s); overload;\n' % method_data
        method_alias.code = pas_lib.unit_procedure_pas % method_data
    
    
    
# =================
# = File Visitors =
# =================

def create_pas_code_for_file(the_file, other):
    '''This is called by the ... and indicates that the code in the passed in file needs to have some the Pas signatures created.'''    
    
    logger.info('Post Processing %s for PAS signature creation', the_file.name)
    
    for member in the_file.members:
        if member.is_class or member.is_struct or member.is_enum or member.is_type:
            # Setup the language data
            member.lang_data['pas'] = LangBasicData(member)
            
            #_do_create_type_code(member)
        elif member.is_module or member.is_library:
            for key, method in member.methods.items():
                # Setup the language data
                method.lang_data['pas'] = LangMethodData(method)
                
                if the_file.name == 'SGSDK':
                    # skip...
                    pass
                else:
                    # Build method signature and code
                    _do_create_pas_signature(method, the_file)

def write_type_alias(the_alias):
    pass
    
def write_pas_interface_code(the_file, other):
    '''Write the code for the Pascal versions'''
    
    if the_file.name == 'SGSDK':
        # skip the library
        return
        
    global _my_writer
    
    #visit the methods of the library
    other = dict()
    other['writer'] = _my_writer
    other['lang_key'] = 'pas'
    the_file.members[0].visit_methods(lang_helper.write_method_signature, other)
    
    for the_type in the_file.members[1:]:
        if not the_type.is_module:
            _my_writer.writeln('type %s = sgTypes.%s;\n' % (the_type.name, the_type.name))

def write_pas_code_files(the_file, other):
    '''Write the code for the Pascal versions'''
    
    if the_file.name == 'SGSDK':
        # skip the library
        return
    
    logger.info('Adding Pascal code for %s', the_file.name)
    
    global _my_writer
    
    #visit the methods of the library
    other = dict()
    other['writer'] = _my_writer
    other['lang_key'] = 'pas'
    the_file.members[0].visit_methods(lang_helper.write_method_code, other)

def write_pas_unit_code():
    """Write the code for the SwinGame.pas unit"""
    
    global _my_writer
    _my_writer = FileWriter('../../Generated/Pascal/lib/SwinGame.pas')
    _my_writer.writeln('// SwinGame.pas was generated on %s' % datetime.now())
    _my_writer.writeln('// ')
    _my_writer.writeln(pas_lib.unit_header_pas % { 
        'uses' : '%s' % ', '.join( [ tpl[0] for tpl in parser_runner.all_units]),
        })
        
    _my_writer.indent()
    
    parser_runner.visit_all_units(write_pas_interface_code)
    _my_writer.outdent()
    _my_writer.writeln(pas_lib.unit_implementation_pas)
    _my_writer.indent()
    parser_runner.visit_all_units(write_pas_code_files)
    _my_writer.outdent()
    
    _my_writer.writeln(pas_lib.footer_txt)
    _my_writer.close()
    


# ===============
# = Entry Point =
# ===============

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    parser_runner.parse_all_units()
    
    parser_runner.visit_all_units(create_pas_code_for_file)
    
    write_pas_unit_code()

if __name__ == '__main__':
    main()

