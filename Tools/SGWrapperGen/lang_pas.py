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

# dictionary for start of method names for copying variable
# length array data
_array_copy_data = {
    'linesarray':               'Line',
    'resolutionarray':          'Resolution',
    'bitmaparray':              'Bmp',
    'longintarray':             'Longint',
    'point2darray':             'Point2D',
    'stringarray':              'String',
    'trianglearray':            'Triangle',
    'fingerarray':              'Finger',
    'spriteeventhandlerarray':  'SpriteEventHandler',
}

_data_switcher = {
    #Pascal type: what values of this type switch to %s = data value
    'String': 'String(%s)'
}


# # dictionary for start of method names for copying fixed
# # length array data
# _array_copy_fixed_data = {
#     'triangle': 'Tri',
#     'matrix2d': 'Matrix',
# }

_names = []


# ============
# = Visitors =
# ============

def type_visitor(the_type):
    '''switch types for the c SwinGame code (code facing the user)'''
    return pas_lib._type_switcher[the_type.name.lower()]

def pas_standard_param_visitor(the_param, last):
    '''Just return the parameter with standard Pascal syntax, without changing types, names or modifiers.'''
    return '%s%s: %s%s' % (
        the_param.modifier + ' ' if the_param.modifier != None else '',
        the_param.name, 
        the_param.data_type.name, 
        '; ' if not last else ''
        )

def pas_library_param_visitor(the_param, last):
    '''Return the parameter changed for the library interface - 
       * Change const to var
       * Strip var/const/out from arrays and strings
       * Make result a standard parameter - it must be a pointer to need to be a result param
    '''
    if the_param.modifier in ['out','var', 'const', 'result'] and (the_param.data_type.name.lower() in ['string'] or the_param.data_type.wraps_array):
        return '%s: %s%s' % (
            the_param.name, 
            type_visitor(the_param.data_type),
            '; ' if not last else ''
            )
    elif the_param.modifier in ['const']:
        return 'var %s: %s%s' % (
            the_param.name, 
            type_visitor(the_param.data_type),
            '; ' if not last else ''
            )        
    elif the_param.modifier in ['result']:
        return '%s: %s%s' % (
            the_param.name, 
            type_visitor(the_param.data_type),
            '; ' if not last else ''
            )        
    else:
        return '%s%s: %s%s' % (
            the_param.modifier + ' ' if the_param.modifier != None else '',
            the_param.name, 
            type_visitor(the_param.data_type),
            '; ' if not last else ''
            )

def pas_library_arg_visitor(arg_str, the_arg, for_param_or_type):
    '''Called for each argument in a call: for Pascal maps PChar to String'''
    if isinstance(for_param_or_type, SGType):
        the_type = for_param_or_type
    else:
        the_type = for_param_or_type.data_type
        
    if the_type.name in _data_switcher:
        #convert data using pattern from _data_switcher
        # print _data_switcher[the_type.name] % arg_str
        return _data_switcher[the_type.name] % arg_str
    else:
        return arg_str


# =========================
# = Code Creation Methods =
# =========================

def _do_create_pas_signature(method):
    '''Create the pascal signature for the passed in method and store it in its lang_data.
       This is used on all methods other than those in the SGSDK library and is used for
       documentation creation.'''
    method_alias = method.alias('pas')
    
    method_data = method_alias.to_keyed_dict(pas_standard_param_visitor, lang_key='pas')
    
    if method_alias.is_function:
        method_alias.signature = 'function %(name)s(%(params)s): %(return_type)s;' % method_data
    else:
        method_alias.signature = 'procedure %(name)s(%(params)s);' % method_data
    
    # print method_alias.signature

def _add_locals_for_pas_library(method):
    ''' Adds local variables for temporary values between type shifting eg. PChar -> String.
        This includes local variables for the following cases:
        * Any parameter that maps the result of a function - these need to be copied to the callers destination (arrays)
        * Any parameter that is an array
        * Any string that is returned out (so all var and out string params)
    '''
    logger.info('Post Processing library for Pascal library creation')
    
    method_alias = method.alias('pas')
    
    for param in method_alias.params:
        if param.maps_result or param.data_type.wraps_array or (param.modifier in ['var', 'out'] and param.data_type.name.lower() in ['string']):
            logger.debug('LOCAL VAR: Adding local var of type %s to %s', param.data_type, method.uname)
            
            param.lang_data['pas'] = LangParamData(param)
            
            local_var = LocalVar()
            local_var.setup_for_parameter(param, 'pas')
            method_alias.local_vars.append(local_var)

def _do_create_pas_library_code(method):
    
    method_alias = method.alias('pas')
    
    data = method_alias.to_keyed_dict(pas_library_param_visitor, lang_key='pas', arg_visitor = pas_library_arg_visitor)
    
    if method_alias.was_function:
        # Method returned a array of some kind... get the result params and change the 'return_type' in data
        result_param = method_alias.params[-1]
        if not result_param.maps_result: #in case of returning var length array
            result_param = method_alias.params[-2]
        
        if not result_param.maps_result or result_param.data_type.name.lower() not in ['string', 'triangle', 'linesarray', 'matrix2d', 'point2darray', 'longintarray','stringarray','bitmaparray','trianglearray','fingerarray', 'resolutionarray']:
            logger.error('CREATE LIB: Unknown parameter return type in %s.', method_alias.name)
            assert False
        
        lines = pas_lib.function_as_procedure_txt
        #? is this needed: data['return_type'] = result_param.data_type.name
    elif method_alias.return_type == None: 
        lines = pas_lib.procedure_txt
    else: 
        lines = pas_lib.function_txt
    
    if len(method_alias.local_vars) > 0:
        var_dec = '\n  var\n'
        temp_process_result = ''
        temp_process_params = ''
        
        for local_var in method_alias.local_vars:
            # Add declaration of variable
            var_dec += '    %s: %s;\n' % (local_var.name, local_var.data_type)
            
            # Add variable processing code...
            type_name = local_var.data_type.name.lower()
            
            if type_name == 'string':
                # Copy strings out - they are passed through using Pascal implicit casting from PChar to String
                temp_process_result += '\n      StrCopy(%s, PChar(%s));' % (local_var.local_for.name, local_var.name)
            #? elif type_name == 'color':
            #     temp_process_result += '\n      %s := %s;' % (local_var.name[:-5], local_var.name)
            
            # elif type_name in _array_copy_fixed_data: #needed for mapped result parameters
            #     if local_var.modifier in [None, 'var', 'const'] and not local_var.maps_result:
            #         temp_process_params += '\n      %sCopyFromPtr(%s, %s);' % (_array_copy_fixed_data[type_name], local_var.name, local_var.name[:-5])
            #     if local_var.modifier in [None, 'var', 'out'] or local_var.maps_result:
            #         temp_process_result += '\n      %sCopyToPtr(%s, %s);' % (_array_copy_fixed_data[type_name], local_var.name[:-5], local_var.name)
            elif type_name in _array_copy_data:
                if local_var.modifier in [None, 'var', 'const'] and not local_var.maps_result:
                    temp_process_params += '\n      %sCopyFromPtr(%s, %s_len, %s);' % (_array_copy_data[type_name], local_var.name[:-5], local_var.name[:-5],local_var.name)
                if local_var.modifier in [None, 'var', 'out'] or local_var.maps_result:
                    temp_process_result += '\n      %sCopyToPtr(%s, %s_len, %s);' % (_array_copy_data[type_name],local_var.name, local_var.name[:-5], local_var.name[:-5])
            else:
                logger.error('CREATE LIB: Unknow local variable type in %s', method_alias.name)
                assert False
        data['vars'] = var_dec[:-1]
        data['process_result'] = temp_process_result
        data['process_params'] = temp_process_params
    else:
        data['vars'] = ''
        data['process_result'] = ''
        data['process_params'] = ''
    
    _names.append(method_alias.name)
    
    method_alias.code = (''.join(lines) + '\n') % data 
    
    # print method_alias.code


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
                    # Add local variables etc.
                    _add_locals_for_pas_library(method)
                    _do_create_pas_library_code(method)
                else:
                    # Build method signature and code
                    _do_create_pas_signature(method)

def write_pas_code_files(the_file, other):
    '''Write the code for the Pascal versions'''
    
    if the_file.name == 'Types':
        logger.info('Writing %s c header', the_file.name)
        lang_c.write_c_header_for(the_file, out_path='../../Generated/Source/src/')
    
    if the_file.name != 'SGSDK': 
        return
    
    logger.info('Writing %s c header', the_file.name)
    lang_c.write_c_header_for(the_file, out_path='../../Generated/Source/src/')
    
    logger.info('Writing %s Pascal code', the_file.name)
    
    my_writer = FileWriter('../../Generated/Source/src/%s.pas' % the_file.name)
    
    my_writer.writeln(pas_lib.header_txt % { 
        'name' : the_file.pascal_name, 
        'uses' : the_file.uses_str(lambda a_file: a_file.pascal_name), 
        'version' : the_file.members[0].version #version from library
        })
    my_writer.indent();
    
    #visit the methods of the library
    other = dict()
    other['writer'] = my_writer
    other['lang_key'] = 'pas'
    the_file.members[0].visit_methods(lang_helper.write_method_code, other)
    
    #write out exports
    my_writer.writeln(pas_lib.exports_header_txt)
    my_writer.indent()
    for name in _names:
        if name == _names[-1]:
            my_writer.write((pas_lib.export_line_txt % {'name': name})[:-2])
            my_writer.write(';')
        else:
            my_writer.write(pas_lib.export_line_txt % {'name': name})
    
    my_writer.outdent();
    my_writer.outdent();
    
    my_writer.writeln(pas_lib.footer_txt)
    my_writer.close()


# ===============
# = Entry Point =
# ===============

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    parser_runner.parse_all_units()
    
    parser_runner.visit_all_units(create_pas_code_for_file)
    parser_runner.visit_all_units(lang_c.create_c_code_for_file) # needed for Type.h and SGSDK.h
    
    parser_runner.visit_all_units(write_pas_code_files)


if __name__ == '__main__':
    main()

