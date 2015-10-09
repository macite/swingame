#!/usr/bin/env python
# encoding: utf-8
"""
lang_c.py

This file contains the scripts for the post processing required for the C programming language

Created by Andrew Cain on 2011-01-04.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging
import sys

from lang_data import LangMethodData, LangBasicData

from sg import parser_runner, wrapper_helper
from sg.sg_cache import logger, find_or_add_file
from sg.print_writer import PrintWriter
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

import c_lib
import lang_helper    

_out_path="../../Generated/C/lib"

# =============================
# = Type Conversion Functions =
# =============================

# Convert types to c code

def type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame code (code facing the user)'''
    return lang_helper.std_type_visitor(c_lib._type_switcher, the_type, modifier)

def cpp_type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame code (code facing the user)'''
    if modifier == 'const' or modifier == 'var' or modifier == 'out':
        modifier = modifier + '-cpp'
    
    return lang_helper.std_type_visitor(c_lib._type_switcher, the_type, modifier)


def adapter_type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame adapter (code that links to the DLL)'''
    return lang_helper.std_type_visitor(c_lib._adapter_type_switcher, the_type, modifier, dict_name = '_adapter_type_switcher')

def c_code_for_adapter_type(the_type, var_name, modifier = None):
    data_type = adapter_type_visitor(the_type, modifier)
    if data_type.find("%s") > -1:
        #type has name placeholder
        return data_type % var_name
    else:
        return data_type + var_name
    

# Convert parameters from the object model to c code

def param_visitor(the_param, last):
    return '%s%s%s' % (type_visitor(the_param.data_type, the_param.modifier), the_param.name, ', ' if not last else '')

def cpp_param_visitor(the_param, last):
    return '%s%s%s' % (cpp_type_visitor(the_param.data_type, the_param.modifier), the_param.name, ', ' if not last else '')


def const_strip_param_visitor(the_param, last):
    ''' Called for each parameter in the function/procedure being mapped. 
        This version strips off the const details to enable call by value. '''
    
    if the_param.modifier != 'const' or the_param.data_type.is_array:
        mod = the_param.modifier
        opt = ''
    else: 
        mod = None
        opt = 'const ' #but keep the const modifier
    
    return '%s%s%s%s' % (opt, type_visitor(the_param.data_type, mod), the_param.name, ', ' if not last else '')

def adapter_param_visitor(the_param, last):
    return '%s%s%s' % (adapter_type_visitor(the_param.data_type, the_param.modifier), the_param.name, ', ' if not last else '')

# Convert arguments to c code

def arg_visitor(arg_str, the_arg, for_param_or_type):
    '''Called for each argument in a call, performs required mappings'''
    if isinstance(for_param_or_type, SGType):
        the_type = for_param_or_type
    else:
        the_type = for_param_or_type.data_type
    
    if arg_str in c_lib._val_switcher:
        arg_str = c_lib._val_switcher[arg_str]
    
    if the_type.name in c_lib._data_switcher:
        #convert data using pattern from _data_switcher
        return c_lib._data_switcher[the_type.name] % arg_str
    else:
        return arg_str

def const_strip_arg_visitor(arg_str, the_arg, for_param_or_type):
    '''
    Called for each argument in a call, performs required mappings.
    This version adds in & operators for all const parameters to 
    enable the creation of a pass by value version.
    '''
    arg_str = arg_visitor(arg_str, the_arg, for_param_or_type)
    
    if isinstance(for_param_or_type, SGType):
        the_type = for_param_or_type
    else:
        the_type = for_param_or_type.data_type
    
    if the_arg != None and isinstance(the_arg, SGParameter) and the_arg.modifier == 'const' and not the_type.is_array:
        return '&' + arg_str
    else:
        return arg_str

def cpp_arg_visitor(arg_str, the_arg, for_param_or_type):
    '''
    Called for each argument in a call, performs required mappings.
    This version adds in & operators for const var and out parameters...
    '''
    arg_str = arg_visitor(arg_str, the_arg, for_param_or_type)

    if isinstance(for_param_or_type, SGType):
        the_type = for_param_or_type
    else:
        the_type = for_param_or_type.data_type

    if the_arg != None and isinstance(the_arg, SGParameter) and the_arg.modifier in ['const', 'var', 'out'] and not (the_type.is_array or the_type.name == 'String'):
        return '&' + arg_str
    else:
        return arg_str


def doc_transform(the_docs):
    docLines = the_docs.splitlines(True)
    return ''.join([line if line == docLines[0] else '// ' + line for line in docLines])


# ===============================================================================
# = Processing Functions - process methods and types from the code object model =
# ===============================================================================

def _do_c_parameter_processing(method):
    '''Add result parameters and arguments for functions that returned strings (converted as C cannot return arrays) and 
       add parameters and arguments for any additional length data passed for arrays (as C does not know length of arrays)'''
    method_data = method.lang_data['c']
    cpp_method_data = method.lang_data['cpp']
    
    # was_function indicates that this function was converted to a procedure
    # we need to do the same thing for the c version...
    if method.method_called.was_function:
        # convert string return types
        result_param = SGParameter('result')
        result_param.data_type = method_data.return_type
        result_param.modifier = 'var'
        
        # copy and morph the parameters
        param_list = list(method_data.params)
        param_list.append(result_param)                
        
        # do the same for the arguments
        arg_list = list(method_data.args)
        arg_list.append(result_param)
                        
        # save in lang_data
        method_data.params        = param_list
        method_data.args          = arg_list
        method_data.return_type   = None
        
        cpp_method_data.params        = param_list
        cpp_method_data.args          = arg_list
        cpp_method_data.return_type   = None
        
    
    if method.method_called.has_length_params:
        # add length parameters to this method
        
        param_list = list(method_data.params)
        arg_list = list(method_data.args)
        
        for param in method.method_called.params:
            if param.is_length_param:
                param_list.append(param)
                arg_list.append(param)
        
        method_data.params    = param_list
        method_data.args      = arg_list
        
        cpp_method_data.params    = param_list
        cpp_method_data.args      = arg_list
    

def _do_cpp_create_method_code(the_method):
    '''Add the signature and code for the passed in function or procedure.'''
    logger.info('VISITING  : Method %s', the_method.name)
    
    method_data = the_method.lang_data['cpp']
    
    details = the_method.to_keyed_dict(cpp_param_visitor, cpp_type_visitor, cpp_arg_visitor, doc_transform, lang_key='cpp')
    
    # Create signature
    method_data.signature   = '%(return_type)s %(name_lower)s(%(params)s);' % details
    
    if method_data.is_function:
        details['the_call']     = arg_visitor('%(calls.name)s(%(calls.args)s)' % details, None, method_data.return_type)
        method_data.code        = c_lib.module_cpp_function_txt % details
    else:
        method_data.code        = c_lib.module_cpp_method_txt % details
        
    # if method_data.has_const_params():
    #     # Create a version with const parameters passed by value - this avoids having to create const pointers to data in the C SwinGame
    #     details = the_method.to_keyed_dict(const_strip_param_visitor, type_visitor, const_strip_arg_visitor, doc_transform, lang_key='c')
    #     
    #     # details['uname'] = details['uname'] + '_byval'
    #     # details['uname_lower'] = details['uname_lower'] + '_byval'
    #     # details['name'] = details['uname']
    #     # 
    #     bval_sig = '%(return_type)s %(name_lower)s(%(params)s);' % details;
    #     method_data.signature   += '\n' + bval_sig;
    #     
    #     if method_data.is_function:
    #         details['the_call']  = const_strip_arg_visitor('%(calls.name)s(%(calls.args)s)' % details, None, method_data.return_type)
    #         method_data.code    += (c_lib.module_cpp_function_txt % details)
    #     else:
    #         method_data.code    += (c_lib.module_cpp_method_txt % details)
    #     
    #     # Also write to header
    #     # other['header writer'].write('%(return_type)s' % details % details['uname_lower'])
    #     # other['header writer'].write('(%(params)s);' % details) 
    #     # other['header writer'].writeln('')
    # 
    # 
    # # print method_data.signature
    # # print method_data.code
    # # print '-----'

def _do_c_create_method_code(the_method):
    '''Add the signature and code for the passed in function or procedure.'''
    logger.info('VISITING  : Method %s', the_method.name)
    
    if (the_method.uname != the_method.name) or the_method.has_const_params() or the_method.has_var_params() or the_method.has_out_params():
        _do_cpp_create_method_code(the_method)
    
    method_data = the_method.lang_data['c']
    
    details = the_method.to_keyed_dict(param_visitor, type_visitor, arg_visitor, doc_transform, lang_key='c')
    
    # Create signature
    method_data.signature   = '%(return_type)s %(uname_lower)s(%(params)s);' % details
    
    if method_data.is_function:
        details['the_call']     = arg_visitor('%(calls.name)s(%(calls.args)s)' % details, None, method_data.return_type)
        method_data.code        = c_lib.module_c_function_txt % details
    else:
        method_data.code        = c_lib.module_c_method_txt % details
    
    if method_data.has_const_params():
        # Create a version with const parameters passed by value - this avoids having to create const pointers to data in the C SwinGame
        details = the_method.to_keyed_dict(const_strip_param_visitor, type_visitor, const_strip_arg_visitor, doc_transform, lang_key='c')
        
        details['uname'] = details['uname'] + '_byval'
        details['uname_lower'] = details['uname_lower'] + '_byval'
        details['name'] = details['uname']
        
        bval_sig = '%(return_type)s %(uname_lower)s(%(params)s);' % details;
        method_data.signature   += '\n' + bval_sig;
        
        #print method_data.signature
        
        if method_data.is_function:
            details['the_call']  = const_strip_arg_visitor('%(calls.name)s(%(calls.args)s)' % details, None, method_data.return_type)
            method_data.code    += (c_lib.module_c_function_txt % details)
        else:
            method_data.code    += (c_lib.module_c_method_txt % details)
        
        # Also write to header
        # other['header writer'].write('%(return_type)s' % details % details['uname_lower'])
        # other['header writer'].write('(%(params)s);' % details) 
        # other['header writer'].writeln('')
    
    # print method_data.code
    # print '-----'

def _do_create_adapter_code(the_method):
    logger.info('VISITING  : SGSDK Method %s', the_method.name)
    
    method_data = the_method.lang_data['c']
    
    details = the_method.to_keyed_dict(param_visitor, type_visitor, arg_visitor, doc_transform, lang_key='c')
    
    # Create signature
    method_data.signature = '%(return_type)s %(uname)s(%(params)s);' % details
    
    # Create vcpp variable 
    if the_method.is_function:
        method_data.visual_cpp_variable = '%(return_type)s (*var_%(uname)s)(%(params)s);\n%(return_type)s %(uname)s(%(params)s)\n{\n\treturn var_%(uname)s(%(args)s);\n}' % details
    else:
        method_data.visual_cpp_variable = '%(return_type)s (*var_%(uname)s)(%(params)s);\n%(return_type)s %(uname)s(%(params)s)\n{\n\tvar_%(uname)s(%(args)s);\n}' % details
    
    method_data.visual_cpp_variable_init = '\t\tvar_%(uname)s = (%(return_type)s (__cdecl *)(%(params)s)) GetProcAddress( LibraryHandle, "%(uname)s" );' % details
    
    # print method_data.signature

def _do_create_type_code(member):
    '''Create the c code for the type member'''
    
    assert member.is_class or member.is_struct or member.is_enum or member.is_type
    
    member_data = member.lang_data['c']

    if member.is_class or member.is_type or (member.is_struct and member.wraps_array):
        if member.is_pointer_wrapper: # convert to resource pointer
            the_type = member.data_type
            member_data.signature = 'typedef %s;\n' % c_code_for_adapter_type(the_type, member.lower_name, 'ptr-decl')
        elif member.is_data_wrapper: # alias of another type
            assert len(member.fields) == 1
            the_type = member.fields['data'].data_type
            member_data.signature = 'typedef %s;\n' % c_code_for_adapter_type(the_type, member.lower_name)
        elif member.wraps_array:
            assert len(member.fields) == 1
            the_type = member.fields['data'].data_type
            member_data.signature = 'typedef %s;\n' % c_code_for_adapter_type(the_type, member.lower_name)
        elif member.data_type.is_procedure:
            assert member.data_type.method != None
            #typedef float(*pt2Func)(float, float);
            m = member.data_type.method
            member_data.signature = 'typedef %s;\n' % c_code_for_adapter_type(member.data_type, m.lower_name)
        else:
            logger.error('CREATE C  : Unknown class type for %s', member.uname)
            assert False
        
    elif member.is_struct:
        if member.same_as:
            member_data.signature = 'typedef %s %s;\n' % (member.same_as.lower_name, member.lower_name)
        else:
            #typedef struct %s_struct { } struct;
            member_data.signature = 'typedef struct _%s { \n' % member.lower_name
            
            #write the fields
            for field in member.field_list:
                member_data.signature += '    %s;\n' % c_code_for_adapter_type(field.data_type, field.lower_name)
            
            member_data.signature += '} %s;\n' % member.lower_name
        
    elif member.is_enum:
        #enum id { list }
        member_data.signature = 'typedef enum { \n    '
        member_data.signature += ',\n    '.join([wrapper_helper.upper_name(v) for v in member.values])
        member_data.signature += '\n} %s;\n' % member.lower_name
    
    # print member_data.signature

# ========================
# = Save C code to files =
# ========================

def write_cpp_signature(method, other):
    '''
    Output the C++ signatures after the C signatures
    '''
    writer = other['writer']
    
    if len(method.lang_data['cpp'].signature) > 0:
        writer.writeln(method.lang_data['cpp'].signature)
    
    return other


def write_c_signature(method, other):
    writer = other['writer']
    
    writer.writeln(method.lang_data['c'].signature)
    
    return other

def write_c_code(method, other):
    writer = other['writer']
    
    if len(method.lang_data['cpp'].signature) > 0:
        writer.writeln(method.lang_data['cpp'].code)
    
    writer.write(method.lang_data['c'].code)
    
    return other

def write_cpp_method_var(method, other):
    writer = other['writer']
    
    method_data = method.lang_data['c']
    
    writer.writeln(method_data.visual_cpp_variable);
    
    return other

def write_cpp_library_code(method, other):
    writer = other['writer']
    
    method_data = method.lang_data['c']
    
    writer.writeln(method_data.visual_cpp_variable_init);
    
    return other

def write_c_header_for(the_file, out_path):
    writer = FileWriter('%s/%s.h'% (out_path, the_file.name))
    
    writer.writeln(c_lib.module_header_header_txt % { 
        'name' : the_file.name,
        'pascal_name' : the_file.pascal_name
        })
    
    # Write the imports
    if the_file.name == 'SGSDK':
        writer.writeln(c_lib.lib_import_header_txt % {'name': 'Types'})
        writer.writeln('#ifdef __cplusplus\nextern "C" {\n#endif\n')
    else:
        for a_file in the_file.uses:
            if a_file.name != None:
                writer.writeln(c_lib.lib_import_header_txt % {'name': a_file.name})
    writer.writeln('')
    
    other = { 'writer': writer, 'SGSDK': the_file.name == 'SGSDK' }
    
    seen_types = list()
    need_types = dict()
    
    #visit types
    for member in the_file.members:
        seen_types.append(member.lower_name)
        
        if member.is_class:
            # if member.lower_name + "_data" not in seen_types:
            #     # print "not seen", member.lower_name + "_data"
            #     need_types[member.lower_name + "_data"] = member
            # else:
            write_c_signature(member, other)
        if member.is_struct or member.is_enum or member.is_type:
            write_c_signature(member, other)
            if member.lower_name in need_types.keys():
                write_c_signature(need_types[member.lower_name], other)
    
    #visit methods
    for member in the_file.members:
        if member.is_module or member.is_library:
            member.visit_methods(write_c_signature, other)
    
    if the_file.name == 'SGSDK':
        writer.writeln('#ifdef __cplusplus\n}\n#endif\n')
    else:
        # Output C++ signatures
        writer.writeln('')
        writer.writeln('#ifdef __cplusplus')
        writer.writeln('// C++ overloaded functions')
        for member in the_file.members:
            if member.is_module or member.is_library:
                member.visit_methods(write_cpp_signature, other)
        writer.writeln('')
        writer.writeln('#endif')
        writer.writeln('')
    
    writer.writeln(c_lib.module_header_footer_txt)
    writer.close()

def write_c_body_for(the_file, out_path):
    if not the_file.has_body: return
    
    writer = FileWriter('%s/%s.c'% (out_path, the_file.name))
    
    writer.writeln(c_lib.module_c_header_txt % { 
        'name' : the_file.name,
        'pascal_name' : the_file.pascal_name
        })
    
    # Write the imports
    for a_file in the_file.uses:
        if a_file.name != None:
            writer.writeln(c_lib.lib_import_header_txt % {'name': a_file.name})
    writer.writeln('')
    
    other = { 'writer': writer }
    
    print the_file
    
    if the_file.name == 'SGSDK':
        writer.writeln('#ifdef __cplusplus')
        writer.writeln('#ifdef _MSC_VER')
        
        writer.writeln('#define WIN32_LEAN_AND_MEAN')
        writer.writeln('#include <Windows.h>')
        writer.writeln('')
        the_file.members[0].visit_methods(write_cpp_method_var, other)
        writer.writeln('')
        writer.writeln('struct DLLLoader')
        writer.writeln('{')
        writer.writeln('	DLLLoader()')
        writer.writeln('	{')
        writer.writeln('		LibraryHandle = LoadLibrary( "SGSDK.dll" );')
        writer.writeln('')
        
        the_file.members[0].visit_methods(write_cpp_library_code, other)
        
        writer.writeln('	}')
        writer.writeln('')
        writer.writeln('	~DLLLoader()')
        writer.writeln('	{')
        writer.writeln('		if( LibraryHandle != NULL )')
        writer.writeln('		{')
        writer.writeln('			FreeLibrary( LibraryHandle );')
        writer.writeln('		}')
        writer.writeln('	}')
        writer.writeln('')
        writer.writeln('	HMODULE LibraryHandle;')
        writer.writeln('};')
        writer.writeln('')
        writer.writeln('static DLLLoader Loader;')
        
        writer.writeln('#endif')
        writer.writeln('#endif')
    else:
        # Visit_methods
        for member in the_file.members:
            if member.is_module:
                member.visit_methods(write_c_code, other)
    
    writer.close()


# =================================================================
# = Entry Point - this is called for processing then file writing =
# =================================================================

def create_c_code_for_file(the_file, other):
    '''This is called by the ... and indicates that the code in the passed in file needs to have some post processing for the C language.'''    
    
    logger.info('Post Processing %s for C wrapper creation', the_file.name)
    
    for member in the_file.members:
        if member.is_class or member.is_struct or member.is_enum or member.is_type:
            # Setup the language data
            member.lang_data['c'] = LangBasicData(member)
            member.lang_data['cpp'] = LangBasicData(member)
            
            _do_create_type_code(member)
        elif member.is_module or member.is_library:
            for key, method in member.methods.items():
                # Setup the language data
                method.lang_data['c'] = LangMethodData(method)
                method.lang_data['cpp'] = LangMethodData(method)
                
                if the_file.name == 'SGSDK':
                    _do_create_adapter_code(method)
                else:
                    # Process parameters - adding length and result parameters to functions/procedures
                    _do_c_parameter_processing(method)
                    
                    # Build method signature and code
                    _do_c_create_method_code(method)
            

def write_c_code_files(the_file, other, out_path=_out_path):
    '''Save the c code to file'''
    
    write_c_header_for(the_file, out_path)
    if the_file.has_body:
        write_c_body_for(the_file, out_path)



def main():
    logging.basicConfig(level=logging.ERROR,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    parser_runner.parse_all_units()
    parser_runner.visit_all_units(create_c_code_for_file)
    parser_runner.visit_all_units(write_c_code_files)

if __name__ == '__main__':
    main()
