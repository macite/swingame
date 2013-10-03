#!/usr/bin/env python
# encoding: utf-8
"""
create_pas_lib.py

This script creates the code for the dynamic SwinGame library.

Steps include:
1: Parsing swingame source files
2: 

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

from lang_pas import create_pas_code_for_file, write_pas_code_files
from lang_c import create_c_code_for_file
from sg import parser_runner

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    parser_runner.parse_all_units()
    parser_runner.visit_all_units(create_pas_code_for_file)
    parser_runner.visit_all_units(create_c_code_for_file) # needed for Type.h and SGSDK.h
    parser_runner.visit_all_units(write_pas_code_files)

if __name__ == '__main__':
    main()



# import logging
# import sys
# 
# from sg import parser_runner
# from sg.sg_cache import logger
# from sg.print_writer import PrintWriter
# from sg.file_writer import FileWriter
# from sg.sg_parameter import SGParameter
# 
# import create_c_library
# # import write_c_lib_header, _load_data
# 
# #my_writer = PrintWriter()
# my_writer = FileWriter('../../Dist/Source/src/sgsdk.pas')
# _header = ''
# _footer = ''
# _procedure_lines = None
# _function_lines = None
# _function_as_procedure = None
# _exports_header = ''
# _type_switcher = {
#     'single': 'Single',
#     'longint': 'Longint',
#     'soundeffect': 'SoundEffect',
#     'music': 'Music',
#     'string': 'PChar',
#     'color': 'LongWord',
#     'timer': 'Timer',
#     'byte': 'Byte',
#     'resourcekind': 'ResourceKind',
#     'longword': 'Longword',
#     'uint16': 'UInt16',
#     'bitmap': 'Bitmap',
#     'matrix2d': 'SinglePtr',
#     'triangle': 'Point2DPtr',
#     'linesegment': 'LineSegment',
#     'point2d': 'Point2D',
#     'vector': 'Vector',
#     'rectangle': 'Rectangle',
#     'sprite': 'Sprite',
#     'linesarray': 'LineSegmentPtr',
#     'font': 'Font',
#     'fontalignment': 'FontAlignment',
#     'fontstyle': 'FontStyle',
#     'mousebutton': 'MouseButton',
#     'boolean': 'Boolean',
#     'keycode': 'KeyCode',
#     
#     'longintarray':     'LongintPtr',
#     'bitmaparray':      'BitmapPtr',
#     'point2darray':     'Point2DPtr',
#     'trianglearray':    'Point2DPtr',
#         
#     'spriteendingaction': 'SpriteEndingAction',
#     'circle': 'Circle',
#     'map': 'Map',
#     'maptag': 'MapTag',
#     'spritekind': 'SpriteKind',
#     'freenotifier': 'FreeNotifier',
#     
#     'shapeprototype': 'ShapePrototype',
#     'character': 'Character',
#     'shape': 'Shape',
#     
#     'shapekind': 'ShapeKind',
#     'animation': 'Animation',
#     'animationscript': 'AnimationScript',
#     'stringarray': 'StringPtr',
#     'collisiontestkind':    'CollisionTestKind',
#     
#     'bitmapcell':       'BitmapCell',
#     
# }
# 
# # dictionary for start of method names for copying variable
# # length array data
# _array_copy_data = {
#     'linesarray': 'Line',
#     'bitmaparray': 'Bmp',
#     'longintarray': 'Longint',
#     'point2darray': 'Point2D',
#     'stringarray':  'String',
#     'trianglearray':  'Triangle',
# }
# 
# # dictionary for start of method names for copying fixed
# # length array data
# _array_copy_fixed_data = {
#     'triangle': 'Tri',
#     'matrix2d': 'Matrix',
# }
# 
# _names = []
# 
# def _load_data():
#     global _header, _footer, _procedure_lines, _function_lines 
#     global _exports_header, _export_line, _function_as_procedure
#     
#     f = open('./pas_lib/header.txt')
#     _header = f.read()
#     f.close()
#     
#     f = open('./pas_lib/footer.txt')
#     _footer = f.read()
#     f.close()
#     
#     f = open('./pas_lib/exports_header.txt')
#     _exports_header = f.read()
#     f.close()
#     
#     f = open('./pas_lib/export_line.txt')
#     _export_line = f.read()
#     f.close()
#     
#     f = open('./pas_lib/procedure.txt')
#     _procedure_lines = f.readlines()
#     f.close()
#     
#     f = open('./pas_lib/function.txt')
#     _function_lines = f.readlines()
#     f.close()
#     
#     f = open('./pas_lib/function_as_procedure.txt')
#     _function_as_procedure = f.readlines()
#     f.close()
#     
#     create_c_library.load_data()
# 
# def param_visitor(the_param, last):
#     if the_param.modifier in ['out','var', 'const', 'result'] and (the_param.data_type.name.lower() in ['string','triangle'] or the_param.data_type.wraps_array):
#         return '%s: %s%s' % (
#             the_param.name, 
#             _type_switcher[the_param.data_type.name.lower()], 
#             '; ' if not last else ''
#             )
#     elif the_param.modifier in ['const']:
#         return 'var %s: %s%s' % (
#             the_param.name, 
#             _type_switcher[the_param.data_type.name.lower()], 
#             '; ' if not last else ''
#             )        
#     elif the_param.modifier in ['result']:
#         return '%s: %s%s' % (
#             the_param.name, 
#             _type_switcher[the_param.data_type.name.lower()], 
#             '; ' if not last else ''
#             )        
#     else:
#         return '%s%s: %s%s' % (
#             the_param.modifier + ' ' if the_param.modifier != None else '',
#             the_param.name, 
#             _type_switcher[the_param.data_type.name.lower()], 
#             '; ' if not last else ''
#             )
# 
# def arg_visitor(arg_str, the_arg, for_param):
#     '''Ensures data type consistency for all var/out parameters'''
#     # if for_param.modifier in ['var','out']:
#     #     #ensure exact same type for PChar and Color
#     #     if for_param.data_type.name.lower() in ['string', 'color']:
#     #         return the_arg + '_temp'
#         
#     return arg_str
# 
# def method_visitor(the_method, other):
#     data = the_method.to_keyed_dict(param_visitor, arg_visitor = arg_visitor)
#     
#     if the_method.was_function:
#         result_param = the_method.params[-1]
#         if not result_param.maps_result: #in case of returning var length array
#             result_param = the_method.params[-2]
#         
#         if not result_param.maps_result or result_param.data_type.name.lower() not in ['string', 'triangle', 'linesarray', 'matrix2d', 'point2darray', 'longintarray','stringarray','bitmaparray','trianglearray']:
#             logger.error('CREATE LIB: Unknown parameter return type in %s.', the_method.name)
#             assert False
#         lines = _function_as_procedure
#         data['return_type'] = _type_switcher[result_param.data_type.name.lower()]
#     elif the_method.return_type == None: 
#         lines = _procedure_lines
#     else: 
#         lines = _function_lines
#     
#     if len(the_method.local_vars) > 0:
#         temp = '\n  var\n'
#         temp_process_result = ''
#         temp_process_params = ''
#         for local_var in the_method.local_vars:
#             temp += '    %s: %s;\n' % (local_var.name, local_var.data_type)
#             type_name = local_var.data_type.name.lower()
#             if type_name == 'string':
#                 temp_process_result += '\n      StrCopy(%s, PChar(%s));' % (local_var.name[:-5], local_var.name)
#             elif type_name == 'color':
#                 temp_process_result += '\n      %s := %s;' % (local_var.name[:-5], local_var.name)
#             elif type_name in _array_copy_fixed_data: #needed for mapped result parameters
#                 if local_var.modifier in [None, 'var', 'const'] and not local_var.maps_result:
#                     temp_process_params += '\n      %sCopyFromPtr(%s, %s);' % (_array_copy_fixed_data[type_name], local_var.name, local_var.name[:-5])
#                 if local_var.modifier in [None, 'var', 'out'] or local_var.maps_result:
#                     temp_process_result += '\n      %sCopyToPtr(%s, %s);' % (_array_copy_fixed_data[type_name], local_var.name[:-5], local_var.name)
#             elif type_name in _array_copy_data:
#                 if local_var.modifier in [None, 'var', 'const'] and not local_var.maps_result:
#                     temp_process_params += '\n      %sCopyFromPtr(%s, %s_len, %s);' % (_array_copy_data[type_name], local_var.name[:-5], local_var.name[:-5],local_var.name)
#                 if local_var.modifier in [None, 'var', 'out'] or local_var.maps_result:
#                     temp_process_result += '\n      %sCopyToPtr(%s, %s_len, %s);' % (_array_copy_data[type_name],local_var.name, local_var.name[:-5], local_var.name[:-5])
#             else:
#                 logger.error('CREATE LIB: Unknow local variable type in %s', the_method.name)
#                 assert False
#         data['vars'] = temp[:-1]
#         data['process_result'] = temp_process_result
#         data['process_params'] = temp_process_params
#     else:
#         data['vars'] = ''
#         data['process_result'] = ''
#         data['process_params'] = ''
#     
#     _names.append(the_method.name)
#     
#     for line in lines:
#         my_writer.write(line % data) 
#     my_writer.write('\n')
# 
# def post_parse_process(the_lib):
#     '''Adds local variables for temporary values between type shifting eg. PChar -> String.'''
#     logger.info('Post Processing library for Pascal library creation')
#     
#     for key, method in the_lib.methods.items():
#         for param in method.params:
#             if param.maps_result or param.data_type.wraps_array or (param.modifier in ['var', 'out'] and param.data_type.name.lower() in ['string','color']):
#                 logger.debug('Create lib: Adding local var of type %s to %s', param.data_type, method.uname)
#                 local_var = SGParameter(param.name + '_temp')
#                 local_var.data_type = param.data_type
#                 local_var.modifier = param.modifier
#                 local_var.maps_result = param.maps_result
#                 local_var.local_for = param
#                 method.local_vars.append(local_var)
#                 param.maps_to_temp = True
# 
# def file_visitor(the_file, other):
#     '''Called for each file read in by the parser'''
#     
#     # Write out the types in a C header file
#     if the_file.name == 'Types':
#         logger.info('Processing types in %s', the_file.name)
#         create_c_library.write_c_lib_module(the_file)
#         return
#         
#     if the_file.name != 'SGSDK':
#         logger.info('skipping %s', the_file.name)
#         return
#     
#     post_parse_process(the_file.members[0])
#     
#     logger.info('Creating Pascal Library')
#     
#     create_c_library.write_c_lib_header(the_file, True)
#     
#     my_writer.writeln(_header % { 
#         'name' : the_file.pascal_name, 
#         'uses' : the_file.uses_str(lambda a_file: a_file.pascal_name), 
#         'version' : the_file.members[0].version #version from library
#         })
#     my_writer.indent();
#     
#     #visit the methods of the library
#     the_file.members[0].visit_methods(method_visitor, None)
#     
#     #write out exports
#     my_writer.writeln(_exports_header)
#     my_writer.indent()
#     for name in _names:
#         if name == _names[-1]:
#             my_writer.write((_export_line % {'name': name})[:-2])
#             my_writer.write(';')
#         else:
#             my_writer.write(_export_line % {'name': name})
#     
#     my_writer.outdent();
#     my_writer.outdent();
#     
#     my_writer.writeln(_footer)
#     my_writer.close()
# 
# def main():
#     logging.basicConfig(level=logging.WARNING,
#                         format='%(asctime)s - %(levelname)s - %(message)s',
#                         stream=sys.stdout)
#     
#     _load_data()    
#     parser_runner.run_for_all_units(file_visitor)
# 
# 
# if __name__ == '__main__':
#     main()
# 
