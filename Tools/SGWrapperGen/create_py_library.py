#!/usr/bin/env python
# encoding: utf-8
"""
create_py_lib.py

Created by Clinton Woodward on 2009-07-09.
Copyright (c) 2009. All rights reserved.

# stage 1: a simple ctypes binding wrapper for the sgsdk.dll methods
# stage 2: a compile python module to call sgsdk.dll using cython (~pyrex)

SwinGame/     - the module name
  __init__.py - pulls in other modules as needed (for SwinGame)
  core.py     - core application (gameloop) methods
  audio.py    - music sound ...
  physics.py  - collisions ...
  ...
  sgsdk.py - contains the mapping of all functions

"""

import logging
import sys

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file
from sg.print_writer import PrintWriter
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

import py_lib # template definitions

_out_path="../../Generated/Python"


_procedure_lines = None
_function_lines = None
_exports_header = ''

# Used by USER FACING code - things returned/used by to the user
BLAH_type_switcher = {
    None : {    
        #Pascal type: what it maps to
        'single': 'float %s',
        'longint': 'int %s',
        'soundeffect': 'SoundEffect %s',
        'music': 'Music %s',
        'string': 'const char *%s',
        'boolean': 'bool %s',
        'byte': 'unsigned char %s',
        'timer': 'Timer %s',
        'color': 'Color %s',
        'resourcekind': 'ResourceKind %s',
        'longword': 'unsigned int %s',
        'bitmap': 'Bitmap %s',
        'pointer': 'void *%s',
        'single[0..2][0..2]': 'float %s[3][3]',
        '^bitmapdata': 'BitmapData *%s',
        '^spritedata': 'SpriteData *%s',
        '^timerdata': 'TimerData *%s',
        'boolean[0..n - 1][0..n - 1]': 'bool *%s',
        'bitmap[0..n - 1]': 'Bitmap *%s',
        'spritekind': 'SpriteKind %s',
        'longint[0..n - 1]': 'int *%s',
        'vector': 'Vector %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        'point2d': 'Point2D %s',
        'point2dptr': 'Point2D *%s',
        'point2d[0..2]': 'Point2D %s[3]',
        'point2d[0..n - 1]': 'Point2D *%s',
        '^linesegment': 'LineSegment *%s',
        'linesegment': 'LineSegment %s',
        'sprite': 'Sprite %s',
        'rectangle': 'Rectangle %s',
        'triangle': 'Triangle %s',
        'linesarray': 'LinesArray %s',
        'linesegmentptr': 'LineSegment *%s',
        'font': 'Font %s',
        'fontalignment': 'FontAlignment %s',
        'fontstyle': 'FontStyle %s',
        'mousebutton': 'MouseButton %s',
        'uint16': 'unsigned short %s',
        'singleptr': 'float *%s',
        'keycode': 'KeyCode %s',
        'bitmapptr': 'Bitmap *%s',
        '^bitmap': 'Bitmap *%s',
        'longintptr': 'int *%s',
        '^longint': 'int *%s',
        'collisionside': 'CollisionSide %s',
        'longint[0..n - 1][0..n - 1]': 'int *%s',
        'mapdata': 'MapData %s',
        'mapanimationdata[0..n - 1]': 'MapAnimationData *%s',
        'maplayerdata[0..n - 1]': 'MapLayerData *%s',
        'mapcollisiondata': 'MapCollisionData %s',
        'maptagdetails[0..n - 1][0..23]': 'MapTagDetails *%s[24]',
        '^maprecord': 'MapRecord *%s',
        'map': 'Map %s',
        'maptag': 'MapTag %s',
        'tile': 'Tile %s',
        'circle': 'Circle %s',
        'point2darray': 'Point2D *%s',
        'matrix2d': 'Matrix2D %s',
        None: 'void %s'
    },
    'const' : {
        'point2d': 'const Point2D *%s',
        'linesegment': 'const LineSegment *%s',
        'rectangle': 'const Rectangle *%s',
        'matrix2d': 'const Matrix2D %s',
        'vector': 'const Vector *%s',
        'linesarray': 'const LinesArray %s',
        'triangle': 'const Triangle %s',
        'bitmaparray': 'const Bitmap %s',
        'longintarray': 'const int *%s',
        'circle': 'const Circle *%s',
    },
    'var' : {
        'soundeffect': 'SoundEffect *%s',
        'music': 'Music *%s',
        'timer': 'Timer *%s',
        'string': 'char *%s',
        'triangle': 'Triangle %s',
        'linesarray': 'LinesArray %s',
        'font': 'Font *%s',
        'bitmap': 'Bitmap *%s',
        'sprite': 'Sprite *%s',
        'matrix2d': 'Matrix2D %s',
        'map': 'Map *%s',
        'point2darray': 'Point2D *%s',
        'longintarray': 'int *%s',
    },
    'out' : {
        'string': 'char *%s',
        'byte': 'unsigned char *%s',
        'color': 'unsigned int *%s',
        'timer': 'Timer *%s',
        'point2d': 'Point2D *%s',
#        'triangle': 'Triangle *%s'
        'longint': 'int *%s',
        'linesegment': 'LineSegment *%s',
    },
    'return' : {
        None: 'void %s',
        'boolean': 'bool %s',
        'music': 'Music %s',
        'soundeffect': 'SoundEffect %s',
        'single': 'float %s',
        'point2d': 'Point2D %s',
        'longint': 'int %s',
        'timer': 'Timer %s',
        'byte': 'unsigned char %s',
        'color': 'Color %s',
        'longword': 'unsigned int %s',
        'vector': 'Vector %s',
        'circle': 'Circle %s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'bitmap': 'Bitmap %s',
        'collisionside': 'CollisionSide %s',
        'font': 'Font %s',
        'map': 'Map %s',
        'sprite': 'Sprite %s',
        'fontstyle': 'FontStyle %s',
        'maptag': 'MapTag %s',
        'maptile': 'MapTile %s',
        'string': 'String %s',
        'linesarray': 'LineSegment *%s',
        'matrix2d': 'Matrix2D %s',
        'point2darray': 'Point2D *%s',
        'triangle': 'Triangle %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        'spritekind': 'SpriteKind %s',
    },
}

# POST-PROCESSING - used in wrapped method bodies
_data_switcher = {
    #Pascal type: what values of this type switch to %s = data value
    'Boolean': '%s != 0'
}

# literal substitution
BLAH_val_switcher = {
    'True': 'true',
    'False': 'false'
}

#------------------------------------------------------------------------------
# c_int, c_float, c_char_p, c_byte, c_uint32, c_uint16, c_void_p, c_bool

_adapter_type_switcher = {
    # used in the declaration of types (struct/record ->Structure)
    None: {
        'single': "('%s', c_float)", #'float %s',
        'longint': "('%s', c_int)", #'int %s',
        'soundeffect': "('%s', c_void_p)", #'void *%s',
        'music': "('%s', c_void_p)", #'void *%s',
        'string': "('%s', c_char_p)", #'const char *%s',
        'boolean': "('%s', c_bool)", #'int %s',
        'byte': "('%s', c_byte)", #'unsigned char %s',
        'color': "('%s', c_uint32)", #'unsigned int %s',
        'timer': "('%s', c_void_p)", #'void *%s',
        'resourcekind': "('%s', c_int)", #'int %s',
        'longword': "('%s', c_uint32)", #'unsigned int %s',
        'bitmap': "('%s', c_void_p)", #'void *%s',
        'rectangle': "('%s', Rectangle)", #'Rectangle %s',
        'linesegment': "('%s', LineSegment)", #'LineSegment %s',
        'triangle': "('%s', Triangle)", #'Triangle %s',
        'point2d': "('%s', Point2D)", #'Point2D %s',
        'sprite': "('%s', c_void_p)", #'void *%s',
        'linesarray': 'LinesArray %s',
        'font': "('%s', c_void_p)", #'void *%s',
        'fontalignment': "('%s', c_int)", #'int %s',
        'fontstyle': "('%s', c_int)", #'int %s',
        'mousebutton': "('%s', c_int)", #'int %s',
        'uint16': "('%s', c_uint16)", #'unsigned short %s',
        'vector': "('%s', Vector)", #'Vector %s',
        'spriteendingaction': "('%s', c_int)", #'SpriteEndingAction %s',
        'keycode': "('%s', c_int)", #'KeyCode %s',
        'matrix2d': "('%s', Matrix2D)", #'Matrix2D %s',
        'collisionside': "('%s', c_int)", #'CollisionSide %s',
        'map': 'MapRecord *%s',
        'maptag': "('%s', c_int)", #'Event %s',
        'maptile': "('%s', MapTile)", #'Tile %s',
        'circle': "('%s', Circle)", #'Circle %s',
        'point2darray': 'Point2D *%s',
        'psdl_surface': "('%s', c_void_p)", #'void *%s',
        'boolean[0..n - 1][0..n - 1]': 'bool *%s',
        'boolean[0..n - 1]': 'bool *%s',
        'bitmap[0..n - 1]': 'void *%s',
        'spritekind': "('%s', c_int)", #'int %s',
        'longint[0..n - 1]': 'int *%s',
        'longintarray': 'int *%s',
        'longint[0..n - 1][0..n - 1]': 'int *%s',
        'mapdata': "('%s', MapData)", #'MapData %s',
        'mapanimationdata[0..n - 1]': 'MapAnimationData *%s',
        'maplayerdata[0..n - 1]': 'MapLayerData *%s',
        'mapcollisiondata': "('%s', MapCollisionData)", #'CollisionData %s',
        'maptagdetails[0..n - 1][0..23]': 'MapTagDetails *%s[24]',
        'point2dptr': 'Point2D *%s',
        'point2d[0..n - 1]': 'Point2D *%s',
        'point2d[0..2]': 'Point2D %s[3]',
        'linesegmentptr': 'LineSegment *%s',
        'single[0..2][0..2]': 'float %s[3][3]',
        'singleptr': 'float *%s',
        'longintptr': 'int *%s',
        'bitmapptr': 'void *%s',
        None: 'void %s'
    },
    'const' : {
        'point2d': 'const Point2D *%s',
        'linesegment': 'const LineSegment *%s',
        'rectangle': 'const Rectangle *%s',
        'matrix2d': 'const Matrix2D %s',
        'triangle': 'const Triangle %s',
        'vector': 'const Vector *%s',
        'linesarray': 'const LinesArray %s',
        'longintarray': 'const int *%s',
        'bitmaparray': 'const Bitmap %s',
        'circle': 'const Circle *%s'
    },
    'var': {
        'soundeffect': 'SoundEffect *%s',
        'music': 'Music *%s',
        'timer': 'Timer *%s',
        'byte': 'unsigned char *%s',
        'string': 'char *%s',
        'triangle': 'Triangle %s',
        'linesarray': 'LinesArray %s',
        'font': 'Font *%s',
        'bitmap': 'Bitmap *%s',
        'sprite': 'Sprite *%s',
        'map': 'Map *%s'
    },
    'out': {
        'string': 'char *%s',
        'byte': 'unsigned char *%s',
        'color': 'unsigned int *%s',
        'timer': 'void *%s',
        'point2d': 'Point2D *%s',
#        'triangle': 'Triangle *%s',
        'linesarray': 'LinesArray %s',
        'longint': 'int *%s',
        'linesegment': 'LineSegment *%s',
    },
    'result' : {
        'string': 'char *%s',
        'linesarray': 'LineSegment *%s',
        'matrix2d': 'Matrix2D %s',
        'point2darray': 'Point2D *%s',
        'triangle': 'Triangle %s',
        'longintarray': 'int *%s',
    },
    # Mapping of the return type of a function
    'return' : {
        None: 'void %s',
        'boolean': 'int %s',
        'music': 'Music %s',
        'soundeffect': 'SoundEffect %s',
        'single': 'float %s',
        'point2d': 'Point2D %s',
        'longint': 'int %s',
        'timer': 'Timer %s',
        'byte': 'unsigned char %s',
        'color': 'unsigned int %s',
        'longword': 'unsigned int %s',
        'vector': 'Vector %s',
        'circle': 'Circle %s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'bitmap': 'Bitmap %s',
        'collisionside': 'CollisionSide %s',
        'font': 'Font %s',
        'map': 'Map %s',
        'sprite': 'Sprite %s',
        'fontstyle': 'FontStyle %s',
        'event': 'Event %s',
        'tile': 'Tile %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        'spritekind': 'SpriteKind %s',
    }

}

_names = []


def arg_visitor(arg_str, the_arg, for_param_or_type):
    '''Called for each argument in a call, performs required mappings'''
    if isinstance(for_param_or_type, SGType):
        the_type = for_param_or_type
    else:
        the_type = for_param_or_type.data_type
        
    if the_type.name in _data_switcher:
        #convert data using pattern from _data_switcher
        return _data_switcher[the_type.name] % arg_str
    else:
        return arg_str

def adapter_type_visitor(the_type, modifier = None):
    '''switch types for the python SwinGame adapter (links to DLL)'''
    name = None if the_type is None else the_type.name.lower()
    if name not in _adapter_type_switcher[modifier]:
        logger.error('CREATE   : Error changing adapter type %s - %s', modifier, the_type)
    
    return _adapter_type_switcher[modifier][name]

def adapter_param_visitor(the_param, last):
    name = the_param.name.lower()
    if name not in _adapter_type_switcher[the_param.modifier]:
        logger.error('CREATE   : Error visiting adapter parameter %s - %s', the_param.modifier, the_param.data_type.name)
        return 'XXX'
        #assert False
    
    return '%s%s' % (
        _adapter_type_switcher[the_param.modifier][name] % the_param.name,
        ', ' if not last else '')

def type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame library'''
    name = None if the_type is None else the_type.name.lower()
    if name not in _type_switcher[modifier]:
        logger.error('CREATE  : Error changing model type %s - %s', modifier, the_type)
        assert False
    
    return _type_switcher[modifier][name]

def param_visitor(the_param, last):
    return '%s%s' % (
        type_visitor(the_param.data_type, the_param.modifier) % the_param.name,
        ', ' if not last else '')

def method_visitor(the_method, other):
    details = the_method.to_keyed_dict(
        other['param visitor'], 
        other['type visitor'], 
        other['arg visitor'])
    # check and cleanup doc strings ... % need to be %%, + line indent
    if '%' in details['doc']:
        details['doc'] = details['doc'].replace('%','%%')
    details['doc'] = '\n   '.join([ line for line in details['doc'].splitlines() ])

    if other['writer'] != None: 
        if the_method.is_function:
            #%(calls.name)s(%(calls.args)s)
#            print details
            details['the_call'] = other['arg visitor']('%(calls.name)s(%(calls.args)s)' % details, None, the_method.return_type)
            other['writer'].write(py_lib.function % details % the_method['uname'])
        else:
            other['writer'].write(py_lib.method % details)
    
    return other


def write_methods_for(member, other):
    '''Write out member methods'''
    member.visit_methods(method_visitor, other)

def write_type_for(member, other):
    '''Write out a type (class, struct, enum, typedef)'''
    assert member.is_class or member.is_struct or member.is_enum or member.is_type
    writer = other['writer']
    # CLASS, TYPE, STRUCT(ARRAY)
    if member.is_class or member.is_type or (member.is_struct and member.wraps_array):
        #convert to resource pointer
        if member.is_pointer_wrapper:
            # assert len(member.fields) == 1
            the_type = member.data_type
            writer.writeln('typedef %s;\n' % adapter_type_visitor(the_type, None) % member.name)
        elif member.is_data_wrapper:
            assert len(member.fields) == 1
            the_type = member.fields['data'].data_type
            writer.writeln('typedef %s;\n' % adapter_type_visitor(the_type) % member.name)
        elif member.wraps_array:
            assert len(member.fields) == 1
            the_type = member.fields['data'].data_type
            writer.writeln('typedef %s;\n' % adapter_type_visitor(the_type) % member.name)
        else:
            logger.error('CREATE PYTHON  : Unknown class type for %s', member.uname)
            assert False
    # PURE STRUCT
    elif member.is_struct:
        #class Point2D(Structure): # record/struct;
        #    _fields_ = [('x', c_float), (), ... ]
        writer.writeln('class %s(Structure):' % member.name)
        writer.writeln("    '''%s\n    '''" % '\n   '.join(member.doc.splitlines()))
        writer.writeln('    _fields_ = [')
        for field in member.field_list:
            writer.writeln('        %s,' % adapter_type_visitor(field.data_type) % field.name)
        writer.writeln('    ]\n')
    # PURE ENUM
    elif member.is_enum:
        #enum id { list } 
        # basic -> (...) = map(c_int, range(length(...))
        # class object FontAlignment.Left etc
        #other['writer'].write(str(member) + str(dir(member)))
        writer.writeln('class %s(object): # enum' % member.name)
        writer.writeln("    '''%s\n    '''" % '\n   '.join(member.doc.splitlines()))
        for i, v in enumerate(member.values):
            if '=' in v:
                v = v.replace(' = ', ' = c_int(') + ')' # use provided indicies
            else:
                v = v + ' = c_int(%d)' % i # need to add indicies
            writer.writeln('    %s' % v)
        writer.writeln('')
        

def write_py_module(the_file):
    '''Write the header and c file to wrap the attached files detials'''
    mod = FileWriter('%s/%s.py' % (_out_path, the_file.name))
    
    mod.writeln(py_lib.header % { 
        'name' : the_file.name,
        'pascal_name' : the_file.pascal_name}
    )
    
    for a_file in the_file.uses:
        if a_file.name != None:
            mod.writeln("import %s\n" % a_file.name)
    mod.writeln('')
    
    #process all methods
    other = {
        'writer': mod,
        'type visitor': type_visitor,
        'param visitor': param_visitor,
        'arg visitor': arg_visitor
    }

    for m in the_file.members:
        if m.is_class or m.is_struct or m.is_enum or m.is_type:
            write_type_for(m, other)        
        elif m.is_module:
            write_methods_for(m, other)

    mod.close()

def post_parse_process(the_file):
    ''' the c modules also wrap array return values and adds length parameters for arrays.'''
    logger.info('Post Processing %s for Python wrapper creation', the_file.name)
    
    for member in the_file.members:
        for key, method in member.methods.items():
            if method.method_called.was_function:
                #convert string return types
                result_param = SGParameter('result')
                result_param.data_type = method.return_type
                result_param.modifier = 'var'
                param_list = list(method.params)
                param_list.append(result_param)
                method.params = tuple(param_list)
                arg_list = list(method.args)
                arg_list.append(result_param)
                method.args = arg_list
                method.return_type = None
            if method.method_called.has_length_params:
                #add length parameters to this method
                for param in method.method_called.params:
                    if param.is_length_param:
                        param_list = list(method.params)
                        param_list.append(param)
                        method.params = tuple(param_list)
                        arg_list = list(method.args)
                        arg_list.append(param)
                        method.args = arg_list

def file_visitor(the_file, other):
    '''Called for each file read in by the parser'''
    if the_file.name == 'SGSDK':
        return
    post_parse_process(the_file)
    print the_file.name
    logger.info('Creating Python SwinGame Module %s', the_file.name)
    write_py_module(the_file)

def main():
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        stream=sys.stdout)
    
    #load_data()
    parser_runner.run_for_all_units(file_visitor)


if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass

    main()
