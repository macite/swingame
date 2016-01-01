#!/usr/bin/env python
# encoding: utf-8
"""
parser_runner.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

from sg_cache import find_or_add_file, find_or_add_class, find_or_add_type, all_files, logger
from sg_pas_parser import SGPasParser
from sg_parameter import SGParameter

# All units - used as arguments to "find_or_add_file" in "run_for_all_units()"
# Note: this does not include any "discovered" units like sgShared.pas
all_units = [
    ('sgTypes', 'Types', '../../CoreSDK/src/sgTypes.pas'), #read in types first...
    ('sgAnimations', 'Animations', '../../CoreSDK/src/sgAnimations.pas'),
    ('sgAudio', 'Audio', '../../CoreSDK/src/sgAudio.pas'),
    ('sgCamera', 'Camera', '../../CoreSDK/src/sgCamera.pas'),
    ('sgGeometry', 'Geometry', '../../CoreSDK/src/sgGeometry.pas'),
    ('sgGraphics', 'Graphics', '../../CoreSDK/src/sgGraphics.pas'),
    ('sgImages', 'Images', '../../CoreSDK/src/sgImages.pas'),
    ('sgInput', 'Input', '../../CoreSDK/src/sgInput.pas'),
    ('sgNetworking', 'Networking', '../../CoreSDK/src/sgNetworking.pas'),
    ('sgPhysics', 'Physics', '../../CoreSDK/src/sgPhysics.pas'),
    ('sgResources', 'Resources', '../../CoreSDK/src/sgResources.pas'),
    ('sgSprites', 'Sprites', '../../CoreSDK/src/sgSprites.pas'),
    ('sgText', 'Text', '../../CoreSDK/src/sgText.pas'),
    ('sgTimers', 'Timers', '../../CoreSDK/src/sgTimers.pas'),
    ('sgUtils', 'Utils', '../../CoreSDK/src/sgUtils.pas'),
    ('sgUserInterface', 'UserInterface', '../../CoreSDK/src/sgUserInterface.pas'),
    ('sgArduino', 'Arduino', '../../CoreSDK/src/sgArduino.pas'),
    ('sgDrawingOptions', 'DrawingOptions', '../../CoreSDK/src/sgDrawingOptions.pas'),
    ('sgWindowManager', 'WindowManager', '../../CoreSDK/src/sgWindowManager.pas'),
]



def _add_parameter(the_method, param):
    param_list = list(the_method.params)
    param_list.append(param)
    the_method.params = tuple(param_list)
    return len(the_method.params) - 1
    

def method_process_visitor(the_method, other):
    '''Process a method prior to rendering. This does all of the transformations
    that go into the functions and procedures of the SGSDK unit.'''
    
    # Change functions with string or array return type to have a result parameter
    if the_method.return_type != None and (the_method.return_type.wraps_array or the_method.return_type.name.lower() in ['string']):
        #print 'PARS RUN PARAMS', [p.name for p in the_method.params]
        result_param = SGParameter('result')
        
        for param in the_method.params:
            if 'result' == param.name:
                logger.error('PARSER RUN: Error adding result parameter to %s', the_method.name)
                assert False
        _add_parameter(the_method, result_param)
        
        result_param.maps_result = True
        result_param.modifier = 'result'
        
        result_param.data_type = the_method.return_type
        
        the_method.return_type = None
        the_method.was_function = True
        #print 'PARS RUN PARAMS', [p.name for p in the_method.params]
        
    #Replace any variable length arrays
    orig_params = the_method.params
    for param in orig_params:
        #does it wrap a variable length array
        if param.data_type.array_wrapper:
            the_method.has_length_params = True
            logger.debug('PARSER RUN: Altering variable length array of %s\'s parameter %s', the_method.name, param.name)
            # old_type = param.data_type
            #param.data_type = param.data_type #.fields[0].data_type
            len_param = SGParameter('%s_len' % param.name)
            len_param.is_length_param = True
            len_param.data_type = find_or_add_type('Longint')
            len_param.modifier = param.modifier if param.modifier is ['out', 'var'] else None
            len_param.length_of = param
            param.has_length_param = True
            param.length_idx = _add_parameter(the_method, len_param)
    return other

def post_parse_process(the_file):
    '''Create temporary variables for out/var string parameters, and string 
    return types'''
    logger.info('Post parsing library')
    
    the_file.members[0].visit_methods(method_process_visitor, None)

def run_for_all_units(file_visitor):
    '''Parse all standard known units listed in the module variable `all_units`.
    The `file_visitor` is then given the chance to process each unit, including
    the generated sgsdk.pas, in turn.
    '''
    parse_all_units()
    visit_all_units(file_visitor)

def parse_all_units():
    parser = SGPasParser()
    
    lib_file = find_or_add_file('SGSDK','SGSDK','./sgsdk.pas')
    
    # Build up a cache of the unit files (excludes sgsdk.pas)
    files = [ find_or_add_file(*args) for args in all_units ]
    
    # Parse each unit file (excluding sgsdk.pas)
    for a_file in files: 
        parser.parse(a_file)

    # Create the "lib" class, and checks all its methods to ensure that the 
    # method arguments, and the called-by parameters, match.
    find_or_add_class('lib').check_methods()
    
    # Tell the lib file about each of the known (or discovered) unit files
    lib_file.uses.extend([ f for f in all_files().values() if f != lib_file ] )
    lib_file.members.append(find_or_add_class('lib'))
    post_parse_process(lib_file)

def visit_all_units(file_visitor):    
    logger.info('Processing files')
    files = [ unit[0] for unit in all_units ] + ['SGSDK']
    for each_file in files:
        logger.debug('Visiting file %s', each_file)
        find_or_add_file(each_file).visit(file_visitor, None)

def show_file(the_file, other):
    print 'Done', the_file.name

def main():
    logging.basicConfig(level=logging.WARNING,
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        stream=sys.stdout)
    run_for_all_units(show_file)


if __name__ == '__main__':
    main()

