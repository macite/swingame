#!/usr/bin/env python
# encoding: utf-8
"""
lang_helper.py

Created by Andrew Cain on 2011-01-05.
Copyright (c) 2011 Swinburne University. All rights reserved.
"""

import sys
import os

from sg.sg_parameter import SGParameter
from sg.sg_property import SGProperty
from sg.sg_method import SGMethod
from sg.sg_cache import logger, find_or_add_type

# from prepared_method import PreparedMethod


_hasError = False
_dieOnError = False

def hasError():
    global _hasError
    return _hasError

# ============
# = Visitors =
# ============

def write_method_code(method, other):
    other['writer'].writeln(method.alias(other['lang_key']).code)
    return other

def write_method_signature(method, other):
    other['writer'].writeln(method.alias(other['lang_key']).signature)
    return other


def std_type_visitor(the_dict, the_type, modifier = None, dict_name = '_type_switcher'):
    '''
    switch types for the SwinGame library.
    
    Params:
     - the_dict:    The dictionary with the type changes with modifier keys
     - the_type:    The type being checked
     - modifier:    The modifier for the type
     - dict_name:   The name of the dictionary for error reporting
    '''
    key = the_type.name.lower() if the_type != None else None
    
    if modifier == 'result': modifier = 'return'
    
    if key not in the_dict[modifier]:
        logger.error('HELPER    : Error changing model type %s - %s', modifier, the_type)
        logger.error('          : Add \'%s[%s]\': \'%s\': \'????\',', dict_name, modifier, the_type.name.lower())
        
        global _hasError, _dieOnError
        _hasError = True
        
        if _dieOnError: 
            assert False
        else:
            the_dict[modifier][key] = "UNKNOWN"
        
    return the_dict[modifier][key]

def map_data_value(the_dict, key, the_type, result):
    '''
    Returns the code needed to map the swingame (Pascal) data in result to
    the type used by language X
    
    Params:
     - the_dict:    The dictionary with the type changes with modifier keys
     - key:         The key to search for in the_dict
     - the_type:    The type being checked
     - modifier:    The modifier for the type
    '''
    if the_type.name.lower() in the_dict[key]:
        # print the_type.name.lower(), ' -> ', the_dict[key][the_type.name.lower()]
        return the_dict[key][the_type.name.lower()] % result
        
    #If this is a 
    if the_type.pointer_wrapper and key == 'return_val':
        logger.error('HELPER   : Error pointer wrapper without return data mapping - %s', the_type)
        
        global _hasError, _dieOnError
        _hasError = True
        
        if _dieOnError: 
            assert False
        else:
            the_dict[key][the_type.name.lower()] = "UNKNOWN"
        
        
    return result

# ===================================
# = Adding data to a PreparedMethod =
# ===================================

def add_local_var_for_param(to_method, for_sg_parameter):
    '''
    Add a local variable to_method for_parameter. This is used for array
    processing in cases where the array data need to be marshalled between
    the program and the DLL and back.
    
    PARAMS:
    - to_method : the PerparedMethod that needs the local variable added.
    - for_sg_parameter : the parameter that will have a local variable
    '''
    
    logger.debug('HELPER   : Adding local for %s in %s', for_sg_parameter.name, to_method.method_name);
    
    local_var = LocalVar()
    local_var.setup_for_parameter(for_sg_parameter)
    to_method.local_vars.append(local_var)
    
    return local_var

def add_local_var_for_result(to_method):
    '''
    Adds a local variable for the result of a function. This can be used to add processing
    after the call and before the value is returned. Also needed for any function that has
    been turned into a procedure (ones that return any form of array).
    '''
    
    sg_method = to_method.sg_method
    
    if sg_method.method_called != None and sg_method.method_called.was_function:
        # The called method has been turned into a procedure, so create a local variable for its result parameter
        result_param = sg_method.method_called.params[-1]
        if not result_param.maps_result: #in case of returning var length array
            result_param = sg_method.method_called.params[-2]
        
        assert result_param.maps_result
        
        local_var = add_local_var_for_param(to_method, result_param)
    else:
        # Ignore call for void functions...
        if to_method.return_type is None: return
        
        local_var = LocalVar()
        local_var.setup_for_result(to_method.return_type)
        to_method.local_vars.append(local_var)
    
    if sg_method.method_called != None and sg_method.method_called.was_function:
        to_method.args.append(var) # and pass it as an additional argument

def add_length_local(to_method, for_sg_parameter):
    
    var_name = for_param.local_var_name() + '_len'
    local_var = SGParameter(var_name)
    local_var.is_length_param = True
    local_var.local_for = for_param
    local_var.data_type = find_or_add_type('Longint')
    local_var.modifier = for_param.modifier
    local_var.length_of = for_param
    
    for_param.length_param = local_var
    for_param.has_length_param = True
    
    to_method.local_vars.append(local_var)
    to_method.args.append(local_var.name)
    
    return local_var

def add_length_args(to_method, len_str):
    '''
    Adds length arguments, and possibly local variables, to the passed in method based
    upon the method that it calls. This scans the method looking for length parameters
    and adding in the argument code needed to pass the length of the called parameter.
    
    The len_str is used to contain the format of the expression to be passed to the
    called method when the argument is passed. This will be passed a single string
    containing the name of the parameter that it is the length of (this will also be a
    parameter in this method, meaning that a %s can be used to place this value within
    the expression).
    '''
    # for the parameters in the called method...
    for param in to_method.method_called.params:
        if param.is_length_param:
            #possibly need an extra local for this... if out or return...
            if param.length_of.maps_result:
                # need to indicate the size of the returned array...
                if to_method.fixed_result_size > 0:
                    to_method.args.append(str(to_method.fixed_result_size))
                else:
                    var = add_length_local(to_method, to_method.get_variable(param.length_of.name))
            elif param.modifier == 'out':
                var = add_length_local(to_method, to_method.get_variable(param.length_of.name))
            elif not param.data_type.is_struct:
                to_method.args.append(len_str % param.length_of.name)
            else:
                to_method.args.append(len_str % param.length_of.local_var_name())

def add_local_var_processing(the_method, details, local_variable_switcher):
    '''
    This processing adds variable declaration code, pre-call processing code
    and post-call processing code to the details for the_method. These values
    stored in details['vars'], details['pre_call'] and details['post_call'].
    
    Params:
     - the_method:              The method to process the local variables of
     - details:                 The dictionary into which the output is written
     - local_variable_switcher: The dictionary containing the type switching
                                and code details. This dictionary must contain:
                                
        'declare':              Key used to get code to declare variable - is passed variable name (+ size if array).
        'length-of':            Used for the code that calculates the length of an array (or string).
        'initialise-param':     The code used to copy data from the language (arrays and string) into 
                                data that can be passed to SwinGame. This code goes into 'pre_call'.
        'process-param':        This is the code for 'post_call' processing for parameters. Called on
                                standard parameters of arrays - (as array is a pointer and contents updated when
                                dynamic arrays are used).
        'process-out-param':    This is the code for 'post_call' processing for out parameters.
        'process-result':       This is the 'post_call' processing for the result of a function. This code
                                is used to return the result. This will be the last 'post_call' code.
                            
    '''
    
    if len(the_method.local_vars) > 0:
        temp = ''
        temp_process_params = details['pre_call']
        temp_process_result = details['post_call']
        
        # process all local variables
        # ---------------------------
        # This performs the processing of the local variables added to the method.
        # The basic idea is to perform the processing necessary to take the data from language X
        # and convert it so it can be passed to Pascal for processing. Once the processing
        # is complete the code also needs to copy the data back from Pascal to language X again for
        # any var,out or array parameters and the return result.
        for local_var in the_method.local_vars:
            #Get the lowercase name of the type
            lower_type = local_var.data_type.name.lower()
            
            #Setup dictionary:
            #  - %(var)s = local variable name
            #  - %(param)s = parameter name
            #  - %(size)s = size expression (literal or call)
            var_details = dict()
            var_details['var'] = local_var.name
            var_details['param'] = local_var.local_for.name if local_var.local_for != None else None
            var_details['modifier'] = 'const ' if local_var.modifier == 'const' else ''
            
            if local_var.pass_through:
                #just passes out return value:
                #TODO: fix these so that the code generated is not just C like code: i.e. Type result; and return result;...
                temp += '%(return_type)s result;\n    ' % details #need to adjust for non-c style syntax - call specific code
                temp_process_result = temp_process_result + '\n    return %s;' % local_var.name;
                continue #no further processing for this type
            elif local_var.length_of != None:
                #This is a variable that is the length of a given array.
                if the_method.fixed_result_size > 0: #if its fixed size, we know its length
                    var_details['size'] = the_method.fixed_result_size
                elif the_method.length_call != None:
                    #Otherwise, we need to call the length function.
                    
                    #TODO: change these... call creator to exclude return/result = keyword (need call.expr)
                    # the issue is that a return is added to the length_call. Here we want just the call
                    # to fix create a call.expr = call expression that is used to create the return blah; and (blah) here
                    var_details['size'] = details['length_call'].replace('return ', '').replace('result = ', '')
                    # print var_details['size']
                # TODO: fix this so that variable declaration is not hard coded (and C style)
                
                # print the_method.name, '->', the_method.method_called.name
                temp = 'int %(var)s = %(size)s;\n    ' % var_details + temp
                continue #no further processing of these variables
            elif local_var.data_type.name.lower() == 'string':
                #This is a string, need to know its size...
                if local_var.modifier in ['var', 'const', None]:
                    var_details['size'] = std_type_visitor(local_variable_switcher, local_var.data_type, 'length-of', 'local_variable_switcher') % var_details
                else: #out or return
                    var_details['size'] = '2048'
            elif local_var.maps_result and isinstance(local_var, SGParameter):
                #This is the result local variable used to return a value
                if local_var.has_length_param: #is it an array?
                    if local_var.length_param != None: #it is variable length
                        var_details['size'] = local_var.length_param.name
                    else: #it is fixed, so just hard code size
                        var_details['size'] = the_method.fixed_result_size
                else:
                    var_details['size'] = 'unknown'
            elif local_var.modifier in ['var', 'const', None] and local_var.data_type.array_wrapper:
                #We have a local variable that is an array
                #Read the length of an array from its length... - variable length only
                var_details['size'] = std_type_visitor(local_variable_switcher, local_var.data_type, 'length-of', 'local_variable_switcher') % var_details
            else:
                # Not a type that has a size...
                var_details['size'] = ''
            
            #Add code to declare local variable
            temp += std_type_visitor(local_variable_switcher, local_var.data_type, 'declare', 'local_variable_switcher') % var_details
            
            #Add code to initialise local variable
            if local_var.modifier in ['var', 'const', None]:
                temp_process_params += std_type_visitor(local_variable_switcher, local_var.data_type, 'initialise-param', 'local_variable_switcher') % var_details
            # else:
            #     print 'No pre-processing for ', local_var
            
            if local_var.modifier in ['out', 'var']:
                #Add code to process out and var parameters
                temp_process_result = std_type_visitor(local_variable_switcher, local_var.data_type, 'process-out-param', 'local_variable_switcher') % var_details + temp_process_result
            elif local_var.modifier == None and local_var.data_type.wraps_array:
                #Add code to process arrays passed without const = being changed in SwinGame
                temp_process_result = std_type_visitor(local_variable_switcher, local_var.data_type, 'process-param', 'local_variable_switcher') % var_details + temp_process_result
                # print temp_process_result
            elif local_var.maps_result:
                temp_process_result = std_type_visitor(local_variable_switcher, local_var.data_type, 'process-result', 'local_variable_switcher') % var_details + temp_process_result
            
        # if the_method.has_out_params and not the_method.was_function:
        #     #need to store result... add a new local variable...
        #     # var_details = dict()
        #     # var_details['var'] = 'temp_result'
        #     # var_details['param'] = local_var.local_for.name if local_var.local_for != None else None
        #     # var_details['modifier'] = 'const ' if local_var.modifier == 'const' else ''
        #     # 
        #     # temp += local_variable_switcher['declare'][lower_type] % var_details
        #     print 'here', the_method.name
            
        details['vars'] = temp
        details['post_call'] = temp_process_result
        details['pre_call'] = temp_process_params
        
    else:
        details['vars'] = ''


def create_property_for_field(in_class, field):
    '''Creates a property to access the '''
    
    if field.data_type.wraps_array:
        logger.error('HELPER    : Error structure with array fields must be set to via_pointer')
        
        global _hasError, _dieOnError
        _hasError = True
        
        if _dieOnError: 
            assert False
    
    prop = SGProperty(field.name)
    prop.in_class = in_class
    prop.data_type = field.data_type
    
    getter = SGMethod('get' + field.pascalName)
    getter.return_type = field.data_type
    getter.in_class = in_class
    getter.field_name = field.name
    getter.is_getter = True
    
    setter = SGMethod('set' + field.pascalName)
    setter.params.append(SGParameter('value'))
    setter.params[0].data_type = field.data_type
    setter.in_class = in_class
    setter.field_name = field.name
    setter.is_setter = True
    
    prop.set_getter(getter)
    prop.set_setter(setter)
    
    return prop
    
def upper_name(string):
    if string == None: return None
    result = ''
    last_was_us = False
    last_was_lc = False
    for i,c in enumerate(string):
        if c.islower():
            result += c.upper()
            last_was_us = False
            last_was_lc = True
        elif c in ['_','-']:
            last_was_us = True
            result += '_'
        elif c.isupper():
            if (not last_was_us) and last_was_lc and (i > 0):
                result += '_'
            result += c
            last_was_us = False
            last_was_lc = False
        else:
            result += c
            last_was_us = False
    return result

def lower_name(string):
    if string == None: return None
    string = string.replace('2D', '2d')
    result = ''
    last_was_us = False
    last_was_lc = False
    for i,c in enumerate(string):
        if c.islower():
            result += c
            last_was_us = False
            last_was_lc = True
        elif c in ['_','-']:
            last_was_us = True
            result += '_'
        elif c.isupper():
            if (not last_was_us) and last_was_lc and (i > 0):
                result += '_'
            result += c.lower()
            last_was_us = False
            last_was_lc = False
        else:
            result += c
            last_was_us = False
    return result

# ===================================================
# = Type conversion dictionary population functions =
# ===================================================

def _add_to_dict(into_dict, details_dict, ident_tupple):
    #find all the part
    for key,val in details_dict.iteritems():
        to_ins = val
        for idx,part in enumerate(ident_tupple):
            to_ins = to_ins.replace('#%d#' % (idx + 1), ident_tupple[idx])
        # print 'inserting -> ', key, ':', to_ins
        
        if  ident_tupple[0] in into_dict[key]:
            print 'ERROR: Adding into type dictionary : ', into_dict[key][ident_tupple[0]]
            print 'key: ', key, 'type', ident_tupple[0]
            assert False
        
        into_dict[key][ident_tupple[0]] = to_ins

def build_type_dictionary(type_dictionary_creation_data, dicts):
    """Builds the conversion dictionary."""
    my_keys = dicts.keys()
    
    for type_mapping in type_dictionary_creation_data:
        #print type_mapping
        # Process each type in this type mapping
        for identifier_tupple in type_mapping['identifiers']:
            for a_key in my_keys:
                _add_to_dict(dicts[a_key], type_mapping[a_key], identifier_tupple)

