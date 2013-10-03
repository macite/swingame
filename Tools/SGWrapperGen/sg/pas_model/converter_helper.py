_templates = dict()
from pascal_parser.pas_parser_utils import logger, raise_error

converters = {}

def load_templates(lib, extension):
    import glob
    import os
    
    path = os.path.join(os.path.dirname(os.path.realpath(__file__)), lib)
    # print 'here ', path
    file_paths = glob.glob(path + '*' + extension)
    if len(file_paths) > 0:
        for path in file_paths:
            file = open(path, "r")
            template = ''
            for line in file.readlines():
                template += line
            _templates[os.path.basename(path)] = template
            file.close()
    else:
        raise_error(("Template path is empty: %s" % path), '', is_critical=True)

def get_template(name):
    if name is None: return None

    if (name in _templates):
        return _templates[name]
    else:
        raise_error(("Error getting template: " + name), '', is_critical=True) 

#--------------------------------------------------------------------------------------------------

def camel_case_name(string):
    """
    Converts a pascal case name into a camel case name
    eg. ConvertCase -> convertCase
    """
    return string.lower()[0] + string[1:]

def upper_name(string):
    """
    ConvertCase -> CONVERTCASE
    """
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
    """
    Converts pascal case into c style casing
    ConvertCase -> convert_case
    """
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

#--------------------------------------------------------------------------------------------------

def convert_type(the_dict, the_type, modifier = None, dict_name = '_type_switcher'):
    '''
    switch types for the SwinGame library.
    
    Params:
     - the_dict:    The dictionary with the type changes with modifier keys
     - the_type:    The type being checked
     - modifier:    The modifier for the type
     - dict_name:   The name of the dictionary for error reporting
    '''

    if the_type == None: return 'NONE'  # TODO: work out how to automate this... shouldn't ever happen in reality
    if len(the_dict) > 0: 
        key = the_type.name.lower()
    
        if modifier == 'result': modifier = 'return'        # C ism? What is this used for?
    
        if key not in the_dict[modifier]:
            logger.error('HELPER    : Error changing model type %s - %s', modifier, the_type)
            logger.error('          : Add \'%s[%s]\': \'%s\': \'????\',', dict_name, modifier, the_type.name.lower())
                
            # hack solution to fix some SwinGame types not being 
            the_dict[modifier][key] = the_type.name
            print '**** ', the_type.name
        
        return the_dict[modifier][key]
    else:
        return the_type.name    # for Pascal the names stay the same...

def convert_operator(the_dict, the_operator, dict_name = '_operator_conversion_table'):
    '''
    converts operators from Pascal to other language operators
    Does not fail if the operator wasn't found, the returned value
    will be the same as the current operator
    '''
    key = the_operator.value.lower() if the_operator != None else None
    
    if key not in the_dict:
        logger.debug('          : Adding \'%s[%s]\'????\',', dict_name, the_operator.value.lower())
        the_dict[key] = the_operator.value
        
    return the_dict[key]

#--------------------------------------------------------------------------------------------------

def apply_indents(str, indentation):
    result = """"""
    for line in str.split('\n'):
        result += (indentation * '    ') + line + '\n'
    result = result.rstrip()
    return result