from pascal_parser.pas_parser_utils import raise_error, logger

_loaded_units = dict()
_files = dict()

def files():
    return _files

def remove_file_named(name):
    if get_file_named(name.lower()) != None:
        del _files[name.lower()]

def get_file_named(name):
    '''
    '''
    if name != None:
        return _files[name.lower()]
    else:
        return None

def add_file(file):
    '''
    '''
    if file != None:
        _files[file.name.lower()] = file
        if file.contains_kind == 'unit':
            _loaded_units[file.name.lower()] = file
            logger.info("File Cache:        Added unit: %s", file.name)
        else:
            logger.info("File Cache:        Added program: %s", file.name)
    else:
        raise_error("File Cache:       Unable to add None unit", 'FileIO', is_critical=False)

def get_unit_named(name):
    '''
    gets a unit of a specific name from the unit cache
    '''
    if (name != None) and (name.lower() in _loaded_units):
        return _loaded_units[name.lower()]
    else:
        return None