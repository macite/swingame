from pas_parser_utils import logger 

_loaded_types = dict()

def find_or_add_type(name):
    if name == None: return None
    from types.pas_type import PascalType
    
    name_key = name.lower()
    
    if name_key in _loaded_types:
        return _loaded_types[name_key]
    else:
        logger.debug('Cache     : Created Type %s', name)
        result = PascalType(name)
        _loaded_types[name_key] = result
        return result