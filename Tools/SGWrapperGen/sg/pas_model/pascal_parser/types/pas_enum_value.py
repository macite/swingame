from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalEnumValue(object):
    """
    Describes a number used in expressions
    """
    
    def __init__(self, name, value):
        self._name = name
        self._value = value
        self._code = dict()


    @property
    def code(self):
        return self._code
        
    @property
    def kind(self):
        return 'enumeration value'

    @property
    def name(self):
        return self._name

    @property
    def value(self):
        return self._value

    def to_code(self):
        import converter_helper

        my_data = dict()

        my_data['pas_lib_identifier'] = self._name 
        my_data['c_lib_identifier'] = converter_helper.upper_name(self._name)
        my_data['value'] = self._value  # integers are the same always...

        for (name, module) in converter_helper.converters.items():
            self._code[name] = module.enum_value_template % my_data
        
        self._code['pas_lib_reference'] = self._name 
        self._code['c_lib_reference'] = converter_helper.upper_name(self._name)
