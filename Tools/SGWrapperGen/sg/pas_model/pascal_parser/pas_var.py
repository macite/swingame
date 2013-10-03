class PascalVariable(object):
    """
    Describes a single variable in pascal
        Contains a name and type
    """
    
    def __init__(self, name, type, modifier = None, is_parameter=False):
        self._type = type
        self._name = name
        self._modifier = modifier
        self._is_parameter = is_parameter
        """
        Stores the line of code for each of the language converters
            eg. code["c_lib"] returns the code to declare a variable in c
        """
        self._code = dict()     

    @property
    def code(self):
        return self._code
                
    @property  
    def modifier(self):
        return self._modifier

    @property
    def type(self):
        return self._type

    @property
    def name(self):
        return self._name

    @property
    def kind(self):
        return 'variable'

    @property
    def is_record(self):
        return (type.kind == 'record')

    def to_code(self):
        '''
            Creates a _code entry for each of the converter modules

        '''
        import converter_helper
        my_data = dict()

        my_data['pas_lib_identifier'] = self._name 
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        for (name, module) in converter_helper.converters.items():
            # types need to be evaluated in the loop because they are module specific
            if self._type.kind is 'array':
                self._code[name] = module.convert_array_declaration(self, self._is_parameter)
            else:
                my_data[name + '_type'] = converter_helper.convert_type(module._type_switcher, self._type, self._modifier)
                self._code[name] = module.variable_template % my_data
            self._code[name + "_reference"] = my_data[name + "_identifier"]
