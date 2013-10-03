class PascalOperator(object):
    """
    Describes a operator used in expressions
    """
    
    def __init__(self, value):
        self._value = value
        self._code = dict()

    @property
    def code(self):
        return self._code
                
    @property
    def kind(self):
        return 'operator'

    @property
    def value(self):
        return self._value

    def to_code(self):

        import converter_helper

        for (name, module) in converter_helper.converters.items():
            self._code[name] = converter_helper.convert_operator(module._operator_conversion_table, self)