class PascalBool(object):
    """
    Describes a number used in expressions
    """
    
    def __init__(self, value):
        self._value = value
        self._code = dict()

    @property
    def code(self):
        return self._code
        
    @property
    def kind(self):
        return 'boolean'

    @property
    def value(self):
        return self._value

    def __str__(self):
        return str(self.value)

    def to_code(self):
        import converter_helper
        for (name, module) in converter_helper.converters.items():
            self._code[name] = self._value
