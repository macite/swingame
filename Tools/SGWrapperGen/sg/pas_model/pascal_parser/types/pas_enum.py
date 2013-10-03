from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pas_enum_value import PascalEnumValue
class PascalEnum(object):
    """
    Describes a number used in expressions
    """
    
    def __init__(self, name, block):
        self._values = list()   # list of tuples
        self._code = dict()
        self._name = name
        self._block = block

    @property
    def code(self):
        return self._code

    @property
    def values(self):
        return self._values
        
    @property
    def kind(self):
        return 'enumeration'

    @property
    def name(self):
        return self._name

    def get_low(self):
        return self._values[0]

    def get_high(self):
        return self._values[len(self._values)-1]

    def parse(self, tokens):
        current_value = 0
        tokens.match_token(TokenKind.Symbol, '(')
        while not tokens.match_lookahead(TokenKind.Symbol, ')', consume=True):
            name = tokens.match_token(TokenKind.Identifier).value
            if tokens.match_lookahead(TokenKind.Operator, '=', consume=True) or tokens.match_lookahead(TokenKind.Operator, ':=', consume=True): #assigned value
                current_value = eval(tokens.match_token(TokenKind.Number).value)
            self._values.append(PascalEnumValue(name, current_value))
            current_value += 1
            tokens.match_lookahead(TokenKind.Symbol, ',', consume=True) #consume commas

    def to_code(self,indentation = 0):
        import converter_helper

        for value in self._values:
            value.to_code()
        my_data = dict()
        # seperators need to be set for every new package
        #   these could go in the __init__ for each library?
        my_data['c_lib_seperator'] = ','
        my_data['pas_lib_seperator'] = ','

        my_data['pas_lib_identifier'] = self._name 
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        for (name, module) in converter_helper.converters.items():
            enum_values = ""
            for index in range(len(self._values)):
                enum_values += self._values[index].code[name]

                if index < (len(self._values)-1):
                    enum_values += my_data[name + '_seperator']
            my_data["values"] = enum_values
            self._code[name] = module.enum_template % my_data