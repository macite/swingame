from pascal_parser.pas_parser_utils import raise_error, logger
class PascalArguments(object):
    """
    Describes a single variable in pascal
        Contains a name and type
    """
    
    def __init__(self, block):
        self._contents = list()     # expressions
        """
        Stores the line of code for each of the language converters
            eg. code["c_lib"] returns the code to declare a variable in c
        """
        self._block = block
        self._code = dict()     

    @property
    def code(self):
        return self._code
                
    @property  
    def contents(self):
        return self._contents

    @property
    def kind(self):
        return 'arguments'

    def parse(self, tokens):
        """
            parses an set of arguments of a function call
                consumes the enclosing braces
        """
        from pas_expression import PascalExpression
        from tokeniser.pas_token_kind import TokenKind
        logger.debug("parsing arguments")
        tokens.match_token(TokenKind.Symbol, '(')
        while True:
            if (tokens.match_lookahead(TokenKind.Symbol, ')', consume=True) or tokens.match_lookahead(TokenKind.Symbol, ',', consume=True)):
                logger.debug("finished parsing arguments")
                break
            newExpression = PascalExpression(self._block)
            newExpression.parse(tokens)
            self._contents.append(newExpression)

    def to_code(self):
        '''
            Creates a _code entry for each of the converter modules

        '''
        import converter_helper
        my_data = dict()

        my_data['c_lib_seperator'] = ','
        my_data['pas_lib_seperator'] = ','

        for expression in self._contents:
            expression.to_code();

        for (name, module) in converter_helper.converters.items():
            # types need to be evaluated in the loop because they are module specific
            args = ''
            for index in range(len(self._contents)):
                args += self._contents[index].code[name]
                if index < (len(self._contents)-1):
                    args += my_data[name + '_seperator']
            self._code[name] = args
