from tokeniser.pas_token_kind import TokenKind
from pas_parser_utils import logger

class PascalOperatorOverload(object):
    """
    The PascalOperatorOverload parses an overload declaration
    """

    def __init__(self, block):
        self._operator = None
        self._parameters = None #PacalParameters
        self._result = None #PascalVariable
        self._code = dict()
        self._block = block
        self._parameter_list = list()

    @property 
    def name(self):
        return 'overload ' + self._operator

    @property
    def code(self):
        return self._code

    @property
    def kind(self):
        return 'operator overload'

    def add_parameter(self, var_to_add):
        self._parameter_list.append(var_to_add)

    def parse(self, tokens):
        from pascal_parser.pas_parser_utils import parse_type
        from pascal_parser.pas_var import PascalVariable
        from pascal_parser.pas_param_declaration import PascalParameterDeclaration
        logger.debug("Parsing overload declaration")
        tokens.match_token(TokenKind.Identifier, 'operator')
        self._operator = tokens.match_token(TokenKind.Operator).value
        self._parameters = PascalParameterDeclaration(self._block)
        self._parameters.parse(tokens, self)

        name = tokens.match_token(TokenKind.Identifier)
        tokens.match_token(TokenKind.Symbol, ':')
        type = parse_type(tokens, self._block)

        self._result = PascalVariable(name, type)
        tokens.match_token(TokenKind.Symbol, ';')
        logger.debug("Finished parsing operator declaration")

    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        
        self._expression.to_code()
        if (self._statement.kind == 'compound statement'):
            self._statement.to_code()
        else:
            self._statement.to_code(indentation+1)

        for (name, module) in converter_helper.converters.items():
            self._code[name] = module.if_statement_template % {'expression' : self._expression.code[name], 'statement' : self._statement.code[name] }






