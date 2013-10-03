from tokeniser.pas_token_kind import TokenKind
from pas_parser_utils import logger
from pas_expression import PascalExpression

class PascalIfStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self, block):
        self._expression = None
        self._statement = None
        self._else_statement = None
        self._block = block
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def block(self):
        return self._block

    @property
    def expression(self):
        return self._expression

    @property
    def statement(self):
        return self._statement

    @property
    def kind(self):
        return 'if statement'

    def parse(self, tokens):
        from pas_parser_utils import parse_statement
        logger.debug("Parsing if statement")
        tokens.match_token(TokenKind.Identifier, 'if')
        self._expression = PascalExpression(self._block)
        self._expression.parse(tokens)
        self._statement = parse_statement(tokens, self._block)
        # expression will consume the 'then' delimiter
        if tokens.match_lookahead(TokenKind.Identifier, 'else') :
            tokens.match_token(TokenKind.Identifier, 'else')
            self._else_statement = parse_statement(tokens, self._block)
        tokens.match_lookahead(TokenKind.Symbol, ';', consume=True)

        logger.debug("Finished parsing if statement")

    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        
        self._expression.to_code()
        self._statement.to_code()
        if not (self._else_statement is None):
            self._else_statement.to_code()

        for (name, module) in converter_helper.converters.items():
            if_data = dict()
            if_data['expression'] = self._expression.code[name] 
            statement = self._statement.code[name]

            if_data['statement'] = statement
            if_data['else'] = ''

            if not (self._else_statement is None):
                statement = self._else_statement.code[name]
                if_data['else'] = module.else_statement_template % { "statement" : statement }

            self._code[name] = module.if_statement_template % if_data
            self._code[name] = converter_helper.apply_indents(self._code[name], module.indenter['statement'])


