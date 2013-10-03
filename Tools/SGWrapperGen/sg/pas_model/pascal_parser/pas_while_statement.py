from pas_token_kind import TokenKind
from pas_parser_utils import logger
from pas_expression import PascalExpression

class PascalWhileStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self, block):
        self._expression = None
        self._statement = None
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
        return 'while statement'

    def parse(self, tokens):
        from pas_parser_utils import parse_statement
        logger.debug("Parsing while statement")
        tokens.match_token(TokenKind.Identifier, 'while')
        self._expression = PascalExpression(self._block)
        self._expression.parse(tokens)
        self._statement = parse_statement(tokens, self._block)
        # expression will consume the 'then' delimiter
        logger.debug("Finished parsing while statement")

    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        
        self._expression.to_code()
        self._statement.to_code()

        for (name, module) in converter_helper.converters.items():
            self._code[name] = module.while_statement_template % {'expression' : self._expression.code[name], 'statement' : self._statement.code[name] }

