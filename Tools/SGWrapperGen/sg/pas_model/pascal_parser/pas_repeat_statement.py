from tokeniser.pas_token_kind import TokenKind
from pas_parser_utils import logger
from pas_expression import PascalExpression

class PascalRepeatStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self, block):
        self._expression = None
        self._block = block
        self._statements = list()
        self._code = dict()     

    @property
    def code(self):
        return self._code

    @property
    def expression(self):
        return self._expression

    @property
    def statements(self):
        return self._statements

    @property
    def kind(self):
        return 'repeat statement'

    def parse(self, tokens):
        from pas_parser_utils import parse_statement
        logger.debug("Parsing repeat statement")
        tokens.match_token(TokenKind.Identifier, 'repeat')
        while not tokens.match_lookahead(TokenKind.Identifier, 'until'):
            self._statements.append(parse_statement(tokens, self._block))
        tokens.match_token(TokenKind.Identifier, 'until')
        self._expression = PascalExpression(self._block)
        self._expression.parse(tokens)
        # expression will consume the 'then' delimiter
        logger.debug("Finished parsing repeat statement")

    def to_code(self, indentation=0):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        
        self._expression.to_code()

        for statement in self._statements:
            statement.to_code()

        for (name, module) in converter_helper.converters.items():
            statements = ''
            for statement in self._statements:
                statements += statement.code[name] + module.statement_seperator + '\n'
            #statements = converter_helper.apply_indents(statements, module.indenter['statement'])
            self._code[name] = module.repeat_statement_template % {'expression' : self._expression.code[name], 'statements' : statements }
            self._code[name] = converter_helper.apply_indents(self._code[name], module.indenter['statement'])

