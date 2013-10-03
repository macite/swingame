from tokeniser.pas_token_kind import TokenKind
from pas_parser_utils import logger
from pas_expression import PascalExpression

class PascalCaseStatement(object):
    """
    The PascalCaseStatement object parses a case statement
    """

    def __init__(self, block):
        self._expression = None
        self._case = list(()) # (constant_list, statement)
        self._else_statements = list()
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
        return 'case statement'

    def parse(self, tokens):
        from pas_parser_utils import parse_statement
        logger.debug("Parsing case statement")
        tokens.match_token(TokenKind.Identifier, 'case')
        self._expression = PascalExpression(self._block)
        self._expression.parse(tokens)
        # expression will consume the 'of' keyword
        while not (tokens.match_lookahead(TokenKind.Identifier, 'end') or tokens.match_lookahead(TokenKind.Identifier, 'else') or tokens.match_lookahead(TokenKind.Identifier, 'otherwise')):
            constant = None
            if (tokens.match_lookahead(TokenKind.Number)):
                constant = tokens.match_token(TokenKind.Number).value
            elif (tokens.match_lookahead(TokenKind.String)):
                constant = tokens.match_token(TokenKind.String.value)
                if len(constant) > 1:
                    raise_error(("Constant expected: %s" %tokens.next_token()), '', is_critical=False)
            elif tokens.match_lookahead(TokenKind.Identifier):
                constant = self._block.resolve_variable(tokens.match_token(TokenKind.Identifier).value)
            else:
                raise_error(("CaseStatement:        Ordinal type expected: %s", tokens.next_token()), '', is_critical=False)

            tokens.match_token(TokenKind.Symbol, ':')

            statement = parse_statement(tokens, self._block)
            tokens.match_lookahead(TokenKind.Symbol, ';', consume=True)
            self._case.append(tuple((constant, statement)))
        if tokens.match_lookahead(TokenKind.Identifier, 'else', consume=True) or tokens.match_lookahead(TokenKind.Identifier, 'otherwise', consume=True):
            while not (tokens.match_lookahead(TokenKind.Identifier, 'end', consume=True)):
                self._else_statements.append(parse_statement(tokens, self._block))
        else:
            tokens.match_token(TokenKind.Identifier, 'end')
        tokens.match_token(TokenKind.Symbol, ';')

        logger.debug("Finished parsing if statement")

    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        for case in self._case:
            case[1].to_code()   # statement to code...
        for statement in self._else_statements:
            statement.to_code()
        self._expression.to_code()

        if len(self._else_statements) > 0:
            for statement in self._else_statements:
                statement.to_code()
        cases = ''
        for name, module in converter_helper.converters.items():
            expression = self._expression.code[name]
            cases = ''
            else_statements = ''
            # convert case statements
            for case in self._case:
                cases += module.case_template % {'constant' : case[0], 'statement' : case[1].code[name] }
            cases = converter_helper.apply_indents(cases, module.indenter['cases'])
            # convert else statements if they exist
            if len(self._else_statements) > 0:
                for statement in self._else_statements:
                    else_statements += statement.to_code[name]
            self._code[name] = module.case_statement_template % { 'expression' : expression, 'cases' : cases , 'else' : else_statements }


