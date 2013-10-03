from tokeniser.pas_token_kind import TokenKind
from pas_parser_utils import logger, token_has_values, parse_statement

from pas_assignment_statement import AssignmentStatement

class PascalCompoundStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    as well as store a list of statements
    """

    def __init__(self, block, is_block_statement=False):
        self._is_block_statement = is_block_statement
        self._statements = list()
        self._block = block
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def statements(self):
        return self._statements

    @property
    def block(self):
        return self._block

    @property
    def kind(self):
        return 'compound statement'

    def parse(self, tokens):
        logger.debug("Parsing compound statement")
        if tokens.match_lookahead(TokenKind.Identifier, 'begin'):
            tokens.match_token(TokenKind.Identifier, 'begin')
        while (True):
            # compound statement currently consumes the end keyword, but not the symbol ';' or '.'
            if tokens.match_lookahead(TokenKind.Identifier, 'end'):
                tokens.match_token(TokenKind.Identifier, 'end')             # consume end token
                break
            elif tokens.match_lookahead(TokenKind.Symbol, ';') or tokens.match_lookahead(TokenKind.Symbol, '.'):
                # consumed end already -> somehow?
                tokens.match_token(TokenKind.Symbol)
                break   
            elif tokens.match_lookahead(TokenKind.Identifier, 'until'):
                # repeat..until needs to have the until token so that it knows it's at the end
                break

            else:
                self._statements.append(parse_statement(tokens, self._block))

        logger.debug("Finished parsing compound statement")

    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        

        for statement in self._statements:
            statement.to_code()

        for (name, module) in converter_helper.converters.items():
            statements = ""
            # if the statement is the program's statement and it's in pascal convert it
            # otherwise don't...
            if (self._block.parent != None) or (self._block.parent == None and name == 'pas_lib') :
                for statement in self._statements:
                    statements += statement.code[name] + module.statement_seperator + '\n'
                    
                if self._is_block_statement:
                    #statements = converter_helper.apply_indents(statements, module.indenter['block_compound_statement'])
                    self._code[name] = module.block_compound_statement_template % { "statements": statements }
                else:
                    #statements = converter_helper.apply_indents(statements, module.indenter['statement'])
                    self._code[name] = module.compound_statement_template % { "statements": statements }

            else:
                self._code[name] = ''

            


                




