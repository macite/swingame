from pas_parser_utils import logger

from pas_function_call import PascalFunctionCall
from types.pas_number import PascalNumber
from types.pas_string import PascalString
from types.pas_operator import PascalOperator
from types.pas_string import PascalString
from types.pas_bool import PascalBool
from pas_var_reference import PascalVariableReference

class PascalExpression(object):
    """
    The Expression statement stores all the information required to evaluate an expression
    """

    # stores operators in one queue, and operands in another
    
    def __init__(self, owner_block, innerExpr = False):
        self._contents = list()
        self._block = owner_block
        self._code = dict()
        self._innerExpr = innerExpr

    @property
    def code(self):
        return self._code

    @property
    def kind(self):
        return 'expression'

    def parse(self, tokens):
        """
        this method parses the expression
        """
        from tokeniser.pas_token_kind import TokenKind
        logger.debug('Parsing %s expression' % ('inner' if self._innerExpr else ''))
        newContent = None
        while (True):
            # expression over?
            if tokens.match_lookahead(TokenKind.Symbol, ';') or tokens.match_lookahead(TokenKind.Symbol, ',') or tokens.match_lookahead(TokenKind.Identifier, 'then') or tokens.match_lookahead(TokenKind.Identifier, 'do') or tokens.match_lookahead(TokenKind.Identifier, 'of') or tokens.match_lookahead(TokenKind.Symbol, ']'):
                if self._innerExpr:
                    raise_error(('Inner expression terminator expected, %s: ', tokens.next_token()), '', is_critical=False)
                tokens.next_token() # consume the delimiter
                logger.debug('Expression ended')
                break
            # Inner expression ended
            elif tokens.match_lookahead(TokenKind.Symbol, ')'):
                if self._innerExpr:
                    tokens.match_token(TokenKind.Symbol, ')') # consume the delimiter
                    logger.debug('Inner expression ended')
                break
            # starts with an Identifier
            elif tokens.match_lookahead(TokenKind.Identifier):
                # Function Call
                if tokens.lookahead(2)[1].value is '(':
                    newContent = PascalFunctionCall(self._block, inExpr=True)
                    newContent.parse(tokens)
                # Variable
                else:
                    newContent = PascalVariableReference(self._block)
                    newContent.parse(tokens)
            # Number
            elif tokens.match_lookahead(TokenKind.Number):
                newContent = PascalNumber(tokens.next_token().value)
            # string
            elif tokens.match_lookahead(TokenKind.String):
                newContent = PascalString(tokens.next_token().value)
            # Operator
            elif tokens.match_lookahead(TokenKind.Operator):                
                newContent = PascalOperator(tokens.match_token(TokenKind.Operator).value)
            # inner expression
            elif tokens.match_lookahead(TokenKind.Symbol, '('):
                tokens.next_token() # consume the delimiter
                newContent = PascalExpression(self._block, innerExpr = True)
                newContent.parse(tokens)
            # Boolean
            elif tokens.match_lookahead(TokenKind.Boolean):
                newContent = PascalBool(tokens.match_token(TokenKind.Boolean).value)
                
            else:
                raise_error(('Unknown expression token... %s' % str(tokens.next_token().value)), '', is_critical=False)

            self._contents.append(newContent)

    @property
    def contents(self):
        return self._contents

    def __str__(self):
        result = None
        for item in self._contents:
            result += str(item)
        return result

    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        
        for part in self._contents:
            part.to_code()

        for (name, module) in converter_helper.converters.items():
            expression = ""

            for part in self._contents:
                if part.kind == 'variable' or part.kind == 'record field':
                    expression += part.code[name + '_reference']   # variable[name] returns variable declaration
                else:
                    expression += part.code[name]
                expression += ' '
            
            if (self._innerExpr):
                self._code[name] = module.inner_expression_template % { "expression": expression }
            else:
                self._code[name] = module.expression_template % { "expression": expression }