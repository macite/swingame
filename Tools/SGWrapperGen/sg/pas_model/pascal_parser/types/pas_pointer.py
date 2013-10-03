from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalPointer(object):
    """
    Describes a type pointer...
    """
    
    def __init__(self, block):
        self._points_to = None      # PascalType  
        self._points_to_identifier = ''
        self._function_pointer = False
        self._block = block
        self._name = ''

    @property
    def name(self):
        return self._name

    def set_name(self, name):
        self._name = name

    @property
    def kind(self):
        return 'pointer'

    @property
    def is_function_pointer(self):
        return self._function_pointer

    @property
    def points_to(self):
        return self._points_to

    def assign_type(self, type):
        self._points_to = type

    def parse(self, tokens):
        from pascal_parser.pas_parser_utils import parse_type
        from pascal_parser.pas_function import PascalFunction
        if tokens.match_lookahead(TokenKind.Symbol, '^', consume=True):
            self._points_to_identifier = tokens.match_token(TokenKind.Identifier).value
        elif tokens.match_lookahead(TokenKind.Identifier, 'procedure') or tokens.match_lookahead(TokenKind.Identifier, 'function'):
            self._points_to = PascalFunction(self._block, tokens, func_pointer = True)
            self._points_to.parse(tokens, is_forward=True)
            self._function_pointer = True
        else:
            raise_error(("Invalid pointer declaration: ", tokens.next_token()), '', is_critical=False)