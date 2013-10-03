from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalArray(object):
    """
    Describes an array
    """
    
    def __init__(self, owner):
        self._dimensions = list()   # list of tuple (low_idx, high_idx)
        self._nested_type = None    #PascalType...
        self._name = ''
        self._owner = owner
        
    @property
    def name(self):
        return self._name

    @property
    def dimensions(self):
        return self._dimensions

    def set_name(self, name):
        self._name = name
              
    @property
    def nested_type(self):
        return self._nested_type

    @property
    def kind(self):
        return 'array'

    def get_dimensions(n=1):
        """
        returns the nth set of dimensions of an array
        """
        if n <= len(self._dimensions): return self._dimensions[n-1]
    
    def parse(self, tokens):
        from pascal_parser.pas_parser_utils import parse_type
        # from pas_type_cache import 

        tokens.match_token(TokenKind.Identifier, 'array')
        # array [0..n,0..m] of
        if tokens.match_lookahead(TokenKind.Symbol, '[', consume=True):
            while True:
                low_idx = 0
                high_idx = 0
                if (tokens.match_lookahead(TokenKind.Number)):
                    low_idx = tokens.match_token(TokenKind.Number).value
                    tokens.match_token(TokenKind.Symbol, '.')
                    tokens.match_token(TokenKind.Symbol, '.')
                    high_idx = tokens.match_token(TokenKind.Number).value
                 
                # It's another ordinal type...   
                elif (tokens.match_lookahead(TokenKind.Identifier)):
                    # it's a type in the case of enumerations... could be a variable in the case of constants
                    idx_type = self._owner.resolve_type(tokens.match_token(TokenKind.Identifier).value)
                    if idx_type.kind is 'enumeration':
                        low_idx = idx_type.get_low().value
                        high_idx = idx_type.get_high().value
                else:
                    logger.error("Array:        Ordinal type expected: %s", tokens.next_token())  
                self._dimensions.append((low_idx,high_idx))
                if tokens.match_lookahead(TokenKind.Symbol, ']', consume=True):
                    break
                tokens.match_token(TokenKind.Symbol, ',')
        else:
            self._dimensions.append((0,'n - 1'))
            
        # of type...
        tokens.match_token(TokenKind.Identifier, 'of')
        self._nested_type = parse_type(tokens, self._owner) #recursive call
            
        self._name = self._nested_type.name + ''.join(['[%s..%s]' % (low_idx, high_idx) for low_idx, high_idx in self._dimensions])

        
