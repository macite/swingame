from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalType(object):
    """
    Describes a general type... Integer, String etc...
    That does not need to record any additional information 
    """
    
    def __init__(self, name=''):
        self._name = name
        
    @property
    def name(self):
        return self._name

    def set_name(self, name):
        self._name = name

    @property
    def kind(self):
        return self._name

    def parse(self, tokens):
        self._name = tokens.match_token(TokenKind.Identifier).value
        self._type = self._name
