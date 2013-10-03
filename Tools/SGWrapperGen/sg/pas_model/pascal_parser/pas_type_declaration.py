from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import raise_error, logger, reservedWords, token_has_values
from pascal_parser.types.pas_type_cache import *
from pascal_parser.types.pas_enum import PascalEnum
from pascal_parser.types.pas_pointer import PascalPointer
from pascal_parser.types.pas_record import PascalRecord


class PascalTypeDeclaration(object):
    """
    
    """
   
    def __init__(self, parent):
        """
        parent is the current block the type declaration is 
        being initialised from
        """

        #parent block
        self._parent = parent       
        self._name = ''
        # stores the type name (key) and type declaration
        self._type_declarations = dict()    
        self._forward_pointer_declarations = dict()
        self._code = dict()
        self._comments = list()
        self._meta_comment = None

    @property
    def code(self):
        return self._code
    @property
    def name(self):
        return self._name

    @property
    def types(self):
        return self._type_declarations

    @property
    def kind(self):
        return 'type declaration'

    def parse(self, tokens):
        from pas_parser_utils import parse_type
        from pascal_parser.tokeniser.pas_meta_comment import PascalMetaComment
        self._comments = tokens.get_comments()

        self._meta_comment = PascalMetaComment(tokens)
        self._meta_comment.process_meta_comments()

        tokens.match_token(TokenKind.Identifier, 'type')

        while True:
            m_comment = PascalMetaComment(tokens)
            m_comment.process_meta_comments()
            new_type = None

            if token_has_values(tokens.lookahead()[0], reservedWords):
                break

            type_name = tokens.match_token(TokenKind.Identifier).value
            tokens.match_token(TokenKind.Operator, '=')
            # enumeration...
            if tokens.match_lookahead(TokenKind.Symbol, '('):
                new_type = PascalEnum(type_name, self._parent)
                new_type.parse(tokens)
                tokens.match_token(TokenKind.Symbol, ';')
            #record...
            elif (tokens.match_lookahead(TokenKind.Identifier, 'packed', consume=True) and tokens.match_lookahead(TokenKind.Identifier, 'record', consume=True)) or tokens.match_lookahead(TokenKind.Identifier, 'record', consume=True):
                new_type = PascalRecord(type_name, self._parent)
                new_type.parse(tokens)
                tokens.match_token(TokenKind.Symbol, ';')
            # other type...
            else:
                if tokens.match_lookahead(TokenKind.Identifier) and tokens.lookahead(2)[1].value == '.':
                    # sgTypes.type... so consume the extra identifier and dot
                    tokens.match_token(TokenKind.Identifier)
                    tokens.match_token(TokenKind.Symbol, '.')

                new_type = parse_type(tokens, self._parent)
                tokens.match_token(TokenKind.Symbol, ';')

            # forward declarations... 
            if (new_type.kind is 'pointer') and (not new_type.is_function_pointer):
                self._forward_pointer_declarations[new_type.name] = new_type
                new_type.set_name(type_name)
            # parse new type and add it to the type declaration
            self._type_declarations[type_name] = new_type
            self._parent._types[type_name] = new_type

        # resolve any pointer declarations
        for name, type in self._forward_pointer_declarations.items():
            pointer_type = self._parent.resolve_type(type._points_to_identifier)
            type.assign_type(pointer_type)
        
    def to_code(self, indentation=0):
        '''

        '''
        import converter_helper

        for key, type in self._type_declarations.items():
            type.to_code()

        my_data = dict()
        # seperators need to be set for every new package
        #   these could go in the __init__ for each library?
        for (key, type) in self._type_declarations.items():
            type.to_code(indentation)

        for (name, module) in converter_helper.converters.items():
            declaration = ""
            index = 0
            for (key, type) in self._type_declarations.items():
                declaration += type.code[name]
            declaration = converter_helper.apply_indents(declaration, module.indenter['types'])
            self._code[name] =  module.type_declaration_template % {"declaration" : declaration}