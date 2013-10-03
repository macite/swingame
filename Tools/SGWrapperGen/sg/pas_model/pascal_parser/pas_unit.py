from tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import _parse_compound_statement, logger
from pas_function import PascalFunction
from pas_var_declaration import PascalVarDeclaration
from pascal_parser.pas_type_declaration import PascalTypeDeclaration
from pascal_parser.pas_uses_clause import PascalUsesClause
from pascal_parser.pas_operator_overload import PascalOperatorOverload

class PascalUnit(object):
    """
    The model object to represent a pascal unit:
    """
    
    def __init__(self, file):
        self._name = None
        self._file = file
        self._uses_clause = PascalUsesClause(self._file)
        self._interface = list()
        self._implementation = list()
        self._function_forward_declarations = list()
        self._parsed = False
        self._variables = dict()
        self._functions = dict()
        self._types = dict()
        self._contents = list()
        self._code = dict()

    @staticmethod
    def create_from(file, variable_names, type_names, function_names):
        from pascal_parser.pas_var import PascalVariable
        from pascal_parser.types.pas_type import PascalType
        result = PascalUnit(file)
        if not (variable_names is None):
            for (name, type) in variable_names:
                result.variables[name] = (PascalVariable(name, type))
        if not (type_names is None):
            for type_name in type_names:
                result.types[type_name] = (PascalType(type_name))
        if not (function_names is None):
            for function_name in function_names:
                result.function_declarations.append(function_name)
        return result


    @property
    def code(self):
        return self._code

    @property
    def uses(self):
        return self._uses_clause.units

    @property
    def file(self):
        return self._file

    @property
    def variables(self):
        return self._variables

    @property
    def types(self):
        return self._types

    @property
    def function_declarations(self):
        return self._function_forward_declarations

    @property
    def functions(self):
        return self._functions

    @property
    def is_parsed(self):
        return self._parsed

    @property
    def name(self):
        return self._name

    def resolve_type(self, type_name):
        #check myself
        for (name, type) in self._types.items():
            if (type_name.lower() == name.lower()):
                return type
        return self._file.resolve_type(type_name)

    def parse(self, tokens):
        """
        Parses the entire pascal unit
        expects: 'unit name;' at the start
        """
        from pascal_parser.tokeniser.pas_meta_comment import PascalMetaComment
        # read unit header
        tokens.match_token(TokenKind.Identifier, 'unit');
        self._name = tokens.match_token(TokenKind.Identifier)._value;
        tokens.match_token(TokenKind.Symbol, ';')

        # read interface
        tokens.match_token(TokenKind.Identifier, 'interface')

        # if there is a uses clause - read it
        if (tokens.match_lookahead(TokenKind.Identifier, 'uses')):
            self._uses_clause.parse(tokens)
            self._interface.append(self._uses_clause)

        # read declarations
        while (True):

            self._comments = tokens.get_comments()

            self._meta_comment = PascalMetaComment(tokens)
            self._meta_comment.process_meta_comments()
            # variable declaration part
            if (tokens.match_lookahead(TokenKind.Identifier, 'var')):
                # read variable declaration part
                # and append to current active variables
                # local variables can overwrite global variables 
                current_part = PascalVarDeclaration(self)
                current_part.parse(tokens)
                self._variables.update(current_part.variables)
            elif (tokens.match_lookahead(TokenKind.Identifier, 'procedure') or tokens.match_lookahead(TokenKind.Identifier, 'function')):
                current_part = PascalFunction(self, tokens)
                self._function_forward_declarations.append(current_part)
                current_part.parse(tokens, is_forward=True)
            elif (tokens.match_lookahead(TokenKind.Identifier, 'type')):
                current_part = PascalTypeDeclaration(self)
                current_part.parse(tokens) 
                self._types.update(current_part.types)
            elif (tokens.match_lookahead(TokenKind.Identifier, 'operator')):
                  temp = PascalOperatorOverload(self)
                  temp.parse(tokens)
            elif tokens.match_lookahead(TokenKind.Identifier, 'implementation'):
                break
            else:
                raise_error(('Unknown unit token...' + str(tokens.next_token())), '', is_critical=False)
            self._interface.append(current_part)
        self._parsed = True
        init_present = False
        tokens.match_lookahead(TokenKind.Identifier, 'implementation')
        logger.info("Parsed unit header: %s (%s)", self._file.filename, self._name)
        ## if there is a uses clause - read it
        #if (tokens.match_lookahead(TokenKind.Identifier, 'uses')):
        #    uses_clause = PascalUsesClause(self._file)
        #    uses_clause.parse(tokens, do_resolve=False)
        #while (True):
        #    if (tokens.match_lookahead(TokenKind.Identifier, 'procedure') or tokens.match_lookahead(TokenKind.Identifier, 'function')):
        #        current_part = PascalFunction(self, tokens, func_pointer=False)
        #        for func in self._function_forward_declarations:
        #            if (current_part.name == func.name):            # replace with method fingerprint
        #                current_part = func
        #                self._function_forward_declarations.remove(func)
        #                self._functions[func.name] = func
        #                func.parse(tokens, is_forward=False)
        #        self._functions[current_part.name] = current_part
        #    elif tokens.match_lookahead(TokenKind.Identifier, 'end'):
        #        break
        #    elif tokens.match_lookahead(TokenKind.Identifier, 'begin') or tokens.match_lookahead(TokenKind.Identifier, 'initialization'):
        #        init_present = True
        #        break
        #    else:
        #        logger.error('Unknown unit token...' + str(tokens.next_token()))
        #        assert False
        #    self._implementation.append(current_part)
        #if (len(self._function_forward_declarations)) > 0:
        #    logger.error("Unable to resolve forward function declarations: ", self._function_forward_declarations)
        #    assert False

        ## read initialization?
        #if (init_present):
        #    tokens.match_token(TokenKind.Identifier)
        #    _parse_compound_statement(tokens, None)
        #    if tokens.match_lookahead(TokenKind.Identifier, 'finalization'):
        #        _parse_compound_statement(tokens, None)
        #tokens.match_token(TokenKind.Identifier, 'end')
            
  

