from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import raise_error, logger, parse_statement
from pas_var import PascalVariable

class PascalFunction(object):
    """
    PascalFunction is a container for a procedure or function in Pascal
    """   
    def __init__(self, parent, tokens, func_pointer = False):
        """
        parent is the current block the function is 
        being initialised from... current block is the function's parent
        """
        from pas_block import PascalBlock
        from pas_param_declaration import PascalParameterDeclaration

        self._parent = parent                 #parent block
        self._result = None    # PascalVariable
        self._parameters = PascalParameterDeclaration(self._parent)
        self._block = PascalBlock(parent)
        self._name = ''
        self._return_type = None    #PascalType
        self._func_pointer = func_pointer
        self._isFunction = False
        self._code = dict()
        self._comments = tokens.get_comments()
        self._tokens = tokens
        self._is_forward = False
        self._modifiers = list()
        self._possible_modifiers = ['cdecl', 'overload', 'forward']

        # function / procedure
        if (tokens.match_lookahead(TokenKind.Identifier, 'function')):
            tokens.match_token(TokenKind.Identifier, 'function')
            self._isFunction = True
        else:
            tokens.match_token(TokenKind.Identifier, 'procedure')
            
        # name
        if not func_pointer:
            self._name = tokens.match_token(TokenKind.Identifier).value
        else:
            self._name = 'function' if self._isFunction else 'procedure'
        
        # ( parameter declaration )
        
        self._parameters.parse(tokens, self)


    @property
    def code(self):
        return self._code
    @property
    def name(self):
        return self._name

    @property
    def method_fingerprint(self):
        result = ' '
        for var in self._parameters.variables:
            result += var.type.name + ' '
        return self._name + "(" + result + ")"

    @property
    def parameters(self):
        return self._parameters

    def add_parameter(self, variable):
        self._block._variables[variable.name] = variable

    @property
    def return_type(self):
        return self._return_type

    @property
    def isFunction(self):
        return self._isFunction

    @property
    def parent(self):
        return self._parent

    @property
    def block(self):
        return self._block

    @property
    def result(self):
        return self._result

    @property
    def kind(self):
        return 'function'

    def parse(self, tokens, is_forward=False):
        from pas_parser_utils import parse_type
        self._result = None
        self._return_type = None

        if (self._isFunction):
            # read return type
            # : type
            self._tokens.match_token(TokenKind.Symbol, ':')
            type = parse_type(self._tokens, self._block)
            self._return_type = type
            self._block._variables['result'] = PascalVariable('result', type)
            self._result = self._block.resolve_variable('result')

        # modifiers '; modifier; modifier...; 
        while True:
            if tokens.lookahead(2)[1].value.lower() in self._possible_modifiers:
                self._tokens.match_token(TokenKind.Symbol, ';')
                if (self._tokens.match_lookahead(TokenKind.Identifier, 'forward')):
                    self._is_forward = True
                    is_forward = True
                self._modifiers.append(self._tokens.match_token(TokenKind.Identifier).value.lower())
            else:
                break;
        if not self._func_pointer:
            self._tokens.match_token(TokenKind.Symbol, ';')
        if not is_forward:
            self._block.parse(tokens)
        
    def to_code(self, indentation):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper 

        my_data = dict()

        my_data['pas_lib_identifier'] = self._name 
        if (self._isFunction):
            my_data['pas_lib_type'] = self._return_type.name
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        self._parameters.to_code()
        self._block.to_code(indentation)

        for (name, module) in converter_helper.converters.items():
            # types need to be evaluated in the loop because they are module specific
            comments = ''
            code = """"""
            for current_line in self._comments:
                comments += module.comment_template % { "comment" : current_line}
            my_data[name + '_comments'] = comments
            my_data[name + '_parameters'] = self._parameters.code[name]
            # if the function isn't a forward declaration then it doesn't have a block
            if (not self._is_forward):
                my_data[name + '_block'] = self._block.code[name]
            else:
                my_data[name + '_block'] = ''

            if (self._isFunction):
                my_data[name + '_type'] = converter_helper.convert_type(module._type_switcher, self._return_type)
                code = module.function_declaration_template % my_data
            else:
                code = module.procedure_declaration_template % my_data

            self._code[name] = converter_helper.apply_indents(code, indentation)