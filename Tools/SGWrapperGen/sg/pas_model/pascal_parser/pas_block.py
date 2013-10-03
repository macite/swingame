from pascal_parser.tokeniser.pas_token_kind import TokenKind

from pascal_parser.pas_parser_utils import raise_error, logger, parse_statement

from pas_var_declaration import PascalVarDeclaration 
from pas_function import PascalFunction
from pas_type_declaration import PascalTypeDeclaration
from pas_compound_statement import PascalCompoundStatement

class PascalBlock(object):
    """
    The PascalBlock object stores the entire pascal block
    """

    #declaration part
        #constants
        #type
        #var
        #procedure/function
    #statement part

    
    def __init__(self, parent, file=None):
        self._parent = parent                 #parent block
        self._compound_statement = None
        # list with block contents (in order)
        self._contents = list()
        self._variables = dict()
        self._functions = dict()
        self._types = dict()
        self._code = dict()     
        self._file = file

    @property
    def code(self):
        return self._code

    @property
    def parent(self):
        return self._parent

    @property
    def compound_statement(self):
        return self._compound_statement

    @property
    def contents(self):
        return self._contents

    @property
    def variables(self):
        return self._variables

    def parse(self, tokens):
        # while the next token is not begin (start of compound statement), scan the block for declarations
        while (True):
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
                self._functions[current_part.name] = current_part
                current_part.parse(tokens)
            elif (tokens.match_lookahead(TokenKind.Identifier, 'type')):
                current_part = PascalTypeDeclaration(self)
                current_part.parse(tokens)
                self._types.update(current_part.types)
            elif tokens.match_lookahead(TokenKind.Identifier, 'begin'):
                break
            else:
                raise_error(('Unknown block token...' + str(tokens.next_token())), '', is_critical=False)
            self._contents.append(current_part)
        # at this point we must be at a begin
        
        self._compound_statement = PascalCompoundStatement(self, True)
        self._compound_statement.parse(tokens)
                                                   
        tokens.match_token(TokenKind.Symbol)

    def resolve_variable(self, var_name):
        #check myself
        for (name, declared_variable) in self._variables.items():
            if (var_name.lower() == declared_variable.name.lower()):
                return declared_variable
        # check parent
        if self.parent != None:
            # recursive call for all parents
            # will always return the answer if it exists
            return self.parent.resolve_variable(var_name)
         
        # finally, it could be an enumeration value...
        for (name, type) in self._types.items():
            if type.kind is 'enumeration':
                for val in type.values:
                    if val.name.lower() == var_name.lower():
                        return val

        return self._file.resolve_variable(var_name)

    def resolve_function_call(self, function):
        #check myself
        for (name, declared_function) in self._functions.items():
            if (function.name.lower() == declared_function.name.lower()):
                return declared_function
        # check parent
        if self.parent != None:
            # recursive call for all parents
            # will always return the answer if it exists
            return self.parent.resolve_function_call(function) 
        return self._file.resolve_function_call(function)

    def resolve_type(self, type_name):
        #check myself
        for (name, type) in self._types.items():
            if (type_name.lower() == name.lower()):
                return type
        # check parent
        if self.parent != None:
            # recursive call for all parents
            # will always return the answer if it exists
            return self.parent.resolve_type(type_name) 
        return self._file.resolve_type(type_name)


    def to_code(self, indentation=0):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        
        for part in self._contents:
            part.to_code(indentation)

        if (self._compound_statement != None):
            self._compound_statement.to_code()

        for (name, module) in converter_helper.converters.items():
            part_code = ""
            lang_data = dict()
            for part in self._contents:
                part_code += part.code[name]
            lang_data['declarations'] = part_code;

            if (self._compound_statement is None):
                lang_data['statement'] = ''
            else:
                lang_data['statement'] = self._compound_statement.code[name]

            self._code[name] = module.block_template % lang_data
            self._code[name] = converter_helper.apply_indents(self._code[name], indentation)