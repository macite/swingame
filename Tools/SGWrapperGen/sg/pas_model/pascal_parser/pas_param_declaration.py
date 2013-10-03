from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import raise_error, logger


class PascalParameterDeclaration(object):
    """
    The PascalVarDeclaration describes a variable declaration in Pascal
    It stores the identifier-type pairs in the declaration
    """

    # var
    # 'identifier' : 'type' ;

    
    def __init__(self, parent):
        # vars stores the variables declared in this declaration in a dictionary
        # name of the variable is the key, type is the value
        self._block = parent
        self._vars = list()
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def kind(self):
        return 'parameter declaration'

    @property
    def variables(self):
        return self._vars

    def parse(self, tokens, method):
        """Read in the parameters declared in a function, procedure or operator"""
        from pas_parser_utils import _parse_identifier_list, reservedWords, parse_type
        from pas_var import PascalVariable
        from types.pas_type import PascalType

        tokens.match_token(TokenKind.Symbol, '(')
        #look for parameters
        while not tokens.match_lookahead(TokenKind.Symbol, ')'): #consume ) at end
            param_tok, other_tok = tokens.lookahead(2)
            modifier = None
            #Look for modifier
            if param_tok.kind == TokenKind.Identifier:
                # First value is modifier
                if other_tok.kind == TokenKind.Identifier: 
                    modifier = tokens.match_token(TokenKind.Identifier).value
                # No modifier found
                else:
                    modifier = None

            # get parameter names
            parameters = [tokens.match_token(TokenKind.Identifier).value]
            while tokens.match_lookahead(TokenKind.Symbol, ',', True):
                #there is a list of parameters
                parameters.append(tokens.match_token(TokenKind.Identifier).value)
            
            # colon seperates identifiers and type
            tokens.match_token(TokenKind.Symbol, ':')
            the_type = parse_type(tokens, self._block)   # reads the type and returns PascalType
            
            for parameter_name in parameters:
                toAdd = PascalVariable(parameter_name, the_type, modifier, is_parameter=True)
                self._vars.append(toAdd)
                method.add_parameter(toAdd)
                logger.debug('Parser    : Adding parameter %s (%s) to %s', parameter_name, the_type, method.name)

            tokens.match_lookahead(TokenKind.Symbol, ';', consume=True) # is there a semi-colon seperator? eat it.
            
        tokens.match_token(TokenKind.Symbol, ')')
                
    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper

        for param in self._vars:
            param.to_code()
        my_data = dict()
        # seperators need to be set for every new package
        #   these could go in the __init__ for each library?
        my_data['c_lib_seperator'] = ','
        my_data['pas_lib_seperator'] = ';'

        for (name, module) in converter_helper.converters.items():
            parameters = ""
            for index in range(len(self._vars)):
                if self._vars[index].type.kind is 'array':
                    parameters += module.convert_array_declaration(self._vars[index], is_parameter=True)
                else:
                    var_data = dict()
                    var_data['identifier'] = self._vars[index].code[name + '_reference']
                    var_data['type'] = converter_helper.convert_type(module._type_switcher, self._vars[index].type, self._vars[index]._modifier)
                    parameters += module.parameter_template % (var_data)

                if index < (len(self._vars)-1):
                    parameters += my_data[name + '_seperator']
            self._code[name] = parameters