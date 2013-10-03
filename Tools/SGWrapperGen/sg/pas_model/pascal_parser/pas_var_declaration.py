from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import raise_error, logger


class PascalVarDeclaration(object):
    """
    The PascalVarDeclaration describes a variable declaration in Pascal
    It stores the identifier-type pairs in the declaration
    """

    # var
    # 'identifier' : 'type' ;

    
    def __init__(self, block):
        # vars stores the variables declared in this declaration in a dictionary
        # name of the variable is the key, type is the value
        self._vars = dict()
        self._code = dict()
        self._block = block

    @property
    def code(self):
        return self._code

    @property
    def kind(self):
        return 'variable_declaration'

    @property
    def variables(self):
        return self._vars

    def parse(self, tokens):
        from pas_parser_utils import parse_type
        from pas_parser_utils import _parse_identifier_list, reservedWords
        from pas_var import PascalVariable

        paramDeclaration = False
        logger.debug("Parsing variable declaration")
        
        tokens.match_token(TokenKind.Identifier, 'var')

        variables = dict()
        while True:
            modifier = None
            # (modifier) identifier list, ':', type, ';'

            idList = _parse_identifier_list(tokens)
            tokens.match_token(TokenKind.Symbol, ':')
            type = parse_type(tokens, self._block)
                # assign value at creation... consume value, expression?
                #PascalExpression(self._block).parse(tokens)
            if tokens.match_lookahead(TokenKind.Operator, '=', consume=True):
                tokens.next_token()   # $ consume the assigned value... not needed for now...
                tokens.next_token()   # number...
            tokens.match_lookahead(TokenKind.Symbol, ';', consume=True)

            for varName in idList:
                if not varName in self._vars:
                    self._vars[varName] = PascalVariable(varName, type, modifier)  # create and assign the PascalVariable
                    logger.debug("Parsed variable : " + varName + " : " + type.name)
                else:
                    logger.error("Duplicate variable identifier found: " + str(tokens.next_token()))
                    assert(False)

            if tokens.match_one_lookahead(reservedWords) or tokens.match_lookahead(TokenKind.Symbol, ')'):
                logger.debug("Finished parsing variable declaration")
                break

    def to_code(self, indentation=0):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper

        for (key, variable) in self._vars.items():
            variable.to_code()

        for (name, module) in converter_helper.converters.items():
            variables = ""
            for (key, variable) in self._vars.items():
                variables += variable.code[name]
            variables = converter_helper.apply_indents(variables, module.indenter['variable'])
            self._code[name] = module.variable_decl_template % { "variables": variables }
           
