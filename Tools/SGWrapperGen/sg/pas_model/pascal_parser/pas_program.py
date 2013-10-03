from tokeniser.pas_token_kind import TokenKind
from pas_block import PascalBlock
from pas_uses_clause import PascalUsesClause

class PascalProgram(object):
    """The model object to represent a pascal program:
    
    Syntax for a program is:
        program = "program", identifier, ";", [uses clause], block, "." ;
        block = "begin", { statement }+(";"), "end" ;
    """
    
    def __init__(self, file):
        self._file = file
        self._name = None
        self._uses = None
        self._block = None  # program block
        self._code = dict()
        self._meta_comment = None

    @property
    def code(self):
        return self._code

    @property
    def uses(self):
        return self._uses.units

    @property
    def name(self):
        return self._name

    @property
    def block(self):
        return self._block
    
    def resolve_function_call(self, function):
        # for all the units I have included
        for unit in self._uses._units:
            # check all the functions that are declared in the unit
            result = unit.points_to.contents.resolve_function_call(function)
            if result != None:
                return result
            #for (key, declared_function) in unit.points_to.contents.functions.items():
            #    if (function.name == declared_function.name):
            #        return declared_function
        return None

    def resolve_variable(self, var):
        for unit in self._uses._units:
            # check all the functions that are declared in the unit
            for (key, variable) in unit.points_to.contents.variables.items():
                if (var == variable.name):
                    return variable
        return None

    def parse(self, tokens):
        """
        Parses the entire pascal program
        expects: 'program name;' at the start
        """
        tokens.match_token(TokenKind.Identifier, 'program');
        self._name = tokens.match_token(TokenKind.Identifier)._value;
        tokens.match_token(TokenKind.Symbol, ';')
        if (tokens.match_lookahead(TokenKind.Identifier, 'uses')):
            self._uses = PascalUsesClause(self._file)
            self._uses.parse(tokens)
        # Read block
        self._block = PascalBlock(None, self._file)
        self._block.parse(tokens)

    def to_code(self):
        import converter_helper
        if (self._uses != None):
            self._uses.to_code()
        self._block.to_code()

        for (name, module) in converter_helper.converters.items():
            my_data = dict()
            if (self._uses != None):
                my_data[name +'_uses'] = self._uses.code[name]

            my_data[name + '_name'] = self._name
            my_data[name + '_block'] = self._block.code[name]
            
            self._code[name] = module.program_template % my_data
      

    