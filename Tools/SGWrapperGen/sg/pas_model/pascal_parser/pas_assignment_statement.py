from tokeniser.pas_token_kind import TokenKind
from pas_expression import PascalExpression
from types.pas_operator import PascalOperator
from pas_var_reference import PascalVariableReference
from pascal_parser.pas_parser_utils import raise_error, logger
class AssignmentStatement(object):
    """
    The assignment statement stores the information about an assignment statement
    """

    def __init__(self, block):
        self._operand = None
        self._operator = None       # += *= /= -= := token
        self._expression = None
        self._block = block
        self._code = dict()

    @property
    def code(self):
        return self._code

    def parse(self, tokens):
        varName = ''

        self._operand = PascalVariableReference(self._block)
        self._operand.parse(tokens)

        operatorValue = tokens.match_token(TokenKind.Operator).value
        self._operator = PascalOperator(operatorValue)

        self._expression = PascalExpression(self._block)
        self._expression.parse(tokens)

    @property
    def kind(self):
        return 'assignment'

    @property
    def operand(self):
        return self._operand

    @property
    def operator(self):
        return self._operator

    @property
    def expression(self):
        return self._expression

    @property
    def block(self):
        return self._block

    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        my_data = dict()
        self._operand.to_code()
        #my_data['pas_lib_operand'] = self._operand.code['pas_lib_reference']
        #my_data['c_lib_operand'] = converter_helper.lower_name(self.operand.name)

        self._operator.to_code()
        self._expression.to_code()

        for (name, module) in converter_helper.converters.items():
            # operator / expression
            my_data[name + '_operand'] = self._operand.code[name + '_reference']
            my_data[name + '_expression'] = self._expression.code[name]
            my_data[name + '_operator'] = self._operator.code[name]
            self._code[name] = module.assignment_template % my_data

            self._code[name] = converter_helper.apply_indents(self._code[name], module.indenter['statement'])
            