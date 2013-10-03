from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import raise_error, logger
from pascal_parser.pas_unit_reference import PascalUnitReference

class PascalUsesClause(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self, file_owner):
        self._units = list()
        self._code = dict()
        self._file_owner = file_owner

    @property
    def code(self):
        return self._code

    @property
    def units(self):
        return self._units

    @property
    def kind(self):
        return 'uses_clause'

    def parse(self, tokens, do_resolve=True):
        logger.debug("Parsing uses clause")
        tokens.match_token(TokenKind.Identifier, 'uses')
        while (True):
            if (tokens.match_lookahead(TokenKind.Symbol, ';')):
                tokens.match_token(TokenKind.Symbol, ';')
                break
            elif (tokens.match_lookahead(TokenKind.Symbol, ',')):
                tokens.match_token(TokenKind.Symbol, ',')

            elif (tokens.match_lookahead(TokenKind.Identifier)):
                new_reference = PascalUnitReference()
                new_reference.parse(tokens, self._file_owner, do_resolve)
                self._units.append(new_reference)
            else:
                raise_error(('Error reading uses clause: ' + str(tokens.next_token())), '', is_critical=False)
        logger.debug("Finished parsing uses clause")

    def to_code(self):
        import converter_helper
        for reference in self._units:
            reference.to_code()

        my_data = dict()
        my_data["c_lib_seperator"] = ''
        my_data["pas_lib_seperator"] = ","

        for (name, module) in converter_helper.converters.items():
            unit_references = ""
            for index in range(len(self._units)):

                unit_references += self._units[index].code[name]

                if index < (len(self._units)-1):
                    unit_references += my_data[name + '_seperator']
            self._code[name] = module.uses_clause_template % {"references" : unit_references}


                




