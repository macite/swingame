from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import raise_error, logger
class PascalRecord(object):
    """
    Describes a record in pascal
    """
    
    def __init__(self, name, block):
        self._name = name
        self._fields = list()   # list of tuples
        self._code = dict()
        self._block = block

    @property
    def code(self):
        return self._code
        
    def set_name(self, name):
        self._name = name

    @property
    def kind(self):
        return 'record'

    @property
    def name(self):
        return self._name

    def has_field(self, name):
        for field in self._fields:
            if field.name == name:
                return True
        return False

    def get_field(self, name):
        for field in self._fields:
            if field.name == name:
                return field
        return None

    def parse(self, tokens):
        from pascal_parser.types.pas_record_field import PascalRecordField
        from pascal_parser.pas_parser_utils import _parse_identifier_list, parse_type
        while not tokens.match_lookahead(TokenKind.Identifier, 'end', consume=True):
            modifier = None
            # (modifier) identifier list, ':', type, ';'

            idList = _parse_identifier_list(tokens)
            tokens.match_token(TokenKind.Symbol, ':')
            type = parse_type(tokens, self._block)

            tokens.match_token(TokenKind.Symbol, ';')

            for varName in idList:
                if not varName in self._fields:
                    field = PascalRecordField(varName, type, self)
                    self._fields.append(field)
                    logger.debug("Parsed field : " + varName + " : " + type.name)

    def to_code(self, indentation=0):
        import converter_helper

        my_data = dict()

        my_data['pas_lib_identifier'] = self._name 
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        for field in self._fields:
            field.to_code()

        for (name, module) in converter_helper.converters.items():
            my_data["fields"] = ""
            for field in self._fields:
                my_data["fields"] += field.code[name] + '\n'
            my_data["fields"] = converter_helper.apply_indents(my_data['fields'], module.indenter["record_field"])
            
            self._code[name] = module.record_template % my_data