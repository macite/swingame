from tokeniser.pas_token_kind import TokenKind
from pas_parser_utils import logger, raise_error

class PascalUnitReference(object):
    """
    The model object to represent a reference to a unit:
    """
    
    def __init__(self):
        self._name = None
        self._points_to = None  # PascalUnit
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def name(self):
        return self._name

    @property
    def points_to(self):
        return self._points_to
    
    def parse(self, tokens, file_owner, do_resolve=True):
        """
        Parses the entire pascal program
        expects: 'program name;' at the start
        """
        self._name = tokens.match_token(TokenKind.Identifier).value;
        if do_resolve:
            self._points_to = file_owner.resolve_unit_reference(self)
            if self._points_to is None:
                raise_error(("Unable to resolve unit reference: %s in %s" % (self._name, file_owner.name)), '', is_critical=False)

    def to_code(self):
        import converter_helper
        my_data = dict()
        my_data['pas_lib_identifier'] = self._name 
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        for (name, module) in converter_helper.converters.items():
            self._code[name] = module.unit_reference_template % my_data