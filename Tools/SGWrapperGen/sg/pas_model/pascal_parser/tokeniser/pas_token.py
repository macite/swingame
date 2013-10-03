class Token(object):
    """
    The token stores a kind-value pair that describes a word in Pascal
    It also stores details about the word such as line position.
    """

    def __init__(self, kind, value, lineNumber, columnNumber, file):
        self._kind = kind
        self._value = value
        self._lineNumber = lineNumber
        self._lineColumn = columnNumber
        self._file = file

    def __str__ (self):
        return self._file + " : (" + str(self._kind) + ' : "' + str(self._value) + '") at ' + str(self._lineNumber) + ',' + str(self._lineColumn)

    @property
    def kind(self):
        return self._kind

    @property
    def filename(self):
        return self._file

    @property
    def value(self):
        return self._value

    @property
    def line(self):
        return self._lineNumber

    @property
    def column(self):
        return self._lineColumn