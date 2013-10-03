class TokenKind:
    """
    Enumeration-type object that stores the different kinds of tokens that are
    available for use.

    Number,         # such as 1234, 123.45, -123, +123.4
    Comment,        # single // or multi-line (* ... *), { ... } 
    MetaComment,   # start with /// ...
    Identifier      # identifier name starting with alpha, including 
                    # alpha-numeric characters and the _ character
    Attribute,      # name starting with @... inside meta comment block 
                    # follows the id name character rules
    Operator,       # one of + - / * ** := < > <= >= <>
    Symbol,         # one of ':;,.()'
    Boolean,        # either true or false
    String          # alpha-numeric characters enclosed in single quotes
    """

    Identifier = 'Identifier'
    Number = 'Number'
    Comment = 'Comment'
    MetaComment = 'Meta Comment'
    Attribute = 'Attribute'
    Operator = 'Operator'
    Symbol = 'Symbol'
    Boolean = 'Boolean'
    String = 'String'



