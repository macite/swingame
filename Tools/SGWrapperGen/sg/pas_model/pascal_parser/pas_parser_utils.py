from tokeniser.pas_token_kind import TokenKind

import logging

reservedWords = [   (TokenKind.Identifier, 'begin'),      (TokenKind.Identifier, 'const'), 
                    (TokenKind.Identifier, 'type'),       (TokenKind.Identifier, 'var'), 
                    (TokenKind.Identifier, 'procedure'),  (TokenKind.Identifier, 'function'),
                    (TokenKind.Identifier, 'end'),        (TokenKind.Identifier, 'implementation')]

assignmentOperators = [(TokenKind.Operator, '+='),  (TokenKind.Operator, '-='), 
                        (TokenKind.Operator, '/='),  (TokenKind.Operator, '*='), 
                        (TokenKind.Operator, ':=')]

operators   =   [   (TokenKind.Operator, '+'), (TokenKind.Operator, '-'), (TokenKind.Operator, '/'), (TokenKind.Operator, '*'),
                    (TokenKind.Operator, '<'), (TokenKind.Operator, '>'), (TokenKind.Operator, '<>'), (TokenKind.Operator, '=')]   


# Logger object is used to log events to console
logger = logging.getLogger("sgLogger")

def raise_error(logger_msg, exception_msg, is_critical=False):
    """
    This method will log a message to the screen.
    It will also raise an exception.
     - logger_msg : the message to be passed to the logger
     - is_critical : if True the parser is stopped
     - exception_msg : the message to be displayed in the exception
    """
    logger.error(logger_msg)
    if is_critical:
        assert False
    else:
        raise Exception(exception_msg)

def parse_type(tokens, block):
    """
    parses the type then checks to see if the type already exists in the cache,
    if it already exists the old type is returned, otherwise the new type
    is added to the cache and returned.
    """
    from types import pas_array, pas_pointer, pas_type, pas_type_cache
    new_type = None
    # array...
    if tokens.match_lookahead(TokenKind.Identifier, 'array'):
        new_type = pas_array.PascalArray(block)
        new_type.parse(tokens)
    # pointer to type or function/procedure...
    elif tokens.match_lookahead(TokenKind.Symbol, '^') or tokens.match_lookahead(TokenKind.Identifier, 'procedure') or tokens.match_lookahead(TokenKind.Identifier, 'function'):
        new_type = pas_pointer.PascalPointer(block)
        new_type.parse(tokens)
    # already declared type...
    # every composite type will consist of a previously declared type...
    elif tokens.match_lookahead(TokenKind.Identifier):
        new_type = block.resolve_type(tokens.match_token(TokenKind.Identifier).value)
    else:
        raise_error(("Unknown type: ", tokens.next_token()), '', is_critical=False)

    if (new_type is None):
        tok = tokens.next_token()
        raise_error(("Unable to resolve type: %s", tok), '', is_critical=False)
    return new_type
          
def token_has_values(token, list_values):
    """
    This method searches through all the values in a list and returns true
    if the token has a value equivalent to an element in the list.
    """
    for value in list_values:
        if value[1] == token.value:     # value is a tuple, where the second element is a string value
            if value[0] == token.kind:
                return True
    return False

def parse_statement(tokens, block):
    """
    parses a statement and returns the class describing the statement
    can parse compound statements.
    """
    result = None
    if tokens.match_lookahead(TokenKind.Identifier, 'begin'):
        result = _parse_compound_statement(tokens, block)
    elif tokens.match_lookahead(TokenKind.Identifier, 'if'):
        result = _parse_if_statement(tokens, block)
    elif tokens.match_lookahead(TokenKind.Identifier, 'case'):
        result = _parse_case_statement(tokens, block)
    elif tokens.match_lookahead(TokenKind.Identifier, 'while'):
        result = _parse_while_statement(tokens, block)
    elif tokens.match_lookahead(TokenKind.Identifier, 'repeat'):
        result = _parse_repeat_statement(tokens, block)
    elif tokens.match_lookahead(TokenKind.Identifier):
        # function/procedure call
        if (tokens.lookahead(2)[1].value == '('):
            result = _parse_procedure_call_statement(tokens, block)
        # { variable } ( '+=', '-=', '/=', '*=' ) { variable }
        # assignment statement
        elif _is_assignment_statement(tokens):
            result = _parse_assignment_statement(tokens, block)
        # something else?
        else:
            raise_error(("Unknown statement : " + str(tokens.next_token())), '', is_critical=False)
    else:
        raise_error(("Unrecognised token: " + str(tokens.next_token())), '', is_critical=False)
    return result

# Checks to see if the statement is an assignment statement...
# only checks for a depth of 1...
# identifier.identifier 'operator'
# identifier[ ] 'operator'
# identifier 'operator'         
def _is_assignment_statement(tokens, current_index=1):
    current_idx = current_index
    # identifier 'operator'
    if token_has_values(tokens.lookahead(current_idx + 1)[current_idx], assignmentOperators):
        return True
    # identifier.identifier 'operator'
    elif ((tokens.lookahead(current_idx+1)[current_idx].value == '.') and (tokens.lookahead(current_idx+2)[current_idx+1].kind is TokenKind.Identifier)):
        return _is_assignment_statement(tokens, current_idx + 2)    # move past . symbol to the next identifier
        #identifier [ ] 'operator
    elif (tokens.lookahead(current_idx+1)[current_idx].value == '['):
        # move to the end of the expression...
        while tokens.lookahead(current_idx+1)[current_idx].value != ']':
            current_idx += 1
        return _is_assignment_statement(tokens, current_idx+1)  # move past the array dereference to the next symbol
    else:
        raise_error(("Error in Assignment Statement: %s..." % tokens.lookahead(current_idx+1)[current_idx]), '', is_critical=False)

def _parse_compound_statement(tokens, block):
    from pas_compound_statement import PascalCompoundStatement 

    result = PascalCompoundStatement(block)
    result.parse(tokens)
    return result

def _parse_if_statement(tokens, block):
    from pas_if_statement import PascalIfStatement 

    result = PascalIfStatement(block)
    result.parse(tokens)
    return result

def _parse_while_statement(tokens, block):
    from pas_while_statement import PascalWhileStatement 

    result = PascalWhileStatement(block)
    result.parse(tokens)
    return result

def _parse_repeat_statement(tokens, block):
    from pas_repeat_statement import PascalRepeatStatement 

    result = PascalRepeatStatement(block)
    result.parse(tokens)
    return result

def _parse_assignment_statement(tokens, block):
    from pas_assignment_statement import AssignmentStatement

    result = AssignmentStatement(block)
    result.parse(tokens)
    return result

def _parse_case_statement(tokens, block):
    from pas_case_statement import PascalCaseStatement

    result = PascalCaseStatement(block)
    result.parse(tokens)
    return result

def _parse_procedure_call_statement(tokens, block):
    from pas_function_call import PascalFunctionCall

    result = PascalFunctionCall(block, inExpr = False)
    result.parse(tokens)
    return result

def _parse_identifier_list(tokens):
    """
    This method parses a Pascal identifier list, it returns a list of
    variable identifiers
    """
    # { identifier }+(',')
    names = list()
    while True:
        nameTok = tokens.match_token(TokenKind.Identifier)
        names.append(nameTok._value)
        if not tokens.match_lookahead(TokenKind.Symbol, ',', consume=True):
            return names

def _parse_variable_declaration(tokens):
    from pas_var_declaration import PascalVarDeclaration
    result = PascalVarDeclaration()
    result.parse
    return result


    