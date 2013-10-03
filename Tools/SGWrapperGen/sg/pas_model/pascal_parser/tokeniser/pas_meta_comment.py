from pascal_parser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import raise_error, logger
class PascalMetaComment(object):
    """
    
    """
    def __init__(self, tokens):
        self._attributes = dict()
        self._comments = list()
        self._tokens = tokens

        self._processors = {
            TokenKind.Attribute     : self.process_attribute,
            TokenKind.Comment       : self.process_comment,
            TokenKind.MetaComment   : self.process_meta_comment
        }

        self._attribute_processors = {
            'param'                     : self.process_id_list_and_comment_attribute,
            'class'                     : self.process_id_attribute,
            'type'                      : self.process_id_attribute,
            'enum'                      : self.process_id_attribute,
            'module'                    : self.process_id_attribute,
            'header'                    : self.process_id_attribute,
            'static'                    : self.process_true_attribute,
            'struct'                    : self.process_id_attribute,
            'lib'                       : self.process_lib_attribute, 
            'note'                      : self.process_note_attribute,
            'uname'                     : self.process_id_attribute,
            'sn'                        : self.process_line_attribute,
            'csn'                       : self.process_line_attribute,
            'field'                     : self.process_field_attribute,
            'constructor'               : self.process_true_attribute,
            'dispose'                   : self.process_true_attribute,
            'method'                    : self.process_method_attribute, #TODO: implement
            'overload'                  : self.process_overload_attribute,
            'version'                   : self.process_number_attribute,
            'setter'                    : self.process_id_attribute,
            'getter'                    : self.process_id_attribute,
            'returns'                   : self.process_comment_attribute,
            #'swingame'                  : self.process_swingame_attribute,
            'pointer_wrapper'           : self.process_true_attribute,
            'no_free_pointer_wrapper'   : self.process_true_attribute,
            'data_wrapper'              : self.process_true_attribute,
            'array_wrapper'             : self.process_true_attribute,
            'fixed_array_wrapper'       : self.process_true_attribute,
            'ignore'                    : self.process_true_attribute,
            'self'                      : self.process_number_attribute,
            'see'                       : self.process_idlist_attribute,
            'like'                      : self.process_id_attribute,
            'via_pointer'               : self.process_true_attribute,
            'fixed_result_size'         : self.process_number_attribute,
            'sameas'                    : self.process_type_attribute,
            'calls'                     : self.process_id_attribute,
            'length'                    : self.process_id_attribute,
            'updatesArrayParam'         : self.process_numbers_attribute,
            'doc_group'                 : self.process_id_attribute,
            'doc_types'                 : self.process_idlist_attribute,
            'doc_idx'                   : self.process_number_attribute,                       # Allows items to be ordered in the documentation
            'doc_details'               : self.process_true_attribute,                     # Moves documentation to a details file
        }

    def process_meta_comments(self):
        logger.debug('Parser    : Starting to process meta comments: clearing old comments and attributes')
        tok = self._tokens.lookahead(1)[0] #_next_token()
        
        attrs_started = False
        while tok.kind in [TokenKind.MetaComment, TokenKind.Attribute, TokenKind.Comment]:
            if tok.kind == TokenKind.Attribute: 
                attrs_started = True
            if attrs_started and tok.kind == TokenKind.MetaComment:
                tok = self._tokens.next_token() #actually read token
                if len(tok.value) > 0:
                    logger.error('Parser Error %s: Found additional meta comment after start of attributes', 
                        tok)
                    assert False
            else:
                tok = self._tokens.next_token() #actually read token
                self._processors[tok.kind](tok)
            tok = self._tokens.lookahead(1)[0]

    #def _read_to_eol(self):
    #    current_line = self._tokens.lookahead(1)[0].line
    #    result = ''
    #    while current_line == self._tokens.lookahead(1)[0].line:
    #        result += self._tokens.next_token().value
    #    return result

    #def _read_comment(self):
    #    result = ''
    #    current_token = self._tokens.next_token()
    #    current_line = current_token.line
    #    last_line = current_line
    #    while (True):            

    #        if (current_token.kind is TokenKind.Attribute):
    #            break

    #        if (current_line - last_line > 0):
    #            if (current_token.kind != TokenKind.MetaComment):
    #                break

    #        result += current_token.value
    #        
    #        current_token = self._tokens.next_token()
    #        last_line = current_line
    #        current_line = current_token.line
    #    return result

    def process_attribute(self, token):
        logger.debug('Parser    : Processing attribute: %s', token.value)
        self._attribute_processors[token.value](token)

    def process_comment(self, token):
        logger.log(logging.DEBUG - 1, 'Parser    : Processing comment: %s', token[1])
    
    def process_comment_attribute(self, token):
        '''read the id and attach comments etc as attributes'''
        doc_tok = self._tokens.read_to_end_of_comment()
        self._add_attribute(token.value, doc_tok)

    def _add_attribute(self, attr, val):
        logger.debug('Parser    : Adding attribute %s with value %s',attr,val)
        if attr in self._attributes and self._attributes[attr] != None:
            logger.warning('Parser    : Added attribute twice %s = %s', attr, val)
        self._attributes[attr] = val

    def _append_attribute(self, attr, val):
        logger.debug('Parser    : Appending attribute %s with value %s to %s',attr,val, attr + 's')
        if (attr + 's') in self._attributes.keys():
            self._attributes[attr + 's'].append(val)
        else:
            self._attributes[attr + 's'] = [val]

    def process_line_attribute(self, token):
        '''read the remainder of the line for the token'''
        line = self._tokens.read_to_eol()
        self._add_attribute(token.value, line)

    def process_id_id_attribute(self, token):
        '''Process an attribute that is followed by a two identifiers'''
        tok1 = self._tokens.match_token(TokenKind.Identifier) #load id of thing...
        tok2 = self._tokens.match_token(TokenKind.Identifier) #load id of thing...
        self._add_attribute(token.value, [tok1.value, tok2.value])

    def process_id_attribute(self, token):
        '''Process an attribute that is followed by a single identifier'''
        tok = self._tokens.match_token(TokenKind.Identifier) #load id of thing...
        self._add_attribute(token.value, tok.value)

    def process_type_attribute(self, token):
        '''Process an attribute that is followed by a single type identifier'''
        from pascal_parser.pas_parser_utils import parse_type
        the_type = self._tokens.read_to_eol()
        #tok = self._match_token('id') #load id of the type...
        self._add_attribute(token.value, the_type)

    def process_idlist_attribute(self, token):
        '''Process an attribute followed by a list of comma separated identifiers.
        eg. @see id1, id2 ...
        '''
        tok = self._tokens.match_token(TokenKind.Identifier)
        ids = [tok.value]
        while self._tokens.match_lookahead(TokenKind.Symbol, ',', consume=True):
            ids.append(self._tokens.match_token(TokenKind.Identifier).value)
        self._add_attribute(token.value, ids) 

    def process_field_attribute(self, token):
        '''Have encountered @field - create a field for the generated class to use
        '''
        from pascal_parser.pas_parser_utils import parse_type

        tok = self._tokens.match_token(TokenKind.Identifier) #field name
        field_name = tok.value
        
        self._tokens.match_token(TokenKind.Symbol, ':')
        the_type = self._tokens.read_to_eol()
        
        data_type = the_type
        self._append_attribute('field', [field_name, the_type])

    def process_true_attribute(self, token):
        self._add_attribute(token.value, True)
    
    def process_id_list_and_comment_attribute(self, token):
        '''read the id and attach comments etc as attributes'''
        param_toks = list()
        
        param_toks.append(self._tokens.match_token(TokenKind.Identifier).value)
        
        # read ahead for a , between each identifier in the list
        while self._tokens.peek_next_char() == ',':
            self._tokens.match_lookahead(TokenKind.Symbol, ',', consume=True)
            param_toks.append(self._tokens.match_token(TokenKind.Identifier).value)
        
        doc_tok = self._tokens.read_to_end_of_comment() #self._read_comment()
        
        # print 'DOC TOK:', doc_tok
        
        for param_tok in param_toks:
            self._append_attribute(token.value, [param_tok, doc_tok])
            if len(param_toks) > 1:
                # if there is more than one token then also add the list of
                # related tokens
                # print 'related_' + token[1], param_tok, param_toks
                self._append_attribute('related_' + token.value, [param_tok, param_toks])
    
    #def process_lib_attribute(self, token):
    #    '''
    #    process a lib attribute = what to call in the library
    #    
    #    Steps: 
    #    1: find method (and possibly args)
    #    2: find method in library (all names are unique here)
    #    3: read in args
    #    4: add attribute 'calls' to indicate the processed method calls the
    #       method from the library
    #    '''
    #    #find method
    #    if self._match_lookahead(TokenKind.Identifier) and not (self._match_lookahead(TokenKind.Identifier, 'procedure') or self._match_lookahead(TokenKind.Identifier, 'function')):
    #        uname = self._match_token(TokenKind.Identifier).value
    #        
    #        self._setup_lib_method(uname)
    #        
    #        if self._match_lookahead('symbol', '(', True):
    #            self._add_attribute('called_by_lib', False)
    #            #the lib attr has arguments
    #            args = list()
    #            if not self._match_lookahead('symbol', ')'):
    #                while True:
    #                    #args are ids, number, or strings
    #                    args.append(self._match_one_token([('id',None), ('number', None), ('string', None), ('boolean', None)]))
    #                    
    #                    #if not comma following - end args
    #                    if not self._match_lookahead('symbol', ',', True): break;
    #            close_tok = self._match_token('symbol', ')')
    #        else: 
    #            args = None
    #            self._add_attribute('called_by_lib', True)
    #            self._add_attribute('uname', uname)
    #    else:
    #        #No id token... use method name
    #        args = None
    #        method = None
    #        self._add_attribute('called_by_lib', True)
    #        self._add_attribute('method_called', None)
    #    
    #    self._add_attribute('args', args)
    
    def process_note_attribute(self, token):
        self._add_attribute('note', self._tokens.read_to_end_of_comment()) #self._read_comment())
    
    def process_value_attribute(self, token):
        self._add_attribute(token.kind, token.value)
    
    def process_number_attribute(self, token):
        num_tok = self._tokens.match_token(TokenKind.Number)
        self._add_attribute(token.value, eval(num_tok.value))
    
    def process_numbers_attribute(self, token):
        num_tok = self._tokens.match_token(TokenKind.Number)
        self._append_attribute(token.value, eval(num_tok.value))
    
    def process_meta_comment(self, token):
        logger.debug('Parser    : Processing meta comment: %s', token.value)
        self._comments.append(token.value)

    def process_lib_attribute(self, token):
        # TODO: setup properly...
        '''
        process a lib attribute = what to call in the library
        
        Steps: 
        1: find method (and possibly args)
        2: find method in library (all names are unique here)
        3: read in args
        4: add attribute 'calls' to indicate the processed method calls the
           method from the library
        '''
        #find method
        if self._tokens.match_lookahead(TokenKind.Identifier) and not (self._tokens.match_lookahead(TokenKind.Identifier, 'procedure') or self._tokens.match_lookahead(TokenKind.Identifier, 'function')):
            uname = self._tokens.match_token(TokenKind.Identifier).value
            
            #self._setup_lib_method(uname)
            
            if self._tokens.match_lookahead(TokenKind.Symbol, '(', consume=True):
                self._add_attribute('called_by_lib', False)
                #the lib attr has arguments
                args = list()
                if not self._tokens.match_lookahead('symbol', ')'):
                    while True:
                        #args are ids, number, or strings
                        args.append(self._tokens.match_one_token([(TokenKind.Identifier,None), (TokenKind.Number, None), (TokenKind.String, None), (TokenKind.Boolean, None)]))
                        
                        #if not comma following - end args
                        if not self._tokens.match_lookahead(TokenKind.Symbol, ',', consume=True): break;
                close_tok = self._tokens.match_token(TokenKind.Symbol, ')')
            else: 
                args = None
                self._add_attribute('called_by_lib', True)
                self._add_attribute('uname', uname)
        else:
            #No id token... use method name
            args = None
            method = None
            self._add_attribute('called_by_lib', True)
            self._add_attribute('method_called', None)
        
        self._add_attribute('args', args)


    def process_method_attribute(self, token):
        if token.value == 'method':
            name = self._tokens.match_token(TokenKind.Identifier).value
        # elif token[1] == 'constructor':
        #     if self._match_lookahead('id') and not (self._match_lookahead('id', 'procedure') or self._match_lookahead('id', 'function')):
        #         name = self._match_token('id')[1]
        #     else:
        #         name = 'Create'
        #     self.process_true_attribute(token)
        
        self._add_attribute('class_method', name)

    def process_overload_attribute(self, token):
        # TODO: Implement properly
        name_tok = self._tokens.match_token(TokenKind.Identifier)
        uname_tok = self._tokens.match_token(TokenKind.Identifier)
        
        #method = name_tok
        #method.uname = uname_tok[1]
        #self._add_attribute('class_method', method)