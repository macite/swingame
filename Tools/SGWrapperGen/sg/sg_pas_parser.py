#!/usr/bin/env python
# encoding: utf-8
"""
SGPasParser.py

Created by Andrew Cain on 2009-05-26.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

from sg_pas_tokeniser import SGPasTokeniser
from sg_code_module import SGCodeModule
from sg_type import SGType
from sg_field import SGField
from sg_method import SGMethod
from sg_cache import find_or_add_class, find_or_add_type, logger, find_or_add_file, all_types

class SGPasParser():
    def __init__(self):
        self._tokeniser = SGPasTokeniser()
        self._processors = {
            'attribute': self.process_attribute,
            'comment': self.process_comment,
            'meta comment': self.process_meta_comment
        }
        self._file_processors = {
            'unit': self.process_unit
        }
        self._attribute_processors = {
            'param': self.process_id_list_and_comment_attribute,
            'class': self.process_id_attribute,
            'type': self.process_id_attribute,
            'enum': self.process_id_attribute,
            'module': self.process_id_attribute,
            'header': self.process_id_attribute,
            'static': self.process_true_attribute,
            'struct': self.process_id_attribute,
            'lib':self.process_lib_attribute,
            #'has_pointer': self.process_true_attribute,
            'note': self.process_note_attribute,
            'uname': self.process_id_attribute,
            'sn': self.process_line_attribute,
            'csn': self.process_line_attribute,
            'field': self.process_field_attribute,
            'constructor': self.process_true_attribute, # method_attribute,
            'dispose': self.process_true_attribute,
            'method': self.process_method_attribute,
            'overload': self.process_overload_attribute,
            'version': self.process_number_attribute,
            'setter': self.process_id_attribute,
            'getter': self.process_id_attribute,
            'returns': self.process_comment_attribute,
            'swingame': self.process_swingame_attribute,
            'pointer_wrapper': self.process_true_attribute,
            'no_free_pointer_wrapper': self.process_true_attribute,
            'data_wrapper': self.process_true_attribute,
            'array_wrapper': self.process_true_attribute,
            'fixed_array_wrapper': self.process_true_attribute,
            'ignore': self.process_true_attribute,
            'self': self.process_number_attribute,
            'see': self.process_idlist_attribute,
            'like': self.process_id_attribute,
            'via_pointer': self.process_true_attribute,
            'fixed_result_size': self.process_number_attribute,
            'sameas': self.process_type_attribute,
            'calls': self.process_id_attribute,
            'length': self.process_id_attribute,
            'updatesArrayParam': self.process_numbers_attribute,
            'doc_group': self.process_id_attribute,
            'doc_types': self.process_idlist_attribute,
            'doc_idx': self.process_number_attribute,                       # Allows items to be ordered in the documentation
            'doc_details': self.process_true_attribute,                     # Moves documentation to a details file
            # 'output_skip': self.process_true_attribute,
        }
        self._block_header_processors = {
            'type': self.process_block_types,
            'procedure': self.process_method_decl,
            'function': self.process_method_decl,
            'operator': self.process_operator_decl,
            'var': self.process_variable_decl
        }
        self._lookahead_toks = []
        self._meta_comments = []
        self._attributes = {}
        self._ordered_attributes = []
        self._current_file = None
        
    
    def parse(self, a_file):
        #clear all existing data
        self._lookahead_toks = []
        self._meta_comments = []
        self._attributes = {}
        self._ordered_attributes = []
        self._current_file = a_file
        self._tokeniser.tokenise(a_file.filename)
        
        #read the meta comments before the node
        self.process_meta_comments()
        #read the token after meta-comments
        tok = self._next_token()
        
        if tok[0] == 'id' and tok[1] in ['unit','library']:
            try:
              self._file_processors[tok[1]](tok)
            except:
              logger.error("Parse Error on file %s" % a_file.filename)
              raise
        else:
            logger.error('Parse Error %s: found %s expected unit or library', 
                self._tokeniser.line_details(), tok[1])
            assert False
    
    def _apply_attributes_to(self, model_element):
        '''
        Processing of the model element is complete, now apply
        all of the attributes we read in the order they were read
        '''
        #apply all attributes
        for attr_name, attr in self._ordered_attributes:
            model_element.set_tag(attr_name, attr)
        
        #add all meta comments to the model_element
        map(lambda f: model_element.add_doc(f), self._meta_comments)
        
        #clear all 
        self._attributes = {}
        self._ordered_attributes = []
        self._meta_comments = []
    
    def _create_model_element(self, kind):
        name = self._get_attribute('name')
        if kind == SGCodeModule:
            result = find_or_add_class(name)
            if not result in self._current_file.members:
                self._current_file.members.append(result)
                result.file_line_details = self._tokeniser.line_details()
                result.meta_comment_line_details = self._tokeniser.meta_comment_line_details()
        elif kind == SGType:
            assert False
            #result = find_or_add_type(name)
        else:
            result = kind(name)
            result.file_line_details = self._tokeniser.line_details()
            result.meta_comment_line_details = self._tokeniser.meta_comment_line_details()
            
        result.in_file = self._current_file
        logger.debug('Parser    : Creating model element: %s with kind:%s', name, kind)
        return result
    
    def _next_token(self):
        current_token = None
        while current_token == None or current_token[0] == 'comment':
            if len(self._lookahead_toks) > 0:
                current_token = self._lookahead_toks[0]
                self._lookahead_toks = self._lookahead_toks[1:]
            else:
                current_token = self._tokeniser.next_token()
            if current_token[0] == 'comment':
                logger.debug('Parser    : Skipping comment: %s', current_token[1])
        return current_token
    
    def _lookahead(self,count=1):
        logger.debug('Parser    : Looking ahead %d', count)
        while len(self._lookahead_toks) < count:
            current_token = self._tokeniser.next_token()
            while current_token[0] == 'comment':
                current_token = self._tokeniser.next_token()
            self._lookahead_toks.append(current_token)
        return self._lookahead_toks
    
    def _match_lookahead(self, token_kind, token_value = None, consume = False):
        logger.debug('Parser    : Looking to find %s (%s)%s', token_kind, 
            token_value if token_value != None else 'any',
            ' will consume' if consume else '')
        token = self._lookahead(1)[0]
        result = token[0] == token_kind and (token_value == None or token_value == token[1].lower())
        if consume and result:
            self._match_token(token_kind, token_value)
        return result
    
    def _match_token(self, token_kind, token_value = None):
        tok = self._next_token()
        
        if tok[0] != token_kind or (token_value != None and token_value != tok[1].lower()):
            logger.error('Parse Error %s: found a %s (%s) expected %s (%s)', 
                self._tokeniser.line_details(), 
                tok[0], tok[1], token_kind, token_value)
            assert False
            
        logger.debug('Parser    : Matched token %s (%s)', tok[0], tok[1])
        return tok
    
    def _match_one_token(self, token_kind_lst):
        matched = False
        tok = self._next_token()
        
        for token_kind,token_value in token_kind_lst:
            if tok[0] == token_kind and (token_value == None or token_value == tok[1]):
                matched = True
                logger.debug('Parser    : Matched %s with %s', tok[0], tok[1])
                break
            
        if not matched:
            logger.error('Parser Error %s: unexpected %s(%s) expected %s', 
                self._tokeniser.line_details(), 
                tok[0], tok[1], 
                map(lambda n: '%s(%s)' % (n[0],n[1]),token_kind_lst))
            assert False
        return tok
    
    def process_meta_comments(self):
        logger.debug('Parser    : Starting to process meta comments: clearing old comments and attributes')
        #self._meta_comments = []
        #self._attributes = {}
        tok = self._lookahead(1)[0] #_next_token()
        
        attrs_started = False
        while tok[0] in ['meta comment','attribute','comment']:
            if tok[0] == 'attribute': 
                attrs_started = True
            if attrs_started and tok[0] == 'meta comment':
                tok = self._next_token() #actually read token
                if len(tok[1]) > 0:
                    logger.error('Parser Error %s: Found additional meta comment after start of attributes', 
                        self._tokeniser.line_details())
                    assert False
            else:
                tok = self._next_token() #actually read token
                self._processors[tok[0]](tok)
            tok = self._lookahead(1)[0]
    
    def process_unit(self, token):
        tok = self._match_token('id')
        unit_name = tok[1]
        name = self._get_attribute('module', unit_name)
        self._add_attribute('name',name)
        
        logger.info('-' * 70)
        logger.info(' Parser    : Processing unit: %s', unit_name)
        logger.info('-' * 70)
        
        #self._check_attributes(['class','static'],'unit ' + unit_name)
        self._check_meta_comments(1, 'unit ' + unit_name + '\'s declaration')
                
        #Create the unit
        unit = self._create_model_element(SGCodeModule)
        unit.module_kind = 'module'
        unit.is_static = True
        logger.info(' Parser    : Creating class %s', unit.name)
        
        self._apply_attributes_to(unit)
        
        #unit name;
        tok = self._match_token('symbol', ';')
        
        #interface - ignore comments
        #self._skip_comments('unit ' + unit_name + '\'s interface')
        self._match_token('id', 'interface')
        
        #check 'uses'
        tok = self._lookahead()[0]
        if tok[0] == 'id' and tok[1] == 'uses':
            tok = self._next_token()
            self.process_uses_clause(tok)
            logger.info('-' * 70)
        
        
        while True:
            logger.debug('Parser    : At start of loop to process elements in unit')
            #read comments if they follow this position, otherwise leave 
            #  attributes as they are
            if self._match_lookahead('meta comment'): self.process_meta_comments()
            
            #Process the body of the unit's interface
            tok = self._lookahead()[0] #check the next token
            
            if tok[0] == 'id' and tok[1] == 'implementation':
                logger.info(' Parser    : Found end of unit\'s interface')
                logger.info('-' * 70)
                break
            
            #interface contains types, functions, procedures, var, const, operator
            tok = self._match_one_token([['id','type'],
                ['id','function'],
                ['id','procedure'],
                ['id','const'],
                ['id','operator'],
                ['id','var']])
            self._block_header_processors[tok[1]](unit, tok);
        
        logger.info(' Parser    : Finished processing unit. Resulting class is:\n%s', str(unit))
    
    def process_comment(self, token):
        logger.log(logging.DEBUG - 1, 'Parser    : Processing comment: %s', token[1])
    
    def process_attribute(self, token):
        logger.debug('Parser    : Processing attribute: %s', token[1])
        self._attribute_processors[token[1]](token)
    
    def _append_attribute(self, attr, val):
        logger.debug('Parser    : Appending attribute %s with value %s to %s',attr,val, attr + 's')
        if (attr + 's') in self._attributes.keys():
            self._attributes[attr + 's'].append(val)
        else:
            self._attributes[attr + 's'] = [val]
            self._ordered_attributes.append([attr + 's', self._attributes[attr + 's']])
    
    def _add_attribute(self, attr, val):
        logger.debug('Parser    : Adding attribute %s with value %s',attr,val)
        if attr in self._attributes and self._attributes[attr] != None:
            logger.warning('Parser    : Added attribute twice %s = %s', attr, val)
        self._attributes[attr] = val
        self._ordered_attributes.append([attr, val])
    
    def _get_attribute(self, attr, default=None):
        if attr in self._attributes:
            return self._attributes[attr]
        return default
    
    def _check_attributes(self, attrs, desc):
        for key in self._attributes.keys():
            if not key in attrs:
                logger.warning('Parser Warning: found unknown attribute %s while processing %s at %s',key,desc, self._tokeniser.line_details())
    
    def _check_meta_comments(self, count, desc):
        if len(self._meta_comments) != count:
            logger.error('Parser Error %s: expected %d but foung %d meta comments for %s', 
                self._tokeniser.line_details(), count, len(self._meta_comments), desc)
            sys.exit(-1)
    
    def _check_non_commented(self, desc):
        self._check_meta_comments(0, desc)
        self._check_attributes([], desc)
    
    def process_line_attribute(self, token):
        '''read the remainder of the line for the token'''
        line = self._tokeniser.read_to_eol()
        self._add_attribute(token[1], line)
    
    def process_id_list_and_comment_attribute(self, token):
        '''read the id and attach comments etc as attributes'''
        param_toks = list()
        
        param_toks.append(self._match_token('id')[1])
        
        # read ahead for a , between each identifier in the list
        # the list must have 1 or more parameters
        while self._tokeniser.peekNextChar() == ',':
            self._match_lookahead('symbol', ',', True)
            param_toks.append(self._match_token('id')[1])
        
        doc_tok = self._tokeniser.read_to_end_of_comment()
        
        # print 'DOC TOK:', doc_tok
        
        for param_tok in param_toks:
            self._append_attribute(token[1], [param_tok, doc_tok])
            if len(param_toks) > 1:
                # if there is more than one token then also add the list of
                # related tokens
                # print 'related_' + token[1], param_tok, param_toks
                self._append_attribute('related_' + token[1], [param_tok, param_toks])
    
    def process_comment_attribute(self, token):
        '''read the id and attach comments etc as attributes'''
        doc_tok = self._tokeniser.read_to_end_of_comment()
        self._add_attribute(token[1], doc_tok)

    def _skip_comments(self, desc):
        self.process_meta_comments()
        self._check_non_commented(desc)
    
    def process_uses_clause(self, token):
        '''Read the list of units referred to be the uses clause'''
        self._check_non_commented('uses clause')
        
        while True:
            tok = self._match_token('id')
            self._current_file.uses.append(find_or_add_file(tok[1]))
            logger.debug('Parser    : Found using unit %s', tok[1])
            
            #found a token/unit
            next_tok = self._match_token('symbol')
            if next_tok[1] == ';': 
                break; #found end
            elif next_tok[1] != ',':
                logger.error('Parser Error %s: expected , or ; but found %s at %s', 
                    self._tokeniser.line_details(), next_tok[1])
                sys.exit(-1)
    
    def process_id_id_attribute(self, token):
        '''Process an attribute that is followed by a two identifiers'''
        tok1 = self._match_token('id') #load id of thing...
        tok2 = self._match_token('id') #load id of thing...
        self._add_attribute(token[1], [tok1[1], tok2[1]])
    
    def process_id_attribute(self, token):
        '''Process an attribute that is followed by a single identifier'''
        tok = self._match_token('id') #load id of thing...
        self._add_attribute(token[1], tok[1])
    
    def process_type_attribute(self, token):
        '''Process an attribute that is followed by a single type identifier'''
        the_type = self._read_type_usage()
        #tok = self._match_token('id') #load id of the type...
        self._add_attribute(token[1], the_type)
    
    def process_idlist_attribute(self, token):
        '''Process an attribute followed by a list of comma separated identifiers.
        eg. @see id1, id2 ...
        '''
        tok = self._match_token('id')
        ids = [tok[1]]
        while self._match_lookahead('symbol', ',', True):
            ids.append(self._match_token('id')[1])
        self._add_attribute(token[1], ids)  
    
    def process_field_attribute(self, token):
        '''Have encountered @field - create a field for the generated class to use
        '''
        tok = self._match_token('id') #field name
        field = SGField(tok[1])
        
        #todo: proper type matching...
        tok = self._match_token('symbol', ':')
        #tok = self._match_token('id')
        #the_type = find_or_add_type(tok[1]) #todo: should be matching type for this...
        the_type = self._read_type_usage()
        
        field.data_type = the_type
        self._append_attribute('field', field)
    
    def process_true_attribute(self, token):
        self._add_attribute(token[1], True)
    
    def process_swingame_attribute(self, token):
        tok = self._match_token('number')
        lib = find_or_add_class('lib')
        lib.version = tok[1]
    
    def process_method_attribute(self, token):
        if token[1] == 'method':
            name = self._match_token('id')[1]
        # elif token[1] == 'constructor':
        #     if self._match_lookahead('id') and not (self._match_lookahead('id', 'procedure') or self._match_lookahead('id', 'function')):
        #         name = self._match_token('id')[1]
        #     else:
        #         name = 'Create'
        #     self.process_true_attribute(token)
        
        method = SGMethod(name)
        self._add_attribute('class_method', method)
    
    def process_overload_attribute(self, token):
        name_tok = self._match_token('id')
        uname_tok = self._match_token('id')
        
        method = SGMethod(name_tok[1])
        method.uname = uname_tok[1]
        self._add_attribute('class_method', method)
    
    def _setup_lib_method(self, uname):
        the_lib = find_or_add_class('lib')
        method = the_lib.find_method(uname, self._current_file.name)
        
        if method == None: 
            method = SGMethod(uname)
            method.in_file = self._current_file
            method.in_class = the_lib
            method.in_class.add_member(method)
        
        self._add_attribute('method_called', method)
        
        return method
    
    def process_lib_attribute(self, token):
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
        if self._match_lookahead('id') and not (self._match_lookahead('id', 'procedure') or self._match_lookahead('id', 'function')):
            uname = self._match_token('id')[1]
            
            self._setup_lib_method(uname)
            
            if self._match_lookahead('symbol', '(', True):
                self._add_attribute('called_by_lib', False)
                #the lib attr has arguments
                args = list()
                if not self._match_lookahead('symbol', ')'):
                    while True:
                        #args are ids, number, or strings
                        args.append(self._match_one_token([('id',None), ('number', None), ('string', None), ('boolean', None)]))
                        
                        #if not comma following - end args
                        if not self._match_lookahead('symbol', ',', True): break;
                close_tok = self._match_token('symbol', ')')
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
    
    def process_note_attribute(self, token):
        self._add_attribute('note', self._tokeniser.read_to_end_of_comment())
    
    def process_value_attribute(self, token):
        self._add_attribute(token[0], token[1])
    
    def process_number_attribute(self, token):
        num_tok = self._match_token('number')
        self._add_attribute(token[1], eval(num_tok[1]))
    
    def process_numbers_attribute(self, token):
        num_tok = self._match_token('number')
        self._append_attribute(token[1], eval(num_tok[1]))
    
    def process_meta_comment(self, token):
        logger.log(logging.DEBUG - 1, 'Parser    : Processing meta comment: %s', token[1])
        self._meta_comments.append(token[1])
    
    def process_block_types(self, block, token):
        '''Reads the types within a type declaration'''
        logger.info(' Parser    : Processing types')
        logger.info('-' * 70)
        
        self.process_meta_comments()
        
        #following type... in pascal
        while True:
            type_name = self._match_token('id')[1] #read the types name
            self._match_token('operator','=') #read the =
            
            the_type = find_or_add_type(type_name)
            the_type.file_line_details = self._tokeniser.line_details()
            the_type.meta_comment_line_details = self._tokeniser.meta_comment_line_details()
            
            self._parse_type_declaration(the_type)
            
            if 'type' or 'class' or 'struct'in the_type.keys():
                self._add_class(the_type)
            
            self.process_meta_comments()
            
            tok1, tok2 = self._lookahead(2)
            #looking for... type_name = 
            if tok2[0] != 'operator' or tok2[1] != '=' or tok1[0] != 'id':
                logger.info('-' * 70)
                logger.debug('Parser    : At end of block types')
                break
    
    def _add_class(self, the_type):
        logger.info(' Parser    : Adding class %s', the_type.name)
        new_class = find_or_add_class(the_type.name)
        new_class.in_file = self._current_file
        if new_class not in self._current_file.members:
            self._current_file.members.append(new_class)
        new_class.setup_from(the_type)
    
    def _read_variable_list(self):
        '''
        reads in a list of variables. i.e. 
        name, name2: type
        This expects at least one variable
        '''
        names = []
        
        #read name, name... :
        while True:
            name = self._match_token('id')[1]
            names.append(name)
            if self._match_lookahead('symbol', ':', True): break
            self._match_token('symbol', ',') #comma separated
        
        #read ...: the_type
        the_type = self._read_type_usage()
        #expand out so each is is its own
        return [(name, the_type) for name in names]
    
    def _parse_type_declaration(self, the_type):
        '''
        Parse a type from the next token, add details to the_type.
        this is called for each type declaration... type_name = BLAH
        Need to process BLAH and add details to the_type
        '''
        #check what kind of type it is...
        if self._match_lookahead('symbol', '(', True):
            values = []
            while not self._match_lookahead('symbol',')'):
                temp = self._match_token('id')[1]
                if self._match_lookahead('operator', '=', True) or self._match_lookahead('operator', ':=', True): #assigned value
                    values.append('%s = %s' % (temp, self._match_token('number')[1]))
                else:
                    values.append(temp)
                self._match_lookahead('symbol', ',', True) #consume commas
            self._match_token('symbol',')') #read close bracket
            the_type.values = tuple(values)
        elif (self._match_lookahead('id', 'packed', True) and self._match_lookahead('id', 'record', True)) or self._match_lookahead('id', 'record', True):
            #packed record field: Type end;
            while not self._match_lookahead('id', 'end', True):
                #read field
                variables = self._read_variable_list()
                self._match_token('symbol', ';')
                for name, data_type in variables:
                    field = SGField(name)
                    field.data_type = data_type
                    the_type.fields.append(field)
        elif self._match_lookahead('id', 'array') or self._match_lookahead('symbol', '^'):
            #is pointer or array
            the_type.clone(self._read_type_usage())
        elif self._match_lookahead('id', 'procedure', True):
            #procedure type: read ( params );
            self._match_token('symbol', '(')
            m = SGMethod(the_type.name)
            m.in_file = self._current_file
            self._read_params(m)
            the_type.is_procedure = True
            the_type.method = m
            if self._lookahead(2)[1][1] == 'cdecl':
                self._match_token('symbol', ';')
                self._match_token('id', 'cdecl')
        elif self._match_lookahead('id'):
            other_id = self._match_token('id')[1]
            other_type = find_or_add_type(other_id)
            the_type.related_type = other_type
        else:
            tok = self._next_token()
            logger.error('Parser Error %s: unknown type %s(%s)', 
                self._tokeniser.line_details(), tok[0], tok[1])
            assert False
                
        if the_type.related_type != None:
            logger.info(' Parser    : Setup type %s = %s', the_type, the_type.related_type)
        elif the_type.is_enum:
            logger.info(' Parser    : Setup type %s = %s', the_type, the_type.values)
        else:
            logger.info(' Parser    : Setup type %s = %s', the_type, the_type.fields)
        self._apply_attributes_to(the_type)
        self._match_token('symbol', ';')
    
    def _read_type_usage(self):
        '''Read a type identifier, or array of type, etc. and return a SGType'''
        if self._match_lookahead('id', 'array', True):
            #found an array
            dimensions = []
            # array [0..n,0..m] of
            if self._match_lookahead('symbol', '[', True):
                while True:
                    low_idx = self._match_token('number')[1]
                    self._match_token('symbol', '.')
                    self._match_token('symbol', '.')
                    high_idx = self._match_token('number')[1]
                    dimensions.append((low_idx,high_idx))
                    
                    if self._match_lookahead('symbol', ']', True):
                        break
                    self._match_token('symbol', ',')
            else:
                dimensions.append((0,'n - 1'))
            
            # of type...
            self._match_token('id', 'of')
            nested_type = self._read_type_usage() #recursive call
            
            name = nested_type.name + ''.join(['[%s..%s]' % (low_idx, high_idx) for low_idx, high_idx in dimensions])
            was_new = not name in all_types()
            
            result = find_or_add_type(name)
            if was_new:
                result.dimensions = dimensions
                result.nested_type = nested_type
            return result
        elif self._match_lookahead('symbol', '^', True):
            other_id = self._match_token('id')[1]
            other_type = find_or_add_type(other_id)
            
            name = '^' + other_type.name
            
            was_new = not name in all_types()
            
            result = find_or_add_type(name)
            if was_new:
                result.is_pointer = True
                result.related_type = other_type
            return result
        else:
            id_tok = self._match_token('id')
            return find_or_add_type(id_tok[1])
    
    def process_variable_decl(self, block, token):
        '''
        '''
        
        while True:
            names = [self._match_token('id')[1]] #the name of the var
            
            while self._match_lookahead('symbol', ',', True): #separated by ,'s
                names.append(self._match_token('id')[1]) #add next name.
            
            self._match_token('symbol',':') # ... : type
            the_type = self._read_type_usage()
            self._match_token('symbol', ';')
            
            #apply type to all names, and add to block as field
            for name in names:
                field = SGField(name)
                self._apply_attributes_to(field)
                field.is_static = True #these are globals
                field.data_type = the_type
                
                if not field.is_ignored:
                    block.add_member(field)
                else:
                    logger.info('Parser    : Ignoring field %s', name)
            
            #read the next batch of comments
            self.process_meta_comments()
            
            #variable list is ended by ...
            tok = self._lookahead(1)[0]
            if tok[1].lower() in ['function', 'procedure', 'type', 'var', 'const', 'implementation']:
                break
    
    def _read_params(self, method):
        """Read in the parameters declared in a function, procedure or operator"""
        #look for parameters
        while not self._match_lookahead('symbol', ')'): #consume ) at end
            param_tok, other_tok = self._lookahead(2)
            if param_tok[0] == 'id':
                if other_tok[0] == 'id': #first one is a modifier
                    modifier = self._next_token()[1]
                else:
                    modifier = None
                param_toks = [self._match_token('id')]
            
                while self._match_lookahead('symbol', ',', True):
                    #there is a list of parameters
                    param_toks.append(self._match_token('id'))
            
                colon_tok = self._match_token('symbol', ':')
                the_type = self._read_type_usage()
            
                for param_tok in param_toks:
                    param = method.create_parameter(param_tok[1])
                    param.data_type = the_type
                    param.modifier = modifier
                    logger.debug('Parser    : Adding parameter %s (%s) to %s', param.name, param.data_type, method.name)
            
                if not self._match_lookahead('symbol', ';', True): break
            else:
                logger.error('Parser    : Error in parameter list %s', self._tokeniser.line_details())
        
        self._match_token('symbol', ')')
    
    def process_operator_decl(self, block, token):
        '''process a operator read from a unit.'''
        
        op_kind = self._match_token('operator')
        open_tok = self._match_token('symbol', '(')
        
        #add to the class (e.g. Core module: which creates lib + other)
        self._add_attribute('name', op_kind[1]) #name comes from operator
        method = self._create_model_element(SGMethod)
        method.in_class = block
        method.is_operator = True
        
        self._read_params(method)
        
        result_tok = self._match_token('id')
        colon_tok = self._match_token('symbol', ':')
        the_type = self._read_type_usage()
        method.return_type = the_type
        logger.debug('Parser    : Set return type of operator %s to %s', method.name, method.return_type)
        
        end_tok = self._match_token('symbol', ';')
        
        self._apply_attributes_to(method)
        method.complete_method_processing()
        block.add_member(method)
    
    def process_method_decl(self, block, token):
        '''process a method read from a unit.
        
        block is the block that contains the method, token is the first token.
        At the end of this the method has been read in and added to block.
        '''
        name_tok = self._match_token('id') #name of function/procedure
        open_tok = self._match_token('symbol', '(')
        
        #add to the class (e.g. Core module: which creates lib + other)
        self._add_attribute('name', name_tok[1]) #name comes from function/procedure
        method = self._create_model_element(SGMethod)
        method.in_class = block
        
        self._read_params(method)
        
        if token[1] == 'function': #return return details
            colon_tok = self._match_token('symbol', ':')
            the_type = self._read_type_usage()
            method.return_type = the_type
            logger.debug('Parser    : Set return type of method %s to %s', method.name, method.return_type)
        
        end_tok = self._match_token('symbol', ';')
        
        #check overload ;
        if self._match_lookahead('id', 'overload', True):
            self._match_token('symbol', ';')
            self._add_attribute('overload', True)
        
        #logger.info('Adding method %s.%s(%s)', block.name, method.name, method.param_string())
        if self._get_attribute('called_by_lib', False) and self._get_attribute('method_called') == None:
            self._setup_lib_method(method.uname)
        
        self._apply_attributes_to(method)
        method.complete_method_processing()
        block.add_member(method)
    
if __name__ == '__main__':
    import nose
    nose.run()

