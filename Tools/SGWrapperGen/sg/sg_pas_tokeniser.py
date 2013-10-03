#!/usr/bin/env python
# encoding: utf-8
"""
SGPasTokeniser.py

Created by Andrew Cain on 2009-05-26.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
from sg_cache import logger

class SGPasTokeniser(object):
    
    def __init__(self):
        self.pas_lines = []
        self._char_no = -1
        self._token_start = -1
        self._meta_comment_start = -1
        self._meta_comment_line = -1
        self._line_no = 0 #starts at first line
        self._filename = 'supplied data'
    
    def tokenise(self, filename):
        '''Initialises the tokeniser with characters loaded from the specified 
        filename. Call `next_token` process each token.
        '''
        
        if isinstance(filename, list):
            logger.debug('Tokenising list')
            self.pas_lines = filename
        else:
            logger.debug('Tokenising %s', filename)
            self._filename = filename
            # print filename
            f = open(filename)
            self.pas_lines = f.readlines()
            f.close()
        
        self._char_no = -1
        self._line_no = 0 #starts at first line
        self._token_val = 'none'
    
    def line_details(self):
        '''Return string with line no. and filename details.'''
        return 'char %d line %d in %s' % (self._token_start + 1, self._line_no + 1, self._filename)
    
    def meta_comment_line_details(self):
        ''' Return string with line no. and filename details of the start of the meta comment for the 
            current element.'''
        return 'char %d line %d in %s' % (self._meta_comment_start + 1, self._meta_comment_line + 1, self._filename)
    
    def _next_char(self):
        '''Returns the next character from the input file.'''
        self._char_no += 1
        #print self._char_no, ' ', self._line_no
        if len(self.pas_lines[self._line_no]) <= self._char_no:
            return '\n'
        return self.pas_lines[self._line_no][self._char_no]
    
    def _peek(self, chars):
        '''Return a string with next number of "chars" line or \\n if at or 
        past the line end. '''
        if len(self.pas_lines[self._line_no]) <= self._char_no + 1:
            result = '\n'
        else:
            result = self.pas_lines[self._line_no][self._char_no + 1:self._char_no + chars + 1]
        #print 'peeking at', result
        return result
        
    def peekNextChar(self):
        n = 1
        ch = self._peek(n)
        
        while ch == ' ' or ch == '/t':
            n = n + 1
            ch = self._peek(n)
        
        return ch
    
    def _read_matching(self, start, match_fn):
        '''Return a string starting with the "start" character and moving
        along the current line (increasing self._char_no) until the match_fn 
        returns True on the current new character of the line. Result does 
        *not* include the last matched character. 
        '''
        result = start
        cha = self._next_char();
        #print 'matching', cha
        while match_fn(cha, result):
            result += cha
            cha = self._next_char();
            #print 'matching', cha
        self._char_no -= 1 #step back
        return result
        
    def _advance_line(self):
        '''Move the line index to the next line. Reset the line character no to
        the initial value (-1).'''
        self._line_no += 1
        self._char_no = -1
    
    def _read_until(self, start, end_fn):
        '''Return a string starting with (and including) the "start" character
        and moving along the stream of file characters (across multiple lines 
        if needed) and stopping when the end_fn returns True on the current result
        of characters. 
        '''
        result = start
        cha = self._next_char();
        result += cha
        #print 'until', result
        while not end_fn(result):
            if cha == '\n':
                self._advance_line()
            cha = self._next_char();
            result += cha
            #print 'until', result
        #self._char_no -= 1 #step back
        return result
    
    def _match_and_read(self, cha):
        '''Looks at (peeks) the next character so see if it matches `cha`. 
        If `cha` does match then the character cursor is moved and True is 
        returned, otherwise False is returned.
        '''
        #print 'matching and reading', cha, ' = ', self._peek(1)
        if self._peek(1) == cha:
            self._next_char()
            return True
        else:
            return False
    
    def read_to_eol(self):
        '''Read and return a string with the rest of the current line, 
        stripped of any starting or trailing whitespace characters. The cursor 
        is advanced to the next line.
        '''
        result = self.pas_lines[self._line_no][self._char_no + 1:-1]
        self._advance_line()
        return result.strip()
    
    def read_to_end_of_comment(self):
        '''Read and return a string starting from the current cursor position
        and reading up to end of the "comment" section. The end of a comment is
        indicated by the discovery of either an attribute (starting with "@"), 
        or the end of the comment section. Leading whitespace is stripped. 
        '''
        cha = self._next_char();
        result = ''
        line = ''
        while True:
            if cha == '@': #end at start of attribute
                self._char_no -= 1
                return result.strip()
            elif cha == '\n': #skip to new line
                #test for start for start of meta comment
                self._advance_line()
                result += '\n'
                cha = self._next_char();
                while cha == ' ' or cha == '\t':
                    cha = self._next_char();
                
                self._char_no -= 1 #back up from last character
                
                if self._peek(3) != '///':
                    return result.strip()
                else:
                    self._char_no += 3
                line = ''
            else:
                line += cha
                result += cha
            cha = self._next_char();
        
    
    def next_token(self):
        '''Find and return a tuple with the next token details. Tuple contains 
        the token (type, value) as strings. The token types and details are: 
        
            number,       # such as 1234, 123.45, -123, +123.4
            comment,      # single // or multi-line (* ... *), { ... } 
            meta comment, # start with /// ...
            id,           # identifier name starting with alpha, including 
                          # alpha-numeric characters and the _ character
            attribute,    # name starting with @... inside meta comment block 
                          # follows the id name character rules
            operator,     # one of + - / * ** := < > <= >= <>
            symbol,       # one of ':;,.()'
        
        '''
        
        def num_match(cha, tmp):
            '''Checks for a number in format ##, ##.#. Returns False when at the 
            end of a number.'''
            if cha in '1234567890':
                return True
            elif cha == '.' and '.' not in tmp:
                return self._peek(1) in '1234567890'
            else:
                return False
        
        while (True):
            t = self._next_char();
            self._token_start = self._char_no
            
            # Ignore white space characters
            if t == ' ' or t == '\t': #ignore white space
                pass
            # Move to next line (if at end of line)
            elif t == '\n': 
                self._advance_line()
            # Numbers (int or float style format
            elif t in '1234567890' or (t in '-+' and self._peek(1) in '1234567890'): #is digit or +/-
                result = ('number', self._read_matching(t, num_match))
                logger.debug('Tokenisier: read %s - %s', result[0], result[1])
                return result
            # Comment, single line // or meta comment line ///
            elif t == '/' and self._peek(1) == '/': #start of comment
                if self._match_and_read('/'):
                    if self._match_and_read('/'):
                        kind = 'meta comment'
                        self._meta_comment_start = self._char_no
                        self._meta_comment_line = self._line_no
                        comment = self.read_to_end_of_comment()
                    else:
                        kind = 'comment'
                        comment = self.read_to_eol()
                    result = (kind, comment)
                else:
                    result = ('error', t)
                logger.debug('Tokenisier: read %s', result[0])
                return result
            # Attribute identified by an @ symbol then a name
            elif t == '@':
                name = self._read_matching('', lambda cha, tmp: cha.isalnum() or cha == '_')
                result = ('attribute', name)
                logger.debug('Tokenisier: read %s - %s', result[0], result[1])
                return result
            # Identifier (id) of alphanumeric characters including 
            elif t.isalpha():
                name = self._read_matching(t, lambda cha, tmp: cha.isalnum() or cha == '_')
                if name.lower() in ['true','false']:
                    result = ('boolean', name)
                else:
                    result = ('id', name)
                logger.debug('Tokenisier: read %s - %s', result[0], result[1])
                return result
            #Bound Comment
            elif t == '{' or (t == '(' and self._peek(1) == '*'):
                if t == '(' and self._match_and_read('*'):
                    comment = self._read_until('', lambda temp: temp[-2:] == '*)')
                    result = ('comment', comment[:-2])
                elif t == '{':
                    comment = self._read_until('', lambda temp: temp[-1:] == '}')
                    result = ('comment', comment[:-1])
                logger.log(logging.DEBUG, 'Tokenisier: read %s', result[0])
                return result
            # Operator
            elif (t == ':' and self._peek(1) == '=') or t in '=+-*/><':
                if t == ':' and self._match_and_read('='):
                    result = ('operator', ':=')
                elif t == '*' and self._match_and_read('*'):
                    result = ('operator', '**')
                elif t == '<' and self._match_and_read('>'):
                    result = ('operator', '<>')
                elif t in '<>' and self._match_and_read('='):
                    result = ('operator', t + '=')
                else:
                    result = ('operator', t)
                return result
            # Symbol
            elif t in '(),:;[].^':
                result = ('symbol', t)
                logger.debug('Tokenisier: read %s - %s', result[0], result[1])
                return result
            # Catch any single quotes inside a string value.
            elif t == "'":
                string = self._read_until('', lambda temp: (temp[-1:] == "'") and (not self._match_and_read("'")))
                result = ('string', string[:-1])
                logger.debug('Tokenisier: read %s - %s', result[0], result[1])
                return result
            # Hmm.. unknown token. What did we forget? 
            else:
                logger.error("Unknown token type: "+t)
                return ('error', t)
    

#----------------------------------------------------------------------------=

def test_basic():
    
    def assert_next_token(tokeniser, result):
        token = tokeniser.next_token()
        assert token == result, "Token: "+str(token) + " Expected: "+str(result)
    
    lines = [
        '// Hello World\n', 
        '/// Special Comment\n', 
        '///@test(attr)\n', 
        '12345 123.45\n',
        '{ test multi line 1234\n', 
        'comments} end\n', 
        '/// @another(attr,attr2) \'a\'\'end\'\n',
        '0. 2..5 3a 0.1.2\n',
        """'''blah'''\n""",
        '^test (* comment *)\n',
        'True False\n'
    ]
    t = SGPasTokeniser()
    
    t.tokenise(lines)
    
    assert_next_token(t, ('comment', 'Hello World'))
    assert_next_token(t, ('meta comment', 'Special Comment'))

    assert_next_token(t, ('attribute', 'test'))
    assert_next_token(t, ('symbol', '('))
    assert_next_token(t, ('id', 'attr'))
    assert_next_token(t, ('symbol', ')'))
    
    assert_next_token(t, ('number', '12345'))
    assert_next_token(t, ('number', '123.45'))
    
    assert_next_token(t, ('comment', ' test multi line 1234\ncomments'))
    assert_next_token(t, ('id', 'end'))
    
    assert_next_token(t, ('meta comment', ''))
    assert_next_token(t, ('attribute', 'another'))
    assert_next_token(t, ('symbol', '('))
    assert_next_token(t, ('id', 'attr'))
    assert_next_token(t, ('symbol', ','))
    assert_next_token(t, ('id', 'attr2'))
    assert_next_token(t, ('symbol', ')'))
    assert_next_token(t, ('string', 'a\'end'))

    assert_next_token(t, ('number', '0'))
    assert_next_token(t, ('symbol', '.'))
    
    assert_next_token(t, ('number', '2'))
    assert_next_token(t, ('symbol', '.'))
    assert_next_token(t, ('symbol', '.'))
    assert_next_token(t, ('number', '5'))

    assert_next_token(t, ('number', '3'))
    assert_next_token(t, ('id', 'a'))
    
    assert_next_token(t, ('number', '0.1'))
    assert_next_token(t, ('symbol', '.'))
    assert_next_token(t, ('number', '2'))
    
    assert_next_token(t, ('string', "'blah'"))
    
    assert_next_token(t, ('symbol', "^"))
    assert_next_token(t, ('id', "test"))
    assert_next_token(t, ('comment', " comment "))
    
    assert_next_token(t, ('boolean', "True"))
    assert_next_token(t, ('boolean', "False"))

if __name__ == '__main__':
    test_basic()

