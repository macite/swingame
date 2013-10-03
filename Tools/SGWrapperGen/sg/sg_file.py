#!/usr/bin/env python
# encoding: utf-8
"""
sgfile.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os

class SGFile(object):
    '''Represents a source code file'''
    
    def __init__(self, pascal_name, name, filename):
        self._uses = []
        self._members = []
        self.pascal_name = pascal_name
        self.name = name
        self.filename = filename
        self.has_body = True
    
    uses = property(lambda self: self._uses, 
        None, None, 'The files that this file uses.')
    
    members = property(lambda self: self._members, 
        None, None, 'The members within this file.')
    
    def visit(self, visitor, other):
        visitor(self, other)
    
    def visit_members(self, visitor, other):
        for member in self.members:
            visitor(member, other)
    
    def uses_str(self, visitor):
        return '%s;' % ', '.join([ visitor(a_file) for a_file in self.uses ])
    
    def __repr__(self):
        return self.pascal_name
    
def main():
  pass


if __name__ == '__main__':
  main()

