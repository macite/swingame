#!/usr/bin/env python
# encoding: utf-8
"""
print_writer.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os

class PrintWriter(object):
    def __init__(self):
        self._indent = 0
    
    def indent(self):
        self._indent += 1
    
    def outdent(self):
        self._indent -= 1
    
    def write(self,data):
        print '%s%s' % ('  ' * self._indent, data),
    
    def writeln(self,data=''):
        print '%s%s' % ('  ' * self._indent, data)
    
    def close(self):
        pass
    

def main():
    pass


if __name__ == '__main__':
    main()

