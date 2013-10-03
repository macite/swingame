#!/usr/bin/env python
# encoding: utf-8
"""
file_writer.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os
import wrapper_helper

class FileWriter(object):
    def __init__(self, filename):
        self._indent = 0
        self._at_newline=True
        self.out_file = open(filename, 'w')
        self._echo_to = None
    
    echo_to = property(lambda self: self._echo_to, lambda self,value: self.set_echo_to(value), None, "The file data is echoed to, in addition to the out file.")

    def set_echo_to(self, value):
        self._echo_to = value

    def indent(self, count = 1):
        if self._echo_to:
            self._echo_to.indent(count)
        self._indent += count
    
    def outdent(self, count = 1):
        if self._echo_to:
            self._echo_to.outdent(count)
        self._indent -= count

    def _write(self,data):
        if wrapper_helper.hasError(): return
        
        # if _echo_to:
        #     _echo_to._write(data)

        if self._at_newline:
            self.out_file.write('%s%s' % ('  ' * self._indent, data)),
        else:
            self.out_file.write('%s' % data),
        
        self._at_newline = data[-1] == "\n"
    
    def write(self,data):
        if wrapper_helper.hasError(): return
        
        if self._echo_to:
            self._echo_to.write(data)

        lines = data.splitlines(True)
        for line in lines:
            self._write(line)
    
    def writeln(self,data=''):
        if wrapper_helper.hasError(): return
        
        if self._echo_to:
            self._echo_to.writeln(data)

        lines = data.splitlines(True)
        for line in lines:
            self._write(line)
        self._at_newline = True
        self.out_file.write('\n')
    
    def close(self):
        if self._echo_to:
            self._echo_to.close()

        self.out_file.close()
    

def main():
    pass


if __name__ == '__main__':
    main()

