#!/usr/bin/env python
# encoding: utf-8

import logging

class NullHandler(logging.Handler):
    def emit(self, record):
        pass
        
_h = NullHandler()
logger = logging.getLogger("SGWrapperGen")
logger.addHandler(_h)

