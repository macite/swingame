#!/usr/bin/env python
# encoding: utf-8
"""
all_tests.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import nose
from nose.tools import * 

from sg_method import *
from sg_code_module import *
from sg_metadata_container import *
from sg_tag import *
from sg_parameter import *
from sg_property import *
from sg_library import *
from sg_field import *
from sg_pas_tokeniser import *

if __name__ == '__main__':
    nose.main()

