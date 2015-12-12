#!/usr/bin/env python
# encoding: utf-8
"""
SGMetaDataContainer.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging

logger = logging.getLogger("SGWrapperGen")

class SGMetaDataContainer(object):
    """A container for meta data tag information"""
    
    def __init__(self, known_tags = None):
        """initialise the container setting up the tags dictionary"""
        self._known_tags = known_tags if known_tags != None else []
        self._known_tags.extend(['note','name','version','in_file', 'ignore', 'file_line_details', 'meta_comment_line_details', 'doc_group', 'deprecated'])
        
        self.tags = {}
        self.doc = ""
        self.notes = []
        self.is_ignored = False
        self.file_line_details = None
        self.meta_comment_line_details = None
        self.doc_group = None
        
        # lang_data is a dictionary of all of the language specific data added to these
        # types during the post parse processing
        self.lang_data = {}
    
    def alias(self, lang_key):
        if lang_key is None:
            return self
        elif not self.lang_data.has_key(lang_key):
            return self
        else:
            return self.lang_data[lang_key]
    
    def add_doc(self, doc):
        """adds documentation to the meta data container"""
        self.doc = doc
    
    def add_note(self, note):
        '''Adds a note to the node'''
        self.notes.append(note)
    
    def set_tag(self, tag, other = None):
        '''sets the tag for the meta data container with optional other data.
           
           parameters:
           tag = name or SGTag object, 
           other = list of data (default None)
        '''
        if isinstance(tag, SGTag):
            self._check_known_tags(tag)
            logger.log(logging.DEBUG - 1, "MetaData  : Adding %s", str(tag))
            self.tags[tag.title] = tag
        else:
            self.set_tag(SGTag(tag, other))
    
    
    def _check_known_tags(self, tag):
        if not tag.title in self._known_tags:
            logger.error('Model Error: Unknown tag @%s added for %s', tag.title, str(self.__class__))
            assert False
    
    def keys(self):
        return self.tags.keys()
    
    def __getitem__(self, key):
        return self.tags[key]
    
    def __setitem__(self, key, value):
        self.tags[key] = value
    
    name = property(lambda self: self['name'].other, lambda self,name: self.set_tag('name', name), None, "The name of the element.")
    is_ignored = property(lambda self: self['ignore'].other, lambda self,value: self.set_tag('ignore', value), None, "The element is ignored?")
    in_file = property(lambda self: self['in_file'].other, lambda self,value: self.set_tag('in_file', value), None, "The file containing the element.")
    version = property(lambda self: self['version'].other, lambda self,version: self.set_tag('version', version), None, "The version of the element.")
    doc_group = property(lambda self: self['doc_group'].other, lambda self,doc_group: self.set_tag('doc_group', doc_group), None, "The documentation group.")
    
    file_line_details = property(lambda self: self['file_line_details'].other, 
        lambda self,value: self.set_tag('file_line_details', value), 
        None, 'The details of where in the file this is from.')

    meta_comment_line_details = property(lambda self: self['meta_comment_line_details'].other, 
        lambda self,value: self.set_tag('meta_comment_line_details', value), 
        None, 'The details of where in the file the comment is.')
    
    def visit(self, visitor):
        visitor(self)
    
    def lower_name(self):
        import wrapper_helper
        return wrapper_helper.lower_name(self.name)
    
    lower_name = property(lower_name, None, None, "The name as name_lower_case")
    
    def upper_name(self):
        import wrapper_helper
        return wrapper_helper.upper_name(self.name)
    
    upper_name = property(upper_name, None, None, "The name as NAME_UPPER_CASE")


from sg_tag import SGTag

def test_meta_data_creation():
    """tests the basic construction of the container"""
    cont = SGMetaDataContainer()
    
    assert len(cont.tags) == 0


def test_md_creation_with_tag():
    """tests the setting of a tag"""
    cont = SGMetaDataContainer()
    tag = SGTag("tag")
    
    cont.set_tag(tag)
    
    assert len(cont.tags) == 1
    assert cont.tags["tag"] == tag
    assert "tag" in cont.tags


def test_set_tag_data():
    """tests the creation of tags using passed in data"""
    cont = SGMetaDataContainer()
    
    cont.set_tag(10)
    assert 10 in cont.tags
    
    cont.set_tag("test")
    assert "test" in cont.tags
    
    cont.set_tag("blah", ["blah"])
    assert "blah" == cont.tags["blah"].other[0]


if __name__ == '__main__':
    import nose
    nose.run()
    