#!/usr/bin/env python
# encoding: utf-8
"""
SGMethod.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging
import sys
import copy

from sg_metadata_container import SGMetaDataContainer
from sg_parameter import SGParameter
from sg_property import SGProperty
from sg_cache import find_or_add_class, logger

class SGMethod(SGMetaDataContainer):
    """A SGMethod represents a function or a procedure in SwinGame."""
    def __init__(self, name):
        """Initialise the SGMethod, setting its name."""
        SGMetaDataContainer.__init__(self, ['uname','static','operator',
            'is_constructor','return_type','other_class','is_destructor',
            'method','overload','returns','is_setter','is_getter','is_external', 
            'called_by_lib', 'my_class', 'class_method','in_property', 'called_by',
            'method_called', 'args', 'self', 'see', 'like', 'mimic_destructor', 
            'fixed_result_size', 'length', 'calls', 'sn', 'doc_idx', 'doc_details'])
        self.name = name
        self.uname = name
        self.sn = None
        self.params = list()
        #self.param_cache = {}
        self.return_type = None
        self.returns = ''
        self.is_operator = False
        self.in_class = None
        self.is_static = False
        self.is_constructor = False
        self.is_destructor = False
        self.mimic_destructor = False
        self.is_external = False
        self.is_getter = False
        self.is_setter = False
        self.is_class_method = False
        self.in_property = None
        self.called_by_lib = False
        self.method_called = None
        self.args = None
        self.class_method = None
        self.other_class = None
        self.called_by = list()
        self.self_pos = 1
        self.fixed_result_size = -1
        self.doc_idx = 9
        self.doc_details = False
        
        self.local_vars = list()    # to remove - now in lang_data
        self.was_function = False
        self.has_length_params = False
        self.length_call = None
        self.field_name = None
    
    def to_keyed_dict(self, param_visitor, type_visitor = None, arg_visitor = None, doc_transform = None, call_creater = None, special_visitor = None, lang_key = None):
        '''Returns a dictionary containing the details of this function/procedure
        
        The param_visitor is called to convert each parameter to a string. 
        param_visitor(the_param, last)
        
        Params:
        - param_visitor = visitor function used to convert parameters into strings
        - type_visitor = visitor to turn types into strings
        - arg_visitor = visitor to turn arguments into strings
        - doc_transform = visitor to change comments for insertion into code output
        - call_creater = function to call to create the call from this method to the library method
        - special_visitor = function to convert sn and csn values into Objective-C method names
        - lang_key = key to get the language alterations stored in lang_data (as a LangMethodData)
        '''
        import wrapper_helper
        
        alias = self.alias(lang_key)
        
        result = dict()
        result['doc']           = doc_transform(alias.doc) if doc_transform != None else alias.doc
        result['name']          = alias.name
        result['name_lower']    = wrapper_helper.lower_name(alias.name)
        result['camel_name']    = alias.name.lower()[0] + alias.name[1:]
        result['uname']         = alias.uname
        result['uname_lower']   = wrapper_helper.lower_name(alias.uname)
        result['camel_uname']   = alias.uname.lower()[0] + alias.uname[1:]
        
        if alias.sn != None:
            # real_params = list()
            # 
            # for p in alias.params:
            #     if not p.is_length_param:
            #         real_params.append(p)
            
            #print alias.name, real_params, alias.sn
            
            temp_sn  = alias.sn
            
            if special_visitor != None:
                # print alias.sn, alias.params
                temp = alias.sn % tuple([special_visitor(param, param == alias.params[-1]) for param in alias.params])
                
                result['sn'] = temp
                # result['sn.sel'] = (temp_sn % tuple(['' for param in real_params])).replace(' ', '')
        else:
            if special_visitor != None:
                temp = ':'.join([special_visitor(param, param == alias.params[-1]) for param in alias.params])
                
                if len(alias.params) > 1 : print "Need sn for " + alias.uname + " - " + temp
            else:
                temp = ':'.join([param.name for param in alias.params])
            
            # result['sn.sel'] = alias.uname
            if len(alias.params) > 0: 
                temp = ':' + temp
            #     for p in alias.params:
            #         result['sn.sel'] += ':'
                
            temp = alias.uname + temp
            
            #print temp
            result['sn'] = temp
            
        result['in_class']      = alias.in_class.name
        result['return_type']   = alias.return_type if type_visitor == None else type_visitor(alias.return_type, 'return')
        result['returns']       = '' if alias.return_type == None else 'return '
        result['params']        = alias.param_string(param_visitor, lang_key=lang_key)
        result['args']          = alias.args_string_for_self(arg_visitor, lang_key=lang_key)
        
        if alias.method_called != None:
            result['calls.file.pascal_name']    = alias.method_called.in_class.in_file.pascal_name
            result['calls.file.name']           = alias.method_called.in_class.in_file.name
            result['calls.file.filename']       = alias.method_called.in_class.in_file.filename
            result['calls.class']               = alias.method_called.in_class.name
            result['calls.name']                = alias.method_called.name
            result['calls.args']                = alias.args_string_for_called_method(arg_visitor, lang_key=lang_key)
        
        result['static']            = 'static ' if alias.is_static or alias.in_class.is_static else '' #TODO: fix this for VB - parameter
        result['field.name']        = alias.field_name
        result['field.name_lower']  = wrapper_helper.lower_name(alias.field_name)
        
        if alias.length_call != None:
            if alias.in_property != None or alias.is_class_method: #replace first argument with 'self'
                old_arg = alias.length_call.args[0]
                alias.length_call.args[0] = 'self.pointer'
                result['length_call'] = alias.length_call.to_keyed_dict(param_visitor, type_visitor, arg_visitor, doc_transform, call_creater, lang_key=lang_key)['the_call']
                alias.length_call.args[0] = old_arg
            else:
                result['length_call'] = alias.length_call.to_keyed_dict(param_visitor, type_visitor, arg_visitor, doc_transform, call_creater, lang_key=lang_key)['the_call']
        
        if alias.is_operator:
            result['operator'] = alias.name
        
        result['the_call'] = call_creater(result, alias) if call_creater != None else None
        
        return result
    
    name = property(lambda self: self['name'].other, 
        lambda self,name: self.set_tag('name', name), 
        None, 'The name of the method.')
    
    def spaced_name(self):
        import wrapper_helper
        return wrapper_helper.spaced_name(self.name)
    
    sn = property(lambda self: self['sn'].other, 
        lambda self,name: self.set_tag('sn', name), 
        None, 'The special name of the method - for objective c')
    
    uname = property(lambda self: self['uname'].other, 
        lambda self,uname: self.set_tag('uname', uname), 
        None, 'The uname of the method.')
    
    in_property = property(lambda self: self['in_property'].other, 
        lambda self,value: self.set_tag('in_property', value), 
        None, 'The property the method is in (or None).')
    
    is_setter = property(lambda self: self['is_setter'].other, 
        lambda self,value: self.set_tag('is_setter', value), 
        None, 'The method is a setter.')
    
    is_getter = property(lambda self: self['is_getter'].other, 
        lambda self,value: self.set_tag('is_getter', value), 
        None, 'The method is a getter.')
    
    is_static = property(lambda self: self['static'].other, 
        lambda self,value: self.set_tag('static', value), 
        None, 'Is the method associated with a class.')
    
    called_by_lib = property(lambda self: self['called_by_lib'].other, 
        lambda self,value: self.set_tag('called_by_lib', value), 
        None, 'Is the method associated with a class.')
    
    self_pos = property(lambda self: self['self'].other, 
        lambda self,value: self.set_tag('self', value), 
        None, 'The position of the self param in the method for a class method.')
    
    called_by = property(lambda self: self['called_by'].other, 
        lambda self,value: self.set_tag('called_by', value), 
        None, 'The methods that call this method.')
    
    is_operator = property(lambda self: self['operator'].other, 
        lambda self,value: self.set_tag('operator', value), 
        None, 'Is the method an operator overload?')
    
    is_constructor = property(lambda self: self['is_constructor'].other, 
        lambda self,value: self.set_tag('is_constructor', value), 
        None, 'Is the method a constructor?')
    
    is_destructor = property(lambda self: self['is_destructor'].other, 
        lambda self,value: self.set_tag('is_destructor', value), 
        None, 'Is the method a destructor?')
    
    mimic_destructor = property(lambda self: self['mimic_destructor'].other, 
        lambda self,value: self.set_tag('mimic_destructor', value), 
        None, 'Is the method a destructor?')
    
    is_external = property(lambda self: self['is_external'].other, 
        lambda self,value: self.set_tag('is_external', value), 
        None, 'Is the method from the library?')
    
    in_class = property(lambda self: self['my_class'].other, 
        lambda self,value: self.set_tag('my_class', value), 
        None, 'the class containing the method')
    
    other_class = property(lambda self: self['other_class'].other, 
        lambda self,value: self.set_tag('other_class', value), 
        None, 'the class of this methods clone')
    
    return_type = property(lambda self: self['return_type'].other, 
        lambda self,return_type: self.set_tag('return_type', return_type), 
        None, "The return type of the method.")

    returns = property(lambda self: self['returns'].other, 
        lambda self,returns: self.set_tag('returns', returns), 
        None, "The returns documentation.")

    
    class_method = property(lambda self: self['class_method'].other, 
        lambda self,return_type: self.set_tag('class_method', return_type), 
        None, "The class method that duplicates this methods behaviour.")
    
    is_function = property (lambda self: self.return_type != None,
        None, None, 'The method is a function'
        )
    
    args = property(lambda self: self['args'].other, 
        lambda self,value: self.set_tag('args', value), 
        None, "The arguments passed to the method called by this method.")
    
    method_called = property(lambda self: self['method_called'].other, 
        lambda self,value: self.set_tag('method_called', value), 
        None, "The method called by this method.")
        
    fixed_result_size = property(lambda self: self['fixed_result_size'].other, 
        lambda self,value: self.set_tag('fixed_result_size', value), 
        None, 'The size for a variable length array returned is fixed (and known)')
    
    length_call = property(lambda self: self['length'].other, 
        lambda self,value: self.set_tag('length', value), 
        None, 'The property to get the length of the returned array...')
    
    doc_idx = property(lambda self: self['doc_idx'].other, 
        lambda self,value: self.set_tag('doc_idx', value), 
        None, 'The order that this should appear in the documentation/processing.')
    
    doc_details = property(lambda self: self['doc_details'].other, 
        lambda self,value: self.set_tag('doc_details', value), 
        None, 'The documentation should appear in a details file.')
    
    @property
    def signature(self):
        return (self.name, tuple([(p.modifier + ' ' if p.modifier != None else '') + str(p.data_type) for p in self.params]))
    
    # def cache_parameter(self, param):
    #     """
    #     Adds a parameter to a temporary cache during loading of the Method.
    #     
    #     The cache does not need to be loaded in the methods parameter order.
    #     Parameters are then loaded using createParameter
    #     """
    #     
    #     self.param_cache[param.name] = param
    
    def create_parameter(self, name):
        """creates a new parameter with the indicated name, or fetches it from 
        the cache. The new parameter is returned, and added to the params 
        list."""
        result = SGParameter(name)
        result.file_line_details = self.file_line_details
        self.params.append(result)
        return result
    
    def has_parameter(self, name):
        """check if the method has a parameter with the matching name"""
        for par in self.params:
            if par.name == name:
                return True
        return False
    
    def set_tag(self, title, other = None):
        if title == "params":
            #process parameter comments
            for param_details in other:
                param_name = param_details[0]
                param_doc = param_details[1]
                
                done = False
                for param in self.params:
                    if param.name == param_name:
                        param.add_doc(param_doc)
                        done = True
                        break
                if not done:
                    logger.error('Method    : Unable to find parameter %s for %s', param_name, self.uname)
                    assert False
        elif title == 'related_params':
            # The parameters have related documentation
            # The passed in details has the param name, and the list of its related
            # parameters
            for related_param_details in other:
                param_name = related_param_details[0]
                related_params = related_param_details[1]
                
                # Find the parameter
                done = False
                for param in self.params:
                    # if this is the parameter...
                    if param.name == param_name:
                        # then add related docs
                        param.add_related_params(related_params)
                        done = True
                        break
                if not done:
                    logger.error('Method    : Unable to find parameter %s for %s', param_name, self.uname)
                    assert False
                
        elif title == 'updatesArrayParams':
            # check which parameters are being updated and mark so that checking ignores them
            for idx in other:
                self.params[idx - 1].being_updated = True
        elif title == "class":
            #the class indicates that the @method is for this other class...
            from sg_code_module import SGCodeModule
            from sg_library import SGLibrary
            if other == None: 
                super(SGMethod,self).set_tag(title, None)
                return
            elif isinstance(other, SGCodeModule):
                other_class = other
            elif isinstance(other, SGLibrary):
                other_class = other
            else:
                other_class = find_or_add_class(other)
            
            super(SGMethod,self).set_tag('other_class', other_class)
        elif title == 'getter' or title == 'setter':
            # 1: mark as getter/setter
            super(SGMethod,self).set_tag('is_' + title, True) 
            # 2: set property name
            self.in_property = other
            # 3: mark for later processing
            mthd = SGMethod(other + ' ' + title)
            super(SGMethod,self).set_tag('class_method', mthd)
        elif title == 'constructor':
            const = SGMethod(self.other_class.name)
            const.is_constructor = True
            super(SGMethod,self).set_tag('class_method', const)
        elif title == 'dispose':
            dest = SGMethod("~" + self.other_class.name)
            dest.is_destructor = True
            self.mimic_destructor = True
            super(SGMethod,self).set_tag('class_method', dest)
        elif title == 'csn':
            #assign the 'csn' class special name to the class method
            if self.class_method == None:
                logger.error('Model Error: Method %s has a csn before the method/property/constructor definition - or should be sn.', self.name)
                assert False
            self.class_method.set_tag('sn', other)
        else:
            super(SGMethod,self).set_tag(title, other)
    
    def in_same_class_as_class_method(self):
        if self.class_method == None:
            return False
        return self.class_method.in_class == self.in_class
    
    def clone_to(self, other):
        #dont copy uname...
        other.in_file = self.in_file
        other.return_type = self.return_type
        other.file_line_details = self.file_line_details
        other.doc = self.doc
        other.doc_idx = self.doc_idx
        other.doc_details = self.doc_details
        
        #dont copy sn... unless other is static
        if other.sn == None and other.is_static:
            other.sn = self.sn
        # else:
        #     print other.sn
        
        other.length_call = self.length_call
        other.fixed_result_size = self.fixed_result_size
        
        if self.is_static or other.is_constructor:
            other.params = self.params
            other.is_static = self.is_static
        else:
            #remove the self parameter
            other.params = self.params[0:self.self_pos - 1] + self.params[self.self_pos:]
        
        if self.other_class != None:
            other.in_class = self.other_class
        else:
            other.in_class = self.in_class
    
    def calls(self, method, args=None):
        """indicate which method this method calls, and args if any"""
        if self.method_called != None:
            logger.error('Model Error: Changing method called by %s', self.name)
            assert False
        self.method_called = method
        self.args = list()
        
        for arg in args:
            if not isinstance(arg, SGParameter):
                self.args.append(arg)
            else:
                self.args.append(self.get_parameter(arg.name))
    
    def _process_args(self):
        '''
        Convert args to parameters, fields, and literals
        '''
        new_args = list()
        args = self.args
        if args == None:
            self.args = list(self.params)
            return
        for argv in args:
            if argv[0] in ['number', 'string', 'boolean']:
                new_args.append(argv[1])
            elif argv[0] in ['id']:
                param = self.get_parameter(argv[1])
                if param != None:
                    new_args.append(param)
                else:
                    field = self.in_class.get_field(argv[1])
                    if field != None:
                        new_args.append(field)
                    else:
                        logger.error('Method    : Error cannot find %s in method %s', argv[1], self.uname)
                        assert False
            else:
                logger.error('Method    : Error unknown type of argument in %s - %s', self.uname, argv[0])
        self.args = new_args
    
    def setup_lib_method(self, lib_method):
        '''
        Setup the library method
        
        Set:
            return type
            parameters
            calls
        '''
        if lib_method.file_line_details == None:
            lib_method.file_line_details = []
        lib_method.file_line_details.append(self.file_line_details)
        
        if self.called_by_lib: #Set up the call from the library to this method
            logger.debug('Method    : Setting %s in library to call %s', lib_method, self)
            lib_method.return_type = self.return_type #set return type
            
            lib_method.params = []  #add parameters
            for p in self.params:
                lib_method.params.append(p.clone())
            lib_method.calls(self, self.args) #..calls this method at other end
    
    def create_and_add_property(self, class_method):
        '''
        The added class method is actually a property, so
        create and add a property or update the existing property.
        '''
        property_name = self.in_property
        #get or create the property
        if property_name in self.other_class.properties:
            prop = self.other_class.properties[property_name]
        else:
            prop = SGProperty(property_name)
            prop.in_class = self.other_class;
            prop.is_static = self.is_static;
            prop.in_file = self.in_file
            #add property to class
            self.other_class.add_member(prop)
        
        #setup name of method and its position in the property
        if self.is_getter:
            class_method.name = 'get' + property_name
            class_method.is_getter = True
            self.is_getter = False #transfer to other methods
            prop.getter = class_method
        elif self.is_setter:
            class_method.name = 'set' + property_name
            prop.setter = class_method
            class_method.is_setter = True
            self.is_setter = False #transfer to other methods
            #class_method.params[0].name = 'value'
        else:
            logger.error('Property is not a getter or a setter: %s - %s', self.name, property_name)
            assert False
        
        #change uname as well...
        class_method.uname = class_method.name
        class_method.in_property = prop
        
        self.in_property = None
    
    def _setup_class_method(self, class_method):
        '''
        Setup the class's method.
        
        The class method is a clone of the current method, in 
        a class wrapper. This comes from @method or @overload
        in the pascal source.
        
        Steps:
            1: clone self to class_method
            2: add it to its class or property
            3: alter args (add pointer field access)
        '''
        self.clone_to(class_method) #copy self into other
        class_method.is_class_method = True
        
        #if the class method is actually a property...
        if self.is_getter or self.is_setter:
            self.create_and_add_property(class_method)
        else:
            class_method.in_class.add_member(class_method) #add to its class
        
        #static methods and constructors directly call the method
        #  instance methods change the first argument for the pointer field
        if class_method.is_static or class_method.is_constructor:
            #use self's args - i.e. it calls the method in the same way
            args = list(self.args)
        else:  #other is an instance (with ptr)
            if self.args == None or len(self.args) < self.self_pos:
                logger.error('Class method calling a method without parameter for self pointer... ' + self.name)
                assert False
            
            args = list(self.args)
            #change the old argument for the self pointer
            if class_method.in_class.is_pointer_wrapper:
                args[self.self_pos - 1] = 'self.pointer'
            elif class_method.in_class.wraps_array:
                args[self.self_pos - 1] = 'self.data'
            else:
                args[self.self_pos - 1] = 'self'
            #add in self's arguments (-1st which is pointer)
        
        #set class method to call the same method this does
        class_method.calls(self.method_called, args)
        
        logger.debug('Method    : Setting up call: %s calls %s with args %s', class_method,
            class_method.method_called, class_method.args)
        
        class_method._check_args_match_params()
    
    def get_parameter(self, name):
        for par in self.params:
            if par.name == name:
                return par
        return None
        
    def get_variable(self, name):
        '''Get variable from parameters or locals'''
        for par in self.params:
            if par.name == name: return par
        for loc in self.local_vars:
            if loc.name == name: return loc
        return None
        
    
    def complete_method_processing(self):
        '''
        This is called on methods that are read by the parser from the 
        Pascal file.
        
        Set up the call from the library to this method if marked.
        Check the call's validity
        
        Steps:
            1: Get other methods related to this one
            2: Set parameters on library method (if called)
        '''
        logger.info(' Method    : Completing processing of %s', self)
        
        #This is 'the' method it has its params
        self.params = self.params
        
        #Find the length method if it exists
        if self.length_call != None:
            self.length_call = self.in_class.find_method(self.length_call)
        
        #Convert args to appropriate values...
        self._process_args()
        self._check_args_match_params()
        
        #Get other methods
        lib_method = self.method_called
        class_method = self.class_method
        
        #check rules
        if lib_method == None and not self.is_operator:
            logger.error('Method    : Found method %s without lib', self)
            assert False
            
        if lib_method != None:
            #set up library method
            self.setup_lib_method(lib_method)
        
            logger.info(' Method    : %s calls %s', self.name, lib_method.name)
            lib_method.called_by.append(self)
        
        #set up class method
        if class_method != None:
            logger.debug(' Method    : %s is also %s', self.name, class_method)
            self._setup_class_method(class_method)
            logger.info(' Method    : %s calls %s', class_method.name, lib_method.name)
            lib_method.called_by.append(class_method) #library is also called by class
        elif self.is_operator:
            assert self.other_class != None
            self.name = 'operator ' + self.name
            self.other_method = self.in_class.find_method(self['calls'].other)
            self.doc = self.other_method.doc
            self.method_called = self.other_method.method_called
            self.method_called.called_by.append(self) #library is also called by operator
            self.is_static = True
            
            self.in_class.operators[self.signature] = None
            self.in_class = self.other_class
            self.in_class.add_member(self)
            self.args = list(self.params) # operators must match directly
            
        # Cant check here... need to wait until all are read
        # self.check_call_validity()
        # class_method.check_call_validity()
    
    def _check_args_match_params(self):
        '''
        Ensure that the arguments in the call match the available parameters,
        if no arguments are provided copy across read in parameters
        '''
        method_called = self.method_called #self.tags['calls'].other[0] #get called method
        args = self.args #self.tags['calls'].other[1] #the arguments self passes to the method
        
        logger.debug('Method    : Checking arguments used by %s calling %s (%s)', self, method_called, args)
        
        if args == None:
            logger.error('Method    : No arguments that map to parameters for %s', self)
            assert False
        else:
            for arg in args:
                if isinstance(arg, SGParameter) and not self.has_parameter(arg.name):
                    logger.error("Cannot match parameter %s in call to %s from %s", str(arg), str(method_called), self)
                    assert False
    
    def check_arguments(self):
        '''
        Ensure that the arguments in the call match the available parameters,
        if no arguments are provided copy across read in parameters
        '''
        if self.method_called == None:
            logger.error('Method    : Method %s does not call anything. Check attributes. %s', self.uname, self.file_line_details)
            assert False
        
        method_called = self.method_called #get called method
        args = self.args #the arguments self passes to the method
        
        logger.debug('Method    : Checking arguments used by %s calling %s (%s)', self, method_called, args)
        
        if len(args) != len(method_called.params):
            logger.error('Method    : Error in %s calling %s', self.uname, method_called.uname)
            assert False
        
        for arg in args:
            if isinstance(arg, SGParameter):
                if not self.has_parameter(arg.name):
                    logger.error("Cannot match parameter %s in call to %s from %s", str(arg), str(method_called), self)
                    assert False
    
    def args_string_for_self(self, arg_visitor = None, lang_key = None):
        '''The arguments for calling self (used in C# to wrap for exception catching)'''
        alias = self.alias(lang_key)
        params = alias.params
        
        arg_list = [ a.arg_name(lang_key) if isinstance(a, SGParameter) else a for a in params ]
        # arg_list
        
        if arg_visitor != None:
            return ','.join([ arg_visitor(a, params[i], params[i]) for i,a in enumerate(arg_list) ])
        else:
            return ','.join(arg_list)
    
    def args_string_for_called_method(self, arg_visitor = None, lang_key = None):
        '''Get the list of arguments for a class to the library method... ie. the method called by this object'''
        #print self.method_called
        
        alias = self.alias(lang_key)
        called_alias = self.method_called.alias(lang_key)
        
        args = alias.args
        params = called_alias.params
        
        arg_list = [ a.arg_name(lang_key) if isinstance(a, SGParameter) else a for a in args ]
        if arg_visitor != None:
            if len(params) < len(args): return 'ARGS > PARAMS' #TODO: Look into
            return ', '.join([ arg_visitor(a, args[i], params[i]) for i,a in enumerate(arg_list) ])
        else:
            return ', '.join(arg_list)
    
    def param_string(self, param_visitor = None, lang_key = None):
        alias = self.alias(lang_key)
        params = alias.params
        
        if params: 
            if param_visitor == None:
                return ', '.join([str(n) for n in params])
            else:
                return ''.join([param_visitor(param, param == params[-1]) for param in params])
                #i.e. map(lambda param: param_visitor(param, param == self.params[-1]), self.params)
        else: return ''
    
    def __str__(self):
        if self.return_type != None:
            result = str(self.return_type.name) + ' '
        else:
            result = 'void '
        
        if self.in_class != None:
            result += self.in_class.name + '.'
        result += self.uname + '('
        result += self.param_string()
        result += ')'
        
        return result
    
    def visit_params(self, visitor, other):
        for param in self.params:
            visitor(param, param == self.params[-1], other)
    
    def visit_args(self, visitor, other):
        args = self.args
        for arg in args:
            visitor(arg, arg == args[-1], other)
            
    def has_const_params(self):
        for param in self.params:
            # print param.modifier, param.is_array(), ', ', 
            if (param.modifier == 'const' and not param.is_array()): 
                # print
                return True
        # print
        return False
        
    def has_var_params(self):
        for param in self.params:
            # print param.modifier, param.is_array(), ', ', 
            if param.modifier == 'var': 
                # print
                return True
        # print
        return False

    
    def has_out_params(self):
        for param in self.params:
            if param.modifier == 'out': return True
        return False

#
# Test methods
#

def test_method_creation():
    """test the creation of a basic SGMethod"""
    my_method = SGMethod("Test")
    
    assert my_method.name == "Test"
    assert len(my_method.params) == 0
    assert my_method.return_type == None

def test_static_methods():
    """test the creation of a static method"""
    my_method = SGMethod("Test")
    assert False == my_method.is_static
    
    my_method.is_static = True
    assert my_method.is_static

def test_constructor_methods():
    """test the creation of a constructor"""
    my_method = SGMethod("init")
    assert False == my_method.is_constructor
    
    my_method.is_constructor = True
    assert my_method.is_constructor

def test_return_types():
    """test the return type value"""
    my_method = SGMethod("Test")
    
    my_method.return_type = "SoundEffect"
    assert my_method.return_type == "SoundEffect"

def test_basic_method_call_wrapper():
    """test the creation of a simple method wrapper"""
    my_method = SGMethod("test")
    other_method = SGMethod("other")
    
    my_method.calls(other_method)
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called
    assert len(my_method.args) == 0

def test_wrapper_with_params():
    """test the creation of a method wrapper with one parameter in call"""
    my_method = SGMethod("test")
    par = my_method.create_parameter("par1")
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    
    my_method.calls(other_method)
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called
    assert len(my_method.args) == 1
    assert par == my_method.args[0]

def test_wrapper_with_args():
    """test the creation of a method wrapper with non-default args"""
    my_method = SGMethod("test")
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    
    my_method.calls(other_method, ['"test"'])
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called
    assert len(my_method.args) == 1
    assert par1 != my_method.args[0]
    assert '"test"' == my_method.args[0]
    

# @raises(Exception)
# def test_wrapper_with_missing_args():
#     """test the creation of a method wrapper with non-default args"""
#     my_method = SGMethod("test")
#     other_method = SGMethod("other")
#     par1 = other_method.create_parameter("par1")
#     par2 = other_method.create_parameter("par2")
#     
#     assert 2 == len(other_method.params)
#     
#     my_method.calls(other_method, ['"test"'])
#     my_method.check_call_validity();
# 
# @raises(Exception)
# def test_wrapper_with_wrong_args():
#     """test creation of a method wrapper with args with no matching params"""
#     my_method = SGMethod("test")
#     par = my_method.create_parameter("p1")
#     
#     other_method = SGMethod("other")
#     par1 = other_method.create_parameter("par1")
#     
#     my_method.calls(other_method, [par1])
#     my_method.check_call_validity();
# 
# @raises(Exception)
# def test_wrapper_missing_default_args():
#     """test the creation of a call with insufficient params to match args"""
#     my_method = SGMethod("test")
#     other_method = SGMethod("other")
#     par1 = other_method.create_parameter("par1")
#     
#     assert 2 == len(other_method.params)
#     
#     my_method.calls(other_method)
#     my_method.check_call_validity();

if __name__ == '__main__':
    import nose
    from nose.tools import raises
    
    nose.run()
