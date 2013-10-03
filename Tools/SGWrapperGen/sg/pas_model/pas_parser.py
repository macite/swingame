import logging
import sys
import os
import glob

from pas_file import PascalFile
from pascal_parser.pas_parser_utils import raise_error, logger
from pas_converter import run_convert
import converter_helper
from pas_file_cache import add_file, get_file_named, files

def change_file_extension(fileName, new_extension):
    '''
    returns a string with a new file type
    eg. change_file_extension (PascalTest.pas, '.c')
    will return "PascalTest.c"
    '''
    base = os.path.basename(fileName).split('.')[0]
    return (base + new_extension)

def write_file(file_data, destination):
    tabs = 0
    for (name, module) in converter_helper.converters.items():
        dest = os.path.normpath(destination + "/" + module.proper_name + '/')
        filename = change_file_extension(dest + '/' + file_data.filename, module.extension)
        print "Writing file %s to:          %s" %(filename, dest)

        if not os.path.exists(dest):
            os.makedirs(dest)

        file = open(dest + '/' + filename, "w")
        file.write(file_data.code[name])
        file.close()

def run():
    main()

def main():
    logging.basicConfig(level=logging.INFO,
                format='%(asctime)s - %(levelname)s - %(message)s',
                stream=sys.stdout)

    import c_lib
    import pas_lib
    import converter_helper

    converter_helper.converters["c_lib"] = c_lib
    converter_helper.converters["pas_lib"] = pas_lib
    
    script_path     = os.path.realpath(__file__) + '/'
    swingame_path   = os.path.realpath(script_path + '../../../../../') + '/'
    destination     = os.path.join(swingame_path, 'Dist', 'HowTo', 'Source_Code' )
    source          = os.path.join(destination, 'HowTos')
    lib_source      = os.path.join(source, 'lib')
    
    # destination = os.path.normpath("../../Dist/HowTo/Source_Code")

    print "Current Directory:                   %s" %os.getcwd()
    print "Source Directory:                    %s" %source
    print "Library Source Directory:            %s" %lib_source
    print "Destination Directory:               %s" %destination
    print '*' * 70
    
    if not os.path.exists(source):
        raise_error("Source directory does not exist %s" %source, '', is_critical=False)
    if not os.path.exists(lib_source):
        raise_error("Library directory does not exist %s" %lib_source, '', is_critical=False)
    if not os.path.exists(destination):
        raise_error("Destination directory does not exist %s" %destination, '', is_critical=False)

    
    print " Adding Units:"
    print '*' * 70   
    
    # add units in the lib_source directory to the file list
    add_file(PascalFile.create_unit_from('System', None, ['LongInt', 'Byte', 'String', 'Single', 'Pointer', 'LongWord', 'int64', 'Word', 'Integer', 'Boolean'], None))
    add_file(PascalFile.create_unit_from('SysUtils', None, None, None))
    dir_contents = glob.glob(os.path.join(lib_source, "*.pas"))
    if len(dir_contents) > 0:
        for fname in dir_contents:
            try:
                add_file(PascalFile(fname))
            except Exception:
                print " Error adding unit: %s", fname
    else:
        print "Library directory was empty: %s" %lib_source
    print '*' * 70
    print " Adding Programs:"
    print '*' * 70
    # adds files in the source directory to the file list
    dir_contents = glob.glob(os.path.join(source, "*.pas"))
    if len(dir_contents) > 0:
        for fname in dir_contents:
            add_file(PascalFile(fname))
    else:
        print 
        raise_error("Source directory was empty: %s" %source, '', is_critical=True)
    
    print '*' * 70
    print " Parsing files:"
    print '*' * 70
    for (name, file) in files().items():
        if (not file.is_parsed) and (file.contains_kind == 'program'):
            file.parse()
    
    print '*' * 70
    print ' Converting files:'
    print '*' * 70
    for (name, file) in files().items():
        if file.is_parsed and file.contains_kind == 'program':
            run_convert(file)

    print '*' * 70
    print ' Writing files: '
    print '*' * 70
    for (name, file) in files().items():
        if file.is_parsed and file.contains_kind == 'program':
            write_file(file, destination)
        print ".",
    print ' Done.'

if __name__ == '__main__':
    main()
  