#!/usr/bin/python

import subprocess
import shutil
from shutil import make_archive
import glob
import os
from convert_utils import *
from bash import *

# This script is responsible for preparing the files for use by the parser and the bundler
# Steps:
# 1. Prepare How To directory
#       - Clean
#       - Create new
# 2. Copy resources needed by the parser
#       - HowTos
#       - SwinGame library
# 3. Generate resource list
# 4. Parse HowTo files
# 5. Generate templates
# 6. Test HowTo's - remove HowTo's that do not build
# 7. Archive HowTo's

def prepare_directories():
    if os.path.exists(get_how_to_directory()):
        print " Cleaning HowTo folder:"
        # shutil.rmtree(get_how_to_directory())
        run_bash('rm', ['-rf', get_how_to_directory()])
        # removeall(get_how_to_directory())
        # os.rmdir(get_how_to_directory())
    print "  Making:    HowTo folder"
    os.mkdir(get_how_to_directory())
    print "  Making:    Source_Code folder"
    os.mkdir(get_how_to_directory() + 'Source_Code/')
    os.mkdir(get_how_to_directory() + 'Source_Code/HowTos')
    os.mkdir(get_how_to_directory() + 'Source_Code/HowTos/lib/')

    print "  Copying Pascal library for parser"
    source = get_dist_directory() + "Pascal/FPC/lib/"
    destination = get_how_to_directory() + "Source_Code/HowTos/lib/"

    print " - Source:       ", source
    print " - Destination:  ", destination
    
    # shutil.copytree(source)
    
    files_to_copy = glob.glob(os.path.join(source, "sg*.pas"))
    files_to_copy.append(source + "/SwinGame.pas")
    
    for fname in files_to_copy:
        # print " Copying:    " + os.path.basename(fname)
        shutil.copy2(fname, destination)

def zip_how_tos():
    for lang in languages:
        arch_path = os.path.join(get_how_to_directory(), lang["lang"], "Archive")
        if not os.path.exists(arch_path):
            os.mkdir(arch_path)
    
    f = open("../../CoreSDK/test/HowToResources.txt")
    how_to_file_lines = f.readlines()
    f.close()
    for line in how_to_file_lines:
       print '.',
       # if line starts with *... its a new how to...
       if line[0] == "*":
           current_how_to = line[1:].strip()
           for lang in languages:
                root_dir = os.path.expanduser(os.path.join(get_how_to_directory(), lang["lang"], current_how_to))
                how_to_src = root_dir + "/src/" + lang['main file']
                
                print " - Checking How To src file: %s" % how_to_src
                if os.path.exists(how_to_src):
                    path = os.path.join(get_how_to_directory(), lang["lang"], "Archive", current_how_to)
                    print "   -- Creating Archive "
                    print "     - ", current_how_to
                    # make_archive(path, 'zip',root_dir)
                    os.chdir(root_dir)
                    to_zip = path + '.zip'
                    print to_zip
                    run_bash('./clean.sh', [])
                    run_bash('zip', ['-q', '-r', '-y', to_zip, '.', '-x', '.DS_Store' ])
                else:
                    print " - Skipping: %s" % how_to_src
                    print "     - Unable to find source file"
       print

def copy_how_tos():
    """
    Copies all the How To documents from the source directory (CoreSDK/test)
    to Dist/HowTo/Source/HowTos

    How To files are identified by the regex HowTo*.pas
    """

    source = get_test_directory()
    destination = get_how_to_directory() + "/Source_Code/HowTos"

    if not os.path.exists(destination):
        os.makedirs(destination)

    if not os.path.exists(source):
        print "Source path does not exist: %s" %source
        assert False
    
    # print "*" * 70
    # print "Copying How Tos:"
    # print "-- from      %s" % source
    # print "-- To        %s" % destination
    # print "*" * 70
    # all the HowTo's that start with HowTo and are a pascal file
    
    for fname in glob.glob(os.path.join(source, "HowTo*.pas")):
        shutil.copy2(fname, destination)
        # print "Copied: %s" %os.path.basename(fname)
        print '.',
    print
    # print "*" * 70

def parse_how_tos():
    """
    Calls the pascal parser to parse the how to files
    Files are moved into Dist/HowTo/Source_Code/%(Lang_name)
    """
    print "*" * 70
    print "Parsing How Tos:"
    print "*" * 70
    if subprocess.call(["python", get_parser_directory() + "pas_parser.py"]) != 0:
        print "Error parsing How To files."
        assert False

def generate_resource_list():
    """
    This method produces a list of resources used to create the template files.
    The resource_list is put in Dist/HowTo/Source_Code
        - The list should be the same between languages
    """
    print '*' * 70
    print " Generating resource list: "
    print '*' * 70
    if subprocess.call(["python", "create_resources_text_script.py"]) == 0:
        resource_list = get_test_directory() + "/HowToResources.txt"
        shutil.copy2(resource_list, get_how_to_directory() + "/Source_Code")
    else:
        print " Error copying files..."
        quit()

def generate_templates():
    """
    Generates and populates all the template files
    """
    print '*' * 70
    print " Generating templates: "
    print '*' * 70
    if subprocess.call(["python", "package_how_tos.py"]) != 0:
        print " Error packaging how tos"
        quit()
    
def build_all_how_tos():
    """

    """
    print '*' * 70
    print " Building How Tos"
    print '*' * 70
    if subprocess.call(["python", "test_how_tos.py"]) != 0:
        print " Error building How Tos"
        quit()

def main():
    prepare_directories()
    generate_resource_list()
    copy_how_tos()
    parse_how_tos()
    generate_templates()
    build_all_how_tos()
    zip_how_tos()
    
    print "*" * 70
    print "Finished generating how tos"
    print "*" * 70
if __name__ == '__main__':
    main()
