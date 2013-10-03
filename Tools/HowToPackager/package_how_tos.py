#
# Package How Tos
#
# Steps:
# 1: Create How Tos directory in dist
# 2: Copy in the Swingame Template
# 3: Delete GameMain.pas file
# 4: Copy in the required resources into /resources/sub-folder
# 5: Copy in the HowTo.pas file to /src
# 6: package folder to zip ready for upload to website
#

import os
import swin_shutil
# from shutil import make_archive
from convert_utils import *

def build_dir_structure():
    """Build the How To structure in Dist folder"""
    print "  Removing old HowTo folder"
    swin_shutil.rmtree(get_how_to_directory())
    print "  Making new folder structure"
    os.mkdir(get_how_to_directory())
    
    for lang in languages:
        print "  -  Adding language", lang["lang"]
        os.mkdir(get_how_to_directory() + "/" + lang["lang"])
        os.mkdir (get_how_to_directory()+ "/Source_Code/" + lang['lang'])

def copy_how_to_template(how_to_name):
    print " - Adding", how_to_name
    
    # copy template to dir for each language
    for lang in languages:
        path = os.path.join(get_how_to_directory(), lang["lang"], how_to_name)
        print "   --", lang["lang"]
        # Copy template
        if os.path.exists(path):
            swin_shutil.rmtree(path)
        swin_shutil.copytree(lang["template"], path, symlinks=True)
        # Delete "main file" from "current_how_to_path"/src
        print "   -- Deleting GameMain file"
        os.remove(path + "/src/" + lang['main file'])
        print "   -- Deleting Godly and SDL13 lib files"
        swin_shutil.rmtree(path + "/lib/godly/")
        swin_shutil.rmtree(path + "/lib/sdl13/")

def copy_how_to_resources(current_how_to, how_to_resource, lang):
    path = "%s/%s" % ("../../CoreSDK/test/Resources",how_to_resource)
    dest = "%s/%s/%s/%s/%s" % (get_how_to_directory(), lang["lang"],current_how_to,"Resources",how_to_resource)
    if os.path.exists(path):
        print "     -- Copying ", path
        print "     -- To ",dest        
        swin_shutil.copy2(path,dest)
    else:
        print "   WARNING -- "+ path +" file does not exist"

def copy_current_how_to_pas_file():
    f = open("../../CoreSDK/test/HowToResources.txt")
    how_to_file_lines = f.readlines()
    f.close()
    for line in how_to_file_lines:
        # if line starts with *... its a new how to...
        if line[0] == "*":
            current_how_to = line[1:].strip()
            for lang in languages:
                path = "%s/%s%s" % (get_how_to_directory() + "/Source_Code/" + lang['lang'] + '/',current_how_to,lang['extension'])
                if os.path.exists(path):            
                    # copy the current how to file into the src directory        
                    dest = "%s/%s/%s/%s/%s" % (get_how_to_directory(), lang["lang"],current_how_to,"src",lang['main file'])
                    print " - Copying GameMain"
                    print "   -- Copying ", path
                    print "   -- To ",dest        
                    swin_shutil.copy2(path,dest)
                else:
                    print "   WARNING -- "+ current_how_to +" file does not exist"
      
def create_templates():
    """
    Copy the language templates to how tos folders for each how to document
    and copy the resources for the how to into the template
    """
    
    print "  Creating How To Templates"
    
    if not os.path.exists("../../CoreSDK/test/HowToResources.txt"):
        from create_resources_text_script import  create_list
        create_list()
        
    f = open("../../CoreSDK/test/HowToResources.txt")
    how_to_file_lines = f.readlines()
    f.close()
    
    current_how_to = None
    
    for line in how_to_file_lines:
        # if line starts with *... its a new how to...
        if line[0] == "*":
            current_how_to = line[1:].strip()
            copy_how_to_template(current_how_to)
        else:
            # its a resource for the current_how_to
            # copy from ../../CoreSDK/test/Resources/"from file" to _base_path/lang/how_toname/Resources/"from file"
            
            for lang in languages:
                current_how_to_resource = line.strip()
                #only pascal available at this time 27 JAN 2012 so must alter to include language when other languages 
                #become available to copy resources WBUCHNER
                #path = "%s/%s/%s/%s" % (_base_path, lang["lang"],"CoreSDK/test/Resources",current_how_to_resource)
                copy_how_to_resources(current_how_to, current_how_to_resource, lang)

def main():
    #build_dir_structure()
    create_templates()
    copy_current_how_to_pas_file()
    #zip_how_to()
if __name__ == '__main__':
    main()