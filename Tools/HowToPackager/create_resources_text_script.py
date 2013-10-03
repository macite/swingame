import os
import re
import shutil
from convert_utils import *
import glob
_load = ["LoadAnimationScriptNamed","LoadAnimationScript","LoadMusic","LoadMusicNamed","LoadSoundEffect","LoadSoundEffectNamed","LoadCharacter","LoadCharacterNamed","LoadBitmap","LoadBitmapWithTransparentColor","LoadBitmapNamed","LoadFont","LoadFontNamed","LoadResourceBundle","LoadResourceBundleWithProgress","LoadResourceBundleNamed","LoadTransparentBitmap","LoadTransparentBitmapNamed", "LoadPanel"]
_pkg  = {
    "TIMER":"timer",
    "ANIM":"animations",
    "LoadAnimationScriptNamed":"animations",
    "LoadAnimationScript":"animations",
    "LoadResourceBundle":"bundles",
    "LoadResourceBundleWithProgress":"bundles",
    "LoadResourceBundleNamed":"bundles",
    "LoadResourceBundle":"bundles",
    "LoadResourceBundleWithProgress":"bundles",
    "LoadResourceBundleNamed":"bundles",
    "LoadMusic":"sounds",
    "LoadMusicNamed":"sounds",
    "LoadSoundEffect":"sounds",
    "BITMAP":"images",
    "SOUND":"sounds",
    "MUSIC":"sounds",
    "LoadSoundEffectNamed":"sounds",
    "LoadCharacter":"images",
    "CHARACTER":"images",
    "LoadCharacterNamed":"images",
    "LoadBitmapNamed":"images",
    "LoadBitmap":"images",
    "LoadBitmapWithTransparentColor":"images",
    "LoadTransparentBitmap":"images",
    "LoadTransparentBitmapNamed":"images",
    "FONT":"fonts",
    "MAP":"maps",
    "LoadFont":"fonts",
    "LoadFontNamed":"fonts",
    "PANEL":"panels",
    "LoadPanel":"panels"}

def get_Resources(word_list,f):
    # print "  --> "+_pkg[word_list[0]]+'/'+word_list[-1]
    f.write(_pkg[word_list[0]]+'/'+word_list[-1]+'\n')
    if _pkg[word_list[0]] == 'bundles':
        # print "Bundled word "+_pkg[word_list[0]]
        load_Bundled_Resources(word_list, f)
    

def get_Resources_Next(word_list, f):
    # print _pkg[word_list[1]]+'/'+word_list[-1]
    f.write(_pkg[word_list[1]]+'/'+word_list[-1]+'\n') 

def load_Bundled_Resources(word_list, f):
    bundle_file =  get_test_resource_directory() + "" + _pkg[word_list[0]] + "/" + word_list[-1]
    bun = open(bundle_file,'r')
    lines_in_bundles_file = bun.readlines()
    for line in lines_in_bundles_file:
        word_list = re.findall(r"[A-Za-z_.-]+",line)
        if _pkg[word_list[0]] == "timer":
            # print "TEST word_list"+_pkg[word_list[0]]
            # print "TIMER FOUND -- passing"
            pass
        else:
            f.write(_pkg[word_list[0]].lower()+ "/" + word_list[-1]+'\n')
            # print _pkg[word_list[0]].lower()+ "/" + word_list[-1]

def load_animation_script_resources(word_list,f):
    resource = []
    bundle_file =  get_test_resource_directory() + "" + _pkg[word_list[0]] + "/" + word_list[2]
    bun = open(bundle_file,'r')
    lines_in_bundles_file = bun.readlines()
    for line in lines_in_bundles_file:
        word_list = re.findall(r"[A-Za-z_.-]+",line)
        if (len(word_list) > 2) and (word_list[0] == "s"):            
            if len(resource) < 1:
                resource.append(word_list[2])
            elif resource[-1] != word_list[2]:
                resource.append(word_list[2])
    for sound in resource:
        f.write("sounds/"+sound+'\n') 
    
def load_panels_resources(word_list,f):
    bundle_file =  get_test_resource_directory() + "panels/" + word_list[2]
    bun = open(bundle_file,'r')
    lines_in_bundles_file = bun.readlines()
    for line in lines_in_bundles_file:
        word_list = re.findall(r"[A-Za-z_.-]+",line)
        if line[0] == 'a' or line[0] == 'b' or line[0] == 'i':
            f.write("images/"+word_list[1]+'\n')
               
def create_list():
    f = open(get_test_directory() + "HowToResources.txt",'w')
    dirList = glob.glob(os.path.join(get_test_directory(), "HowTo*.pas"))
    for fname in dirList:
        newFileName = os.path.basename(fname).split('.')[0]
        fileName = "* "+ newFileName
        f.write(fileName+'\n')
        # print " --Reading :"+fname
        how_to_file = open(fname,'r')
        lines_in_pas_file = how_to_file.readlines()
        
        for line in lines_in_pas_file:
            word_list = re.findall(r"[A-Za-z_0-9.-]+",line)
            if len(word_list) > 0:        
                if word_list[0] in _load or (len(word_list) >= 2 and word_list[1] in _load):
                    if (word_list[0] == _load[0]): load_animation_script_resources(word_list,f)
                    if 'LoadPanel' in word_list: load_panels_resources(word_list,f)
                    if word_list[0] in _pkg:
                        get_Resources(word_list,f)
                    elif (len(word_list) >= 2 and word_list[1] in _pkg):
                        get_Resources_Next(word_list,f)
        how_to_file.close()
    f.close()
                        
def main():
    create_list()

if __name__ == '__main__':
    main()