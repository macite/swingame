#!/usr/bin/python

import sys
import os
import platform
import subprocess
import swin_shutil
from swin_template_utils import *

def main():
    output_header(['Cleaning SwinGame Template Dist Locations'])
    
    clean_locs = [
            dist_folder + "Source/",
            dist_folder + "CPP/",
            dist_folder + "VB/",
            dist_folder + "Documentation/",
            swingame_path + "Generated/",
        ]
    
    deploy_files = deploy_list()
    
    for temp in deploy_files:
        clean_locs.append(temp)
    
    for key, dist in template_details.items():
        clean_locs.append(dist_folder + key)
    
    for loc in clean_locs:
        run_bash('rm', ['-rf', loc] )
    
    run_bash('svn', ['up', swingame_path + "Generated/"])
    
    print(  "\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
