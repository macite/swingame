#!/usr/bin/python

import sys
import os
import platform
import subprocess
import swin_shutil
from swin_template_utils import *

def generate_source_tree():
    generated_folder = [
        "Generated",
        "Generated/C",
        "Generated/C/lib",
        "Generated/CSharp",
        "Generated/CSharp/Code",
        "Generated/CSharp/lib",
        "Generated/Documentation",
        "Generated/Documentation/html",
        "Generated/Documentation/sql",
        "Generated/ObjC",
        "Generated/ObjC/lib",
        "Generated/Pascal",
        "Generated/Pascal/lib",
        "Generated/Python",
        "Generated/Source",
        "Generated/Source/src",
    ]

    if len(swingame_path) > 0: 
        for folder in generated_folder:
            os.mkdir( os.path.join(swingame_path, folder) )

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
    
    generate_source_tree()
    
    print(  "\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
