#!/usr/bin/python

# ==========================================================
# = Everything is ready... clean, bundle, package, deploy! =
# ==========================================================

import sys
import os
import platform
import subprocess
import swin_shutil
from swin_template_utils import *

def main():
    output_header(['Go, Go, GO!'])
    output_line('Use this script when all bundles are ready to deploy')
    
    output_line('Cleaning')
    run_python('clean_all.py', script_path)
    
    output_line('Creating library')
    run_python('create_library.py', script_path)
    
    output_line('Copy in libraries from other OS (win/mac)')
    
    read = raw_input('Enter Y to continue: ')
    if read != 'Y': exit(-1)
    
    output_line('Bundling templates')
    run_python('bundle_templates.py', script_path)
    output_line('Producing zip files, and other packages')
    run_python('produce_templates.py', script_path)
    
    read = raw_input('Recreate How-Tos?: ')
    if read == 'Y' or read == 'y':
        run_python('run_convert_and_package.py', script_path + '../HowToPackager')
    
    
    output_line('Deploying')
    run_python('deploy.py', script_path)
    
    print("\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
