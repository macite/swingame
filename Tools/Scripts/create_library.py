#!/usr/bin/python

import sys
import os
import platform
import subprocess
import swin_shutil
from swin_template_utils import *

# ==========================
# = Create SGSDK libraries =
# ==========================

def rewrite_file(file, pattern, replace_with):
    f = open(file)
    lines = f.readlines()
    f.close()

    new_file = open(file,'w')
    for line in lines:
        new_file.write(line.replace( pattern, replace_with))
    new_file.close

#
# Version data in sgShared.pas and build.sh need to be corrected
#
def rewrite_versions():
    dist_source_folder = dist_folder + "Source/"

    sgShared = dist_source_folder + "src/sgShared.pas"
    rewrite_file(sgShared, "DLL_VERSION = 'TEST BUILD';", "DLL_VERSION = '%s';" % sg_version)

    build = dist_source_folder + "build.sh"

    rewrite_file(build, "VERSION_NO=3.0", "VERSION_NO=%s" % sg_version)
    rewrite_file(build, "VERSION=3.0", "VERSION=%s" % sg_version)


# for compiling SGSDK
def copy_coresdk_to_dist_source():
    generated_source_folder =   generated_folder + "Source/src/"
    template_source_folder =    tempate_folder + "Source/"
    lib_src_folder =            swingame_path + "CoreSDK/libsrc/"
    src_folder =                swingame_path + "CoreSDK/src/"
    lib_folder =                swingame_path + "CoreSDK/lib/"
    staticlib_folder =          swingame_path + "CoreSDK/staticlib/"

    dist_source_folder =            dist_folder + "Source/"
    dist_source_src_folder =        dist_source_folder + "src/"
    dist_source_lib_folder =        dist_source_folder + "lib/"
    dist_source_staticlib_folder =  dist_source_folder + "staticlib/"

    copy_without_git(template_source_folder, dist_source_folder)
    copy_without_git(lib_folder, dist_source_lib_folder, overwrite = False)
    copy_without_git(staticlib_folder, dist_source_staticlib_folder, overwrite = False)
    flat_copy_without_git(generated_source_folder, dist_source_src_folder)
    flat_copy_without_git(lib_src_folder, dist_source_src_folder)
    flat_copy_without_git(src_folder, dist_source_src_folder)


_sgsdk_creation_script_options = {
    # 'Mac OS X': [ ['-IOS'], ['-badass','-static'], ['-godly','-static'], ['-static'], ['-badass','-framework'], ['-godly','-framework'], [ '-framework' ]],     # default must be last (for framework creation)
    'Mac OS X': [ [ '-framework' ], None, ['-static']],     # default must be last (for framework creation)
    'Windows':  [ [ '-win32' ], [ '-win64' ] ],
    'Linux':  [None],
}

def create_sgsdk_library():
    """ The first step in creating the SwinGame templates is to create the
        SGSDK framework/dll. This needs to be created for each of the various
        backend versions supported by the platform."""

    sgsdk_build_script = swingame_path + 'Dist/Source/build.sh'

    output_header(['Generating the code for SGSDK.pas'])
    run_python('create_sgsdk_code.py')

    output_line('Copying code to dist source directory')
    copy_coresdk_to_dist_source()

    output_line('Setting version to %s' % sg_version )
    rewrite_versions()

    output_header(['Compiling SGSDK framework/dll'])
    option_list = _sgsdk_creation_script_options[get_os_name()]

    for opt in option_list:
        run_bash(sgsdk_build_script, opt)


if __name__ == '__main__':
    output_header(['Packaging SwinGame Templates'])

    # Create the framework/dll
    create_sgsdk_library()
