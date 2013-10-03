#!/usr/bin/python

#
# Test how tos
#
# Steps:
# 1: Opens a Swingame How To
# 2: Executes the how to
# 3: Asks tester if program executes correctly
# 4: Asks tester if program  performed as expected

import os
import shutil
import subprocess
import sys
import glob
import platform
import posixpath 
from convert_utils import *

_enumLoad = [ {"load": "BitmapNamed"}]
_base_path = "../../Dist/HowTo/"
_base_path_Source_Code = "../../Dist/HowTo/Source_Code/"

#Getting OS Name
def get_os_name():
  osName = platform.system()
  if osName == "Darwin":
    return "Mac OS X"
  elif osName == "Linux":
    return "Linux"
  else:
    return "Windows"

def open_how_to():
    failed_list = []
    bash = ''
    if (get_os_name() == "Windows"):
      bash = 'bash ';

    for lang in languages:     
        path = _base_path + lang['lang']
        how_to_list = glob.glob((path + '//' + "HowTo*"))
        for how_to in how_to_list:
            print "*" * 70
            build_sh = posixpath.normpath("%s/build.sh" % how_to)
            build_sh = build_sh.replace("\\", "/")
            run_sh = posixpath.normpath("%s/run.sh" % how_to)
            run_sh = run_sh.replace("\\", "/")
            print " - Building %s" % build_sh #os.path.basename(how_to)
            if subprocess.call("%s./%s" % (bash, build_sh)) == 0:
                print "     - Build succeeded"
                # print " - Running %s " % run_sh #os.path.basename(how_to)
                # print "     - ", "%s./%s" % (bash, run_sh)
                # subprocess.call("%s./%s" % (bash, run_sh))
            else:
                print "     - Build failed."
                print "     - Removing %s" %how_to
                failed_list.append((os.path.basename(how_to), lang['lang']))
                shutil.rmtree(how_to)
    print '*' * 70
    print ' - Failed builds:'
    for name, lang in failed_list:
        print '     - %s : %s' %(lang, name)
    print '*' * 70


def main():
    open_how_to();
if __name__ == '__main__':
    main()