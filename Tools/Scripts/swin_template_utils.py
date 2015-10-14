import os
import sys
import platform
import subprocess
import swin_shutil

# IMPORTANT: Change version here... will copy itself where needed
sg_version         ="3.6"

script_path        = os.path.dirname(os.path.realpath(__file__)) + '/'
swingame_path      = os.path.realpath(script_path + '../..') + '/'
python_script_dir  = os.path.realpath(script_path + '../SGWrapperGen') + '/'

dist_folder        = swingame_path + "Dist/"
produced_folder    = dist_folder + 'SwinGame %s/' % sg_version
generated_folder   = swingame_path + "Generated/"
tempate_folder     = swingame_path + "Templates/"
coresdk_folder     = swingame_path + "CoreSDK/"
lib_folder         = coresdk_folder + "lib"
staticlib_folder   = coresdk_folder + "staticlib"

# library_paths = {
#     "staticlib" : {
#         "iOS"       : os.path.join(staticlib_folder, "godly", "ios"),
#         "Mac OS X"  : os.path.join(staticlib_folder, "godly", "mac"),
#     },
#     "lib" : {
#         "Mac OS X"  : os.path.join(lib_folder, "mac"),
#         "Windows"  : os.path.join(lib_folder, "win"),
#     }
# }
# 
# ====================
# = Helper functions =
# ====================

def get_os_name():
    """ Returns the name of the Operating System."""
    osName = platform.system()
    if osName == "Darwin":
        return "Mac OS X"
    elif osName == "Linux":
        return "Linux"
    else:
        return "Windows"

_has_output = False

def output_header(msgs):
    global _has_output
    if _has_output: 
        print ""
    _has_output = True
    print "--------------------------------------------------"
    for line in msgs:
        print '  ', line
    print "--------------------------------------------------"

def output_line(msg):
    print '  * ', msg


def run_python(script_name, base_path=python_script_dir):
    output_line('Running python script: ' + script_name)
    proc = subprocess.Popen(["python", base_path + script_name], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = proc.communicate()
    
    if proc.returncode != 0:
        print "Error running script: ", script_name
        print out, err
        quit()

def run_bash(script_name, opts):
    if get_os_name() == "Windows":
        exec_list = ["bash", script_name]
    else:
        exec_list = [script_name]
    
    if opts:
        if isinstance(opts, list):
            exec_list.extend(opts)
        else:
            exec_list.append(opts)
    
    output_line('Running bash script: ' + str(exec_list))
    
    proc = subprocess.Popen(exec_list, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = proc.communicate()
    
    if proc.returncode != 0:
        print "Error running script: ", script_name, " ", opts
        print out, err
        
        if (get_os_name() == "Windows"):
          print ("Make sure you have msys/bin in your environment PATH variable")
        
        quit()

def copy_without_git(src,dest,overwrite = True):
    """tree copy without git and copy symbolic links. Overwrite delete destination tree from root..."""
    
    if(os.path.isdir(dest) and overwrite):
        # print("    Cleaning %s" % dest)
        swin_shutil.rmtree(dest,ignore_errors = True)
        
    # print ("    Copying %s to %s" % (src,dest))
    swin_shutil.copytree(src,dest,symlinks = True,ignore =
        swin_shutil.ignore_patterns(".git*"))

def flat_copy_without_git(src, dest):
    """copy every file not in git into a flat destination."""
    # print("    Copying all files in %s to %s" % (src, dest))
    if(not os.path.isdir(dest)):
        os.mkdir(dest)

    for root, dirs, files in os.walk(src):
        for f in files:
            if(root.find('.git*') == -1):
                fullpath = os.path.join(root, f)
                swin_shutil.copy(fullpath,dest)

# ======================================================================
# = Language specific function called within the Template details dict =
# ======================================================================

def make_sh_exec(specificdist_folder, dist_dict):
  for filename in os.listdir(specificdist_folder):
    if filename[-3:] == ".sh":
      run_bash("chmod", ["a+x", specificdist_folder + filename])

def rename_c_to_cpp(specificdist_folder, dist_dict):
    """Rename the C files CPP."""
    output_line('Renaming C files CPP for C++')
    
    lib_folder = os.path.join(specificdist_folder, 'lib')
    # print lib_folder
    for filename in os.listdir(lib_folder):
        # print filename
        if filename.endswith(".c"):
            full_name = os.path.join(lib_folder, filename)
            os.rename(full_name, full_name[:-1] + 'cpp')

def build_csharp_lib():
    """The C# code has been generated, now turn it into a DLL"""
    output_line('Compiling .NET class library')

    dirs = {
        'cs_generated_lib_dir':     os.path.join(swingame_path, 'Generated','CSharp','lib'),
        'cs_generated_code_dir':    os.path.join(swingame_path, 'Generated','CSharp','Code'),
        'cs_lib_dir':               os.path.join(swingame_path, 'Templates','CSharp','Library')
    }
    
    if get_os_name() == "Windows":
        csc = ['csc', '-t:library', '-r:System.dll', '-unsafe', '-define:DEBUG', '-debug+', '-out:%(cs_generated_lib_dir)s\\SwinGame.dll' % dirs, '%(cs_lib_dir)s\\*.cs' % dirs, '%(cs_generated_code_dir)s\\*.cs' % dirs]
    else:
        csc = ['mcs', '-t:library', '-unsafe', '-r:System.dll', '-define:DEBUG', '-debug+', '-out:%(cs_generated_lib_dir)s/SwinGame.dll' % dirs, '%(cs_lib_dir)s/*.cs' % dirs, '%(cs_generated_code_dir)s/*.cs' % dirs]

    proc = subprocess.Popen(csc, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = proc.communicate()

    if proc.returncode != 0:
        print "Error compiling C# DLL"
        print out, err

        if (get_os_name() == "Windows"):
            print "Make sure you have csc in your environment PATH variable"
        else:
            print "Make sure you have gmcs in your path"
        quit()

    if dirs["cs_generated_code_dir"] != '':
        # path = dirs["cs_generated_code_dir"] + '/*.cs'
        [os.remove(os.path.join(dirs["cs_generated_code_dir"],f)) for f in os.listdir(dirs["cs_generated_code_dir"]) if f.endswith(".cs")]

def bndl_vsproj(specificdist_folder, dist_dict):
    """Package up the SwinGame C# project = move SGSDK.dll"""
    
    output_line('Moving dll in SwinGame C# project')
    
    sgsdk = os.path.join(specificdist_folder, 'lib', 'win', 'SGSDK.dll')
    sgsdk_dest = os.path.join(specificdist_folder, 'lib', 'SGSDK.dll')
    
    # print sgsdk, sgsdk_dest
    if os.path.exists(sgsdk):
      run_bash('mv', [sgsdk, sgsdk_dest] )
    else:
      print >> sys.stderr, 'Missing Windows DLL for C# projects'

def pkg_vs_installer(dist_dict, tmp_dir, to_dir):
    """Package up the SwinGame C# installer"""
    
    vs_temp_folder      = os.path.join(tempate_folder, 'Visual Studio', dist_dict['template_loc'])
    replace_file        = dist_dict['replace_file']
    replace_file_dir    = dist_dict['replace_dir']
    search_str          = dist_dict['search_for']
    replace_str         = dist_dict['replace_with']
    dest_tmp            = dist_dict['template_loc']
    proj_zip_name       = dist_dict['proj_zip_name']
    
    output_line('Creating Visual Studio Template Structure')
    
    # Replace 'MyGame' with '$safeprojectname$.src' in GameMain.cs
    # Replate 'Mono' with '$safeprojectname$ in Mono.vbproj
    o = open("New%s" % replace_file,"a") #open for append
    if replace_file_dir:
        game_main = os.path.join(to_dir, replace_file_dir, replace_file)
    else:
        game_main = os.path.join(to_dir, replace_file)
    
    # print game_main
    
    for line in open(game_main):
       line = line.replace(search_str,replace_str)
       o.write(line) 
    o.close()
    # print 'HERE!'
    run_bash('mv', ['New%s' % replace_file, game_main] )
    
    tmp_vs_dir = os.path.join(tmp_dir, 'Visual Studio', dest_tmp) + '/'
    if os.path.exists(to_dir + '/lib/win/SGSDK.dll'):
        run_bash('mv', [to_dir + '/lib/win/SGSDK.dll', to_dir + '/lib/SGSDK.dll'] )
    else:
        print >> sys.stderr, 'Missing Windows dll for Visual Studio Template'
    
    # Make the Visual Studio directory
    os.makedirs(tmp_vs_dir)
    
    # Copy in template files
    copy_without_git(vs_temp_folder, tmp_vs_dir)
    
    # Create the project zip
    os.chdir(to_dir)
    to_zip = tmp_vs_dir + proj_zip_name
    run_bash('zip', ['-q', '-r', '-y', to_zip, '.', '-x', '.DS_Store' ])
    
    # Zip it all together
    os.chdir(tmp_vs_dir)
    to_zip = produced_folder + dist_dict['pkg_name']
    output_line('Creating Template Installer: %s' % dist_dict['pkg_name'] )
    run_bash('zip', ['-q', '-r', '-y', to_zip, '.', '-x', '.DS_Store' ])

# def pkg_csharp_installer(dist_dict, tmp_dir, to_dir):
#     vs_temp_folder = os.path.join(tempate_folder, 'Visual Studio', 'Express C# 08')
#     pkg_vs_installer(tmp_dir, to_dir, 'GameMain.cs', 'src', 'MyGame', "$safeprojectname$.src", vs_temp_folder, 'cs', 'SwinGame C# Project.zip', 'C#')
#     
# def pkg_vb_installer(dist_dict, tmp_dir, to_dir):
#     vs_temp_folder = os.path.join(tempate_folder, 'Visual Studio', 'Express VB 08')
#     pkg_vs_installer(tmp_dir, to_dir, 'Mono.vbproj', None, 'Mono', "$safeprojectname$", vs_temp_folder, 'vb', 'SwinGame VB Project.zip', 'VB')

def pkg_rename_cpp(dist_dict, tmp_dir, to_dir):
    """rename the cpp files from c_blah to cpp_blah"""
    
    
# ===============================
# = Template details dictionary =
# ===============================


template_details = {
    'Pascal':   {
              'script':       'create_pascal_library.py',
              
              'use_sgsdk':    False,
              'libsgsdk':     False,
              
              'copy_dist':    [
                  { 
                    'target':         'fpc',
                    'source':         'FPC',
                    'os':             ['Mac OS X', 'Windows', 'Linux'],
                    'lib':            None,
                    'libs':           [
                                        ('staticlib/sdl2/mac','lib/mac'),
                                        ('lib/sdl2/win','lib/win'),
                                      ],
                    'post_copy':        make_sh_exec
                  },
                  {
                    'target':     'iOS',
                    'os':         ['iOS'],
                    'lib':        'staticlib',
                  },
              ],
              'pre_copy_script': None,
          },
      'C':    {
              'script':       'create_c_library.py',
              
              'use_sgsdk':    True,
              'libsgsdk':     False,
              
              'copy_dist':    [
                  { 
                    'lang':          'CPP',
                    'target':        'gpp',
                    'os':            ['Mac OS X', 'Windows', 'Linux'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      [make_sh_exec, rename_c_to_cpp]
                  },
                  { 
                    'lang':          'CPP',
                    'target':        'NetBeans',
                    'os':            ['Linux','Mac OS X','Windows'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      rename_c_to_cpp
                  },
                  { 
                    'lang':          'CPP',
                    'target':        'CodeBlocks-Linux',
                    'os':            ['Linux'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      rename_c_to_cpp
                  },
                  { 
                    'lang':          'CPP',
                    'target':        'CodeBlocks-Mac',
                    'os':            ['Mac OS X'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      rename_c_to_cpp
                  },
                  { 
                    'lang':          'CPP',
                    'target':        'CodeBlocks-Win',
                    'os':            ['Windows'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      rename_c_to_cpp
                  },
                  { 
                    'lang':          'CPP',
                    'target':        'Eclipse-Mac',
                    'os':            ['Mac OS X'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      rename_c_to_cpp
                  },
                  { 
                    'lang':          'CPP',
                    'target':        'Eclipse-Windows',
                    'os':            ['Windows'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      rename_c_to_cpp
                  },
                  { 
                    'lang':          'CPP',
                    'target':        'VisualStudio',
                    'os':            ['Windows'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      rename_c_to_cpp
                  },
                  { 
                    'lang':          'CPP',
                    'target':        'xcode 4',
                    'os':            ['Mac OS X'],
                    'lib':           'lib',
                    'staticsgsdk':    False,
                    'post_copy':      rename_c_to_cpp
                  },
                  { 
                    'target':     'gcc',
                    'os':         ['Mac OS X', 'Windows', 'Linux'],
                    'lib':        'lib',
                    'staticsgsdk':    False,
                    'post_copy':      make_sh_exec
                  },
                  { 
                    'target':     'xcode 3',
                    'os':         ['Mac OS X'],
                    'lib':        'lib',
                    'staticsgsdk':    False,
                  },
                  {
                      'target':       'xcode 4',
                      'os':           [ 'Mac OS X' ],
                      'lib':          'lib',
                      'staticsgsdk':  False,
                  },
                  { 
                    'target':       'iOS',
                    'os':           ['iOS'],
                    'lib':          'staticlib/godly',
                    'staticsgsdk':  True,
                  },
              ],
              'pre_copy_script': None,
          },
      'ObjC': {
              'script':       'create_objc_library.py',
              
              'use_sgsdk':    True,
              'libsgsdk':     False,
              
              'copy_dist':    [
                  {
                      'target':       'gcc',
                      'os':           [ 'Mac OS X' ],
                      'lib':          'lib',
                      'staticsgsdk':  False,
                      'post_copy':    make_sh_exec
                  },
                  {
                      'target':       'xcode 3',
                      'os':           [ 'Mac OS X' ],
                      'lib':          'lib',
                      'staticsgsdk':  False,
                  },
                  {
                      'target':       'xcode 4',
                      'os':           [ 'Mac OS X' ],
                      'lib':          'lib',
                      'staticsgsdk':  False,
                  },
              ],
              'pre_copy_script': None,
           },
    'CSharp':   {
            'script':       'create_csharp_library.py',
            'use_sgsdk':    True,
            'libsgsdk':     True,
            'copy_dist':    [
                { 
                  'source':         'Mono',
                  'target':         'mono',
                  'os':             [ 'Mac OS X', 'Windows', 'Linux' ],
                  'lib':            'lib',
                  'staticsgsdk':    False,
                  'post_copy':      make_sh_exec
                },
                { 
                  'source':         'WinCmd',
                  'target':         'WinCmd',
                  'os':             [ 'Mac OS X', 'Windows', 'Linux' ],
                  'lib':            'lib',
                  'staticsgsdk':    False,
                  'post_copy':      make_sh_exec
                },
                { 
                  'source':         'MonoDevelop',
                  'target':         'MonoDevelop',
                  'os':             [ 'Mac OS X', 'Windows', 'Linux' ],
                  'lib':            'lib',
                  'staticsgsdk':    False,
                },
                { 
                  'source':         'VS13',
                  'target':         'vs13',
                  'os':             [ 'Mac OS X', 'Windows', 'Linux' ],
                  'lib':            'lib',
                  'staticsgsdk':    False,
                },                { 
                  'source':         'XamarianStudio',
                  'target':         'XamarianStudio',
                  'os':             [ 'Mac OS X', 'Windows', 'Linux' ],
                  'lib':            'lib',
                  'staticsgsdk':    False,
                },
                { 
                  'target':         'vs08',
                  'source':         'VS08',
                  'os':             [ 'Windows' ],
                  'lib':            'lib/win',
                  'staticsgsdk':    False,
                  'pkg_script':     pkg_vs_installer,
                  'template_loc':   'Express C# 08',
                  'pkg_name':       'C# SwinGame %s 2008 Installer.vsi' % (sg_version),
                  'replace_file':   'GameMain.cs', 
                  'replace_dir':    'src', 
                  'search_for':     'MyGame', 
                  'replace_with':   "$safeprojectname$.src", 
                  'proj_zip_name':  'SwinGame C# Project.zip',
                },
                { 
                  'target':         'vs10',
                  'source':         'VS10',
                  'os':             [ 'Windows' ],
                  'lib':            'lib/win',
                  'staticsgsdk':    False,
                  'pkg_script':     pkg_vs_installer,
                  'template_loc':   'Express C# 10',
                  'pkg_name':       'C# SwinGame %s 2010 Installer.vsi' % (sg_version),
                  'replace_file':   'GameMain.cs', 
                  'replace_dir':    'src', 
                  'search_for':     'MyGame', 
                  'replace_with':   "$safeprojectname$.src", 
                  'proj_zip_name':  'SwinGame C# Project.zip',
                },
                { 
                  'target':         'vs10proj',
                  'source':         'VS10Proj',
                  'os':             [ 'Windows' ],
                  'lib':            'lib/win',
                  'staticsgsdk':    False,
                  'post_copy':    bndl_vsproj,
                },
                { 
                  'lang':           'VB',
                  'source':         'Mono',
                  'target':         'mono',
                  'os':             [ 'Mac OS X', 'Linux', 'Windows' ],
                  'lib':            'lib',
                  'staticsgsdk':    False,
                  'post_copy':      make_sh_exec
                },
                { 
                  'lang':           'VB',
                  'source':         'VS08',
                  'target':         'vs08',
                  'os':             [ 'Windows' ],
                  'lib':            'lib/win',
                  'staticsgsdk':    False,
                  'pkg_script':     pkg_vs_installer,
                  'template_loc':   'Express VB 08',
                  'pkg_name':       'VB SwinGame %s 2008 Installer.vsi' % (sg_version),
                  'replace_file':   'Mono.vbproj', 
                  'replace_dir':    '', 
                  'search_for':     'Mono', 
                  'replace_with':   "$safeprojectname$", 
                  'proj_zip_name':  'SwinGame VB Project.zip',
                },
            ],
            'pre_copy_script': build_csharp_lib,
        },

} # end _template_details

def deploy_list():
    """Returns a list of the files that need to be deployed to the server"""
    
    #hack...
    src_temp_path_name = 'Source of SwinGame %s' % sg_version
    src_temp_path_name = src_temp_path_name.replace(' ', '_').replace('.', '_') + ".zip"
    
    result = list()
    result.append(produced_folder + src_temp_path_name )
    
    for key, lang_dict in template_details.items():
        for dist_dict in lang_dict['copy_dist']:
            result.append(produced_folder + dist_dict['template_path_name'] + '.zip')
            if dist_dict.has_key('pkg_name'):
                result.append(produced_folder + dist_dict['pkg_name'])
    
    return result

def _setup_template_details():
    for key, lang_dict in template_details.items():
        for dist_dict in lang_dict['copy_dist']:
            dist_dict['template_name'] = '%s SwinGame %s %s' % (
                key if not dist_dict.has_key('lang') else dist_dict['lang'], 
                sg_version, 
                str.upper(dist_dict['target']) 
                )
            
            dist_dict['template_path_name'] = dist_dict['template_name'].replace(' ', '_').replace('.', '_')
    
    
    
_setup_template_details()

if __name__ == '__main__':
    print template_details
