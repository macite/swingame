import sys
import os
import platform
import subprocess
import swin_shutil
from swin_template_utils import *

# =========================================
# = Create the various language libraries =
# =========================================

def create_lang_libraries():
  for key, lang_template_dict in template_details.items():
      output_header(['Creating code for %s' % key])
      
      run_python(lang_template_dict['script'])
      
      if lang_template_dict['pre_copy_script']:
          lang_template_dict['pre_copy_script']()
      
      for copy_dist in lang_template_dict['copy_dist']:
          output_line("Packaging " + copy_dist["target"])
          assemble_dist(key, copy_dist, lang_template_dict['use_sgsdk'], None if not copy_dist.has_key('lang') else copy_dist['lang'], lang_template_dict['libsgsdk'])

def create_docs():
    run_python('create_documentation.py')
    
    docs_gen_dir = os.path.join(swingame_path, "Generated", "Documentation")
    docs_dist_dir = os.path.join(swingame_path, "Dist", "Documentation")
    
    if os.path.exists(docs_dist_dir):
        swin_shutil.rmtree(docs_dist_dir)
    
    os.makedirs(docs_dist_dir)
    
    copy_without_git(docs_gen_dir, docs_dist_dir)


#assembles the files for the dist folder
def assemble_dist(language, dist_dict, use_sgsdk, part_from, use_dylib):
    coresdk_folder =            swingame_path + "CoreSDK/"
    lib_src_folder =            swingame_path + "CoreSDK/libsrc/"
    src_folder =                swingame_path + "CoreSDK/src/"
    generated_folder =          swingame_path + "Generated/%s/lib/" % language
    template_folder =           swingame_path + "Templates/"
    langdist_folder =           swingame_path + "Dist/%s/" % (language if not part_from else part_from)
        
    common_template_folder =        template_folder + "Common/"
    lang_template_folder =          template_folder + (language if not part_from else part_from) +'/'
    
    source = dist_dict['target'] if not dist_dict.has_key('source') else dist_dict['source']

    common_lang_template_folder =   lang_template_folder + "Common/"
    specific_template_folder =      lang_template_folder + source + '/'
    
    specificdist_folder =           langdist_folder + dist_dict['target'] + '/'
    specific_dist_lib_folder =      specificdist_folder + "lib/"
    
    copy_lib_dir =  dist_dict["lib"]
    
    #clean dist folder
    # print("\n  Copying common files...")
    copy_without_git(common_template_folder, specificdist_folder)
    
    # print("\n  Copying %s common files..." % language)
    # print common_lang_template_folder
    copy_without_git(common_lang_template_folder, specificdist_folder, overwrite = False)
    
    # print("\n  Copying %s specific files..." % name)
    copy_without_git(specific_template_folder, specificdist_folder, overwrite = False)
    
    # print("\n  Copying %s generated files..." % language)
    copy_without_git(generated_folder, specific_dist_lib_folder, overwrite = False)
    
    # print("\n  Copying lib files...")
    if copy_lib_dir:
        lib_folder =    coresdk_folder + copy_lib_dir
        copy_without_git(lib_folder, specific_dist_lib_folder, overwrite = False)
    else:
        for lib_from, lib_to in dist_dict['libs']:
            lib_from_full = os.path.join(coresdk_folder, lib_from)
            lib_to_full = os.path.join(specificdist_folder, lib_to)
            
            # print("\n Copying %s to %s " % (lib_from_full, lib_to_full))
            copy_without_git(lib_from_full, lib_to_full, overwrite = False)
    
    if language == "Pascal":
        flat_copy_without_git(lib_src_folder, specific_dist_lib_folder)
        flat_copy_without_git(src_folder, specific_dist_lib_folder)
    
    # print("--------------------------------------------------")
    
    if use_sgsdk:
        # print "copying library"
        dist_source_folder = swingame_path + "Dist/Source/"
        
        if "Windows" in dist_dict['os']:
            if not os.path.exists(dist_source_folder+"bin/win"):
                print >> sys.stderr, 'Missing Windows dll from', dist_source_folder+"bin/win"
            else:    
                copy_without_git(dist_source_folder+"bin/win", specificdist_folder+"lib/win", overwrite = False)
            
        if "iOS" in dist_dict['os']:
            if dist_dict['staticsgsdk']:
                # Copy staticlibs
                if not os.path.exists(dist_source_folder+"bin/ios/libSGSDK.a"):
                    print >> sys.stderr, 'Missing static libraries for mac'
                else:
                    swin_shutil.copyfile(dist_source_folder+"bin/ios/libSGSDK.a", specificdist_folder+"lib/ios/libSGSDK.a")
            

        if "Mac OS X" in dist_dict['os']:
            #
            # NASTY HACK... make this neater at some stage
            #
            if not os.path.exists(specificdist_folder+"lib/mac"):
                os.makedirs(specificdist_folder+"lib/mac")

            if dist_dict['staticsgsdk']:
                # Copy staticlibs
                if not os.path.exists(dist_source_folder+"bin/mac/sgsdk-sdl2.a"):
                    print >> sys.stderr, 'Missing static libraries for mac'
                else:
                    print >> sys.stderr, 'Static libraries should be for iOS only!'
                    assert false
                    
                    swin_shutil.copyfile(dist_source_folder+"bin/mac/sgsdk-sdl2.a", specificdist_folder+"lib/mac/libSGSDK.a")
            elif use_dylib:
                if not os.path.exists(dist_source_folder+"bin/mac/libSGSDK.dylib"):
                    print >> sys.stderr, 'Missing dynamic libraries for mac'
                else:
                    swin_shutil.copyfile(dist_source_folder+"bin/mac/libSGSDK.dylib", specificdist_folder+"lib/mac/libSGSDK.dylib")
            else:
                if not os.path.exists(dist_source_folder+"bin/mac/SGSDK.framework"):
                    print >> sys.stderr, 'Missing SwinGame framework for mac'
                else:
                    # Copy framework
                    copy_without_git(dist_source_folder+"bin/mac/SGSDK.framework", specificdist_folder+"lib/mac/SGSDK.framework")

    if dist_dict.has_key('post_copy'):
        if isinstance(dist_dict['post_copy'], list):
            for proc in dist_dict['post_copy']:
                proc(specificdist_folder, dist_dict)
        else:
            dist_dict['post_copy'](specificdist_folder, dist_dict)

def main():
    output_header(['Packaging SwinGame Templates'])
    
    create_lang_libraries()
    create_docs()
    
    print("\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
