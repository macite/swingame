#!/bin/bash

#
# This script contains the sh script functions related to copying
# files without .svn details for SwinGame
#

# copy files from $1 to $1 without copying SVN details
copyWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 -type d ! -path \*.svn\* ! -path \*/. -exec sh -c "if [ ! -d \"${TO_DIR}/{}\" ] ; then mkdir \"${TO_DIR}/{}\" ; fi" \;
    
    # Copy files and links
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec sh -c "cp -R -p \"{}\" \"${TO_DIR}/{}\""  \;
}

# copy frameworks from $1 to $1 without copying SVN details
copyFrameworksWithoutSVN()
{
    FROM_DIR=$1
    TO_DIR=$2
    
    cd "${FROM_DIR}"
    
    # Create directory structure
    find . -mindepth 1 ! -path \*.svn\* ! -path \*/. -type d -path \*.framework\* -exec mkdir "${TO_DIR}/{}" \;
    # Copy files
    find . ! -path \*.svn\* ! -name \*.DS_Store ! -type d -exec cp -R -p {} "${TO_DIR}/{}"  \;
}

# Copy the dist directories from $1 array
DoCopy()
{
    COPY_LIST=$1
    
    for arg in "${COPY_LIST[@]}"; do
        name=`echo $arg | awk -F"," '{print $1}'`
        from=`echo $arg | awk -F"," '{print $2}'`
        to=`echo $arg | awk -F"," '{print $3}'`
        
        if [ ! -d "${to}" ] ; then
            mkdir -p "${to}"
        fi
        
        echo "  ... Copying to $name"
        copyWithoutSVN "$from" "$to"
    done
}
