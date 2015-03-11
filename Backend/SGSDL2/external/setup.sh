#! /bin/sh

echo "--------------------------------------------------"
echo "       Fetching External Libraries for SGSDL2 "
echo "--------------------------------------------------"

DoExit ()
{ 
    echo "An error occurred while fetching external libraries"; 
    cat setup.log    
    exit 1;
}



# Make sure we are in the scripts directory
APP_PATH=`echo $0 | awk '{split($0,patharr,"/"); idx=1; while(patharr[idx+1] != "") { if (patharr[idx] != "/") {printf("%s/", patharr[idx]); idx++ }} }'`
APP_PATH=`cd "$APP_PATH"; pwd` 
cd "$APP_PATH"

if [ -f setup.log ]; then
	rm -f setup.log
fi

# clone all of the SDL libraries
echo "   Cloning SDL"
hg clone http://hg.libsdl.org/SDL 		./SDL >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "   Cloning SDL_image"
hg clone http://hg.libsdl.org/SDL_image	./SDL_image >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "   Cloning SDL_mixer"
hg clone http://hg.libsdl.org/SDL_mixer	./SDL_mixer >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "   Cloning SDL_net"
hg clone http://hg.libsdl.org/SDL_net	./SDL_net >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "   Checking out SDL_gfx"
svn checkout http://svn.code.sf.net/p/sdl2gfx/code/trunk ./SDL_gfx -r r29

# echo "   Cloning SDL_rtf"
# hg clone http://hg.libsdl.org/SDL_rtf	./SDL_rtf >> setup.log
# if [ $? != 0 ]; then DoExit; fi

echo "   Cloning SDL_ttf"
hg clone http://hg.libsdl.org/SDL_ttf	./SDL_ttf >> setup.log
if [ $? != 0 ]; then DoExit; fi

# select used versions
echo "   Selecing SDL version"
cd ./SDL
hg up -r release-2.0.3 >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "   Selecing SDL_image version"
cd ../SDL_image
hg up -r release-2.0.0 >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "   Selecing SDL_mixer version"
cd ../SDL_mixer
hg up -r release-2.0.0 >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "   Selecing SDL_net version"
cd ../SDL_net
hg up -r release-2.0.0 >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "   Selecing SDL_ttf version"
cd ../SDL_ttf
hg up -r release-2.0.12 >> setup.log
if [ $? != 0 ]; then DoExit; fi

echo "--------------------------------------------------"
