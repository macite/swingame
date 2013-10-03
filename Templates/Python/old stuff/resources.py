''' Exposes utility methods to help with resource management. Also provides
    the SwinGame loading screen and final Credit screen. '''

#    procedure LoadResources();
#    procedure FreeResources();
#    function GameFont(fontName): Font;
#    function GameImage(imageName): Bitmap;
#    function GameSound(soundName): SoundEffect;
#    function GameMusic(musicName): Music;
#    function GameMap(mapName): Map;

from . import LoadBitmap, GetPathToResource, GetColor, PlaySoundEffect
import sgsdk
from types import rkImage, rkSound, rkFont, faLeft, faCenter, sgTrue 

# setup a base path for future resource loading.
import sys, os

if sys.platform == "win32":
    _base = os.path.abspath('../') # win32  - assumes sgsdk did chdir("/sgsdk")
elif sys.platform == "darwin":
    _base = os.path.abspath('') # mac (doesn't need to change path back)
else: 
    _base = os.path.abspath('') # linux
print "** Resource base path:", _base


# Used by different apps to provide a list of resources they want managed
# using tuples (name,filename,...) as needed for each resource type 
sg_lists = {'IMAGE': [], # (name,filename)
            'FONT': [],# (name,filename,size)
            'SOUND': [], # (name,filename)
            'MUSIC': [],# (name,filename)
            'MAP': []} # (filename)


# managed lists of resources - used to free them later.
_Images = [] # Array of Bitmap
_Fonts = [] # Array of Font
_Sounds = [] # Array of SoundEffect
_Music = [] # Array of Music
_Maps = [] # Array of Map
   
#------------------------------------------------------------------------------

def GameFont(fontName): # string, return Font
    ''' Get the Font you created with name font.
        PARAMETERS:
        - fontName: name of the font to get
        RETURNS:
        - the font
        EXCEPT:
        - if the font isn't found an error is returned '''
    for name, font in _Fonts:
        if name == fontName:
            return font
    # fall-though - no font by that name so raise
    raise KeyError, 'Font ' + fontName + ' does not exist...'


def GameImage(imageName): # string, return Bitmap
    ''' Get the image you created with name image.
        PARAMETERS:
        - image: name of the image to get
        RETURNS:
        - the image
        EXCEPT:
        - if the image isn't found an error is returned '''
    for name, image in _Images:
        if name == imageName:
            return image
    raise KeyError, 'Image ' + imageName + ' does not exist...'


def GameSound(soundName): # string, return SoundEffect
    ''' Get the soundeffect you created with name sound.
        PARAMETERS:
        - sound: name of the soundeffect to get
        RETURNS:
        - the soundeffect
        EXCEPT:
        - if the soundeffect isn't found an error is returned '''
    for name, sound in _Sounds:
        if name == soundName:
            return sound
    raise KeyError, 'Sound ' + soundName + ' does not exist...'

def GameMap(mapName): # String, returns Map
    ''' Get the map you created with name mapName.
        PARAMETERS:
        - mapName: name of the map to get
        RETURNS:
        - the map
        EXCEPT:
        - if the map isn't found an error is returned '''
    for name, map in _Maps:
        if name == mapName:
            return map
    raise KeyError, 'Map ' + mapName + ' does not exist...'

def GameMusic(musicName): # string, returns Music
    ''' Get the music you created with name music.
        PARAMETERS:
        - music: name of the music to get
        RETURNS:
        - the music
        EXCEPT:
        - if the music isn't found an error is returned '''
    for name, music in _Music:
        if name == musicName:
            return music
    raise KeyError, 'Music ' + musicName + ' does not exist...'

#------------------------------------------------------------------------------

def LoadResources(sleeptime=100):
    ''' Call this to load your resources and to
        display the loading screen. 
        Optional sleeptime parameter is in miliseconds.
        SIDE EFFECTS: '''
    oldW = sgsdk.ScreenWidth()
    oldH = sgsdk.ScreenHeight()

    sgsdk.ChangeScreenSize(800, 600)
    
    #Remove sleeps once "real" game resources are being loaded
    _ShowLoadingScreen()
    _ShowMessage('loading fonts', 0)
    _LoadManagedFonts()
    sgsdk.Sleep(sleeptime)

    _ShowMessage('loading images', 1)
    _LoadManagedImages()
    sgsdk.Sleep(sleeptime)

    _ShowMessage('loading sounds', 2)
    _LoadManagedSounds()
    sgsdk.Sleep(sleeptime)

    _ShowMessage('loading music', 3)
    _LoadManagedMusics()
    sgsdk.Sleep(sleeptime)

    _ShowMessage('loading maps', 4)
    _LoadManagedMaps()
    sgsdk.Sleep(sleeptime)

    _ShowMessage('SwinGame loaded', 5)
    sgsdk.Sleep(sleeptime)
    
    _EndLoadingScreen()

    sgsdk.ChangeScreenSize(oldW, oldH)

def LoadResourcesNoGUI():
    ''' Call this to load your resources. Show NO loading screen or images. '''    
    print "Built using the SwinGame API http://www.swingame.com"
    print "Version:", str(sgsdk.DLLVersion())
    print "Loading: ... ",
    _LoadManagedFonts()
    print "Fonts ...",
    _LoadManagedImages()
    print "Images ...",
    _LoadManagedSounds()
    print "Sounds ...",
    _LoadManagedMusics()
    print "Music ...",
    _LoadManagedMaps()
    print "Maps."
    print "SwinGame Loaded!"

def FreeResources(credits=None, sleeptime=2000):
    ''' Displays the final credit screen - with the text if provided. 
        Then free all of the resources that you have loaded for your game. This 
        should be called at the end of the program. 
        SIDE EFFECTS:
        - Frees all loaded Fonts, Images, Music, Sound, and Maps '''
    # show credits screen 
    splashback = LoadBitmap(GetPathToResource(_base, 'SplashBack.png', rkImage))
    creditfont = sgsdk.LoadFont(GetPathToResource(_base, 'arial.ttf', rkFont), 14)
    sgsdk.ChangeScreenSize(800, 600)    
    sgsdk.DrawBitmap(splashback, 0, 0)    
    # show credit text if provided
    if credits is not None:
        if sys.platform == 'win32':
            credits = credits.replace("\n","\r\n")
        sgsdk.DrawTextLines(credits, GetColor('white'), GetColor('transparent'),
                            creditfont, faLeft, 250, 50, 800-250, 600-50)
    # process, show and wait
    sgsdk.ProcessEvents()
    sgsdk.RefreshScreen()
    sgsdk.Sleep(sleeptime)
    # cleanup credit screen
    sgsdk.FreeBitmap(splashback)
    sgsdk.FreeFont(creditfont)
    # cleanup resources time....
    _FreeManagedFonts()
    _FreeManagedImages()
    _FreeManagedMusics()
    _FreeManagedSounds()
    _FreeManagedMaps()

def FreeResourcesNoGUI():
    ''' Cleanup swingame managed resources...'''
    _FreeManagedFonts()
    _FreeManagedImages()
    _FreeManagedMusics()
    _FreeManagedSounds()
    _FreeManagedMaps()

    
#------------------------------------------------------------------------------

def _NewMap(mapName): # string
    ''' Creates a new Mappy Map from mapFile file.
        PARAMETERS:
         - mapName: The name of the map to load
        SIDE EFFECTS:
        - Loads the map from file into _Maps '''
    newMap = sgsdk.LoadMap(mapName)
    _Maps.append((mapName, newMap))

def _NewFont(fontName, fileName, size): # string, string, int
    ''' Creates a new font.
        PARAMETERS:
        - fontName: name to call font in _Fonts. Used when you access the font.
        - fileName: name of the font file to load. Must be in resources/fonts
        - size: Size of font to load
        SIDE EFFECTS:
        - Loads the font from file into _Fonts'''
    newFont = sgsdk.LoadFont(GetPathToResource(_base, fileName, rkFont), size)
    _Fonts.append((fontName, newFont))

def _NewImage(imageName, fileName): # string string
    ''' Creates a new image.
        PARAMETERS:
        - imageName: name to call image in _images. Used when you access the image.
        - fileName: name of the image file to load. Must be in resources/images
        SIDE EFFECTS:
        - Loads the image from file into _Images '''        
    newImage = LoadBitmap(GetPathToResource(_base, fileName, rkImage))
    _Images.append((imageName, newImage))

def _NewTransparentColourImage(imageName, fileName, transColour): # string string colour
    ''' Creates a new image with transparent key colour.
        PARAMETERS:
        - imageName: name to call image in _images. Used when you access the image.
        - fileName: name of the image file to load. Must be in resources/images
        - transColour: colour of a pixel to be transparent
        SIDE EFFECTS:
        - Loads the image from file into _Images with transparent key colour '''
    newImage = LoadBitmap(GetPathToResource(_base, fileName, rkImage), sgTrue, transColour)
    _Images.append((imageName,newImage))

def _NewTransparentColorImage(imageName, fileName, transColor): # string string colour
    ''' Creates a new image with transparent key color.
        PARAMETERS:
        - imageName: name to call image in _images. Used when you access the image.
        - fileName: name of the image file to load. Must be in resources/images
        - transColor: color of a pixel to be transparent
        SIDE EFFECTS:
        - Loads the image from file into _Images with transparent key color '''
    _NewTransparentColourImage(imageName, fileName, transColor)


def _NewSound(soundName, fileName): # string string
    ''' Creates a new sound.
        PARAMETERS:
        - soundName: name to call sound in _sounds. Used when you access the sound.
        - fileName: name of the sound file to load. Must be in resources/sounds
        SIDE EFFECTS:
        - Loads the sound from file into _sounds '''
    newSound = sgsdk.LoadSoundEffect(GetPathToResource(_base, fileName, rkSound))
    _Sounds.append((soundName, newSound))

def _NewMusic(musicName, fileName): # string string
    ''' Creates a new music.
        PARAMETERS:
        - musicName: name to call music in _musics. Used when you access the music.
        - fileName: name of the music file to load. Must be in resources/musics
        SIDE EFFECTS:
        - Loads the music from file into _musics '''
    newMusic = sgsdk.LoadMusic(GetPathToResource(_base, fileName, rkSound))
    _Music.append((musicName, newMusic))

#------------------------------------------------------------------------------

def _LoadManagedFonts():
    ''' Load the fonts you need for your game as stored in sg_font_list
        [('name1','filename1',size)('name2','filename2',size) ... ]
        SIDE EFFECTS:
        - Loads the game's fonts '''
    _NewFont('ArialLarge', 'arial.ttf', 80)
    _NewFont('Courier', 'cour.ttf', 16)
    for args in sg_lists['FONT']:
        _NewFont(*args)

def _LoadManagedImages():
    ''' Load the images you need for your game  as stored in sg_image_list
        [('name1','filename1')('name2','filename2') ... ]
        SIDE EFFECTS:
        - Loads the game's images '''
    for args in sg_lists['IMAGE']:
        _NewImage(*args)

def _LoadManagedSounds():
    ''' Load the soundeffects you need for your game as stored in sg_sound_list
        [('name1','filename1')('name2','filename2') ... ]
        SIDE EFFECTS:
        - Loads the game's soundeffects '''
    for args in sg_lists['SOUND']:
        _NewSound(*args)

def _LoadManagedMusics():
    ''' Load the music you need for your game as stored in sg_music_list
        [('name1','filename1')('name2','filename2') ... ]
        SIDE EFFECTS:
        - Loads the game's music '''
    for args in sg_lists['MUSIC']:
        _NewMusic(*args)

def _LoadManagedMaps():
    ''' Load the maps you need for your game, as stored in sg_map_list
        [('filename'),('filename2'), ...]
        SIDE EFFECTS:
        - Loads the game's maps '''
    for args in sg_lists['MAP']:
        _NewMap(*args)

#------------------------------------------------------------------------------

def _FreeManagedFonts():
    ''' Frees the fonts that you have loaded.
        SIDE EFFECTS:
        - Frees the game's fonts '''
    global _Fonts
    for _, font in _Fonts:
        sgsdk.FreeFont(font)
    _Fonts = []

def _FreeManagedImages():
    ''' Frees the images that you have loaded.
        SIDE EFFECTS:
        - Frees the game's images '''
    global _Images
    for _, image in _Images:
        sgsdk.FreeBitmap(image)
    _Images = []

def _FreeManagedSounds():
    ''' Frees the images that you have loaded.
        SIDE EFFECTS:
        - Frees the game's images '''
    global _Sounds
    for _, sound in _Sounds:
        sgsdk.FreeSoundEffect(sound)
    _Sounds = []   

def _FreeManagedMusics():
    ''' Frees the music that you have loaded.
        SIDE EFFECTS:
        - Frees the game's music
        - Stops playing any music '''
    global _Music
    sgsdk.StopMusic()
    sgsdk.Sleep(100)
    for _, music in _Music:
        sgsdk.FreeMusic(music)
    _Music = []    

def _FreeManagedMaps():
    ''' Frees the maps that you have loaded.
        SIDE EFFECTS:
        - Frees the game's maps '''
    global _Maps
    for _, map in _Maps:
        sgsdk.FreeMap(map)
    _Maps = []   

#------------------------------------------------------------------------------

# used internally for the SwinGame loader screen
_Background = None # Bitmap
_Animation = None # Bitmap
_LoaderEmpty = None # Bitmap
_LoaderFull = None # Bitmap
_LoadingFont = None #: Font
_StartSound = None #: SoundEffect

def _PlaySwinGameIntro():
    ''' Plays the SwinGame intro. Please leave this in all SwinGames.
        SIDE EFFECTS:
        - Plays the starting sound
        - Draws the background and animation
        - Refreshes screen '''
    ANI_X, ANI_Y = 143, 134
    ANI_W, ANI_H = 546, 327
    ANI_V_CELL_COUNT = 6
    ANI_CELL_COUNT = 11
    PlaySoundEffect(_StartSound)
    sgsdk.Sleep(200)
    for i in range(ANI_CELL_COUNT):
        sgsdk.DrawBitmap(_Background, 0, 0)
        sgsdk.DrawBitmapPart(_Animation, 
                             (i // ANI_V_CELL_COUNT) * ANI_W, 
                             (i % ANI_V_CELL_COUNT) * ANI_H,
                             ANI_W, ANI_H, ANI_X, ANI_Y)
        sgsdk.RefreshScreen()
        sgsdk.ProcessEvents()
        sgsdk.Sleep(20)
    sgsdk.Sleep(1500)

def _ShowLoadingScreen():
    ''' Loads the resourced needed to show the loading screen,
        and plays the intro.
        SIDE EFFECTS:
        - Loads _Background, _Animation, _LoadingFont, and _StartSound
        - Plays Intro '''
    global _Background
    global _Animation, _LoaderEmpty, _LoaderFull, _LoadingFont, _StartSound
    
    _Background = LoadBitmap(GetPathToResource(_base, 'SplashBack.png', rkImage))
    
    sgsdk.DrawBitmap(_Background, 0, 0)
    sgsdk.RefreshScreen()
    sgsdk.ProcessEvents()
    
    _Animation = LoadBitmap(GetPathToResource(_base, 'SwinGameAni.png', rkImage))
    _LoaderEmpty = LoadBitmap(GetPathToResource(_base, 'loader_empty.png', rkImage))
    _LoaderFull = LoadBitmap(GetPathToResource(_base, 'loader_full.png', rkImage))
    _LoadingFont = sgsdk.LoadFont(GetPathToResource(_base, 'arial.ttf', rkFont), 12)
    _StartSound = sgsdk.LoadSoundEffect(GetPathToResource(_base, 'SwinGameStart.ogg', rkSound))

    _PlaySwinGameIntro()

def _ShowMessage(message, number): # string, int
    ''' This plays the "Loading ..." messages while your
        resources are being loaded.
        SIDE EFFECTS:
        - Draws text to the screen
        - Refreshes screen '''
    TX, TY = 310, 493 
    TW, TH = 200, 25
    STEPS = 5
    BG_X, BG_Y = 279, 453
    fullW = (260 * number) // STEPS # div
    
    sgsdk.DrawBitmap(_LoaderEmpty, BG_X, BG_Y)
    sgsdk.DrawBitmapPart(_LoaderFull, 0, 0, fullW, 66, BG_X, BG_Y)
    sgsdk.DrawTextLines(message, 
                        GetColor('white'), 
                        GetColor('transparent'), 
                        _LoadingFont, faCenter, TX, TY, TW, TH)
    sgsdk.RefreshScreen()
    sgsdk.ProcessEvents()    
    
def _EndLoadingScreen():
    ''' Ends the loading screen, and frees the set surfaces.
        SIDE EFFECTS:
        - Clears the screen
        - Frees _LoadingFont, _Background, _Animation, and _StartSound '''
    sgsdk.ProcessEvents()
    sgsdk.Sleep(500)
    sgsdk.ClearScreen(GetColor('black'))
    sgsdk.RefreshScreen()
    sgsdk.FreeFont(_LoadingFont)
    sgsdk.FreeBitmap(_Background)
    sgsdk.FreeBitmap(_Animation)
    sgsdk.FreeBitmap(_LoaderEmpty)
    sgsdk.FreeBitmap(_LoaderFull)
    sgsdk.FreeSoundEffect(_StartSound)
