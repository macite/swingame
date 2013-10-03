'''SwinGame SDK python module.

Loading this module imports and exposes the sgsdk_types and sgsdk routines
(available via the external sgsdk library), as well as some additional helper
functions not directly/easily available.

'''

from ctypes import create_string_buffer, c_float, byref
from sgsdk import * # which also exposes sgsdk.types.*

#=============================================================================

# small list of "known" colour names for use with GetColor('name') 
color_names = {'white':(255, 255, 255, 255), 
               'green':(0, 255, 0, 255), 
               'blue':(0, 0, 255, 255), 
               'black':(0, 0, 0, 255), 
               'red':(255, 0, 0, 255), 
               'yellow':(255, 255, 0, 255), 
               'pink':(255, 20, 147, 255), 
               'turquoise':(0, 206, 209, 255), 
               'grey':(128, 128, 128, 255), 
               'magenta':(255, 0, 255, 255), 
               'transparent':(0, 0, 0, 0) }
    
#=============================================================================    

def GetColor(*args):
    ''' Gets a sgsdk color value using either a single parameter 'name' string 
        or the full set of (r,g,b,a) values. '''
    if len(args) == 1: # eg. 'red'
        return GetColourRGBA(*color_names[args[0]])
    else: # all 4 c_byte values used
        return GetColourRGBA(*args)


# Alias for Get/Set naming rules to MoveSprite(sprite, vector)
# Expects a sprite (pointer) and a position Vector. 
SetSpritePosition = MoveSprite

# sgsdk doesn'y yet have this ... but it should.
def GetSpritePosition(sprite):
    return Vector(GetSpriteX(sprite), GetSpriteY(sprite))

def GetMousePosition():
    ''' Retrieve the current mouse position as a vector. ''' 
    # The current sgsdkGetMouseXY uses out parameters of c_float, so...
    x, y = c_float(), c_float()
    GetMouseXY(byref(x), byref(y))
    return Vector(x.value, y.value)

def LoadBitmap(filename):
    ''' Map to sgsdk function using default values.'''
    return LoadBitmapWithTransparentColor(filename, sgFalse, GetColor('transparent'))

def PlaySoundEffect(sound):
    ''' Map to sgsdk function. Play a sound once (does not loop). '''
    return PlaySoundEffectLoop(sound, 0)
    
def GetPathToResource(base, filename, restype):
    ''' Convenience method to access standard resource files based on type. '''
    result = create_string_buffer(" "*1023) # + 1 null termination char
    GetPathToResourceWithBaseAndKind(base, filename, restype, result)
    return result.value # as python string object

# wrap methods that need string result types
def EndReadingText():
    result = create_string_buffer(" "*1023) # + 1 null termination char
    EndReadingText(result)
    return result.value # as python string object

def TextReadAsASCII():
    result = create_string_buffer(" "*1023) # + 1 null termination char
    TextReadAsASCII(result)
    return result.value # as python string object

