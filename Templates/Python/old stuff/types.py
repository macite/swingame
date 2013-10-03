'''SwinGame SDK types mapped to Python ctypes. 

Supports sgsdk.py in using the sgsdk.dll (windows) or libsgsdk.dylib (mac). 
See __init__.py and sgsdk.py for additional details.

'''

from ctypes import c_int, c_float, Structure
from math import hypot, radians, cos, sin

sg_options = {'debug': True }
     
#==============================================================================
# enumeration types
#==============================================================================

#pylint: disable-msg=W0141

sgTrue, sgFalse = map(c_int, (-1, 0))

CollisionDetectionRange = c_int 
cdrEquals, cdrGreaterThan, cdrLessThan = map(c_int, range(3))

CollisionSide = c_int 
(csTop, csBottom, csLeft, csRight, 
 csTopLeft, csTopRight, csBottomLeft, csBottomRight, 
 csNone) = map(c_int, range(9))

EventKind = c_int # mappy event 0..23
# can't be bothered enumerating all these...

FontAlignment = c_int 
faLeft, faCenter, faRight = map(c_int, (1, 2, 4))

FontStyle = c_int #
fsNormal, fsBold, fsItalic, fsUnderline = map(c_int, range(4))

MouseButton = c_int # 
mbLeft, mbMiddle, mbRight = map(c_int, range(3))

ResourceKind = c_int #
rkFont, rkImage, rkSound, rkMap = map(c_int, range(4))

SpriteEndingAction = c_int # 
seaLoop, seaReverseLoop, seaReverseOnce, seaStop = map(c_int, range(4))

SpriteKind = c_int # 
skStatic, skAnimArray, skAnimMulti = map(c_int, range(3))

#==============================================================================
# Record to ctypes Structure objects
#==============================================================================

class Point2D(Structure): # record; x,y single
    _fields_ = ('x', c_float), ('y', c_float)
    
class Rectangle(Structure): # record; x,y (single), width, height (integer)
    _fields = [('x', c_int), ('y', c_int), 
               ('width', c_int), ('height', c_int)]
               
class Vector(Structure):  #record x,y: single
    '''Some handy vector operations built-in, but not too fancy.'''
    _fields_ = ('x', c_float), ('y', c_float)  

    def __add__(self, rhs): # self + rhs (vector)
        return Vector(self.x+rhs.x, self.y+rhs.y)
    
    def __sub__(self, rhs): # self - rhs (vector)
        return Vector(self.x-rhs.x, self.y-rhs.y)
    
    def __mul__(self, rhs): # self * rhs (scalar)
        return Vector(self.x*rhs, self.y*rhs)

    def __rmul__(self, lhs): # lhs (scalar) * self
        return Vector(self.x*lhs, self.y*lhs)
    
    def __div__(self, rhs): # self / rhs (scalar)
        return Vector(self.x/rhs, self.y/rhs)
    
    def __rdiv__(self, lhs): # lhs (scalar) / self
        return Vector(lhs/self.x, lhs/self.y)
    
    def __str__(self):
        return '[%7.2f, %7.2f]' % (self.x, self.y)
    
    def __eq__(self, other):
        return (self.x == other.x) and (self.y == other.y)

    def length(self):
        return hypot(self.x, self.y)
    magnitude = length # alias
    
    def unit(self):
        mag = self.length()
        return Vector( self.x / mag, self.y / mag)       
    
    def limit(self, limit):
        mag = self.length()
        if mag > limit:
            assert mag > 0, 'vector limit - magnitude must be > 0'
            # create unit vector and scale by the limit value (scalar)
            return Vector(self.x / mag, self.y / mag) * limit
        else:
            return Vector(self.x, self.y) # a copy
        
    def as_tuple(self):
        return (self.x, self.y)
    
    @classmethod
    def OfAngle(cls, angle, magnitude):
        rads = radians(angle)
        return Vector(magnitude * cos(rads), magnitude * sin(rads))
   
class LineSegment(Structure):
    _fields_ = ("startPoint", Point2D), ("endPoint", Point2D)

class Triangle(Structure):
    _fields_ = ("pointA", Point2D), ("pointB", Point2D), ("pointC", Point2D)
    
class Tile(Structure):
    _fields_ = [("xIndex", c_int), ("yIndex", c_int), 
                ("topCorner", Point2D),
                ("pointA", Point2D), ("pointB", Point2D), 
                ("pointC", Point2D), ("pointD", Point2D)]    

class _Keys(object):
    '''Collection of key names and codes'''
    def __init__(self): 
        self.BACK = 8 # SDLK_BACKSPACE;
        self.TAB = 9 # SDLK_TAB;
        self.CLEAR = 12 # SDLK_CLEAR;
        self.RETURN = 13 # SDLK_RETURN;
        self.SHIFT = 304 # SDLK_LSHIFT;
        self.CONTROL = 306 # SDLK_LCTRL;
        self.MENU = 319 # SDLK_MENU;
        self.ALT = 308 # SDLK_LALT;
        self.PAUSE = 19 # SDLK_PAUSE;
        self.CAPITAL = 301 # SDLK_CAPSLOCK;
        self.ESCAPE = 27 # SDLK_ESCAPE;
        self.SPACE = 32 # SDLK_SPACE;
        self.PAGE_UP = 280 # SDLK_PAGEUP;
        self.PAGE_DOWN = 281 # SDLK_PAGEDOWN;
        self.END = 279 # SDLK_END;
        self.HOME = 278 # SDLK_HOME;
        self.LEFT = 276 # SDLK_LEFT;
        self.UP = 273 # SDLK_UP;
        self.RIGHT = 275 # SDLK_RIGHT;
        self.DOWN = 274 # SDLK_DOWN;
        self.PRINT = 316 # SDLK_PRINT;
        self.INSERT = 277 # SDLK_INSERT;
        self.DELETE = 127 # SDLK_DELETE;
        self.HELP = 315 # SDLK_HELP;
        self._0 = 48 # SDLK_0; 
        self._1 = 49 # SDLK_1; 
        self._2 = 50 # SDLK_2; 
        self._3 = 51 # SDLK_3; 
        self._4 = 52 # SDLK_4; 
        self._5 = 53 # SDLK_5; 
        self._6 = 54 # SDLK_6; 
        self._7 = 55 # SDLK_7; 
        self._8 = 56 # SDLK_8; 
        self._9 = 57 # SDLK_9; 
        self.A = 97 # SDLK_A; 
        self.B = 98 # SDLK_B;
        self.C = 99 # SDLK_C;
        self.D = 100 # SDLK_D;
        self.E = 101 # SDLK_E;
        self.F = 102  # SDLK_F;
        self.G = 103 # SDLK_G;
        self.H = 104 # SDLK_H;
        self.I = 105 # SDLK_I;
        self.J = 106 # SDLK_J;
        self.K = 107 # SDLK_K;
        self.L = 108 # SDLK_L;
        self.M = 109 # SDLK_M;
        self.N = 110 # SDLK_N;
        self.O = 111 # SDLK_O;
        self.P = 112 # SDLK_P;
        self.Q = 113 # SDLK_Q;
        self.R = 114 # SDLK_R;
        self.S = 115 # SDLK_S;
        self.T = 116 # SDLK_T;
        self.U = 117 # SDLK_U;
        self.V = 118 # SDLK_V;
        self.W = 119 # SDLK_W;
        self.X = 120 # SDLK_X;
        self.Y = 121 # SDLK_Y;
        self.Z = 122 # SDLK_Z;
        self.LWIN = 311 # SDLK_LSUPER;
        self.RWIN = 312 # SDLK_RSUPER;
        self.APPS = 319 # SDLK_MENU;
        self.SLEEP = 320 # SDLK_POWER;
        self.NUMPAD0 = 256 # SDLK_KP0;
        self.NUMPAD1 = 257 # SDLK_KP1;
        self.NUMPAD2 = 258 # SDLK_KP2;
        self.NUMPAD3 = 259 # SDLK_KP3;
        self.NUMPAD4 = 260 # SDLK_KP4;
        self.NUMPAD5 = 261 # SDLK_KP5;
        self.NUMPAD6 = 262 # SDLK_KP6;
        self.NUMPAD7 = 263 # SDLK_KP7;
        self.NUMPAD8 = 264 # SDLK_KP8;
        self.NUMPAD9 = 265 #SDLK_KP9;
        self.MULTIPLY = 268 # SDLK_KP_MULTIPLY;
        self.ADD = 270 # SDLK_KP_PLUS;
        self.SUBTRACT = 269 # SDLK_MINUS;
        self.DECIMAL = 266 # SDLK_KP_PERIOD;
        self.PERIOD = 46 # SDLK_PERIOD;
        self.DIVIDE = 267 # SDLK_KP_DIVIDE;
        self.F1 = 282 # SDLK_F1; 
        self.F2 = 283 # SDLK_F2; 
        self.F3 = 284 # SDLK_F3;  
        self.F4 = 285 # SDLK_F4; 
        self.F5 = 286 # SDLK_F5; 
        self.F6 = 287 # SDLK_F6; 
        self.F7 = 288 # SDLK_F7; 
        self.F8 = 289 # SDLK_F8; 
        self.F9 = 290 # SDLK_F9; 
        self.F10 = 291 # SDLK_F10; 
        self.F11 = 292 # SDLK_F11; 
        self.F12 = 293 # SDLK_F12; 
        self.F13 = 294 # SDLK_F13; 
        self.F14 = 295 # SDLK_F14; 
        self.F15 = 296 # SDLK_F15; 
        self.NUMLOCK = 300 # SDLK_NUMLOCK; 
        self.SCROLL = 302 # SDLK_SCROLLOCK; 
        self.LSHIFT = 304 # SDLK_LSHIFT; 
        self.RSHIFT = 303 # SDLK_RSHIFT; 
        self.LCONTROL = 306 # SDLK_LCTRL;
        self.RCONTROL = 305 # SDLK_RCTRL;
        self.LMENU = 310 # SDLK_LMETA; 
        self.LALT = 308 # SDLK_LALT; 
        self.RMENU = 309 # SDLK_RMETA;
        self.RALT = 307 # SDLK_RALT; 
        self.EQUALS = 61 # SDLK_EQUALS; 
        self.COLON = 58 # SDLK_COLON;
        self.SEMICOLON = 59 # SDLK_SEMICOLON;
        self.LESS = 60 # SDLK_LESS;
        self.GREATER = 62 # SDLK_GREATER;
        self.QUESTION = 63#  SDLK_QUESTION;
        self.AT = 64 # SDLK_AT;
        self.COMMA = 44 # SDLK_COMMA;

Keys = _Keys() # easy singleton analogue