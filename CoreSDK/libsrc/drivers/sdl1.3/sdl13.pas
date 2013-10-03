
// 
// Exported by exth2pas
// 
{$mode objfpc}
{$packrecords C}

unit sdl13;
interface
type
      size_t = PtrUInt; 
      int64_t = Int64;
      Uint64_t = QWord;
      Uint64 = QWord;
      int32_t = Longint;
      Uint32_t = Longword;
      Uint32 = Longword;
      int16_t = SmallInt;
      Uint16_t = Word;
      Uint16 = Word;
      int8_t = ShortInt;
      uint8_t = Byte;
      Uint8 = Byte;
       PUint8 = ^Uint8;
      PPUint8 = ^PUint8;
      PSInt16 = ^SInt16;
      wchar_t = WideChar;
      PSDL_Finger = ^SDL_Finger;
      PPSDL_Finger = ^PSDL_Finger;
      PSDL_SysWMmsg = Pointer;
      PSDL_VideoInfo = ^SDL_VideoInfo;
      PPSDL_Rect = ^PSDL_Rect;
    Pchar  = ^char;

    PFILE  = ^FILE;

    Plongint  = ^longint;

    PSDL_assert_data  = ^SDL_assert_data;

    PSDL_atomic_t  = ^SDL_atomic_t;

    PSDL_AudioCVT  = ^SDL_AudioCVT;

    PSDL_AudioSpec  = ^SDL_AudioSpec;

    PSDL_BlendMode  = ^SDL_BlendMode;

    PSDL_Color  = ^SDL_Color;

    PSDL_cond  = Pointer;

    PSDL_Cursor  = Pointer;

    PSDL_DisplayMode  = ^SDL_DisplayMode;

    PSDL_Event  = ^SDL_Event;

    PSDL_EventFilter  = ^SDL_EventFilter;

    PSDL_Joystick  = Pointer;

    PSDL_LogOutputFunction  = ^SDL_LogOutputFunction;

    PSDL_mutex  = Pointer;

    PSDL_Overlay  = ^SDL_Overlay;

    PSDL_Palette  = ^SDL_Palette;

    PSDL_PixelFormat  = ^SDL_PixelFormat;

    PSDL_Point  = ^SDL_Point;

    PSDL_Rect  = ^SDL_Rect;

    PSDL_Renderer  = Pointer;

    PSDL_RendererInfo  = ^SDL_RendererInfo;

    PSDL_RWops  = ^SDL_RWops;

    PSDL_sem  = Pointer;

    PSDL_SpinLock  = ^SDL_SpinLock;

    PSDL_Surface  = ^SDL_Surface;

    PSDL_SysWMinfo  = Pointer;

    PSDL_Texture  = Pointer;

    PSDL_Thread  = Pointer;

    PSDL_Touch  = ^SDL_Touch;

    PSDL_version  = ^SDL_version;

    PSDL_Window  = Pointer;

    Psize_t  = ^size_t;

    PUint16  = ^Uint16;

    PUint32  = ^Uint32;

    Pwchar_t  = ^wchar_t;

      SDL_bool = (
        SDL_FALSE := 0,
        SDL_TRUE := 1
      );

      Sint8 = int8_t;

      Sint16 = int16_t;

      Sint32 = int32_t;

      Sint64 = int64_t;

      SDL_DUMMY_ENUM = (
        DUMMY_ENUM_VALUE := 0
      );

      SDL_iconv_t = Pointer;

      SDL_assert_state = (
        SDL_ASSERTION_RETRY := 0,
        SDL_ASSERTION_BREAK := 1,
        SDL_ASSERTION_ABORT := 2,
        SDL_ASSERTION_IGNORE := 3,
        SDL_ASSERTION_ALWAYS_IGNORE := 4
      );

      SDL_assert_data = record
          always_ignore : longint;
          trigger_count : dword;
          condition : Pchar;
          filename : Pchar;
          linenum : longint;
          _function : Pchar;
          next : PSDL_assert_data;
        end;

      SDL_AssertionHandler = function (data:PSDL_assert_data; userdata:pointer):SDL_assert_state;cdecl;

      SDL_SpinLock = longint;

      SDL_atomic_t = record
          value : longint;
        end;

      SDL_errorcode = (
        SDL_ENOMEM := 0,
        SDL_EFREAD := 1,
        SDL_EFWRITE := 2,
        SDL_EFSEEK := 3,
        SDL_UNSUPPORTED := 4,
        SDL_LASTERROR := 5
      );






      SDL_Thread_ID = dword;

      SDL_ThreadPriority = (
        SDL_THREAD_PRIORITY_LOW := 0,
        SDL_THREAD_PRIORITY_NORMAL := 1,
        SDL_THREAD_PRIORITY_HIGH := 2
      );

      SDL_ThreadFunction = function (data:pointer):longint;cdecl;

      SDL_RWops = record
          seek : function (context:PSDL_RWops; offset:longint; whence:longint):longint;cdecl;
          read : function (context:PSDL_RWops; ptr:pointer; size:size_t; maxnum:size_t):size_t;cdecl;
          write : function (context:PSDL_RWops; ptr:pointer; size:size_t; num:size_t):size_t;cdecl;
          close : function (context:PSDL_RWops):longint;cdecl;
          _type : Uint32;
          hidden : record
              case longint of
                0 : ( stdio : record
                    autoclose : SDL_bool;
                    fp : PFILE;
                  end );
                1 : ( mem : record
                    base : PUint8;
                    here : PUint8;
                    stop : PUint8;
                  end );
                2 : ( unknown : record
                    data1 : pointer;
                  end );
              end;
        end;

      SDL_AudioFormat = Uint16;

      SDL_AudioCallback = procedure (userdata:pointer; stream:PUint8; len:longint);cdecl;

      SDL_AudioSpec = record
          freq : longint;
          format : SDL_AudioFormat;
          channels : Uint8;
          silence : Uint8;
          samples : Uint16;
          padding : Uint16;
          size : Uint32;
          callback : SDL_AudioCallback;
          userdata : pointer;
        end;

      SDL_AudioFilter = procedure (cvt:PSDL_AudioCVT; format:SDL_AudioFormat);cdecl;

      SDL_AudioCVT = record
          needed : longint;
          src_format : SDL_AudioFormat;
          dst_format : SDL_AudioFormat;
          rate_incr : double;
          buf : PUint8;
          len : longint;
          len_cvt : longint;
          len_mult : longint;
          len_ratio : double;
          filters : array[0..9] of SDL_AudioFilter;
          filter_index : longint;
        end;

      SDL_AudioDeviceID = Uint32;

      SDL_AudioStatus = (
        SDL_AUDIO_STOPPED := 0,
        SDL_AUDIO_PLAYING := 1,
        SDL_AUDIO_PAUSED := 2
      );

      SDL_Color = record
          r : Uint8;
          g : Uint8;
          b : Uint8;
          unused : Uint8;
        end;

      SDL_Palette = record
          ncolors : longint;
          colors : PSDL_Color;
          version : Uint32;
          refcount : longint;
        end;

      SDL_PixelFormat = record
          format : Uint32;
          palette : PSDL_Palette;
          BitsPerPixel : Uint8;
          BytesPerPixel : Uint8;
          padding : array[0..1] of Uint8;
          Rmask : Uint32;
          Gmask : Uint32;
          Bmask : Uint32;
          Amask : Uint32;
          Rloss : Uint8;
          Gloss : Uint8;
          Bloss : Uint8;
          Aloss : Uint8;
          Rshift : Uint8;
          Gshift : Uint8;
          Bshift : Uint8;
          Ashift : Uint8;
          refcount : longint;
          next : PSDL_PixelFormat;
        end;

      SDL_Point = record
          x : longint;
          y : longint;
        end;

      SDL_Rect = record
          x : longint;
          y : longint;
          w : longint;
          h : longint;
        end;

      SDL_BlendMode = (
        SDL_BLENDMODE_NONE := $00000000,
        SDL_BLENDMODE_BLEND := $00000001,
        SDL_BLENDMODE_ADD := $00000002,
        SDL_BLENDMODE_MOD := $00000004
      );

      SDL_Surface = record
          flags : Uint32;
          format : PSDL_PixelFormat;
          w : longint;
          h : longint;
          pitch : longint;
          pixels : pointer;
          userdata : pointer;
          locked : longint;
          lock_data : pointer;
          clip_rect : SDL_Rect;
          map : Pointer;
          refcount : longint;
        end;

      SDL_blit = function (src:PSDL_Surface; srcrect:PSDL_Rect; dst:PSDL_Surface; dstrect:PSDL_Rect):longint;cdecl;

      SDL_DisplayMode = record
          format : Uint32;
          w : longint;
          h : longint;
          refresh_rate : longint;
          driverdata : pointer;
        end;

      SDL_WindowFlags = (
        SDL_WINDOW_FULLSCREEN := $00000001,
        SDL_WINDOW_OPENGL := $00000002,
        SDL_WINDOW_SHOWN := $00000004,
        SDL_WINDOW_HIDDEN := $00000008,
        SDL_WINDOW_BORDERLESS := $00000010,
        SDL_WINDOW_RESIZABLE := $00000020,
        SDL_WINDOW_MINIMIZED := $00000040,
        SDL_WINDOW_MAXIMIZED := $00000080,
        SDL_WINDOW_INPUT_GRABBED := $00000100,
        SDL_WINDOW_INPUT_FOCUS := $00000200,
        SDL_WINDOW_MOUSE_FOCUS := $00000400,
        SDL_WINDOW_FOREIGN := $00000800
      );

      SDL_WindowEventID = (
        SDL_WINDOWEVENT_NONE := 0,
        SDL_WINDOWEVENT_SHOWN := 1,
        SDL_WINDOWEVENT_HIDDEN := 2,
        SDL_WINDOWEVENT_EXPOSED := 3,
        SDL_WINDOWEVENT_MOVED := 4,
        SDL_WINDOWEVENT_RESIZED := 5,
        SDL_WINDOWEVENT_SIZE_CHANGED := 6,
        SDL_WINDOWEVENT_MINIMIZED := 7,
        SDL_WINDOWEVENT_MAXIMIZED := 8,
        SDL_WINDOWEVENT_RESTORED := 9,
        SDL_WINDOWEVENT_ENTER := 10,
        SDL_WINDOWEVENT_LEAVE := 11,
        SDL_WINDOWEVENT_FOCUS_GAINED := 12,
        SDL_WINDOWEVENT_FOCUS_LOST := 13,
        SDL_WINDOWEVENT_CLOSE := 14
      );

      SDL_GLContext = pointer;

      SDL_GLattr = (
        SDL_GL_RED_SIZE := 0,
        SDL_GL_GREEN_SIZE := 1,
        SDL_GL_BLUE_SIZE := 2,
        SDL_GL_ALPHA_SIZE := 3,
        SDL_GL_BUFFER_SIZE := 4,
        SDL_GL_DOUBLEBUFFER := 5,
        SDL_GL_DEPTH_SIZE := 6,
        SDL_GL_STENCIL_SIZE := 7,
        SDL_GL_ACCUM_RED_SIZE := 8,
        SDL_GL_ACCUM_GREEN_SIZE := 9,
        SDL_GL_ACCUM_BLUE_SIZE := 10,
        SDL_GL_ACCUM_ALPHA_SIZE := 11,
        SDL_GL_STEREO := 12,
        SDL_GL_MULTISAMPLEBUFFERS := 13,
        SDL_GL_MULTISAMPLESAMPLES := 14,
        SDL_GL_ACCELERATED_VISUAL := 15,
        SDL_GL_RETAINED_BACKING := 16,
        SDL_GL_CONTEXT_MAJOR_VERSION := 17,
        SDL_GL_CONTEXT_MINOR_VERSION := 18
      );

      SDL_Scancode = (
        SDL_SCANCODE_UNKNOWN := 0,
        SDL_SCANCODE_A := 4,
        SDL_SCANCODE_B := 5,
        SDL_SCANCODE_C := 6,
        SDL_SCANCODE_D := 7,
        SDL_SCANCODE_E := 8,
        SDL_SCANCODE_F := 9,
        SDL_SCANCODE_G := 10,
        SDL_SCANCODE_H := 11,
        SDL_SCANCODE_I := 12,
        SDL_SCANCODE_J := 13,
        SDL_SCANCODE_K := 14,
        SDL_SCANCODE_L := 15,
        SDL_SCANCODE_M := 16,
        SDL_SCANCODE_N := 17,
        SDL_SCANCODE_O := 18,
        SDL_SCANCODE_P := 19,
        SDL_SCANCODE_Q := 20,
        SDL_SCANCODE_R := 21,
        SDL_SCANCODE_S := 22,
        SDL_SCANCODE_T := 23,
        SDL_SCANCODE_U := 24,
        SDL_SCANCODE_V := 25,
        SDL_SCANCODE_W := 26,
        SDL_SCANCODE_X := 27,
        SDL_SCANCODE_Y := 28,
        SDL_SCANCODE_Z := 29,
        SDL_SCANCODE_1 := 30,
        SDL_SCANCODE_2 := 31,
        SDL_SCANCODE_3 := 32,
        SDL_SCANCODE_4 := 33,
        SDL_SCANCODE_5 := 34,
        SDL_SCANCODE_6 := 35,
        SDL_SCANCODE_7 := 36,
        SDL_SCANCODE_8 := 37,
        SDL_SCANCODE_9 := 38,
        SDL_SCANCODE_0 := 39,
        SDL_SCANCODE_RETURN := 40,
        SDL_SCANCODE_ESCAPE := 41,
        SDL_SCANCODE_BACKSPACE := 42,
        SDL_SCANCODE_TAB := 43,
        SDL_SCANCODE_SPACE := 44,
        SDL_SCANCODE_MINUS := 45,
        SDL_SCANCODE_EQUALS := 46,
        SDL_SCANCODE_LEFTBRACKET := 47,
        SDL_SCANCODE_RIGHTBRACKET := 48,
        SDL_SCANCODE_BACKSLASH := 49,
        SDL_SCANCODE_NONUSHASH := 50,
        SDL_SCANCODE_SEMICOLON := 51,
        SDL_SCANCODE_APOSTROPHE := 52,
        SDL_SCANCODE_GRAVE := 53,
        SDL_SCANCODE_COMMA := 54,
        SDL_SCANCODE_PERIOD := 55,
        SDL_SCANCODE_SLASH := 56,
        SDL_SCANCODE_CAPSLOCK := 57,
        SDL_SCANCODE_F1 := 58,
        SDL_SCANCODE_F2 := 59,
        SDL_SCANCODE_F3 := 60,
        SDL_SCANCODE_F4 := 61,
        SDL_SCANCODE_F5 := 62,
        SDL_SCANCODE_F6 := 63,
        SDL_SCANCODE_F7 := 64,
        SDL_SCANCODE_F8 := 65,
        SDL_SCANCODE_F9 := 66,
        SDL_SCANCODE_F10 := 67,
        SDL_SCANCODE_F11 := 68,
        SDL_SCANCODE_F12 := 69,
        SDL_SCANCODE_PRINTSCREEN := 70,
        SDL_SCANCODE_SCROLLLOCK := 71,
        SDL_SCANCODE_PAUSE := 72,
        SDL_SCANCODE_INSERT := 73,
        SDL_SCANCODE_HOME := 74,
        SDL_SCANCODE_PAGEUP := 75,
        SDL_SCANCODE_DELETE := 76,
        SDL_SCANCODE_END := 77,
        SDL_SCANCODE_PAGEDOWN := 78,
        SDL_SCANCODE_RIGHT := 79,
        SDL_SCANCODE_LEFT := 80,
        SDL_SCANCODE_DOWN := 81,
        SDL_SCANCODE_UP := 82,
        SDL_SCANCODE_NUMLOCKCLEAR := 83,
        SDL_SCANCODE_KP_DIVIDE := 84,
        SDL_SCANCODE_KP_MULTIPLY := 85,
        SDL_SCANCODE_KP_MINUS := 86,
        SDL_SCANCODE_KP_PLUS := 87,
        SDL_SCANCODE_KP_ENTER := 88,
        SDL_SCANCODE_KP_1 := 89,
        SDL_SCANCODE_KP_2 := 90,
        SDL_SCANCODE_KP_3 := 91,
        SDL_SCANCODE_KP_4 := 92,
        SDL_SCANCODE_KP_5 := 93,
        SDL_SCANCODE_KP_6 := 94,
        SDL_SCANCODE_KP_7 := 95,
        SDL_SCANCODE_KP_8 := 96,
        SDL_SCANCODE_KP_9 := 97,
        SDL_SCANCODE_KP_0 := 98,
        SDL_SCANCODE_KP_PERIOD := 99,
        SDL_SCANCODE_NONUSBACKSLASH := 100,
        SDL_SCANCODE_APPLICATION := 101,
        SDL_SCANCODE_POWER := 102,
        SDL_SCANCODE_KP_EQUALS := 103,
        SDL_SCANCODE_F13 := 104,
        SDL_SCANCODE_F14 := 105,
        SDL_SCANCODE_F15 := 106,
        SDL_SCANCODE_F16 := 107,
        SDL_SCANCODE_F17 := 108,
        SDL_SCANCODE_F18 := 109,
        SDL_SCANCODE_F19 := 110,
        SDL_SCANCODE_F20 := 111,
        SDL_SCANCODE_F21 := 112,
        SDL_SCANCODE_F22 := 113,
        SDL_SCANCODE_F23 := 114,
        SDL_SCANCODE_F24 := 115,
        SDL_SCANCODE_EXECUTE := 116,
        SDL_SCANCODE_HELP := 117,
        SDL_SCANCODE_MENU := 118,
        SDL_SCANCODE_SELECT := 119,
        SDL_SCANCODE_STOP := 120,
        SDL_SCANCODE_AGAIN := 121,
        SDL_SCANCODE_UNDO := 122,
        SDL_SCANCODE_CUT := 123,
        SDL_SCANCODE_COPY := 124,
        SDL_SCANCODE_PASTE := 125,
        SDL_SCANCODE_FIND := 126,
        SDL_SCANCODE_MUTE := 127,
        SDL_SCANCODE_VOLUMEUP := 128,
        SDL_SCANCODE_VOLUMEDOWN := 129,
        SDL_SCANCODE_KP_COMMA := 133,
        SDL_SCANCODE_KP_EQUALSAS400 := 134,
        SDL_SCANCODE_INTERNATIONAL1 := 135,
        SDL_SCANCODE_INTERNATIONAL2 := 136,
        SDL_SCANCODE_INTERNATIONAL3 := 137,
        SDL_SCANCODE_INTERNATIONAL4 := 138,
        SDL_SCANCODE_INTERNATIONAL5 := 139,
        SDL_SCANCODE_INTERNATIONAL6 := 140,
        SDL_SCANCODE_INTERNATIONAL7 := 141,
        SDL_SCANCODE_INTERNATIONAL8 := 142,
        SDL_SCANCODE_INTERNATIONAL9 := 143,
        SDL_SCANCODE_LANG1 := 144,
        SDL_SCANCODE_LANG2 := 145,
        SDL_SCANCODE_LANG3 := 146,
        SDL_SCANCODE_LANG4 := 147,
        SDL_SCANCODE_LANG5 := 148,
        SDL_SCANCODE_LANG6 := 149,
        SDL_SCANCODE_LANG7 := 150,
        SDL_SCANCODE_LANG8 := 151,
        SDL_SCANCODE_LANG9 := 152,
        SDL_SCANCODE_ALTERASE := 153,
        SDL_SCANCODE_SYSREQ := 154,
        SDL_SCANCODE_CANCEL := 155,
        SDL_SCANCODE_CLEAR := 156,
        SDL_SCANCODE_PRIOR := 157,
        SDL_SCANCODE_RETURN2 := 158,
        SDL_SCANCODE_SEPARATOR := 159,
        SDL_SCANCODE_OUT := 160,
        SDL_SCANCODE_OPER := 161,
        SDL_SCANCODE_CLEARAGAIN := 162,
        SDL_SCANCODE_CRSEL := 163,
        SDL_SCANCODE_EXSEL := 164,
        SDL_SCANCODE_KP_00 := 176,
        SDL_SCANCODE_KP_000 := 177,
        SDL_SCANCODE_THOUSANDSSEPARATOR := 178,
        SDL_SCANCODE_DECIMALSEPARATOR := 179,
        SDL_SCANCODE_CURRENCYUNIT := 180,
        SDL_SCANCODE_CURRENCYSUBUNIT := 181,
        SDL_SCANCODE_KP_LEFTPAREN := 182,
        SDL_SCANCODE_KP_RIGHTPAREN := 183,
        SDL_SCANCODE_KP_LEFTBRACE := 184,
        SDL_SCANCODE_KP_RIGHTBRACE := 185,
        SDL_SCANCODE_KP_TAB := 186,
        SDL_SCANCODE_KP_BACKSPACE := 187,
        SDL_SCANCODE_KP_A := 188,
        SDL_SCANCODE_KP_B := 189,
        SDL_SCANCODE_KP_C := 190,
        SDL_SCANCODE_KP_D := 191,
        SDL_SCANCODE_KP_E := 192,
        SDL_SCANCODE_KP_F := 193,
        SDL_SCANCODE_KP_XOR := 194,
        SDL_SCANCODE_KP_POWER := 195,
        SDL_SCANCODE_KP_PERCENT := 196,
        SDL_SCANCODE_KP_LESS := 197,
        SDL_SCANCODE_KP_GREATER := 198,
        SDL_SCANCODE_KP_AMPERSAND := 199,
        SDL_SCANCODE_KP_DBLAMPERSAND := 200,
        SDL_SCANCODE_KP_VERTICALBAR := 201,
        SDL_SCANCODE_KP_DBLVERTICALBAR := 202,
        SDL_SCANCODE_KP_COLON := 203,
        SDL_SCANCODE_KP_HASH := 204,
        SDL_SCANCODE_KP_SPACE := 205,
        SDL_SCANCODE_KP_AT := 206,
        SDL_SCANCODE_KP_EXCLAM := 207,
        SDL_SCANCODE_KP_MEMSTORE := 208,
        SDL_SCANCODE_KP_MEMRECALL := 209,
        SDL_SCANCODE_KP_MEMCLEAR := 210,
        SDL_SCANCODE_KP_MEMADD := 211,
        SDL_SCANCODE_KP_MEMSUBTRACT := 212,
        SDL_SCANCODE_KP_MEMMULTIPLY := 213,
        SDL_SCANCODE_KP_MEMDIVIDE := 214,
        SDL_SCANCODE_KP_PLUSMINUS := 215,
        SDL_SCANCODE_KP_CLEAR := 216,
        SDL_SCANCODE_KP_CLEARENTRY := 217,
        SDL_SCANCODE_KP_BINARY := 218,
        SDL_SCANCODE_KP_OCTAL := 219,
        SDL_SCANCODE_KP_DECIMAL := 220,
        SDL_SCANCODE_KP_HEXADECIMAL := 221,
        SDL_SCANCODE_LCTRL := 224,
        SDL_SCANCODE_LSHIFT := 225,
        SDL_SCANCODE_LALT := 226,
        SDL_SCANCODE_LGUI := 227,
        SDL_SCANCODE_RCTRL := 228,
        SDL_SCANCODE_RSHIFT := 229,
        SDL_SCANCODE_RALT := 230,
        SDL_SCANCODE_RGUI := 231,
        SDL_SCANCODE_MODE := 257,
        SDL_SCANCODE_AUDIONEXT := 258,
        SDL_SCANCODE_AUDIOPREV := 259,
        SDL_SCANCODE_AUDIOSTOP := 260,
        SDL_SCANCODE_AUDIOPLAY := 261,
        SDL_SCANCODE_AUDIOMUTE := 262,
        SDL_SCANCODE_MEDIASELECT := 263,
        SDL_SCANCODE_WWW := 264,
        SDL_SCANCODE_MAIL := 265,
        SDL_SCANCODE_CALCULATOR := 266,
        SDL_SCANCODE_COMPUTER := 267,
        SDL_SCANCODE_AC_SEARCH := 268,
        SDL_SCANCODE_AC_HOME := 269,
        SDL_SCANCODE_AC_BACK := 270,
        SDL_SCANCODE_AC_FORWARD := 271,
        SDL_SCANCODE_AC_STOP := 272,
        SDL_SCANCODE_AC_REFRESH := 273,
        SDL_SCANCODE_AC_BOOKMARKS := 274,
        SDL_SCANCODE_BRIGHTNESSDOWN := 275,
        SDL_SCANCODE_BRIGHTNESSUP := 276,
        SDL_SCANCODE_DISPLAYSWITCH := 277,
        SDL_SCANCODE_KBDILLUMTOGGLE := 278,
        SDL_SCANCODE_KBDILLUMDOWN := 279,
        SDL_SCANCODE_KBDILLUMUP := 280,
        SDL_SCANCODE_EJECT := 281,
        SDL_SCANCODE_SLEEP := 282,
        SDL_NUM_SCANCODES := 512
      );

      SDL_Keycode = Sint32;

      SDL_Keymod = (
        KMOD_NONE := $0000,
        KMOD_LSHIFT := $0001,
        KMOD_RSHIFT := $0002,
        KMOD_LCTRL := $0040,
        KMOD_RCTRL := $0080,
        KMOD_LALT := $0100,
        KMOD_RALT := $0200,
        KMOD_LGUI := $0400,
        KMOD_RGUI := $0800,
        KMOD_NUM := $1000,
        KMOD_CAPS := $2000,
        KMOD_MODE := $4000,
        KMOD_RESERVED := $8000
      );

      SDL_Keysym = record
          scancode : SDL_Scancode;
          sym : SDL_Keycode;
          kmod : Uint16;
          unicode : Uint32;
        end;



      SDL_TouchID = Sint64;

      SDL_FingerID = Sint64;

      SDL_Finger = record
          id : SDL_FingerID;
          x : Uint16;
          y : Uint16;
          pressure : Uint16;
          xdelta : Uint16;
          ydelta : Uint16;
          last_x : Uint16;
          last_y : Uint16;
          last_pressure : Uint16;
          down : SDL_bool;
        end;

      SDL_Touch = record
          FreeTouch : procedure (touch:PSDL_Touch);cdecl;
          pressure_max : single;
          pressure_min : single;
          x_max : single;
          x_min : single;
          y_max : single;
          y_min : single;
          xres : Uint16;
          yres : Uint16;
          pressureres : Uint16;
          native_xres : single;
          native_yres : single;
          native_pressureres : single;
          tilt : single;
          rotation : single;
          id : SDL_TouchID;
          focus : Pointer;
          name : Pchar;
          buttonstate : Uint8;
          relative_mode : SDL_bool;
          flush_motion : SDL_bool;
          num_fingers : longint;
          max_fingers : longint;
          fingers : PPSDL_Finger;
          driverdata : pointer;
        end;

      SDL_GestureID = Sint64;

      SDL_EventType = (
        SDL_FIRSTEVENT := 0,
        SDL_QUIT_EVENT := $100,
        SDL_WINDOW_EVENT := $200,
        SDL_SYSWM_EVENT := 513,
        SDL_KEYDOWN := $300,
        SDL_KEYUP := 769,
        SDL_TEXTEDITING := 770,
        SDL_TEXTINPUT := 771,
        SDL_MOUSEMOTION := $400,
        SDL_MOUSEBUTTONDOWN := 1025,
        SDL_MOUSEBUTTONUP := 1026,
        SDL_MOUSEWHEEL := 1027,
        SDL_INPUTMOTION := $500,
        SDL_INPUTBUTTONDOWN := 1281,
        SDL_INPUTBUTTONUP := 1282,
        SDL_INPUTWHEEL := 1283,
        SDL_INPUTPROXIMITYIN := 1284,
        SDL_INPUTPROXIMITYOUT := 1285,
        SDL_JOYAXISMOTION := $600,
        SDL_JOYBALLMOTION := 1537,
        SDL_JOYHATMOTION := 1538,
        SDL_JOYBUTTONDOWN := 1539,
        SDL_JOYBUTTONUP := 1540,
        SDL_FINGERDOWN := $700,
        SDL_FINGERUP := 1793,
        SDL_FINGERMOTION := 1794,
        SDL_TOUCHBUTTONDOWN := 1795,
        SDL_TOUCHBUTTONUP := 1796,
        SDL_DOLLARGESTURE := $800,
        SDL_DOLLARRECORD := 2049,
        SDL_MULTIGESTURE := 2050,
        SDL_CLIPBOARDUPDATE := $900,
        SDL_DROPFILE := $1000,
        SDL_EVENT_COMPAT1 := $7000,
        SDL_EVENT_COMPAT2 := 28673,
        SDL_EVENT_COMPAT3 := 28674,
        SDL_USER_EVENT := $8000,
        SDL_LASTEVENT := $FFFF
      );

      SDL_WindowEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          event : Uint8;
          padding1 : Uint8;
          padding2 : Uint8;
          padding3 : Uint8;
          data1 : longint;
          data2 : longint;
        end;

      SDL_KeyboardEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          state : Uint8;
          _repeat : Uint8;
          padding2 : Uint8;
          padding3 : Uint8;
          keysym : SDL_Keysym;
        end;

      SDL_TextEditingEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          text : array[0..31] of char;
          start : longint;
          length : longint;
        end;

      SDL_TextInputEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          text : array[0..31] of char;
        end;

      SDL_MouseMotionEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          state : Uint8;
          padding1 : Uint8;
          padding2 : Uint8;
          padding3 : Uint8;
          x : longint;
          y : longint;
          xrel : longint;
          yrel : longint;
        end;

      SDL_MouseButtonEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          button : Uint8;
          state : Uint8;
          padding1 : Uint8;
          padding2 : Uint8;
          x : longint;
          y : longint;
        end;

      SDL_MouseWheelEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          x : longint;
          y : longint;
        end;

      SDL_JoyAxisEvent = record
          _type : Uint32;
          timestamp : Uint32;
          which : Uint8;
          axis : Uint8;
          padding1 : Uint8;
          padding2 : Uint8;
          value : longint;
        end;

      SDL_JoyBallEvent = record
          _type : Uint32;
          timestamp : Uint32;
          which : Uint8;
          ball : Uint8;
          padding1 : Uint8;
          padding2 : Uint8;
          xrel : longint;
          yrel : longint;
        end;

      SDL_JoyHatEvent = record
          _type : Uint32;
          timestamp : Uint32;
          which : Uint8;
          hat : Uint8;
          value : Uint8;
          padding1 : Uint8;
        end;

      SDL_JoyButtonEvent = record
          _type : Uint32;
          timestamp : Uint32;
          which : Uint8;
          button : Uint8;
          state : Uint8;
          padding1 : Uint8;
        end;

      SDL_TouchFingerEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          touchId : SDL_TouchID;
          fingerId : SDL_FingerID;
          state : Uint8;
          padding1 : Uint8;
          padding2 : Uint8;
          padding3 : Uint8;
          x : Uint16;
          y : Uint16;
          dx : Sint16;
          dy : Sint16;
          pressure : Uint16;
        end;

      SDL_TouchButtonEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          touchId : SDL_TouchID;
          state : Uint8;
          button : Uint8;
          padding1 : Uint8;
          padding2 : Uint8;
        end;

      SDL_MultiGestureEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          touchId : SDL_TouchID;
          dTheta : single;
          dDist : single;
          x : single;
          y : single;
          numFingers : Uint16;
          padding : Uint16;
        end;

      SDL_DollarGestureEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          touchId : SDL_TouchID;
          gestureId : SDL_GestureID;
          numFingers : Uint32;
          error : single;
        end;

      SDL_DropEvent = record
          _type : Uint32;
          timestamp : Uint32;
          _file : Pchar;
        end;

      SDL_QuitEvent = record
          _type : Uint32;
          timestamp : Uint32;
        end;

      SDL_UserEvent = record
          _type : Uint32;
          timestamp : Uint32;
          windowID : Uint32;
          code : longint;
          data1 : pointer;
          data2 : pointer;
        end;


      SDL_SysWMEvent = record
          _type : Uint32;
          timestamp : Uint32;
          msg : PSDL_SysWMmsg;
        end;

      SDL_ActiveEvent = record
          _type : Uint32;
          timestamp : Uint32;
          gain : Uint8;
          state : Uint8;
        end;

      SDL_ResizeEvent = record
          _type : Uint32;
          timestamp : Uint32;
          w : longint;
          h : longint;
        end;

      SDL_Event = record
          case longint of
            0 : ( _type : Uint32 );
            1 : ( window : SDL_WindowEvent );
            2 : ( key : SDL_KeyboardEvent );
            3 : ( edit : SDL_TextEditingEvent );
            4 : ( text : SDL_TextInputEvent );
            5 : ( motion : SDL_MouseMotionEvent );
            6 : ( button : SDL_MouseButtonEvent );
            7 : ( wheel : SDL_MouseWheelEvent );
            8 : ( jaxis : SDL_JoyAxisEvent );
            9 : ( jball : SDL_JoyBallEvent );
            10 : ( jhat : SDL_JoyHatEvent );
            11 : ( jbutton : SDL_JoyButtonEvent );
            12 : ( quit : SDL_QuitEvent );
            13 : ( user : SDL_UserEvent );
            14 : ( syswm : SDL_SysWMEvent );
            15 : ( tfinger : SDL_TouchFingerEvent );
            16 : ( tbutton : SDL_TouchButtonEvent );
            17 : ( mgesture : SDL_MultiGestureEvent );
            18 : ( dgesture : SDL_DollarGestureEvent );
            19 : ( drop : SDL_DropEvent );
            20 : ( active : SDL_ActiveEvent );
            21 : ( resize : SDL_ResizeEvent );
          end;

      SDL_eventaction = (
        SDL_ADDEVENT := 0,
        SDL_PEEKEVENT := 1,
        SDL_GETEVENT := 2
      );

      SDL_EventFilter = function (userdata:pointer; event:PSDL_Event):longint;cdecl;

      SDL_HintPriority = (
        SDL_HINT_DEFAULT := 0,
        SDL_HINT_NORMAL := 1,
        SDL_HINT_OVERRIDE := 2
      );

      SDL_LogPriority = (
        SDL_LOG_PRIORITY_VERBOSE := 1,
        SDL_LOG_PRIORITY_DEBUG := 2,
        SDL_LOG_PRIORITY_INFO := 3,
        SDL_LOG_PRIORITY_WARN := 4,
        SDL_LOG_PRIORITY_ERROR := 5,
        SDL_LOG_PRIORITY_CRITICAL := 6,
        SDL_NUM_LOG_PRIORITIES := 7
      );

      SDL_LogOutputFunction = procedure (userdata:pointer; category:longint; priority:SDL_LogPriority; message:Pchar);cdecl;

      SDL_PowerState = (
        SDL_POWERSTATE_UNKNOWN := 0,
        SDL_POWERSTATE_ON_BATTERY := 1,
        SDL_POWERSTATE_NO_BATTERY := 2,
        SDL_POWERSTATE_CHARGING := 3,
        SDL_POWERSTATE_CHARGED := 4
      );

      SDL_RendererFlags = (
        SDL_RENDERER_SOFTWARE := $00000001,
        SDL_RENDERER_ACCELERATED := $00000002,
        SDL_RENDERER_PRESENTVSYNC := $00000004
      );

      SDL_RendererInfo = record
          name : Pchar;
          flags : Uint32;
          num_texture_formats : Uint32;
          texture_formats : array[0..15] of Uint32;
          max_texture_width : longint;
          max_texture_height : longint;
        end;

      SDL_TextureAccess = (
        SDL_TEXTUREACCESS_STATIC := 0,
        SDL_TEXTUREACCESS_STREAMING := 1
      );

      SDL_TextureModulate = (
        SDL_TEXTUREMODULATE_NONE := $00000000,
        SDL_TEXTUREMODULATE_COLOR := $00000001,
        SDL_TEXTUREMODULATE_ALPHA := $00000002
      );



      SDL_TimerCallback = function (interval:Uint32; param:pointer):Uint32;cdecl;

      SDL_TimerID = longint;

      SDL_version = record
          major : Uint8;
          minor : Uint8;
          patch : Uint8;
        end;

      SDL_VideoInfo = record
          flag0 : longint;
          video_mem : Uint32;
          vfmt : PSDL_PixelFormat;
          current_w : longint;
          current_h : longint;
        end;

      SDL_Overlay = record
          format : Uint32;
          w : longint;
          h : longint;
          planes : longint;
          pitches : PUint16;
          pixels : PPUint8;
          hwfuncs : Pointer;
          hwdata : Pointer;
          flag0 : longint;
        end;

      SDL_GrabMode = (
        SDL_GRAB_QUERY := -(1),
        SDL_GRAB_OFF := 0,
        SDL_GRAB_ON := 1
      );


      SDL_WindowID = Pointer;

      SDL_OldTimerCallback = function (interval:Uint32):Uint32;cdecl;

const
      SDL_INIT_TIMER = $00000001;    
      SDL_INIT_AUDIO = $00000010;    
      SDL_INIT_VIDEO = $00000020;    
      SDL_INIT_JOYSTICK = $00000200;    
      SDL_INIT_HAPTIC = $00001000;    
    {*< Don't catch fatal signals  }
      SDL_INIT_NOPARACHUTE = $00100000;    
      SDL_INIT_EVERYTHING = $0000FFFF;    
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
      SDL_BUTTON_LEFT = 1;    
      SDL_BUTTON_MIDDLE = 2;    
      SDL_BUTTON_RIGHT = 3;    
      SDL_BUTTON_X1 = 4;    
      SDL_BUTTON_X2 = 5;    
    { was #define dname def_expr }
      SDL_HAT_CENTERED = $00;    
      SDL_HAT_UP = $01;    
      SDL_HAT_RIGHT = $02;    
      SDL_HAT_DOWN = $04;    
      SDL_HAT_LEFT = $08;    
      SDL_HAT_RIGHTUP = SDL_HAT_RIGHT or SDL_HAT_UP;    
      SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;    
      SDL_HAT_LEFTUP = SDL_HAT_LEFT or SDL_HAT_UP;    
      SDL_HAT_LEFTDOWN = SDL_HAT_LEFT or SDL_HAT_DOWN;    
    {*< Note Not used  }
      SDL_SWSURFACE = $00000000;    
      SDL_SRCALPHA = $00010000;    
      SDL_SRCCOLORKEY = $00020000;    
      SDL_ANYFORMAT = $00100000;    
      SDL_HWPALETTE = $00200000;    
      SDL_DOUBLEBUF = $00400000;    
      SDL_FULLSCREEN = $00800000;    
      SDL_RESIZABLE = $01000000;    
      SDL_NOFRAME = $02000000;    
      SDL_OPENGL = $04000000;    
    {*< Note Not used  }
      SDL_HWSURFACE = $08000001;    
    {*< Note Not used  }
      SDL_ASYNCBLIT = $08000000;    
    {*< Note Not used  }
      SDL_RLEACCELOK = $08000000;    
    {*< Note Not used  }
      SDL_HWACCEL = $08000000;    
      SDL_APPMOUSEFOCUS = $01;    
      SDL_APPINPUTFOCUS = $02;    
      SDL_APPACTIVE = $04;    
      SDL_LOGPAL = $01;    
      SDL_PHYSPAL = $02;    
      SDL_ACTIVE_EVENT = SDL_EVENT_COMPAT1;    
      SDL_VIDEORESIZE = SDL_EVENT_COMPAT2;    
      SDL_VIDEOEXPOSE = SDL_EVENT_COMPAT3;    
      SDL_BUTTON_WHEELUP = 4;    
      SDL_BUTTON_WHEELDOWN = 5;    
      SDL_DEFAULT_REPEAT_DELAY = 500;    
      SDL_DEFAULT_REPEAT_INTERVAL = 30;    
    {*< Planar mode: Y + V + U  (3 planes)  }
      SDL_YV12_OVERLAY = $32315659;    
    {*< Planar mode: Y + U + V  (3 planes)  }
      SDL_IYUV_OVERLAY = $56555949;    
    {*< Packed mode: Y0+U0+Y1+V0 (1 plane)  }
      SDL_YUY2_OVERLAY = $32595559;    
    {*< Packed mode: U0+Y0+V0+Y1 (1 plane)  }
      SDL_UYVY_OVERLAY = $59565955;    
    {*< Packed mode: Y0+V0+Y1+U0 (1 plane)  }
      SDL_YVYU_OVERLAY = $55595659;    
    {*< Signed 16-bit samples  }
      AUDIO_S16LSB = $8010;    
      SDL_ALPHA_TRANSPARENT = 0;    
      SDL_ENABLE = 1;    
    {*< Surface is RLE encoded  }
      SDL_RLEACCEL = $00000002;       
      SDL_WINDOWPOS_UNDEFINED_MASK = $1FFF0000;    
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
      SDL_WINDOWPOS_CENTERED_MASK = $2FFF0000;    
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }   
        SDL_PIXELTYPE_UNKNOWN = 0;
        SDL_PIXELTYPE_INDEX1 = 1;
        SDL_PIXELTYPE_INDEX4 = 2;
        SDL_PIXELTYPE_INDEX8 = 3;
        SDL_PIXELTYPE_PACKED8 = 4;
        SDL_PIXELTYPE_PACKED16 = 5;
        SDL_PIXELTYPE_PACKED32 = 6;
        SDL_PIXELTYPE_ARRAYU8 = 7;
        SDL_PIXELTYPE_ARRAYU16 = 8;
        SDL_PIXELTYPE_ARRAYU32 = 9;
        SDL_PIXELTYPE_ARRAYF16 = 10;
        SDL_PIXELTYPE_ARRAYF32 = 11;
        SDL_BITMAPORDER_NONE = 0;
        SDL_BITMAPORDER_4321 = 1;
        SDL_BITMAPORDER_1234 = 2;
        SDL_PACKEDORDER_NONE = 0;
        SDL_PACKEDORDER_XRGB = 1;
        SDL_PACKEDORDER_RGBX = 2;
        SDL_PACKEDORDER_ARGB = 3;
        SDL_PACKEDORDER_RGBA = 4;
        SDL_PACKEDORDER_XBGR = 5;
        SDL_PACKEDORDER_BGRX = 6;
        SDL_PACKEDORDER_ABGR = 7;
        SDL_PACKEDORDER_BGRA = 8;
        SDL_ARRAYORDER_NONE = 0;
        SDL_ARRAYORDER_RGB = 1;
        SDL_ARRAYORDER_RGBA = 2;
        SDL_ARRAYORDER_ARGB = 3;
        SDL_ARRAYORDER_BGR = 4;
        SDL_ARRAYORDER_BGRA = 5;
        SDL_ARRAYORDER_ABGR = 6;
        SDL_PACKEDLAYOUT_NONE = 0;
        SDL_PACKEDLAYOUT_332 = 1;
        SDL_PACKEDLAYOUT_4444 = 2;
        SDL_PACKEDLAYOUT_1555 = 3;
        SDL_PACKEDLAYOUT_5551 = 4;
        SDL_PACKEDLAYOUT_565 = 5;
        SDL_PACKEDLAYOUT_8888 = 6;
        SDL_PACKEDLAYOUT_2101010 = 7;
        SDL_PACKEDLAYOUT_1010102 = 8;
        SDL_PIXELFORMAT_UNKNOWN = 0;
        SDL_PIXELFORMAT_INDEX1LSB = (((((1 shl 31) or (SDL_PIXELTYPE_INDEX1 shl 24)) or (SDL_BITMAPORDER_4321 shl 20)) or (0 shl 16)) or (1 shl 8)) or (0 shl 0);
        SDL_PIXELFORMAT_INDEX1MSB = (((((1 shl 31) or (SDL_PIXELTYPE_INDEX1 shl 24)) or (SDL_BITMAPORDER_1234 shl 20)) or (0 shl 16)) or (1 shl 8)) or (0 shl 0);
        SDL_PIXELFORMAT_INDEX4LSB = (((((1 shl 31) or (SDL_PIXELTYPE_INDEX4 shl 24)) or (SDL_BITMAPORDER_4321 shl 20)) or (0 shl 16)) or (4 shl 8)) or (0 shl 0);
        SDL_PIXELFORMAT_INDEX4MSB = (((((1 shl 31) or (SDL_PIXELTYPE_INDEX4 shl 24)) or (SDL_BITMAPORDER_1234 shl 20)) or (0 shl 16)) or (4 shl 8)) or (0 shl 0);
        SDL_PIXELFORMAT_INDEX8 = (((((1 shl 31) or (SDL_PIXELTYPE_INDEX8 shl 24)) or (0 shl 20)) or (0 shl 16)) or (8 shl 8)) or (1 shl 0);
        SDL_PIXELFORMAT_RGB332 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED8 shl 24)) or (SDL_PACKEDORDER_XRGB shl 20)) or (SDL_PACKEDLAYOUT_332 shl 16)) or (8 shl 8)) or (1 shl 0);
        SDL_PIXELFORMAT_RGB444 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_XRGB shl 20)) or (SDL_PACKEDLAYOUT_4444 shl 16)) or (12 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_RGB555 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_XRGB shl 20)) or (SDL_PACKEDLAYOUT_1555 shl 16)) or (15 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_BGR555 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_XBGR shl 20)) or (SDL_PACKEDLAYOUT_1555 shl 16)) or (15 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_ARGB4444 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_ARGB shl 20)) or (SDL_PACKEDLAYOUT_4444 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_RGBA4444 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_RGBA shl 20)) or (SDL_PACKEDLAYOUT_4444 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_ABGR4444 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_ABGR shl 20)) or (SDL_PACKEDLAYOUT_4444 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_BGRA4444 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_BGRA shl 20)) or (SDL_PACKEDLAYOUT_4444 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_ARGB1555 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_ARGB shl 20)) or (SDL_PACKEDLAYOUT_1555 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_RGBA5551 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_RGBA shl 20)) or (SDL_PACKEDLAYOUT_5551 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_ABGR1555 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_ABGR shl 20)) or (SDL_PACKEDLAYOUT_1555 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_BGRA5551 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_BGRA shl 20)) or (SDL_PACKEDLAYOUT_5551 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_RGB565 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_XRGB shl 20)) or (SDL_PACKEDLAYOUT_565 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_BGR565 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED16 shl 24)) or (SDL_PACKEDORDER_XBGR shl 20)) or (SDL_PACKEDLAYOUT_565 shl 16)) or (16 shl 8)) or (2 shl 0);
        SDL_PIXELFORMAT_RGB24 = (((((1 shl 31) or (SDL_PIXELTYPE_ARRAYU8 shl 24)) or (SDL_ARRAYORDER_RGB shl 20)) or (0 shl 16)) or (24 shl 8)) or (3 shl 0);
        SDL_PIXELFORMAT_BGR24 = (((((1 shl 31) or (SDL_PIXELTYPE_ARRAYU8 shl 24)) or (SDL_ARRAYORDER_BGR shl 20)) or (0 shl 16)) or (24 shl 8)) or (3 shl 0);
        SDL_PIXELFORMAT_RGB888 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_XRGB shl 20)) or (SDL_PACKEDLAYOUT_8888 shl 16)) or (24 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_RGBX8888 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_RGBX shl 20)) or (SDL_PACKEDLAYOUT_8888 shl 16)) or (24 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_BGR888 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_XBGR shl 20)) or (SDL_PACKEDLAYOUT_8888 shl 16)) or (24 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_BGRX8888 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_BGRX shl 20)) or (SDL_PACKEDLAYOUT_8888 shl 16)) or (24 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_ARGB8888 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_ARGB shl 20)) or (SDL_PACKEDLAYOUT_8888 shl 16)) or (32 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_RGBA8888 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_RGBA shl 20)) or (SDL_PACKEDLAYOUT_8888 shl 16)) or (32 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_ABGR8888 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_ABGR shl 20)) or (SDL_PACKEDLAYOUT_8888 shl 16)) or (32 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_BGRA8888 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_BGRA shl 20)) or (SDL_PACKEDLAYOUT_8888 shl 16)) or (32 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_ARGB2101010 = (((((1 shl 31) or (SDL_PIXELTYPE_PACKED32 shl 24)) or (SDL_PACKEDORDER_ARGB shl 20)) or (SDL_PACKEDLAYOUT_2101010 shl 16)) or (32 shl 8)) or (4 shl 0);
        SDL_PIXELFORMAT_YV12 = ((((Uint32(Uint8('Y'))) shl 0) or ((Uint32(Uint8('V'))) shl 8)) or ((Uint32(Uint8('1'))) shl 16)) or ((Uint32(Uint8('2'))) shl 24);
        SDL_PIXELFORMAT_IYUV = ((((Uint32(Uint8('I'))) shl 0) or ((Uint32(Uint8('Y'))) shl 8)) or ((Uint32(Uint8('U'))) shl 16)) or ((Uint32(Uint8('V'))) shl 24);
        SDL_PIXELFORMAT_YUY2 = ((((Uint32(Uint8('Y'))) shl 0) or ((Uint32(Uint8('U'))) shl 8)) or ((Uint32(Uint8('Y'))) shl 16)) or ((Uint32(Uint8('2'))) shl 24);
        SDL_PIXELFORMAT_UYVY = ((((Uint32(Uint8('U'))) shl 0) or ((Uint32(Uint8('Y'))) shl 8)) or ((Uint32(Uint8('V'))) shl 16)) or ((Uint32(Uint8('Y'))) shl 24);
        SDL_PIXELFORMAT_YVYU = ((((Uint32(Uint8('Y'))) shl 0) or ((Uint32(Uint8('V'))) shl 8)) or ((Uint32(Uint8('Y'))) shl 16)) or ((Uint32(Uint8('U'))) shl 24);
        SDLK_UNKNOWN = 0;
        SDLK_RETURN = #13;
        SDLK_ESCAPE = #27;
        SDLK_BACKSPACE = #8;
        SDLK_TAB = #9;
        SDLK_SPACE = ' ';
        SDLK_EXCLAIM = '!';
        SDLK_QUOTEDBL = '"';
        SDLK_HASH = '#';
        SDLK_PERCENT = '%';
        SDLK_DOLLAR = '$';
        SDLK_AMPERSAND = '&';
        SDLK_QUOTE = 39;
        SDLK_LEFTPAREN = '(';
        SDLK_RIGHTPAREN = ')';
        SDLK_ASTERISK = '*';
        SDLK_PLUS = '+';
        SDLK_COMMA = ',';
        SDLK_MINUS = '-';
        SDLK_PERIOD = '.';
        SDLK_SLASH = '/';
        SDLK_0 = '0';
        SDLK_1 = '1';
        SDLK_2 = '2';
        SDLK_3 = '3';
        SDLK_4 = '4';
        SDLK_5 = '5';
        SDLK_6 = '6';
        SDLK_7 = '7';
        SDLK_8 = '8';
        SDLK_9 = '9';
        SDLK_COLON = ':';
        SDLK_SEMICOLON = ';';
        SDLK_LESS = '<';
        SDLK_EQUALS = '=';
        SDLK_GREATER = '>';
        SDLK_QUESTION = '?';
        SDLK_AT = '@';
        SDLK_LEFTBRACKET = '[';
        SDLK_BACKSLASH = '\';
        SDLK_RIGHTBRACKET = ']';
        SDLK_CARET = '^';
        SDLK_UNDERSCORE = '_';
        SDLK_BACKQUOTE = '`';
        SDLK_a = 'a';
        SDLK_b = 'b';
        SDLK_c = 'c';
        SDLK_d = 'd';
        SDLK_e = 'e';
        SDLK_f = 'f';
        SDLK_g = 'g';
        SDLK_h = 'h';
        SDLK_i = 'i';
        SDLK_j = 'j';
        SDLK_k = 'k';
        SDLK_l = 'l';
        SDLK_m = 'm';
        SDLK_n = 'n';
        SDLK_o = 'o';
        SDLK_p = 'p';
        SDLK_q = 'q';
        SDLK_r = 'r';
        SDLK_s = 's';
        SDLK_t = 't';
        SDLK_u = 'u';
        SDLK_v = 'v';
        SDLK_w = 'w';
        SDLK_x = 'x';
        SDLK_y = 'y';
        SDLK_z = 'z';
        SDLK_CAPSLOCK = LongInt(SDL_SCANCODE_CAPSLOCK) or (1 shl 30);
        SDLK_F1 = LongInt(SDL_SCANCODE_F1) or (1 shl 30);
        SDLK_F2 = LongInt(SDL_SCANCODE_F2) or (1 shl 30);
        SDLK_F3 = LongInt(SDL_SCANCODE_F3) or (1 shl 30);
        SDLK_F4 = LongInt(SDL_SCANCODE_F4) or (1 shl 30);
        SDLK_F5 = LongInt(SDL_SCANCODE_F5) or (1 shl 30);
        SDLK_F6 = LongInt(SDL_SCANCODE_F6) or (1 shl 30);
        SDLK_F7 = LongInt(SDL_SCANCODE_F7) or (1 shl 30);
        SDLK_F8 = LongInt(SDL_SCANCODE_F8) or (1 shl 30);
        SDLK_F9 = LongInt(SDL_SCANCODE_F9) or (1 shl 30);
        SDLK_F10 = LongInt(SDL_SCANCODE_F10) or (1 shl 30);
        SDLK_F11 = LongInt(SDL_SCANCODE_F11) or (1 shl 30);
        SDLK_F12 = LongInt(SDL_SCANCODE_F12) or (1 shl 30);
        SDLK_PRINTSCREEN = LongInt(SDL_SCANCODE_PRINTSCREEN) or (1 shl 30);
        SDLK_SCROLLLOCK = LongInt(SDL_SCANCODE_SCROLLLOCK) or (1 shl 30);
        SDLK_PAUSE = LongInt(SDL_SCANCODE_PAUSE) or (1 shl 30);
        SDLK_INSERT = LongInt(SDL_SCANCODE_INSERT) or (1 shl 30);
        SDLK_HOME = LongInt(SDL_SCANCODE_HOME) or (1 shl 30);
        SDLK_PAGEUP = LongInt(SDL_SCANCODE_PAGEUP) or (1 shl 30);
        SDLK_DELETE = '\177';
        SDLK_END = LongInt(SDL_SCANCODE_END) or (1 shl 30);
        SDLK_PAGEDOWN = LongInt(SDL_SCANCODE_PAGEDOWN) or (1 shl 30);
        SDLK_RIGHT = LongInt(SDL_SCANCODE_RIGHT) or (1 shl 30);
        SDLK_LEFT = LongInt(SDL_SCANCODE_LEFT) or (1 shl 30);
        SDLK_DOWN = LongInt(SDL_SCANCODE_DOWN) or (1 shl 30);
        SDLK_UP = LongInt(SDL_SCANCODE_UP) or (1 shl 30);
        SDLK_NUMLOCKCLEAR = LongInt(SDL_SCANCODE_NUMLOCKCLEAR) or (1 shl 30);
        SDLK_KP_DIVIDE = LongInt(SDL_SCANCODE_KP_DIVIDE) or (1 shl 30);
        SDLK_KP_MULTIPLY = LongInt(SDL_SCANCODE_KP_MULTIPLY) or (1 shl 30);
        SDLK_KP_MINUS = LongInt(SDL_SCANCODE_KP_MINUS) or (1 shl 30);
        SDLK_KP_PLUS = LongInt(SDL_SCANCODE_KP_PLUS) or (1 shl 30);
        SDLK_KP_ENTER = LongInt(SDL_SCANCODE_KP_ENTER) or (1 shl 30);
        SDLK_KP_1 = LongInt(SDL_SCANCODE_KP_1) or (1 shl 30);
        SDLK_KP_2 = LongInt(SDL_SCANCODE_KP_2) or (1 shl 30);
        SDLK_KP_3 = LongInt(SDL_SCANCODE_KP_3) or (1 shl 30);
        SDLK_KP_4 = LongInt(SDL_SCANCODE_KP_4) or (1 shl 30);
        SDLK_KP_5 = LongInt(SDL_SCANCODE_KP_5) or (1 shl 30);
        SDLK_KP_6 = LongInt(SDL_SCANCODE_KP_6) or (1 shl 30);
        SDLK_KP_7 = LongInt(SDL_SCANCODE_KP_7) or (1 shl 30);
        SDLK_KP_8 = LongInt(SDL_SCANCODE_KP_8) or (1 shl 30);
        SDLK_KP_9 = LongInt(SDL_SCANCODE_KP_9) or (1 shl 30);
        SDLK_KP_0 = LongInt(SDL_SCANCODE_KP_0) or (1 shl 30);
        SDLK_KP_PERIOD = LongInt(SDL_SCANCODE_KP_PERIOD) or (1 shl 30);
        SDLK_APPLICATION = LongInt(SDL_SCANCODE_APPLICATION) or (1 shl 30);
        SDLK_POWER = LongInt(SDL_SCANCODE_POWER) or (1 shl 30);
        SDLK_KP_EQUALS = LongInt(SDL_SCANCODE_KP_EQUALS) or (1 shl 30);
        SDLK_F13 = LongInt(SDL_SCANCODE_F13) or (1 shl 30);
        SDLK_F14 = LongInt(SDL_SCANCODE_F14) or (1 shl 30);
        SDLK_F15 = LongInt(SDL_SCANCODE_F15) or (1 shl 30);
        SDLK_F16 = LongInt(SDL_SCANCODE_F16) or (1 shl 30);
        SDLK_F17 = LongInt(SDL_SCANCODE_F17) or (1 shl 30);
        SDLK_F18 = LongInt(SDL_SCANCODE_F18) or (1 shl 30);
        SDLK_F19 = LongInt(SDL_SCANCODE_F19) or (1 shl 30);
        SDLK_F20 = LongInt(SDL_SCANCODE_F20) or (1 shl 30);
        SDLK_F21 = LongInt(SDL_SCANCODE_F21) or (1 shl 30);
        SDLK_F22 = LongInt(SDL_SCANCODE_F22) or (1 shl 30);
        SDLK_F23 = LongInt(SDL_SCANCODE_F23) or (1 shl 30);
        SDLK_F24 = LongInt(SDL_SCANCODE_F24) or (1 shl 30);
        SDLK_EXECUTE = LongInt(SDL_SCANCODE_EXECUTE) or (1 shl 30);
        SDLK_HELP = LongInt(SDL_SCANCODE_HELP) or (1 shl 30);
        SDLK_MENU = LongInt(SDL_SCANCODE_MENU) or (1 shl 30);
        SDLK_SELECT = LongInt(SDL_SCANCODE_SELECT) or (1 shl 30);
        SDLK_STOP = LongInt(SDL_SCANCODE_STOP) or (1 shl 30);
        SDLK_AGAIN = LongInt(SDL_SCANCODE_AGAIN) or (1 shl 30);
        SDLK_UNDO = LongInt(SDL_SCANCODE_UNDO) or (1 shl 30);
        SDLK_CUT = LongInt(SDL_SCANCODE_CUT) or (1 shl 30);
        SDLK_COPY = LongInt(SDL_SCANCODE_COPY) or (1 shl 30);
        SDLK_PASTE = LongInt(SDL_SCANCODE_PASTE) or (1 shl 30);
        SDLK_FIND = LongInt(SDL_SCANCODE_FIND) or (1 shl 30);
        SDLK_MUTE = LongInt(SDL_SCANCODE_MUTE) or (1 shl 30);
        SDLK_VOLUMEUP = LongInt(SDL_SCANCODE_VOLUMEUP) or (1 shl 30);
        SDLK_VOLUMEDOWN = LongInt(SDL_SCANCODE_VOLUMEDOWN) or (1 shl 30);
        SDLK_KP_COMMA = LongInt(SDL_SCANCODE_KP_COMMA) or (1 shl 30);
        SDLK_KP_EQUALSAS400 = LongInt(SDL_SCANCODE_KP_EQUALSAS400) or (1 shl 30);
        SDLK_ALTERASE = LongInt(SDL_SCANCODE_ALTERASE) or (1 shl 30);
        SDLK_SYSREQ = LongInt(SDL_SCANCODE_SYSREQ) or (1 shl 30);
        SDLK_CANCEL = LongInt(SDL_SCANCODE_CANCEL) or (1 shl 30);
        SDLK_CLEAR = LongInt(SDL_SCANCODE_CLEAR) or (1 shl 30);
        SDLK_PRIOR = LongInt(SDL_SCANCODE_PRIOR) or (1 shl 30);
        SDLK_RETURN2 = LongInt(SDL_SCANCODE_RETURN2) or (1 shl 30);
        SDLK_SEPARATOR = LongInt(SDL_SCANCODE_SEPARATOR) or (1 shl 30);
        SDLK_OUT = LongInt(SDL_SCANCODE_OUT) or (1 shl 30);
        SDLK_OPER = LongInt(SDL_SCANCODE_OPER) or (1 shl 30);
        SDLK_CLEARAGAIN = LongInt(SDL_SCANCODE_CLEARAGAIN) or (1 shl 30);
        SDLK_CRSEL = LongInt(SDL_SCANCODE_CRSEL) or (1 shl 30);
        SDLK_EXSEL = LongInt(SDL_SCANCODE_EXSEL) or (1 shl 30);
        SDLK_KP_00 = LongInt(SDL_SCANCODE_KP_00) or (1 shl 30);
        SDLK_KP_000 = LongInt(SDL_SCANCODE_KP_000) or (1 shl 30);
        SDLK_THOUSANDSSEPARATOR = LongInt(SDL_SCANCODE_THOUSANDSSEPARATOR) or (1 shl 30);
        SDLK_DECIMALSEPARATOR = LongInt(SDL_SCANCODE_DECIMALSEPARATOR) or (1 shl 30);
        SDLK_CURRENCYUNIT = LongInt(SDL_SCANCODE_CURRENCYUNIT) or (1 shl 30);
        SDLK_CURRENCYSUBUNIT = LongInt(SDL_SCANCODE_CURRENCYSUBUNIT) or (1 shl 30);
        SDLK_KP_LEFTPAREN = LongInt(SDL_SCANCODE_KP_LEFTPAREN) or (1 shl 30);
        SDLK_KP_RIGHTPAREN = LongInt(SDL_SCANCODE_KP_RIGHTPAREN) or (1 shl 30);
        SDLK_KP_LEFTBRACE = LongInt(SDL_SCANCODE_KP_LEFTBRACE) or (1 shl 30);
        SDLK_KP_RIGHTBRACE = LongInt(SDL_SCANCODE_KP_RIGHTBRACE) or (1 shl 30);
        SDLK_KP_TAB = LongInt(SDL_SCANCODE_KP_TAB) or (1 shl 30);
        SDLK_KP_BACKSPACE = LongInt(SDL_SCANCODE_KP_BACKSPACE) or (1 shl 30);
        SDLK_KP_A = LongInt(SDL_SCANCODE_KP_A) or (1 shl 30);
        SDLK_KP_B = LongInt(SDL_SCANCODE_KP_B) or (1 shl 30);
        SDLK_KP_C = LongInt(SDL_SCANCODE_KP_C) or (1 shl 30);
        SDLK_KP_D = LongInt(SDL_SCANCODE_KP_D) or (1 shl 30);
        SDLK_KP_E = LongInt(SDL_SCANCODE_KP_E) or (1 shl 30);
        SDLK_KP_F = LongInt(SDL_SCANCODE_KP_F) or (1 shl 30);
        SDLK_KP_XOR = LongInt(SDL_SCANCODE_KP_XOR) or (1 shl 30);
        SDLK_KP_POWER = LongInt(SDL_SCANCODE_KP_POWER) or (1 shl 30);
        SDLK_KP_PERCENT = LongInt(SDL_SCANCODE_KP_PERCENT) or (1 shl 30);
        SDLK_KP_LESS = LongInt(SDL_SCANCODE_KP_LESS) or (1 shl 30);
        SDLK_KP_GREATER = LongInt(SDL_SCANCODE_KP_GREATER) or (1 shl 30);
        SDLK_KP_AMPERSAND = LongInt(SDL_SCANCODE_KP_AMPERSAND) or (1 shl 30);
        SDLK_KP_DBLAMPERSAND = LongInt(SDL_SCANCODE_KP_DBLAMPERSAND) or (1 shl 30);
        SDLK_KP_VERTICALBAR = LongInt(SDL_SCANCODE_KP_VERTICALBAR) or (1 shl 30);
        SDLK_KP_DBLVERTICALBAR = LongInt(SDL_SCANCODE_KP_DBLVERTICALBAR) or (1 shl 30);
        SDLK_KP_COLON = LongInt(SDL_SCANCODE_KP_COLON) or (1 shl 30);
        SDLK_KP_HASH = LongInt(SDL_SCANCODE_KP_HASH) or (1 shl 30);
        SDLK_KP_SPACE = LongInt(SDL_SCANCODE_KP_SPACE) or (1 shl 30);
        SDLK_KP_AT = LongInt(SDL_SCANCODE_KP_AT) or (1 shl 30);
        SDLK_KP_EXCLAM = LongInt(SDL_SCANCODE_KP_EXCLAM) or (1 shl 30);
        SDLK_KP_MEMSTORE = LongInt(SDL_SCANCODE_KP_MEMSTORE) or (1 shl 30);
        SDLK_KP_MEMRECALL = LongInt(SDL_SCANCODE_KP_MEMRECALL) or (1 shl 30);
        SDLK_KP_MEMCLEAR = LongInt(SDL_SCANCODE_KP_MEMCLEAR) or (1 shl 30);
        SDLK_KP_MEMADD = LongInt(SDL_SCANCODE_KP_MEMADD) or (1 shl 30);
        SDLK_KP_MEMSUBTRACT = LongInt(SDL_SCANCODE_KP_MEMSUBTRACT) or (1 shl 30);
        SDLK_KP_MEMMULTIPLY = LongInt(SDL_SCANCODE_KP_MEMMULTIPLY) or (1 shl 30);
        SDLK_KP_MEMDIVIDE = LongInt(SDL_SCANCODE_KP_MEMDIVIDE) or (1 shl 30);
        SDLK_KP_PLUSMINUS = LongInt(SDL_SCANCODE_KP_PLUSMINUS) or (1 shl 30);
        SDLK_KP_CLEAR = LongInt(SDL_SCANCODE_KP_CLEAR) or (1 shl 30);
        SDLK_KP_CLEARENTRY = LongInt(SDL_SCANCODE_KP_CLEARENTRY) or (1 shl 30);
        SDLK_KP_BINARY = LongInt(SDL_SCANCODE_KP_BINARY) or (1 shl 30);
        SDLK_KP_OCTAL = LongInt(SDL_SCANCODE_KP_OCTAL) or (1 shl 30);
        SDLK_KP_DECIMAL = LongInt(SDL_SCANCODE_KP_DECIMAL) or (1 shl 30);
        SDLK_KP_HEXADECIMAL = LongInt(SDL_SCANCODE_KP_HEXADECIMAL) or (1 shl 30);
        SDLK_LCTRL = LongInt(SDL_SCANCODE_LCTRL) or (1 shl 30);
        SDLK_LSHIFT = LongInt(SDL_SCANCODE_LSHIFT) or (1 shl 30);
        SDLK_LALT = LongInt(SDL_SCANCODE_LALT) or (1 shl 30);
        SDLK_LGUI = LongInt(SDL_SCANCODE_LGUI) or (1 shl 30);
        SDLK_RCTRL = LongInt(SDL_SCANCODE_RCTRL) or (1 shl 30);
        SDLK_RSHIFT = LongInt(SDL_SCANCODE_RSHIFT) or (1 shl 30);
        SDLK_RALT = LongInt(SDL_SCANCODE_RALT) or (1 shl 30);
        SDLK_RGUI = LongInt(SDL_SCANCODE_RGUI) or (1 shl 30);
        SDLK_MODE = LongInt(SDL_SCANCODE_MODE) or (1 shl 30);
        SDLK_AUDIONEXT = LongInt(SDL_SCANCODE_AUDIONEXT) or (1 shl 30);
        SDLK_AUDIOPREV = LongInt(SDL_SCANCODE_AUDIOPREV) or (1 shl 30);
        SDLK_AUDIOSTOP = LongInt(SDL_SCANCODE_AUDIOSTOP) or (1 shl 30);
        SDLK_AUDIOPLAY = LongInt(SDL_SCANCODE_AUDIOPLAY) or (1 shl 30);
        SDLK_AUDIOMUTE = LongInt(SDL_SCANCODE_AUDIOMUTE) or (1 shl 30);
        SDLK_MEDIASELECT = LongInt(SDL_SCANCODE_MEDIASELECT) or (1 shl 30);
        SDLK_WWW = LongInt(SDL_SCANCODE_WWW) or (1 shl 30);
        SDLK_MAIL = LongInt(SDL_SCANCODE_MAIL) or (1 shl 30);
        SDLK_CALCULATOR = LongInt(SDL_SCANCODE_CALCULATOR) or (1 shl 30);
        SDLK_COMPUTER = LongInt(SDL_SCANCODE_COMPUTER) or (1 shl 30);
        SDLK_AC_SEARCH = LongInt(SDL_SCANCODE_AC_SEARCH) or (1 shl 30);
        SDLK_AC_HOME = LongInt(SDL_SCANCODE_AC_HOME) or (1 shl 30);
        SDLK_AC_BACK = LongInt(SDL_SCANCODE_AC_BACK) or (1 shl 30);
        SDLK_AC_FORWARD = LongInt(SDL_SCANCODE_AC_FORWARD) or (1 shl 30);
        SDLK_AC_STOP = LongInt(SDL_SCANCODE_AC_STOP) or (1 shl 30);
        SDLK_AC_REFRESH = LongInt(SDL_SCANCODE_AC_REFRESH) or (1 shl 30);
        SDLK_AC_BOOKMARKS = LongInt(SDL_SCANCODE_AC_BOOKMARKS) or (1 shl 30);
        SDLK_BRIGHTNESSDOWN = LongInt(SDL_SCANCODE_BRIGHTNESSDOWN) or (1 shl 30);
        SDLK_BRIGHTNESSUP = LongInt(SDL_SCANCODE_BRIGHTNESSUP) or (1 shl 30);
        SDLK_DISPLAYSWITCH = LongInt(SDL_SCANCODE_DISPLAYSWITCH) or (1 shl 30);
        SDLK_KBDILLUMTOGGLE = LongInt(SDL_SCANCODE_KBDILLUMTOGGLE) or (1 shl 30);
        SDLK_KBDILLUMDOWN = LongInt(SDL_SCANCODE_KBDILLUMDOWN) or (1 shl 30);
        SDLK_KBDILLUMUP = LongInt(SDL_SCANCODE_KBDILLUMUP) or (1 shl 30);
        SDLK_EJECT = LongInt(SDL_SCANCODE_EJECT) or (1 shl 30);
        SDLK_SLEEP = LongInt(SDL_SCANCODE_SLEEP) or (1 shl 30);
        SDL_LOG_CATEGORY_APPLICATION = 0;
        SDL_LOG_CATEGORY_ERROR = 1;
        SDL_LOG_CATEGORY_SYSTEM = 2;
        SDL_LOG_CATEGORY_AUDIO = 3;
        SDL_LOG_CATEGORY_VIDEO = 4;
        SDL_LOG_CATEGORY_RENDER = 5;
        SDL_LOG_CATEGORY_INPUT = 6;
        SDL_LOG_CATEGORY_RESERVED1 = 7;
        SDL_LOG_CATEGORY_RESERVED2 = 8;
        SDL_LOG_CATEGORY_RESERVED3 = 9;
        SDL_LOG_CATEGORY_RESERVED4 = 10;
        SDL_LOG_CATEGORY_RESERVED5 = 11;
        SDL_LOG_CATEGORY_RESERVED6 = 12;
        SDL_LOG_CATEGORY_RESERVED7 = 13;
        SDL_LOG_CATEGORY_RESERVED8 = 14;
        SDL_LOG_CATEGORY_RESERVED9 = 15;
        SDL_LOG_CATEGORY_RESERVED10 = 16;
        SDL_LOG_CATEGORY_CUSTOM = 17;
      bm_SDL_VideoInfo_hw_available = $1;
      bp_SDL_VideoInfo_hw_available = 0;
      bm_SDL_VideoInfo_wm_available = $2;
      bp_SDL_VideoInfo_wm_available = 1;
      bm_SDL_VideoInfo_UnusedBits1 = $FC;
      bp_SDL_VideoInfo_UnusedBits1 = 2;
      bm_SDL_VideoInfo_UnusedBits2 = $100;
      bp_SDL_VideoInfo_UnusedBits2 = 8;
      bm_SDL_VideoInfo_blit_hw = $200;
      bp_SDL_VideoInfo_blit_hw = 9;
      bm_SDL_VideoInfo_blit_hw_CC = $400;
      bp_SDL_VideoInfo_blit_hw_CC = 10;
      bm_SDL_VideoInfo_blit_hw_A = $800;
      bp_SDL_VideoInfo_blit_hw_A = 11;
      bm_SDL_VideoInfo_blit_sw = $1000;
      bp_SDL_VideoInfo_blit_sw = 12;
      bm_SDL_VideoInfo_blit_sw_CC = $2000;
      bp_SDL_VideoInfo_blit_sw_CC = 13;
      bm_SDL_VideoInfo_blit_sw_A = $4000;
      bp_SDL_VideoInfo_blit_sw_A = 14;
      bm_SDL_VideoInfo_blit_fill = $8000;
      bp_SDL_VideoInfo_blit_fill = 15;
      bm_SDL_VideoInfo_UnusedBits3 = $FFFF0000;
      bp_SDL_VideoInfo_UnusedBits3 = 16;
      bm_SDL_Overlay_hw_overlay = $1;
      bp_SDL_Overlay_hw_overlay = 0;
      bm_SDL_Overlay_UnusedBits = $FFFFFFFE;
      bp_SDL_Overlay_UnusedBits = 1;
      SDLK_LMETA = SDLK_LGUI;    
      SDLK_RMETA = SDLK_RGUI;    
      KMOD_LMETA = KMOD_LGUI;    
      KMOD_RMETA = KMOD_RGUI;
    {$EXTERNALSYM SDLK_LMETA}
      SDLK_LSUPER = 311; // Left "Windows" key
    {$EXTERNALSYM SDLK_LSUPER}
      SDLK_RSUPER = 312; // Right "Windows" key
  function SDL_BUTTON(X : longint) : longint;  
  function SDL_BUTTON_LMASK : longint; { return type might be wrong }
  { was #define dname def_expr }
  function SDL_BUTTON_MMASK : longint; { return type might be wrong }
  { was #define dname def_expr }
  function SDL_BUTTON_RMASK : longint; { return type might be wrong }
  { was #define dname def_expr }
  function SDL_BUTTON_X1MASK : longint; { return type might be wrong }
  { was #define dname def_expr }
  function SDL_BUTTON_X2MASK : longint; { return type might be wrong }
  function SDL_WINDOWPOS_UNDEFINED_DISPLAY(X : longint) : longint;  
  { was #define dname def_expr }
  function SDL_WINDOWPOS_UNDEFINED : longint; { return type might be wrong }
  function SDL_WINDOWPOS_CENTERED_DISPLAY(X : longint) : longint;  
  { was #define dname def_expr }
  function SDL_WINDOWPOS_CENTERED : longint; { return type might be wrong }
  function SDL_GetPlatform:Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_wcslen(_string:Pwchar_t):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_wcslcpy(dst:Pwchar_t; src:Pwchar_t; maxlen:size_t):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_wcslcat(dst:Pwchar_t; src:Pwchar_t; maxlen:size_t):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_utf8strlcpy(dst:Pchar; src:Pchar; dst_bytes:size_t):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_strrev(_string:Pchar):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_strupr(_string:Pchar):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_strlwr(_string:Pchar):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ltoa(value:longint; _string:Pchar; radix:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ultoa(value:dword; _string:Pchar; radix:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_lltoa(value:Sint64; _string:Pchar; radix:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ulltoa(value:Uint64; _string:Pchar; radix:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_iconv_open(tocode:Pchar; fromcode:Pchar):SDL_iconv_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_iconv_close(cd:SDL_iconv_t):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_iconv(cd:SDL_iconv_t; inbuf:PPchar; inbytesleft:Psize_t; outbuf:PPchar; outbytesleft:Psize_t):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_iconv_string(tocode:Pchar; fromcode:Pchar; inbuf:Pchar; inbytesleft:size_t):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_main(argc:longint; argv:PPchar):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ReportAssertion(_para1:PSDL_assert_data; _para2:Pchar; _para3:Pchar; _para4:longint):SDL_assert_state;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetAssertionHandler(handler:SDL_AssertionHandler; userdata:pointer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetAssertionReport:PSDL_assert_data;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_ResetAssertionReport;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AtomicTryLock(lock:PSDL_SpinLock):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_AtomicLock(lock:PSDL_SpinLock);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_AtomicUnlock(lock:PSDL_SpinLock);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AtomicCAS_(a:PSDL_atomic_t; oldval:longint; newval:longint):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AtomicCASPtr_(a:Ppointer; oldval:pointer; newval:pointer):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetError(fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetError(fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetError:Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_ClearError;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_Error(code:SDL_errorcode);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateMutex:PSDL_mutex;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_mutexP(mutex:PSDL_mutex):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_mutexV(mutex:PSDL_mutex):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_DestroyMutex(mutex:PSDL_mutex);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateSemaphore(initial_value:Uint32):PSDL_sem;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_DestroySemaphore(sem:PSDL_sem);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SemWait(sem:PSDL_sem):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SemTryWait(sem:PSDL_sem):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SemWaitTimeout(sem:PSDL_sem; ms:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SemPost(sem:PSDL_sem):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SemValue(sem:PSDL_sem):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateCond:PSDL_cond;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_DestroyCond(cond:PSDL_cond);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CondSignal(cond:PSDL_cond):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CondBroadcast(cond:PSDL_cond):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CondWait(cond:PSDL_cond; mutex:PSDL_mutex):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CondWaitTimeout(cond:PSDL_cond; mutex:PSDL_mutex; ms:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateThread(fn:SDL_ThreadFunction; name:Pchar; data:pointer):PSDL_Thread;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetThreadName(thread:PSDL_Thread):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ThreadID:SDL_Thread_ID;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetThreadID(thread:PSDL_Thread):SDL_Thread_ID;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetThreadPriority(priority:SDL_ThreadPriority):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_WaitThread(thread:PSDL_Thread; status:Plongint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RWFromFile(fname:Pchar; mode:Pchar):PSDL_RWops;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RWFromFP(fp:PFILE; autoclose:SDL_bool):PSDL_RWops;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RWFromMem(mem:pointer; size:longint):PSDL_RWops;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RWFromConstMem(mem:pointer; size:longint):PSDL_RWops;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AllocRW:PSDL_RWops;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FreeRW(area:PSDL_RWops);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ReadLE16(src:PSDL_RWops):Uint16;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ReadBE16(src:PSDL_RWops):Uint16;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ReadLE32(src:PSDL_RWops):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ReadBE32(src:PSDL_RWops):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ReadLE64(src:PSDL_RWops):Uint64;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ReadBE64(src:PSDL_RWops):Uint64;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WriteLE16(dst:PSDL_RWops; value:Uint16):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WriteBE16(dst:PSDL_RWops; value:Uint16):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WriteLE32(dst:PSDL_RWops; value:Uint32):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WriteBE32(dst:PSDL_RWops; value:Uint32):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WriteLE64(dst:PSDL_RWops; value:Uint64):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WriteBE64(dst:PSDL_RWops; value:Uint64):size_t;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetNumAudioDrivers:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetAudioDriver(index:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AudioInit(driver_name:Pchar):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_AudioQuit;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetCurrentAudioDriver:Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_OpenAudio(desired:PSDL_AudioSpec; obtained:PSDL_AudioSpec):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetNumAudioDevices(iscapture:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetAudioDeviceName(index:longint; iscapture:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_OpenAudioDevice(device:Pchar; iscapture:longint; desired:PSDL_AudioSpec; obtained:PSDL_AudioSpec; allowed_changes:longint):SDL_AudioDeviceID;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetAudioStatus:SDL_AudioStatus;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetAudioDeviceStatus(dev:SDL_AudioDeviceID):SDL_AudioStatus;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_PauseAudio(pause_on:longint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_PauseAudioDevice(dev:SDL_AudioDeviceID; pause_on:longint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LoadWAV_RW(src:PSDL_RWops; freesrc:longint; spec:PSDL_AudioSpec; audio_buf:PPUint8; audio_len:PUint32):PSDL_AudioSpec;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FreeWAV(audio_buf:PUint8);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_BuildAudioCVT(cvt:PSDL_AudioCVT; src_format:SDL_AudioFormat; src_channels:Uint8; src_rate:longint; dst_format:SDL_AudioFormat; 
             dst_channels:Uint8; dst_rate:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ConvertAudio(cvt:PSDL_AudioCVT):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_MixAudio(dst:PUint8; src:PUint8; len:Uint32; volume:longint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_MixAudioFormat(dst:PUint8; src:PUint8; format:SDL_AudioFormat; len:Uint32; volume:longint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LockAudio;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LockAudioDevice(dev:SDL_AudioDeviceID);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UnlockAudio;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UnlockAudioDevice(dev:SDL_AudioDeviceID);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_CloseAudio;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_CloseAudioDevice(dev:SDL_AudioDeviceID);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AudioDeviceConnected(dev:SDL_AudioDeviceID):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetClipboardText(text:Pchar):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetClipboardText:Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasClipboardText:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetCPUCount:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetCPUCacheLineSize:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasRDTSC:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasAltiVec:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasMMX:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_Has3DNow:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasSSE:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasSSE2:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasSSE3:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasSSE41:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasSSE42:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetPixelFormatName(format:Uint32):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_PixelFormatEnumToMasks(format:Uint32; bpp:Plongint; Rmask:PUint32; Gmask:PUint32; Bmask:PUint32; 
             Amask:PUint32):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_MasksToPixelFormatEnum(bpp:longint; Rmask:Uint32; Gmask:Uint32; Bmask:Uint32; Amask:Uint32):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AllocFormat(pixel_format:Uint32):PSDL_PixelFormat;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FreeFormat(format:PSDL_PixelFormat);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AllocPalette(ncolors:longint):PSDL_Palette;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetPixelFormatPalette(format:PSDL_PixelFormat; palette:PSDL_Palette):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetPaletteColors(palette:PSDL_Palette; colors:PSDL_Color; firstcolor:longint; ncolors:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FreePalette(palette:PSDL_Palette);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_MapRGB(format:PSDL_PixelFormat; r:Uint8; g:Uint8; b:Uint8):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_MapRGBA(format:PSDL_PixelFormat; r:Uint8; g:Uint8; b:Uint8; a:Uint8):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GetRGB(pixel:Uint32; format:PSDL_PixelFormat; r:PUint8; g:PUint8; b:PUint8);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GetRGBA(pixel:Uint32; format:PSDL_PixelFormat; r:PUint8; g:PUint8; b:PUint8; 
              a:PUint8);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_CalculateGammaRamp(gamma:single; ramp:PUint16);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasIntersection(A:PSDL_Rect; B:PSDL_Rect):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_IntersectRect(A:PSDL_Rect; B:PSDL_Rect; result:PSDL_Rect):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UnionRect(A:PSDL_Rect; B:PSDL_Rect; result:PSDL_Rect);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_EnclosePoints(points:PSDL_Point; count:longint; clip:PSDL_Rect; result:PSDL_Rect):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_IntersectRectAndLine(rect:PSDL_Rect; X1:Plongint; Y1:Plongint; X2:Plongint; Y2:Plongint):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateRGBSurface(flags:Uint32; width:longint; height:longint; depth:longint; Rmask:Uint32; 
             Gmask:Uint32; Bmask:Uint32; Amask:Uint32):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateRGBSurfaceFrom(pixels:pointer; width:longint; height:longint; depth:longint; pitch:longint; 
             Rmask:Uint32; Gmask:Uint32; Bmask:Uint32; Amask:Uint32):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FreeSurface(surface:PSDL_Surface);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetSurfacePalette(surface:PSDL_Surface; palette:PSDL_Palette):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LockSurface(surface:PSDL_Surface):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UnlockSurface(surface:PSDL_Surface);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LoadBMP_RW(src:PSDL_RWops; freesrc:longint):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SaveBMP_RW(surface:PSDL_Surface; dst:PSDL_RWops; freedst:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetSurfaceRLE(surface:PSDL_Surface; flag:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetColorKey(surface:PSDL_Surface; flag:longint; key:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetColorKey(surface:PSDL_Surface; key:PUint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetSurfaceColorMod(surface:PSDL_Surface; r:Uint8; g:Uint8; b:Uint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetSurfaceColorMod(surface:PSDL_Surface; r:PUint8; g:PUint8; b:PUint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetSurfaceAlphaMod(surface:PSDL_Surface; alpha:Uint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetSurfaceAlphaMod(surface:PSDL_Surface; alpha:PUint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetSurfaceBlendMode(surface:PSDL_Surface; blendMode:SDL_BlendMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetSurfaceBlendMode(surface:PSDL_Surface; blendMode:PSDL_BlendMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetClipRect(surface:PSDL_Surface; rect:PSDL_Rect):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GetClipRect(surface:PSDL_Surface; rect:PSDL_Rect);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ConvertSurface(src:PSDL_Surface; fmt:PSDL_PixelFormat; flags:Uint32):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ConvertSurfaceFormat(src:PSDL_Surface; pixel_format:Uint32; flags:Uint32):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ConvertPixels(width:longint; height:longint; src_format:Uint32; src:pointer; src_pitch:longint; 
             dst_format:Uint32; dst:pointer; dst_pitch:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_FillRect(dst:PSDL_Surface; rect:PSDL_Rect; color:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_FillRects(dst:PSDL_Surface; rects:PSDL_Rect; count:longint; color:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_UpperBlit(src:PSDL_Surface; srcrect:PSDL_Rect; dst:PSDL_Surface; dstrect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LowerBlit(src:PSDL_Surface; srcrect:PSDL_Rect; dst:PSDL_Surface; dstrect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SoftStretch(src:PSDL_Surface; srcrect:PSDL_Rect; dst:PSDL_Surface; dstrect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_UpperBlitScaled(src:PSDL_Surface; srcrect:PSDL_Rect; dst:PSDL_Surface; dstrect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LowerBlitScaled(src:PSDL_Surface; srcrect:PSDL_Rect; dst:PSDL_Surface; dstrect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetNumVideoDrivers:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetVideoDriver(index:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_VideoInit(driver_name:Pchar):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_VideoQuit;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetCurrentVideoDriver:Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetNumVideoDisplays:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetDisplayBounds(displayIndex:longint; rect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetNumDisplayModes(displayIndex:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetDisplayMode(displayIndex:longint; modeIndex:longint; mode:PSDL_DisplayMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetDesktopDisplayMode(displayIndex:longint; mode:PSDL_DisplayMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetCurrentDisplayMode(displayIndex:longint; mode:PSDL_DisplayMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetClosestDisplayMode(displayIndex:longint; mode:PSDL_DisplayMode; closest:PSDL_DisplayMode):PSDL_DisplayMode;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowDisplay(window:PSDL_Window):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetWindowDisplayMode(window:PSDL_Window; mode:PSDL_DisplayMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowDisplayMode(window:PSDL_Window; mode:PSDL_DisplayMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowPixelFormat(window:PSDL_Window):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateWindow(title:Pchar; x:longint; y:longint; w:longint; h:longint; 
             flags:Uint32):PSDL_Window;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateWindowFrom(data:pointer):PSDL_Window;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowID(window:PSDL_Window):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowFromID(id:Uint32):PSDL_Window;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowFlags(window:PSDL_Window):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetWindowTitle(window:PSDL_Window; title:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowTitle(window:PSDL_Window):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetWindowIcon(window:PSDL_Window; icon:PSDL_Surface);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetWindowData(window:PSDL_Window; name:Pchar; userdata:pointer):pointer;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowData(window:PSDL_Window; name:Pchar):pointer;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetWindowPosition(window:PSDL_Window; x:longint; y:longint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GetWindowPosition(window:PSDL_Window; x:Plongint; y:Plongint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetWindowSize(window:PSDL_Window; w:longint; h:longint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GetWindowSize(window:PSDL_Window; w:Plongint; h:Plongint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_ShowWindow(window:PSDL_Window);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_HideWindow(window:PSDL_Window);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_RaiseWindow(window:PSDL_Window);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_MaximizeWindow(window:PSDL_Window);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_MinimizeWindow(window:PSDL_Window);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_RestoreWindow(window:PSDL_Window);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetWindowFullscreen(window:PSDL_Window; fullscreen:SDL_bool):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowSurface(window:PSDL_Window):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_UpdateWindowSurface(window:PSDL_Window):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_UpdateWindowSurfaceRects(window:PSDL_Window; rects:PSDL_Rect; numrects:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetWindowGrab(window:PSDL_Window; grabbed:SDL_bool);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowGrab(window:PSDL_Window):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetWindowBrightness(window:PSDL_Window; brightness:single):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowBrightness(window:PSDL_Window):single;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetWindowGammaRamp(window:PSDL_Window; red:PUint16; green:PUint16; blue:PUint16):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWindowGammaRamp(window:PSDL_Window; red:PUint16; green:PUint16; blue:PUint16):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_DestroyWindow(window:PSDL_Window);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_IsScreenSaverEnabled:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_EnableScreenSaver;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_DisableScreenSaver;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_LoadLibrary(path:Pchar):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_GetProcAddress(proc:Pchar):pointer;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GL_UnloadLibrary;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_ExtensionSupported(extension:Pchar):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_SetAttribute(attr:SDL_GLattr; value:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_GetAttribute(attr:SDL_GLattr; value:Plongint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_CreateContext(window:PSDL_Window):SDL_GLContext;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_MakeCurrent(window:PSDL_Window; context:SDL_GLContext):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_SetSwapInterval(interval:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GL_GetSwapInterval:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GL_SwapWindow(window:PSDL_Window);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GL_DeleteContext(context:SDL_GLContext);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetKeyboardFocus:PSDL_Window;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetKeyboardState(numkeys:Plongint):PUint8;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetModState:SDL_Keymod;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetModState(modstate:SDL_Keymod);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetKeyFromScancode(scancode:SDL_Scancode):SDL_Keycode;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetScancodeFromKey(key:SDL_Keycode):SDL_Scancode;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetScancodeName(scancode:SDL_Scancode):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetScancodeFromName(name:Pchar):SDL_Scancode;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetKeyName(key:SDL_Keycode):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetKeyFromName(name:Pchar):SDL_Keycode;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_StartTextInput;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_StopTextInput;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetTextInputRect(rect:PSDL_Rect);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetMouseFocus:PSDL_Window;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetMouseState(x:Plongint; y:Plongint):Uint8;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  



  function SDL_GetRelativeMouseState(var x:longint; var y:longint):Uint8;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};





  procedure SDL_WarpMouseInWindow(window:PSDL_Window; x:longint; y:longint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetRelativeMouseMode(enabled:SDL_bool):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetRelativeMouseMode:SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateCursor(data:PUint8; mask:PUint8; w:longint; h:longint; hot_x:longint; 
             hot_y:longint):PSDL_Cursor;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateColorCursor(surface:PSDL_Surface; hot_x:longint; hot_y:longint):PSDL_Cursor;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetCursor(cursor:PSDL_Cursor);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetCursor:PSDL_Cursor;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FreeCursor(cursor:PSDL_Cursor);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ShowCursor(toggle:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_NumJoysticks:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickName(device_index:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickOpen(device_index:longint):PSDL_Joystick;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickOpened(device_index:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickIndex(joystick:PSDL_Joystick):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickNumAxes(joystick:PSDL_Joystick):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickNumBalls(joystick:PSDL_Joystick):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickNumHats(joystick:PSDL_Joystick):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickNumButtons(joystick:PSDL_Joystick):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_JoystickUpdate;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickEventState(state:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickGetAxis(joystick:PSDL_Joystick; axis:longint):Sint16;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickGetHat(joystick:PSDL_Joystick; hat:longint):Uint8;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickGetBall(joystick:PSDL_Joystick; ball:longint; dx:Plongint; dy:Plongint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_JoystickGetButton(joystick:PSDL_Joystick; button:longint):Uint8;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_JoystickClose(joystick:PSDL_Joystick);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetTouch(id:SDL_TouchID):PSDL_Touch;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetFinger(touch:PSDL_Touch; id:SDL_FingerID):PSDL_Finger;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RecordGesture(touchId:SDL_TouchID):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SaveAllDollarTemplates(src:PSDL_RWops):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SaveDollarTemplate(gestureId:SDL_GestureID; src:PSDL_RWops):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LoadDollarTemplates(touchId:SDL_TouchID; src:PSDL_RWops):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_PumpEvents;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_PeepEvents(events:PSDL_Event; numevents:longint; action:SDL_eventaction; minType:Uint32; maxType:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasEvent(_type:Uint32):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_HasEvents(minType:Uint32; maxType:Uint32):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FlushEvent(_type:Uint32);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FlushEvents(minType:Uint32; maxType:Uint32);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_PollEvent(event:PSDL_Event):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WaitEvent(event:PSDL_Event):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WaitEventTimeout(event:PSDL_Event; timeout:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_PushEvent(event:PSDL_Event):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_SetEventFilter(filter:SDL_EventFilter; userdata:pointer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetEventFilter(filter:PSDL_EventFilter; userdata:Ppointer):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_AddEventWatch(filter:SDL_EventFilter; userdata:pointer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_DelEventWatch(filter:SDL_EventFilter; userdata:pointer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FilterEvents(filter:SDL_EventFilter; userdata:pointer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_EventState(_type:Uint32; state:longint):Uint8;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RegisterEvents(numevents:longint):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetHintWithPriority(name:Pchar; value:Pchar; priority:SDL_HintPriority):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetHint(name:Pchar; value:Pchar):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetHint(name:Pchar):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_ClearHints;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LoadObject(sofname:Pchar):pointer;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LoadFunction(handle:pointer; name:Pchar):pointer;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UnloadObject(handle:pointer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogSetAllPriority(priority:SDL_LogPriority);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogSetPriority(category:longint; priority:SDL_LogPriority);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LogGetPriority(category:longint):SDL_LogPriority;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogResetPriorities;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_Log(fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_Log(fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogVerbose(category:longint; fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogVerbose(category:longint; fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogDebug(category:longint; fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogDebug(category:longint; fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogInfo(category:longint; fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogInfo(category:longint; fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogWarn(category:longint; fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogWarn(category:longint; fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogError(category:longint; fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogError(category:longint; fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogCritical(category:longint; fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogCritical(category:longint; fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogMessage(category:longint; priority:SDL_LogPriority; fmt:Pchar; args:array of const);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogMessage(category:longint; priority:SDL_LogPriority; fmt:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogMessageV(category:longint; priority:SDL_LogPriority; fmt:Pchar);cdecl;varargs; external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogGetOutputFunction(callback:PSDL_LogOutputFunction; userdata:Ppointer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_LogSetOutputFunction(callback:SDL_LogOutputFunction; userdata:pointer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetPowerInfo(secs:Plongint; pct:Plongint):SDL_PowerState;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetNumRenderDrivers:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetRenderDriverInfo(index:longint; info:PSDL_RendererInfo):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateRenderer(window:PSDL_Window; index:longint; flags:Uint32):PSDL_Renderer;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateSoftwareRenderer(surface:PSDL_Surface):PSDL_Renderer;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetRenderer(window:PSDL_Window):PSDL_Renderer;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetRendererInfo(renderer:PSDL_Renderer; info:PSDL_RendererInfo):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateTexture(renderer:PSDL_Renderer; format:Uint32; access:longint; w:longint; h:longint):PSDL_Texture;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateTextureFromSurface(renderer:PSDL_Renderer; surface:PSDL_Surface):PSDL_Texture;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_QueryTexture(texture:PSDL_Texture; format:PUint32; access:Plongint; w:Plongint; h:Plongint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetTextureColorMod(texture:PSDL_Texture; r:Uint8; g:Uint8; b:Uint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetTextureColorMod(texture:PSDL_Texture; r:PUint8; g:PUint8; b:PUint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetTextureAlphaMod(texture:PSDL_Texture; alpha:Uint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetTextureAlphaMod(texture:PSDL_Texture; alpha:PUint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetTextureBlendMode(texture:PSDL_Texture; blendMode:SDL_BlendMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetTextureBlendMode(texture:PSDL_Texture; blendMode:PSDL_BlendMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_UpdateTexture(texture:PSDL_Texture; rect:PSDL_Rect; pixels:pointer; pitch:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LockTexture(texture:PSDL_Texture; rect:PSDL_Rect; pixels:Ppointer; pitch:Plongint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UnlockTexture(texture:PSDL_Texture);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderSetViewport(renderer:PSDL_Renderer; rect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_RenderGetViewport(renderer:PSDL_Renderer; rect:PSDL_Rect);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetRenderDrawColor(renderer:PSDL_Renderer; r:Uint8; g:Uint8; b:Uint8; a:Uint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetRenderDrawColor(renderer:PSDL_Renderer; r:PUint8; g:PUint8; b:PUint8; a:PUint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetRenderDrawBlendMode(renderer:PSDL_Renderer; blendMode:SDL_BlendMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetRenderDrawBlendMode(renderer:PSDL_Renderer; blendMode:PSDL_BlendMode):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderClear(renderer:PSDL_Renderer):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderDrawPoint(renderer:PSDL_Renderer; x:longint; y:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderDrawPoints(renderer:PSDL_Renderer; points:PSDL_Point; count:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderDrawLine(renderer:PSDL_Renderer; x1:longint; y1:longint; x2:longint; y2:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderDrawLines(renderer:PSDL_Renderer; points:PSDL_Point; count:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderDrawRect(renderer:PSDL_Renderer; rect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderDrawRects(renderer:PSDL_Renderer; rects:PSDL_Rect; count:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderFillRect(renderer:PSDL_Renderer; rect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderFillRects(renderer:PSDL_Renderer; rects:PSDL_Rect; count:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderCopy(renderer:PSDL_Renderer; texture:PSDL_Texture; srcrect:PSDL_Rect; dstrect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RenderReadPixels(renderer:PSDL_Renderer; rect:PSDL_Rect; format:Uint32; pixels:pointer; pitch:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_RenderPresent(renderer:PSDL_Renderer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_DestroyTexture(texture:PSDL_Texture);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_DestroyRenderer(renderer:PSDL_Renderer);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetTicks:Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetPerformanceCounter:Uint64;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetPerformanceFrequency:Uint64;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_Delay(ms:Uint32);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AddTimer(interval:Uint32; callback:SDL_TimerCallback; param:pointer):SDL_TimerID;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_RemoveTimer(id:SDL_TimerID):SDL_bool;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GetVersion(ver:PSDL_version);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetRevision:Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetRevisionNumber:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function hw_available(var a : SDL_VideoInfo) : Uint32;
  procedure set_hw_available(var a : SDL_VideoInfo; __hw_available : Uint32);
  function wm_available(var a : SDL_VideoInfo) : Uint32;
  procedure set_wm_available(var a : SDL_VideoInfo; __wm_available : Uint32);
  function UnusedBits1(var a : SDL_VideoInfo) : Uint32;
  procedure set_UnusedBits1(var a : SDL_VideoInfo; __UnusedBits1 : Uint32);
  function UnusedBits2(var a : SDL_VideoInfo) : Uint32;
  procedure set_UnusedBits2(var a : SDL_VideoInfo; __UnusedBits2 : Uint32);
  function blit_hw(var a : SDL_VideoInfo) : Uint32;
  procedure set_blit_hw(var a : SDL_VideoInfo; __blit_hw : Uint32);
  function blit_hw_CC(var a : SDL_VideoInfo) : Uint32;
  procedure set_blit_hw_CC(var a : SDL_VideoInfo; __blit_hw_CC : Uint32);
  function blit_hw_A(var a : SDL_VideoInfo) : Uint32;
  procedure set_blit_hw_A(var a : SDL_VideoInfo; __blit_hw_A : Uint32);
  function blit_sw(var a : SDL_VideoInfo) : Uint32;
  procedure set_blit_sw(var a : SDL_VideoInfo; __blit_sw : Uint32);
  function blit_sw_CC(var a : SDL_VideoInfo) : Uint32;
  procedure set_blit_sw_CC(var a : SDL_VideoInfo; __blit_sw_CC : Uint32);
  function blit_sw_A(var a : SDL_VideoInfo) : Uint32;
  procedure set_blit_sw_A(var a : SDL_VideoInfo; __blit_sw_A : Uint32);
  function blit_fill(var a : SDL_VideoInfo) : Uint32;
  procedure set_blit_fill(var a : SDL_VideoInfo; __blit_fill : Uint32);
  function UnusedBits3(var a : SDL_VideoInfo) : Uint32;
  procedure set_UnusedBits3(var a : SDL_VideoInfo; __UnusedBits3 : Uint32);
  function hw_overlay(var a : SDL_Overlay) : Uint32;
  procedure set_hw_overlay(var a : SDL_Overlay; __hw_overlay : Uint32);
  function UnusedBits(var a : SDL_Overlay) : Uint32;
  procedure set_UnusedBits(var a : SDL_Overlay; __UnusedBits : Uint32);
  function SDL_Linked_Version:PSDL_version;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_AudioDriverName(namebuf:Pchar; maxlen:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_VideoDriverName(namebuf:Pchar; maxlen:longint):Pchar;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetVideoInfo:PSDL_VideoInfo;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_VideoModeOK(width:longint; height:longint; bpp:longint; flags:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_ListModes(format:PSDL_PixelFormat; flags:Uint32):PPSDL_Rect;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetVideoMode(width:longint; height:longint; bpp:longint; flags:Uint32):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetVideoSurface:PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UpdateRects(screen:PSDL_Surface; numrects:longint; rects:PSDL_Rect);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UpdateRect(screen:PSDL_Surface; x:Sint32; y:Sint32; w:Uint32; h:Uint32);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_Flip(screen:PSDL_Surface):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetAlpha(surface:PSDL_Surface; flag:Uint32; alpha:Uint8):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_DisplayFormat(surface:PSDL_Surface):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_DisplayFormatAlpha(surface:PSDL_Surface):PSDL_Surface;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_WM_SetCaption(title:Pchar; icon:Pchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_WM_GetCaption(title:PPchar; icon:PPchar);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_WM_SetIcon(icon:PSDL_Surface; mask:PUint8);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WM_IconifyWindow:longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WM_ToggleFullScreen(surface:PSDL_Surface):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WM_GrabInput(mode:SDL_GrabMode):SDL_GrabMode;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetPalette(surface:PSDL_Surface; flags:longint; colors:PSDL_Color; firstcolor:longint; ncolors:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetColors(surface:PSDL_Surface; colors:PSDL_Color; firstcolor:longint; ncolors:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetWMInfo(info:PSDL_SysWMinfo):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetAppState:Uint8;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_WarpMouse(x:Uint16; y:Uint16);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_CreateYUVOverlay(width:longint; height:longint; format:Uint32; display:PSDL_Surface):PSDL_Overlay;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_LockYUVOverlay(overlay:PSDL_Overlay):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_UnlockYUVOverlay(overlay:PSDL_Overlay);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_DisplayYUVOverlay(overlay:PSDL_Overlay; dstrect:PSDL_Rect):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_FreeYUVOverlay(overlay:PSDL_Overlay);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GL_SwapBuffers;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetGamma(red:single; green:single; blue:single):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetGammaRamp(red:PUint16; green:PUint16; blue:PUint16):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_GetGammaRamp(red:PUint16; green:PUint16; blue:PUint16):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_EnableKeyRepeat(delay:longint; interval:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_GetKeyRepeat(delay:Plongint; interval:Plongint);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_EnableUNICODE(enable:longint):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SetTimer(interval:Uint32; callback:SDL_OldTimerCallback):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_putenv(variable:Pchar):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_Init(flags:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_InitSubSystem(flags:Uint32):longint;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_QuitSubSystem(flags:Uint32);cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_WasInit(flags:Uint32):Uint32;cdecl;external {$ifdef WINDOWS}'SDL.dll'{$endif};
  procedure SDL_Quit;cdecl; external {$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_SaveBMP(surface: PSDL_Surface; filename: PChar): Integer;
  function SDL_Swap32(D: Uint32): Uint32;
  function SDL_iPhoneKeyboardShow(window : PSDL_Window ): Integer;cdecl;external{$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_iPhoneKeyboardHide(window : PSDL_Window ): Integer;cdecl;external{$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_iPhoneKeyboardIsShown(window : PSDL_Window ): SDL_bool;cdecl;external{$ifdef WINDOWS}'SDL.dll'{$endif};
  function SDL_iPhoneKeyboardToggle(window : PSDL_Window ): Integer;cdecl;external{$ifdef WINDOWS}'SDL.dll'{$endif};

implementation
function SDL_SaveBMP(surface: PSDL_Surface; filename: PChar): Integer;
begin
  Result := SDL_SaveBMP_RW(surface, SDL_RWFromFile(filename, 'wb'), 1);
end;

function SDL_Swap32(D: Uint32): Uint32;
begin
  Result := ((D shl 24) or ((D shl 8) and $00FF0000) or ((D shr 8) and $0000FF00) or (D shr 24));
end;
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SDL_BUTTON(X : longint) : longint;
  begin
    SDL_BUTTON:=1 shl (X-1);
  end;
  { was #define dname def_expr }
  function SDL_BUTTON_LMASK : longint; { return type might be wrong }
    begin
      SDL_BUTTON_LMASK:=SDL_BUTTON(SDL_BUTTON_LEFT);
    end;
  { was #define dname def_expr }
  function SDL_BUTTON_MMASK : longint; { return type might be wrong }
    begin
      SDL_BUTTON_MMASK:=SDL_BUTTON(SDL_BUTTON_MIDDLE);
    end;
  { was #define dname def_expr }
  function SDL_BUTTON_RMASK : longint; { return type might be wrong }
    begin
      SDL_BUTTON_RMASK:=SDL_BUTTON(SDL_BUTTON_RIGHT);
    end;
  { was #define dname def_expr }
  function SDL_BUTTON_X1MASK : longint; { return type might be wrong }
    begin
      SDL_BUTTON_X1MASK:=SDL_BUTTON(SDL_BUTTON_X1);
    end;
  { was #define dname def_expr }
  function SDL_BUTTON_X2MASK : longint; { return type might be wrong }
    begin
      SDL_BUTTON_X2MASK:=SDL_BUTTON(SDL_BUTTON_X2);
    end;
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SDL_WINDOWPOS_UNDEFINED_DISPLAY(X : longint) : longint;
  begin
    SDL_WINDOWPOS_UNDEFINED_DISPLAY:=SDL_WINDOWPOS_UNDEFINED_MASK or X;
  end;
  { was #define dname def_expr }
  function SDL_WINDOWPOS_UNDEFINED : longint; { return type might be wrong }
    begin
      SDL_WINDOWPOS_UNDEFINED:=SDL_WINDOWPOS_UNDEFINED_DISPLAY(0);
    end;
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function SDL_WINDOWPOS_CENTERED_DISPLAY(X : longint) : longint;
  begin
    SDL_WINDOWPOS_CENTERED_DISPLAY:=SDL_WINDOWPOS_CENTERED_MASK or X;
  end;
  { was #define dname def_expr }
  function SDL_WINDOWPOS_CENTERED : longint; { return type might be wrong }
    begin
      SDL_WINDOWPOS_CENTERED:=SDL_WINDOWPOS_CENTERED_DISPLAY(0);
    end;
  function hw_available(var a : SDL_VideoInfo) : Uint32;
    begin
      hw_available:=(a.flag0 and bm_SDL_VideoInfo_hw_available) shr bp_SDL_VideoInfo_hw_available;
    end;
  procedure set_hw_available(var a : SDL_VideoInfo; __hw_available : Uint32);
    begin
      a.flag0:=a.flag0 or ((__hw_available shl bp_SDL_VideoInfo_hw_available) and bm_SDL_VideoInfo_hw_available);
    end;
  function wm_available(var a : SDL_VideoInfo) : Uint32;
    begin
      wm_available:=(a.flag0 and bm_SDL_VideoInfo_wm_available) shr bp_SDL_VideoInfo_wm_available;
    end;
  procedure set_wm_available(var a : SDL_VideoInfo; __wm_available : Uint32);
    begin
      a.flag0:=a.flag0 or ((__wm_available shl bp_SDL_VideoInfo_wm_available) and bm_SDL_VideoInfo_wm_available);
    end;
  function UnusedBits1(var a : SDL_VideoInfo) : Uint32;
    begin
      UnusedBits1:=(a.flag0 and bm_SDL_VideoInfo_UnusedBits1) shr bp_SDL_VideoInfo_UnusedBits1;
    end;
  procedure set_UnusedBits1(var a : SDL_VideoInfo; __UnusedBits1 : Uint32);
    begin
      a.flag0:=a.flag0 or ((__UnusedBits1 shl bp_SDL_VideoInfo_UnusedBits1) and bm_SDL_VideoInfo_UnusedBits1);
    end;
  function UnusedBits2(var a : SDL_VideoInfo) : Uint32;
    begin
      UnusedBits2:=(a.flag0 and bm_SDL_VideoInfo_UnusedBits2) shr bp_SDL_VideoInfo_UnusedBits2;
    end;
  procedure set_UnusedBits2(var a : SDL_VideoInfo; __UnusedBits2 : Uint32);
    begin
      a.flag0:=a.flag0 or ((__UnusedBits2 shl bp_SDL_VideoInfo_UnusedBits2) and bm_SDL_VideoInfo_UnusedBits2);
    end;
  function blit_hw(var a : SDL_VideoInfo) : Uint32;
    begin
      blit_hw:=(a.flag0 and bm_SDL_VideoInfo_blit_hw) shr bp_SDL_VideoInfo_blit_hw;
    end;
  procedure set_blit_hw(var a : SDL_VideoInfo; __blit_hw : Uint32);
    begin
      a.flag0:=a.flag0 or ((__blit_hw shl bp_SDL_VideoInfo_blit_hw) and bm_SDL_VideoInfo_blit_hw);
    end;
  function blit_hw_CC(var a : SDL_VideoInfo) : Uint32;
    begin
      blit_hw_CC:=(a.flag0 and bm_SDL_VideoInfo_blit_hw_CC) shr bp_SDL_VideoInfo_blit_hw_CC;
    end;
  procedure set_blit_hw_CC(var a : SDL_VideoInfo; __blit_hw_CC : Uint32);
    begin
      a.flag0:=a.flag0 or ((__blit_hw_CC shl bp_SDL_VideoInfo_blit_hw_CC) and bm_SDL_VideoInfo_blit_hw_CC);
    end;
  function blit_hw_A(var a : SDL_VideoInfo) : Uint32;
    begin
      blit_hw_A:=(a.flag0 and bm_SDL_VideoInfo_blit_hw_A) shr bp_SDL_VideoInfo_blit_hw_A;
    end;
  procedure set_blit_hw_A(var a : SDL_VideoInfo; __blit_hw_A : Uint32);
    begin
      a.flag0:=a.flag0 or ((__blit_hw_A shl bp_SDL_VideoInfo_blit_hw_A) and bm_SDL_VideoInfo_blit_hw_A);
    end;
  function blit_sw(var a : SDL_VideoInfo) : Uint32;
    begin
      blit_sw:=(a.flag0 and bm_SDL_VideoInfo_blit_sw) shr bp_SDL_VideoInfo_blit_sw;
    end;
  procedure set_blit_sw(var a : SDL_VideoInfo; __blit_sw : Uint32);
    begin
      a.flag0:=a.flag0 or ((__blit_sw shl bp_SDL_VideoInfo_blit_sw) and bm_SDL_VideoInfo_blit_sw);
    end;
  function blit_sw_CC(var a : SDL_VideoInfo) : Uint32;
    begin
      blit_sw_CC:=(a.flag0 and bm_SDL_VideoInfo_blit_sw_CC) shr bp_SDL_VideoInfo_blit_sw_CC;
    end;
  procedure set_blit_sw_CC(var a : SDL_VideoInfo; __blit_sw_CC : Uint32);
    begin
      a.flag0:=a.flag0 or ((__blit_sw_CC shl bp_SDL_VideoInfo_blit_sw_CC) and bm_SDL_VideoInfo_blit_sw_CC);
    end;
  function blit_sw_A(var a : SDL_VideoInfo) : Uint32;
    begin
      blit_sw_A:=(a.flag0 and bm_SDL_VideoInfo_blit_sw_A) shr bp_SDL_VideoInfo_blit_sw_A;
    end;
  procedure set_blit_sw_A(var a : SDL_VideoInfo; __blit_sw_A : Uint32);
    begin
      a.flag0:=a.flag0 or ((__blit_sw_A shl bp_SDL_VideoInfo_blit_sw_A) and bm_SDL_VideoInfo_blit_sw_A);
    end;
  function blit_fill(var a : SDL_VideoInfo) : Uint32;
    begin
      blit_fill:=(a.flag0 and bm_SDL_VideoInfo_blit_fill) shr bp_SDL_VideoInfo_blit_fill;
    end;
  procedure set_blit_fill(var a : SDL_VideoInfo; __blit_fill : Uint32);
    begin
      a.flag0:=a.flag0 or ((__blit_fill shl bp_SDL_VideoInfo_blit_fill) and bm_SDL_VideoInfo_blit_fill);
    end;
  function UnusedBits3(var a : SDL_VideoInfo) : Uint32;
    begin
      UnusedBits3:=(a.flag0 and bm_SDL_VideoInfo_UnusedBits3) shr bp_SDL_VideoInfo_UnusedBits3;
    end;
  procedure set_UnusedBits3(var a : SDL_VideoInfo; __UnusedBits3 : Uint32);
    begin
      a.flag0:=a.flag0 or ((__UnusedBits3 shl bp_SDL_VideoInfo_UnusedBits3) and bm_SDL_VideoInfo_UnusedBits3);
    end;
  function hw_overlay(var a : SDL_Overlay) : Uint32;
    begin
      hw_overlay:=(a.flag0 and bm_SDL_Overlay_hw_overlay) shr bp_SDL_Overlay_hw_overlay;
    end;
  procedure set_hw_overlay(var a : SDL_Overlay; __hw_overlay : Uint32);
    begin
      a.flag0:=a.flag0 or ((__hw_overlay shl bp_SDL_Overlay_hw_overlay) and bm_SDL_Overlay_hw_overlay);
    end;
  function UnusedBits(var a : SDL_Overlay) : Uint32;
    begin
      UnusedBits:=(a.flag0 and bm_SDL_Overlay_UnusedBits) shr bp_SDL_Overlay_UnusedBits;
    end;
  procedure set_UnusedBits(var a : SDL_Overlay; __UnusedBits : Uint32);
    begin
      a.flag0:=a.flag0 or ((__UnusedBits shl bp_SDL_Overlay_UnusedBits) and bm_SDL_Overlay_UnusedBits);
    end;
end.
