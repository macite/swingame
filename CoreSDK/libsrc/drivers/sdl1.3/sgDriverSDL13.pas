unit sgDriverSDL13;
//=============================================================================
// sgDriverSDL13.pas
//=============================================================================
//
//
//
// 
//
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//		- Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
uses
  sgTypes, SDL2;
  
  procedure LoadSDL13Driver();

implementation
	uses sgDriver, sgShared, SysUtils, sgTrace, sgSDL13Utils;
	
	var
	  _Initialised : Boolean = False;
    //321 is Highest Value 321
    _KeyCode     : array[0..321] of LongInt;
	
  procedure InitKeyCodes();
  begin
    _KeyCode[LongInt(vk_Unknown)]    := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_BACKSPACE)]  := LongInt(SDLK_BACKSPACE);     //Hopeful
    _KeyCode[LongInt(vk_TAB)]        := LongInt(SDLK_TAB);                 //Hopeful
    _KeyCode[LongInt(vk_CLEAR)]      := LongInt(SDLK_CLEAR);
    _KeyCode[LongInt(vk_RETURN)]     := LongInt(SDLK_RETURN);           //Hopeful
    _KeyCode[LongInt(vk_PAUSE)]      := LongInt(SDLK_PAUSE);
    _KeyCode[LongInt(vk_ESCAPE)]     := LongInt(SDLK_ESCAPE);
    _KeyCode[LongInt(vk_SPACE)]      := LongInt(SDLK_SPACE);
    _KeyCode[LongInt(vk_EXCLAIM)]    := LongInt(SDLK_EXCLAIM);
    _KeyCode[LongInt(vk_QUOTEDBL)]   := LongInt(SDLK_QUOTEDBL);
    _KeyCode[LongInt(vk_HASH)]       := LongInt(SDLK_HASH);
    _KeyCode[LongInt(vk_DOLLAR)]     := LongInt(SDLK_DOLLAR);
    _KeyCode[LongInt(vk_AMPERSAND)]  := LongInt(SDLK_AMPERSAND);
    _KeyCode[LongInt(vk_QUOTE)]      := LongInt(SDLK_QUOTE);
    _KeyCode[LongInt(vk_LEFTPAREN)]  := LongInt(SDLK_LEFTPAREN);
    _KeyCode[LongInt(vk_RIGHTPAREN)] := LongInt(SDLK_RIGHTPAREN);
    _KeyCode[LongInt(vk_ASTERISK)]   := LongInt(SDLK_ASTERISK);
    _KeyCode[LongInt(vk_PLUS)]       := LongInt(SDLK_PLUS);
    _KeyCode[LongInt(vk_COMMA)]      := LongInt(SDLK_COMMA);
    _KeyCode[LongInt(vk_MINUS)]      := LongInt(SDLK_MINUS);
    _KeyCode[LongInt(vk_PERIOD)]     := LongInt(SDLK_PERIOD);
    _KeyCode[LongInt(vk_SLASH)]      := LongInt(SDLK_SLASH);
    _KeyCode[LongInt(vk_0)]          := LongInt(SDLK_0);
    _KeyCode[LongInt(vk_1)]          := LongInt(SDLK_1);
    _KeyCode[LongInt(vk_2)]          := LongInt(SDLK_2);
    _KeyCode[LongInt(vk_3)]          := LongInt(SDLK_3);
    _KeyCode[LongInt(vk_4)]          := LongInt(SDLK_4);
    _KeyCode[LongInt(vk_5)]          := LongInt(SDLK_5);
    _KeyCode[LongInt(vk_6)]          := LongInt(SDLK_6);
    _KeyCode[LongInt(vk_7)]          := LongInt(SDLK_7);
    _KeyCode[LongInt(vk_8)]          := LongInt(SDLK_8);
    _KeyCode[LongInt(vk_9)]          := LongInt(SDLK_9);
    _KeyCode[LongInt(vk_COLON)]      := LongInt(SDLK_COLON);
    _KeyCode[LongInt(vk_SEMICOLON)]  := LongInt(SDLK_SEMICOLON);
    _KeyCode[LongInt(vk_LESS)]       := LongInt(SDLK_LESS);
    _KeyCode[LongInt(vk_EQUALS)]     := LongInt(SDLK_EQUALS);
    _KeyCode[LongInt(vk_GREATER)]    := LongInt(SDLK_GREATER);
    _KeyCode[LongInt(vk_QUESTION)]   := LongInt(SDLK_QUESTION);
    _KeyCode[LongInt(vk_AT)]         := LongInt(SDLK_AT);
    
    // Skip uppercase letters
    
    _KeyCode[LongInt(vk_LEFTBRACKET)]  := LongInt(SDLK_LEFTBRACKET);
    _KeyCode[LongInt(vk_BACKSLASH)]    := LongInt(SDLK_BACKSLASH);           //Hopeful
    _KeyCode[LongInt(vk_RIGHTBRACKET)] := LongInt(SDLK_RIGHTBRACKET);
    _KeyCode[LongInt(vk_CARET)]        := LongInt(SDLK_CARET);
    _KeyCode[LongInt(vk_UNDERSCORE)]   := LongInt(SDLK_UNDERSCORE);
    _KeyCode[LongInt(vk_BACKQUOTE)]    := LongInt(SDLK_BACKQUOTE);
    _KeyCode[LongInt(vk_a)]            := LongInt(SDLK_a);
    _KeyCode[LongInt(vk_b)]            := LongInt(SDLK_b);
    _KeyCode[LongInt(vk_c)]            := LongInt(SDLK_c);
    _KeyCode[LongInt(vk_d)]            := LongInt(SDLK_d);
    _KeyCode[LongInt(vk_e)]            := LongInt(SDLK_e);
    _KeyCode[LongInt(vk_f)]            := LongInt(SDLK_f);
    _KeyCode[LongInt(vk_g)]            := LongInt(SDLK_g);
    _KeyCode[LongInt(vk_h)]            := LongInt(SDLK_h);
    _KeyCode[LongInt(vk_i)]            := LongInt(SDLK_i);
    _KeyCode[LongInt(vk_j)]            := LongInt(SDLK_j);
    _KeyCode[LongInt(vk_k)]            := LongInt(SDLK_k);
    _KeyCode[LongInt(vk_l)]            := LongInt(SDLK_l);
    _KeyCode[LongInt(vk_m)]            := LongInt(SDLK_m);
    _KeyCode[LongInt(vk_n)]            := LongInt(SDLK_n);
    _KeyCode[LongInt(vk_o)]            := LongInt(SDLK_o);
    _KeyCode[LongInt(vk_p)]            := LongInt(SDLK_p);
    _KeyCode[LongInt(vk_q)]            := LongInt(SDLK_q);
    _KeyCode[LongInt(vk_r)]            := LongInt(SDLK_r);
    _KeyCode[LongInt(vk_s)]            := LongInt(SDLK_s);
    _KeyCode[LongInt(vk_t)]            := LongInt(SDLK_t);
    _KeyCode[LongInt(vk_u)]            := LongInt(SDLK_u);
    _KeyCode[LongInt(vk_v)]            := LongInt(SDLK_v);
    _KeyCode[LongInt(vk_w)]            := LongInt(SDLK_w);
    _KeyCode[LongInt(vk_x)]            := LongInt(SDLK_x);
    _KeyCode[LongInt(vk_y)]            := LongInt(SDLK_y);
    _KeyCode[LongInt(vk_z)]            := LongInt(SDLK_z);
    
    _KeyCode[LongInt(vk_DELETE)]       := LongInt(SDLK_DELETE);
    
    // End of ASCII mapped keysyms
    // International keyboard syms
    
    _KeyCode[LongInt(vk_WORLD_0)]  := LongInt(SDLK_UNKNOWN); // 0xA0
    _KeyCode[LongInt(vk_WORLD_1)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_2)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_3)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_4)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_5)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_6)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_7)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_8)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_9)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_10)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_11)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_12)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_13)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_14)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_15)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_16)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_17)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_18)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_19)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_20)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_21)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_22)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_23)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_24)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_25)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_26)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_27)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_28)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_29)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_30)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_31)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_32)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_33)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_34)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_35)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_36)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_37)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_38)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_39)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_40)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_41)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_42)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_43)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_44)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_45)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_46)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_47)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_48)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_49)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_50)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_51)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_52)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_53)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_54)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_55)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_56)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_57)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_58)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_59)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_60)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_61)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_62)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_63)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_64)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_65)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_66)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_67)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_68)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_69)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_70)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_71)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_72)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_73)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_74)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_75)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_76)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_77)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_78)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_79)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_80)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_81)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_82)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_83)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_84)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_85)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_86)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_87)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_88)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_89)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_90)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_91)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_92)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_93)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_94)] := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_WORLD_95)] := LongInt(SDLK_UNKNOWN); // 0xFF
    
    // Numeric keypad    
    _KeyCode[LongInt(vk_KP0)]         := LongInt(SDLK_KP_0);
    _KeyCode[LongInt(vk_KP1)]         := LongInt(SDLK_KP_1);
    _KeyCode[LongInt(vk_KP2)]         := LongInt(SDLK_KP_2);
    _KeyCode[LongInt(vk_KP3)]         := LongInt(SDLK_KP_3);
    _KeyCode[LongInt(vk_KP4)]         := LongInt(SDLK_KP_4);
    _KeyCode[LongInt(vk_KP5)]         := LongInt(SDLK_KP_5);
    _KeyCode[LongInt(vk_KP6)]         := LongInt(SDLK_KP_6);
    _KeyCode[LongInt(vk_KP7)]         := LongInt(SDLK_KP_7);
    _KeyCode[LongInt(vk_KP8)]         := LongInt(SDLK_KP_8);
    _KeyCode[LongInt(vk_KP9)]         := LongInt(SDLK_KP_9);
    _KeyCode[LongInt(vk_KP_PERIOD)]   := LongInt(SDLK_KP_PERIOD);
    _KeyCode[LongInt(vk_KP_DIVIDE)]   := LongInt(SDLK_KP_DIVIDE);
    _KeyCode[LongInt(vk_KP_MULTIPLY)] := LongInt(SDLK_KP_MULTIPLY);
    _KeyCode[LongInt(vk_KP_MINUS)]    := LongInt(SDLK_KP_MINUS);
    _KeyCode[LongInt(vk_KP_PLUS)]     := LongInt(SDLK_KP_PLUS);
    _KeyCode[LongInt(vk_KP_ENTER)]    := LongInt(SDLK_KP_ENTER);
    _KeyCode[LongInt(vk_KP_EQUALS)]   := LongInt(SDLK_KP_EQUALS);
    
    // Arrows + Home/End pad    
    _KeyCode[LongInt(vk_UP)]       := LongInt(SDLK_UP);
    _KeyCode[LongInt(vk_DOWN)]     := LongInt(SDLK_DOWN);
    _KeyCode[LongInt(vk_RIGHT)]    := LongInt(SDLK_RIGHT);
    _KeyCode[LongInt(vk_LEFT)]     := LongInt(SDLK_LEFT);
    _KeyCode[LongInt(vk_INSERT)]   := LongInt(SDLK_INSERT);
    _KeyCode[LongInt(vk_HOME)]     := LongInt(SDLK_HOME);
    _KeyCode[LongInt(vk_END)]      := LongInt(SDLK_END);
    _KeyCode[LongInt(vk_PAGEUP)]   := LongInt(SDLK_PAGEUP);
    _KeyCode[LongInt(vk_PAGEDOWN)] := LongInt(SDLK_PAGEDOWN);
    
    // Function keys    
    _KeyCode[LongInt(vk_F1)]  := LongInt(SDLK_F1);
    _KeyCode[LongInt(vk_F2)]  := LongInt(SDLK_F2);
    _KeyCode[LongInt(vk_F3)]  := LongInt(SDLK_F3);
    _KeyCode[LongInt(vk_F4)]  := LongInt(SDLK_F4);
    _KeyCode[LongInt(vk_F5)]  := LongInt(SDLK_F5);
    _KeyCode[LongInt(vk_F6)]  := LongInt(SDLK_F6);
    _KeyCode[LongInt(vk_F7)]  := LongInt(SDLK_F7);
    _KeyCode[LongInt(vk_F8)]  := LongInt(SDLK_F8);
    _KeyCode[LongInt(vk_F9)]  := LongInt(SDLK_F9);
    _KeyCode[LongInt(vk_F10)] := LongInt(SDLK_F10);
    _KeyCode[LongInt(vk_F11)] := LongInt(SDLK_F11);
    _KeyCode[LongInt(vk_F12)] := LongInt(SDLK_F12);
    _KeyCode[LongInt(vk_F13)] := LongInt(SDLK_F13);
    _KeyCode[LongInt(vk_F14)] := LongInt(SDLK_F14);
    _KeyCode[LongInt(vk_F15)] := LongInt(SDLK_F15);
    
    // Key state modifier keys    
    _KeyCode[LongInt(vk_NUMLOCK)]   := LongInt(SDLK_NUMLOCKCLEAR);
    _KeyCode[LongInt(vk_CAPSLOCK)]  := LongInt(SDLK_CAPSLOCK);
    _KeyCode[LongInt(vk_SCROLLOCK)] := LongInt(SDLK_SCROLLLOCK);
    _KeyCode[LongInt(vk_RSHIFT)]    := LongInt(SDLK_RSHIFT);
    _KeyCode[LongInt(vk_LSHIFT)]    := LongInt(SDLK_LSHIFT);
    _KeyCode[LongInt(vk_RCTRL)]     := LongInt(SDLK_RCTRL);
    _KeyCode[LongInt(vk_LCTRL)]     := LongInt(SDLK_LCTRL);
    _KeyCode[LongInt(vk_RALT)]      := LongInt(SDLK_RALT);
    _KeyCode[LongInt(vk_LALT)]      := LongInt(SDLK_LALT);
    _KeyCode[LongInt(vk_RMETA)]     := LongInt(SDLK_RMETA);
    _KeyCode[LongInt(vk_LMETA)]     := LongInt(SDLK_LMETA);
    _KeyCode[LongInt(vk_LSUPER)]    := LongInt(SDLK_LSUPER); // Left "Windows" key
    _KeyCode[LongInt(vk_RSUPER)]    := LongInt(SDLK_RSUPER); // Right "Windows" key
    _KeyCode[LongInt(vk_MODE)]      := LongInt(SDLK_MODE); // "Alt Gr" key
    _KeyCode[LongInt(vk_COMPOSE)]   := LongInt(SDLK_UNKNOWN); // Multi-key compose key
    
    // Miscellaneous function keys
    _KeyCode[LongInt(vk_HELP)]   := LongInt(SDLK_HELP);
    _KeyCode[LongInt(vk_PRINT)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_SYSREQ)] := LongInt(SDLK_SYSREQ);
    _KeyCode[LongInt(vk_BREAK)]  := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(vk_MENU)]   := LongInt(SDLK_MENU);
    _KeyCode[LongInt(vk_POWER)]  := LongInt(SDLK_POWER); // Power Macintosh power key
    _KeyCode[LongInt(vk_EURO)]   := LongInt(SDLK_UNKNOWN) // Some european keyboards
  end;
 
  function GetKeyCodeProcedure(val : LongInt) : LongInt;
  begin
    result := _KeyCode[val];
  end;
	
	function GetErrorProcedure() : PChar;
	begin
		result := SDL_GetError();
	end;
  
  procedure QuitProcedure(); 
  begin
    if Assigned(PSDL13Screen(_screen)) then
    begin
      if Assigned(PSDL13Screen(_screen)^.renderer) then 
        SDL_DestroyRenderer(PSDL13Screen(_screen)^.renderer);
      if Assigned(PSDL13Screen(_screen)^.window) then
        SDL_DestroyWindow(PSDL13Screen(_screen)^.window);
      Dispose(PSDL13Screen(_screen));
    end;
    {$ifndef IOS}
    SDL_Quit();
    {$endif}
  end;
  
  procedure InitProcedure(); 
  begin
    if _Initialised then exit;
    _Initialised := true;
    
    if (SDL_INIT(SDL_INIT_VIDEO or SDL_INIT_AUDIO) = -1) then
    begin
      {$IFDEF Trace}
      TraceIf(tlError, 'sgShared', 'ERROR', 'InitialiseSwinGame', GetErrorProcedure());
      {$ENDIF}
      RaiseException('Error loading sdl... ' + GetErrorProcedure());
      exit;
    end;

    {$IFDEF Trace}
      TraceIf(tlInfo, 'sgShared', 'INFO', 'InitialiseSwinGame', 'About to enable unicode');
    {$ENDIF}
    InitKeyCodes();
    _screen := nil;
  end;
  
  
	procedure LoadSDL13Driver();
	begin
		Driver.GetError           := @GetErrorProcedure;
		Driver.GetKeyCode         := @GetKeyCodeProcedure;
		Driver.Quit               := @QuitProcedure;
		Driver.Init               := @InitProcedure;
	end;
end.