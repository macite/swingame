unit sgDriverSDL2;
//=============================================================================
// sgDriverSDL2.pas
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

  procedure LoadSDL2Driver();

implementation
	uses sgDriver, sgShared, SysUtils, sgTrace, sgTypes, sgDriverSDL2Types, sgDriverInput, sgMinSDL2;

	var
	  _Initialised : Boolean = False;

    //TODO: Move the array to SwinGame - init it from driver
    _KeyCode     : array[0..327] of LongInt;

  procedure InitKeyCodes();
  begin
    _KeyCode[LongInt(UnknownKey)]    := LongInt(SDLK_UNKNOWN);
    _KeyCode[LongInt(BackspaceKey)]  := LongInt(SDLK_BACKSPACE);     //Hopeful
    _KeyCode[LongInt(TabKey)]        := LongInt(SDLK_TAB);                 //Hopeful
    _KeyCode[LongInt(ClearKey)]      := LongInt(SDLK_CLEAR);
    _KeyCode[LongInt(ReturnKey)]     := LongInt(SDLK_RETURN);           //Hopeful
    _KeyCode[LongInt(PauseKey)]      := LongInt(SDLK_PAUSE);
    _KeyCode[LongInt(EscapeKey)]     := LongInt(SDLK_ESCAPE);
    _KeyCode[LongInt(SpaceKey)]      := LongInt(SDLK_SPACE);
    _KeyCode[LongInt(ExclaimKey)]    := LongInt(SDLK_EXCLAIM);
    _KeyCode[LongInt(DoubleQuoteKey)]   := LongInt(SDLK_QUOTEDBL);
    _KeyCode[LongInt(HashKey)]       := LongInt(SDLK_HASH);
    _KeyCode[LongInt(DollarKey)]     := LongInt(SDLK_DOLLAR);
    _KeyCode[LongInt(AmpersandKey)]  := LongInt(SDLK_AMPERSAND);
    _KeyCode[LongInt(QuoteKey)]      := LongInt(SDLK_QUOTE);
    _KeyCode[LongInt(LeftParenKey)]  := LongInt(SDLK_LEFTPAREN);
    _KeyCode[LongInt(RightParenKey)] := LongInt(SDLK_RIGHTPAREN);
    _KeyCode[LongInt(AsteriskKey)]   := LongInt(SDLK_ASTERISK);
    _KeyCode[LongInt(PlusKey)]       := LongInt(SDLK_PLUS);
    _KeyCode[LongInt(CommaKey)]      := LongInt(SDLK_COMMA);
    _KeyCode[LongInt(MinusKey)]      := LongInt(SDLK_MINUS);
    _KeyCode[LongInt(PeriodKey)]     := LongInt(SDLK_PERIOD);
    _KeyCode[LongInt(SlashKey)]      := LongInt(SDLK_SLASH);
    _KeyCode[LongInt(Key0)]          := LongInt(SDLK_0);
    _KeyCode[LongInt(Key1)]          := LongInt(SDLK_1);
    _KeyCode[LongInt(Key2)]          := LongInt(SDLK_2);
    _KeyCode[LongInt(Key3)]          := LongInt(SDLK_3);
    _KeyCode[LongInt(Key4)]          := LongInt(SDLK_4);
    _KeyCode[LongInt(Key5)]          := LongInt(SDLK_5);
    _KeyCode[LongInt(Key6)]          := LongInt(SDLK_6);
    _KeyCode[LongInt(Key7)]          := LongInt(SDLK_7);
    _KeyCode[LongInt(Key8)]          := LongInt(SDLK_8);
    _KeyCode[LongInt(Key9)]          := LongInt(SDLK_9);
    _KeyCode[LongInt(ColonKey)]      := LongInt(SDLK_COLON);
    _KeyCode[LongInt(SemicolonKey)]  := LongInt(SDLK_SEMICOLON);
    _KeyCode[LongInt(LessKey)]       := LongInt(SDLK_LESS);
    _KeyCode[LongInt(EqualsKey)]     := LongInt(SDLK_EQUALS);
    _KeyCode[LongInt(GreaterKey)]    := LongInt(SDLK_GREATER);
    _KeyCode[LongInt(QuestionKey)]   := LongInt(SDLK_QUESTION);
    _KeyCode[LongInt(AtKey)]         := LongInt(SDLK_AT);

    // Skip uppercase letters

    _KeyCode[LongInt(LeftBracketKey)]  := LongInt(SDLK_LEFTBRACKET);
    _KeyCode[LongInt(BackSlashKey)]    := LongInt(SDLK_BACKSLASH);           //Hopeful
    _KeyCode[LongInt(RightBracketKey)] := LongInt(SDLK_RIGHTBRACKET);
    _KeyCode[LongInt(CaretKey)]        := LongInt(SDLK_CARET);
    _KeyCode[LongInt(UnderscoreKey)]   := LongInt(SDLK_UNDERSCORE);
    _KeyCode[LongInt(BackquoteKey)]    := LongInt(SDLK_BACKQUOTE);
    _KeyCode[LongInt(AKey)]            := LongInt(SDLK_a);
    _KeyCode[LongInt(BKey)]            := LongInt(SDLK_b);
    _KeyCode[LongInt(CKey)]            := LongInt(SDLK_c);
    _KeyCode[LongInt(DKey)]            := LongInt(SDLK_d);
    _KeyCode[LongInt(EKey)]            := LongInt(SDLK_e);
    _KeyCode[LongInt(FKey)]            := LongInt(SDLK_f);
    _KeyCode[LongInt(GKey)]            := LongInt(SDLK_g);
    _KeyCode[LongInt(HKey)]            := LongInt(SDLK_h);
    _KeyCode[LongInt(IKey)]            := LongInt(SDLK_i);
    _KeyCode[LongInt(JKey)]            := LongInt(SDLK_j);
    _KeyCode[LongInt(KKey)]            := LongInt(SDLK_k);
    _KeyCode[LongInt(LKey)]            := LongInt(SDLK_l);
    _KeyCode[LongInt(MKey)]            := LongInt(SDLK_m);
    _KeyCode[LongInt(NKey)]            := LongInt(SDLK_n);
    _KeyCode[LongInt(OKey)]            := LongInt(SDLK_o);
    _KeyCode[LongInt(PKey)]            := LongInt(SDLK_p);
    _KeyCode[LongInt(QKey)]            := LongInt(SDLK_q);
    _KeyCode[LongInt(RKey)]            := LongInt(SDLK_r);
    _KeyCode[LongInt(SKey)]            := LongInt(SDLK_s);
    _KeyCode[LongInt(TKey)]            := LongInt(SDLK_t);
    _KeyCode[LongInt(UKey)]            := LongInt(SDLK_u);
    _KeyCode[LongInt(VKey)]            := LongInt(SDLK_v);
    _KeyCode[LongInt(WKey)]            := LongInt(SDLK_w);
    _KeyCode[LongInt(XKey)]            := LongInt(SDLK_x);
    _KeyCode[LongInt(YKey)]            := LongInt(SDLK_y);
    _KeyCode[LongInt(ZKey)]            := LongInt(SDLK_z);

    _KeyCode[LongInt(DeleteKey)]       := LongInt(SDLK_DELETE);

    // End of ASCII mapped keysyms

    // Numeric keypad
    _KeyCode[LongInt(KeyPad0)]         := LongInt(SDLK_KP_0);
    _KeyCode[LongInt(KeyPad1)]         := LongInt(SDLK_KP_1);
    _KeyCode[LongInt(KeyPad2)]         := LongInt(SDLK_KP_2);
    _KeyCode[LongInt(KeyPad3)]         := LongInt(SDLK_KP_3);
    _KeyCode[LongInt(KeyPAd4)]         := LongInt(SDLK_KP_4);
    _KeyCode[LongInt(KeyPAd5)]         := LongInt(SDLK_KP_5);
    _KeyCode[LongInt(KeyPad6)]         := LongInt(SDLK_KP_6);
    _KeyCode[LongInt(KeyPad7)]         := LongInt(SDLK_KP_7);
    _KeyCode[LongInt(KeyPAd8)]         := LongInt(SDLK_KP_8);
    _KeyCode[LongInt(KeyPad9)]         := LongInt(SDLK_KP_9);
    _KeyCode[LongInt(KeyPadPeriod)]   := LongInt(SDLK_KP_PERIOD);
    _KeyCode[LongInt(KeyPadDivide)]   := LongInt(SDLK_KP_DIVIDE);
    _KeyCode[LongInt(KeyPadMultiply)] := LongInt(SDLK_KP_MULTIPLY);
    _KeyCode[LongInt(KeyPadMinus)]    := LongInt(SDLK_KP_MINUS);
    _KeyCode[LongInt(KeyPadPlus)]     := LongInt(SDLK_KP_PLUS);
    _KeyCode[LongInt(KeyPadEnter)]    := LongInt(SDLK_KP_ENTER);
    _KeyCode[LongInt(KeyPadEquals)]   := LongInt(SDLK_KP_EQUALS);

    // Arrows + Home/End pad
    _KeyCode[LongInt(UpKey)]       := LongInt(SDLK_UP);
    _KeyCode[LongInt(DownKey)]     := LongInt(SDLK_DOWN);
    _KeyCode[LongInt(RightKey)]    := LongInt(SDLK_RIGHT);
    _KeyCode[LongInt(LeftKey)]     := LongInt(SDLK_LEFT);
    _KeyCode[LongInt(InsertKey)]   := LongInt(SDLK_INSERT);
    _KeyCode[LongInt(HomeKey)]     := LongInt(SDLK_HOME);
    _KeyCode[LongInt(EndKey)]      := LongInt(SDLK_END);
    _KeyCode[LongInt(PageUpKey)]   := LongInt(SDLK_PAGEUP);
    _KeyCode[LongInt(PageDownKey)] := LongInt(SDLK_PAGEDOWN);

    // Function keys
    _KeyCode[LongInt(F1Key)]  := LongInt(SDLK_F1);
    _KeyCode[LongInt(F2Key)]  := LongInt(SDLK_F2);
    _KeyCode[LongInt(F3Key)]  := LongInt(SDLK_F3);
    _KeyCode[LongInt(F4Key)]  := LongInt(SDLK_F4);
    _KeyCode[LongInt(F5Key)]  := LongInt(SDLK_F5);
    _KeyCode[LongInt(F6Key)]  := LongInt(SDLK_F6);
    _KeyCode[LongInt(F7Key)]  := LongInt(SDLK_F7);
    _KeyCode[LongInt(F8Key)]  := LongInt(SDLK_F8);
    _KeyCode[LongInt(F9Key)]  := LongInt(SDLK_F9);
    _KeyCode[LongInt(F10Key)] := LongInt(SDLK_F10);
    _KeyCode[LongInt(F11Key)] := LongInt(SDLK_F11);
    _KeyCode[LongInt(F12Key)] := LongInt(SDLK_F12);
    _KeyCode[LongInt(F13Key)] := LongInt(SDLK_F13);
    _KeyCode[LongInt(F14Key)] := LongInt(SDLK_F14);
    _KeyCode[LongInt(F15Key)] := LongInt(SDLK_F15);

    // Key state modifier keys
    _KeyCode[LongInt(NumLockKey)]   := LongInt(SDLK_NUMLOCKCLEAR);
    _KeyCode[LongInt(CapsLockKey)]  := LongInt(SDLK_CAPSLOCK);
    _KeyCode[LongInt(ScrollLockKey)] := LongInt(SDLK_SCROLLLOCK);
    _KeyCode[LongInt(RightShiftKey)]    := LongInt(SDLK_RSHIFT);
    _KeyCode[LongInt(LeftShiftKey)]    := LongInt(SDLK_LSHIFT);
    _KeyCode[LongInt(RightCtrlKey)]     := LongInt(SDLK_RCTRL);
    _KeyCode[LongInt(LeftCtrlKey)]     := LongInt(SDLK_LCTRL);
    _KeyCode[LongInt(RightAltKey)]      := LongInt(SDLK_RALT);
    _KeyCode[LongInt(LeftAltKey)]      := LongInt(SDLK_LALT);
    _KeyCode[LongInt(RightMetaKey)]     := LongInt(SDLK_RMETA);
    _KeyCode[LongInt(LeftMetaKey)]     := LongInt(SDLK_LMETA);
    _KeyCode[LongInt(LeftSuperKey)]    := LongInt(SDLK_LSUPER); // Left "Windows" key
    _KeyCode[LongInt(RightSuperKey)]    := LongInt(SDLK_RSUPER); // Right "Windows" key
    _KeyCode[LongInt(ModeKey)]      := LongInt(SDLK_MODE); // "Alt Gr" key

    // Miscellaneous function keys
    _KeyCode[LongInt(HelpKey)]   := LongInt(SDLK_HELP);
    _KeyCode[LongInt(SysReqKey)] := LongInt(SDLK_SYSREQ);
    _KeyCode[LongInt(MenuKey)]   := LongInt(SDLK_MENU);
    _KeyCode[LongInt(PowerKey)]  := LongInt(SDLK_POWER); // Power Macintosh power key

    _KeyCode[LongInt(ShiftKey)]     := LongInt(SDLK_LSHIFT);
    _KeyCode[LongInt(CtrlKey)]      := LongInt(SDLK_LCTRL);
    _KeyCode[LongInt(AltKey)]       := LongInt(SDLK_LALT);
    _KeyCode[LongInt(CommandKey)]   := LongInt(SDLK_LSUPER);
    _KeyCode[LongInt(SuperKey)]     := LongInt(SDLK_LSUPER);
    _KeyCode[LongInt(WindowsKey)]   := LongInt(SDLK_LSUPER);
    _KeyCode[LongInt(OptionKey)]    := LongInt(SDLK_LALT);
  end;

  //TODO: move this to SwinGame
  function GetKeyCodeProcedure(val : LongInt) : LongInt;
  begin
    result := _KeyCode[val];
  end;

	function GetErrorProcedure() : PChar;
	begin
		result := 'ERROR TEXT'; //TODO: add this
	end;

  procedure QuitProcedure();
  begin
    //TODO: add this
  end;

  procedure InitProcedure();
  begin
    if _Initialised then exit;
    _Initialised := true;

    InitKeyCodes();
    // WriteLn('pre init');
    _sg_functions^.init();
    // WriteLn('post init');
  end;


	procedure LoadSDL2Driver();
    var
        callbacks: sg_input_callbacks;
	begin
		Driver.GetError           := @GetErrorProcedure;
		Driver.GetKeyCode         := @GetKeyCodeProcedure;
		Driver.Quit               := @QuitProcedure;
		Driver.Init               := @InitProcedure;

        callbacks       := GetInputCallbackFunction();
        _sg_functions   := sg_load(callbacks);
        // WriteLn('Size of functions Pascal: ', sizeof(sg_interface));
        // WriteLn('Network port Pascal: ', HexStr(_sg_functions^.network.network_port));
	end;
end.
