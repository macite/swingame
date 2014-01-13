unit sgDriverInputSDL2;

interface
  uses sgDriverSDL2Types;

  procedure LoadSDL2InputDriver();
  function GetInputCallbackFunction() : sg_input_callbacks;

implementation
  uses sgDriverInput, sgInputBackend, SDL2;

  function IsKeyPressedProcedure(virtKeyCode : LongInt) : Boolean;
  begin
    result := _sg_functions^.input.key_pressed(virtKeyCode) <> 0;
  end;
  
  function CheckQuitProcedure() : Boolean; //TODO: check why this doesn't work correctly from SDL - Cmd + Q should end it
  {$IFDEF FPC}
  var
    keys: PUInt8;
    modS:  SDL_Keymod;
  {$ENDIF} 
  begin
    {$IFDEF FPC}
    keys := SDL_GetKeyboardState(nil);
    modS := SDL_GetModState();
    
    result := ((keys + LongInt(SDL_SCANCODE_Q))^ = 1) and (LongInt(modS) and LongInt(KMOD_LGUI) = LongInt(KMOD_LGUI)) or 
              ((keys + LongInt(SDL_SCANCODE_Q))^ = 1) and (LongInt(modS) and LongInt(KMOD_RGUI) = LongInt(KMOD_RGUI)) or 
              ((keys + LongInt(SDL_SCANCODE_F4))^ = 1) and (LongInt(modS) and LongInt(KMOD_LALT) = LongInt(KMOD_LALT)) or 
              ((keys + LongInt(SDL_SCANCODE_F4))^ = 1) and (LongInt(modS) and LongInt(KMOD_RALT) = LongInt(KMOD_RALT));
                
    {$ELSE}
    result := (IsKeyPressed(SDLK_LALT) and IsKeyPressed(SDLK_F4));
    {$ENDIF}

    result := result or (wind_open and (_sg_functions^.input.window_close_requested(@wind) <> 0));
  end;
  
  procedure ProcessEventsProcedure();
  begin
    _sg_functions^.input.process_events();
  end;
  
  function GetRelativeMouseStateProcedure(var x : LongInt; var y : LongInt) : Byte;
  begin
    result := _sg_functions^.input.mouse_relative_state(@x, @y);
  end;

  function GetMouseStateProcedure(var x : LongInt; var y : LongInt): Byte; 
  begin
    result := _sg_functions^.input.mouse_state(@x, @y);
  end;
  
  function ShowCursorProcedure(toggle : LongInt):LongInt;
  begin
    result := _sg_functions^.input.mouse_cursor_state(toggle);
  end;
  
  //TODO: move this to SwinGame
  function ButtonProcedure(button : LongInt) : LongInt;
  begin
    result := button; //TODO: check
  end;
  
  procedure WarpMouseProcedure(x,y : Word); 
  begin
    _sg_functions^.input.warp_mouse(@wind, x, y);
  end;

  procedure HandleKeydownEventCallback(code: Longint); cdecl;
  begin
    HandleKeydownEvent(code, code);
  end;

  procedure HandleKeyupEventCallback(code: Longint); cdecl;
  begin
    HandleKeyupEvent(code);
  end;

  procedure ProcessMouseEventCallback(code: Longint); cdecl;
  begin
    ProcessMouseEvent(code);
  end;

  procedure DoQuitCallback(); cdecl;
  begin
    DoQuit();
  end;

  procedure HandleInputText(ttext: PChar);
  begin
    //TODO: add SDL supported text entry
  end;

  function GetInputCallbackFunction() : sg_input_callbacks;
  begin
    result.do_quit           := @DoQuitCallback;
    result.handle_key_down   := @HandleKeydownEventCallback;
    result.handle_key_up     := @HandleKeyupEventCallback;
    result.handle_mouse_up   := @ProcessMouseEventCallback;
    result.handle_mouse_down := nil;
    result.handle_input_text := nil;
  end;
  
  procedure LoadSDL2InputDriver();
  begin
    InputDriver.IsKeyPressed  := @IsKeyPressedProcedure;
    InputDriver.CheckQuit     := @CheckQuitProcedure;
    InputDriver.ProcessEvents := @ProcessEventsProcedure;
    InputDriver.GetRelativeMouseState := @GetRelativeMouseStateProcedure;
    InputDriver.GetMouseState := @GetMouseStateProcedure;
    InputDriver.ShowCursor    := @ShowCursorProcedure;
    InputDriver.Button        := @ButtonProcedure;
    InputDriver.WarpMouse     := @WarpMouseProcedure;
  end;

end.