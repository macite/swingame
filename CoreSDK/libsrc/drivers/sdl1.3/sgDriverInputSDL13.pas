unit sgDriverInputSDL13;

interface
  uses sdl13;
  procedure LoadSDL13InputDriver();

      
implementation
  uses sgDriverInput, sgInputBackend, sgInput, sgShared, sgTypes {$IFDEF IOS}, sgDriveriOS    {$ENDIF} ;

  
  function IsKeyPressedProcedure(virtKeyCode : LongInt) : Boolean;
  
  var
    keys: PUint8;
    {$IFNDEF FPC}
    indexAddress: Longword;
    intPtr: ^UInt8;
    {$ENDIF}
  begin
    keys := SDL_GetKeyboardState(nil);
    if keys <> nil then
    begin
      {$IFDEF FPC}
        result := (keys + virtKeyCode)^ = 1;
      {$ELSE}
        indexAddress := Longword(keys) + Longword(virtKeyCode);
        intPtr := Ptr(indexAddress);
        result := intPtr^ = 1;
      {$ENDIF}

      {$IFDEF FPC}
      if not result then
      begin
        if virtKeyCode = SDLK_LSUPER then result := (keys + SDLK_LMETA)^ = 1
        else if virtKeyCode = SDLK_RSUPER then result := (keys + SDLK_RMETA)^ = 1;
      end;
      {$ENDIF}
    end
    else
    begin
      result := false;
    end;
  end;
  
  function GetKeyStateProcedure(): Byte;
  begin
    result := SDL_GetKeyboardState(nil)^;
  end;
  
    function CheckQuitProcedure() : Boolean;
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
    end;

    procedure ProcessEventsProcedure(); 
    type
      KeyRecord = record
        lKeyCode: LongInt;
        lTmpUniCode : LongInt;
        lSendKeyCode : Boolean;
      end;
      KeyRecordArray = Array of KeyRecord;
    var
      event: SDL_EVENT;
      lKeys : KeyRecordArray;
      // lKeyCode: LongInt = -1;
      // lTmpUniCode : LongInt;
      // lSendKeyCode : Boolean = False;
      i : Integer;
    begin
      SetLength(lKeys, 0);
      while SDL_WaitEventTimeout(@event,0) > 0 do
      begin
        case SDL_EventType(event._type) of
          SDL_TextInput                    :lKeys[High(lKeys)].lTmpUniCode := LongInt(event.text.text[0]);
          SDL_QUIT_EVENT                   :DoQuit();
          SDL_KEYDOWN                      :begin
                                              SetLength(lKeys, Length(lKeys) + 1);
                                              lKeys[High(lKeys)].lKeyCode := LongInt(event.key.keysym.sym);
                                              lKeys[High(lKeys)].lTmpUniCode := LongInt(event.key.keysym.unicode);
                                              lKeys[High(lKeys)].lSendKeyCode := True;
                                            end;
          SDL_KEYUP                        :HandleKeyupEvent(event.key.keysym.sym);
          SDL_MOUSEBUTTONUP                :ProcessMouseEvent(event.button.button); 
          {$IFDEF IOS}
          SDL_FINGERDOWN                   :HandleTouchEvent(iOSDriver.ProcessTouchEvent(@event.tfinger), true, false);
          SDL_FINGERUP                     :HandleTouchEvent(iOSDriver.ProcessTouchEvent(@event.tfinger), false, true);
          SDL_FINGERMOTION                 :HandleTouchEvent(iOSDriver.ProcessTouchEvent(@event.tfinger), false, false);
          SDL_JOYAXISMOTION                :HandleAxisMotionEvent(iOSDriver.ProcessAxisMotionEvent());
          {$ENDIF}
        end;
      end;
      for i := Low(lKeys) to High(lKeys) do
        HandleKeydownEvent(lKeys[i].lKeyCode, lKeys[i].lTmpUniCode);
    end;

  
  function ShiftDownProcedure(kycode : LongInt) : Boolean;
  begin
    result := ((kyCode = SDLK_LSHIFT) or (kyCode = SDLK_RSHIFT));
  end;

  function GetRelativeMouseStateProcedure(var x : LongInt; var y : LongInt) : Byte; 
  begin
    result := SDL_GetRelativeMouseState(x,y);
  end;
  
  function GetMouseStateProcedure(var x : LongInt; var y : LongInt): Byte; 
  begin
    result := SDL_GetMouseState(@x,@y);
  end;
  
  function ShowCursorProcedure(toggle:LongInt): LongInt; 
  begin
    result := SDL_ShowCursor(toggle);
  end;
  
  function ButtonProcedure(toggle : LongInt): LongInt; 
  begin
    result := SDL_Button(toggle);
  end;  
  
  procedure WarpMouseProcedure(x,y : Word);
  begin
    // SDL_WarpMouse(x,y);
  end;

  procedure LoadSDL13InputDriver(); 
  begin

    InputDriver.IsKeyPressed          := @IsKeyPressedProcedure;
    InputDriver.CheckQuit             := @CheckQuitProcedure;
    InputDriver.ProcessEvents         := @ProcessEventsProcedure;
    InputDriver.GetKeyState           := @GetKeyStateProcedure;
    InputDriver.GetMouseState         := @GetMouseStateProcedure;
    InputDriver.GetRelativeMouseState := @GetRelativeMouseStateProcedure;
    InputDriver.ShowCursor            := @ShowCursorProcedure;
    InputDriver.Button                := @ButtonProcedure;
    InputDriver.WarpMouse             := @WarpMouseProcedure;
  end;
  
end.