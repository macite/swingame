unit sgDriverInputSDL;

interface
  uses sdl, sdl_ttf;
  procedure LoadSDLInputDriver();

      
implementation
  uses sgDriverInput, sgInputBackend, sgInput, sgShared, sgTypes ;
  

  
  function IsKeyPressedProcedure(virtKeyCode : LongInt) : Boolean;
  
  var
    keys: PUint8;
    {$IFNDEF FPC}
    indexAddress: Longword;
    intPtr: ^UInt8;
    {$ENDIF}
  begin
    keys := SDL_GetKeyState(nil);
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
    result := SDL_GetKeyState(nil)^;
  end;
  
  function CheckQuitProcedure() : Boolean;
    {$IFDEF FPC}
    var
      keys: PUInt8;
    {$ENDIF} 
  begin
    {$IFDEF FPC}
    keys := SDL_GetKeyState(nil);
    result := (((keys + SDLK_LMETA)^ + (keys + SDLK_Q)^ = 2) or
      ((keys + SDLK_RMETA)^ + (keys + SDLK_Q)^ = 2) or
       ((keys + SDLK_LALT)^ + (keys + SDLK_F4)^ = 2) or
       ((keys + SDLK_RALT)^ + (keys + SDLK_F4)^ = 2));

    {$ELSE}
    result := (IsKeyPressed(SDLK_LALT) and IsKeyPressed(SDLK_F4));
    {$ENDIF}
  end;
  
  procedure ProcessEventsProcedure(); 
  var
    event: TSDL_EVENT;
  begin
     //http://sdl.beuc.net/sdl.wiki/SDL_PumpEvents
     //SDL_PumpEvents();

     while SDL_PollEvent(@event) > 0 do
     begin
       case event.type_ of
         SDL_QUITEV:   DoQuit();
         SDL_KEYDOWN:  HandleKeydownEvent(event.key.keysym.sym,event.key.keysym.unicode);
         SDL_KEYUP:    HandleKeyupEvent(event.key.keysym.sym);
         SDL_MOUSEBUTTONUP: ProcessMouseEvent(event.button.button); 
       end;
     end;
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
    result := SDL_GetMouseState(x,y);
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
    SDL_WarpMouse(x,y);
  end;

  procedure LoadSDLInputDriver(); 
  begin
    InputDriver.IsKeyPressed := @IsKeyPressedProcedure;
    InputDriver.CheckQuit := @CheckQuitProcedure;
    InputDriver.ProcessEvents := @ProcessEventsProcedure;
    InputDriver.GetKeyState := @GetKeyStateProcedure;
    InputDriver.GetMouseState := @GetMouseStateProcedure;
    InputDriver.GetRelativeMouseState := @GetRelativeMouseStateProcedure;
    InputDriver.ShowCursor := @ShowCursorProcedure;
    InputDriver.Button := @ButtonProcedure;
    InputDriver.WarpMouse := @WarpMouseProcedure;
  end;
  
end.