unit sgDriverInput;

interface
  uses sgDriverSDL2Types, sgBackendTypes;

  function GetInputCallbackFunction() : sg_input_callbacks;

  function WindowForPointer(p: Pointer): WindowPtr;

  function IsKeyPressed(virtKeyCode : LongInt) : Boolean;
  procedure ProcessEvents();
  function RelativeMouseState(var x : LongInt; var y : LongInt) : Byte;
  function MouseState(var x : LongInt; var y : LongInt): Byte; 
  function ShowCursor(toggle : LongInt): LongInt;
  procedure WarpMouse(x,y : Word); 

implementation
  uses sgInputBackend, sgTypes, sgShared, sgWindowManager;

  function WindowForPointer(p: Pointer): WindowPtr;
  var
    i, count: Integer;
  begin
    count := WindowCount();
    for i := 0 to count - 1 do
    begin
      result := WindowPtr(WindowAtIndex(i));
      if result = p then
      begin
        exit;
      end;
    end;
    result := nil;
  end;

  function IsKeyPressed(virtKeyCode : LongInt) : Boolean;
  begin
    result := _sg_functions^.input.key_pressed(virtKeyCode) <> 0;
  end;
  
  procedure ProcessEvents();
  begin
    _sg_functions^.input.process_events();
  end;
  
  function RelativeMouseState(var x : LongInt; var y : LongInt) : Byte;
  begin
    result := _sg_functions^.input.mouse_relative_state(@x, @y);
  end;

  function MouseState(var x : LongInt; var y : LongInt): Byte; 
  begin
    result := _sg_functions^.input.mouse_state(@x, @y);
  end;
  
  function ShowCursor(toggle : LongInt):LongInt;
  begin
    result := _sg_functions^.input.mouse_cursor_state(toggle);
  end;
  
  procedure WarpMouse(x,y : Word); 
  begin
    if Assigned(_CurrentWindow) then
      _sg_functions^.input.warp_mouse(@_CurrentWindow^.image.surface, x, y);
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

  procedure HandleInputTextCallback(ttext: PChar); cdecl;
  begin
    ProcessTextEntry(ttext);
  end;

  procedure HandleWindowResize(p: Pointer; w, h: Longint); cdecl;
  var
    wind: WindowPtr;
  begin
    wind := WindowForPointer(p);
    if Assigned(wind) then
    begin
      wind^.image.surface.width := w;
      wind^.image.surface.height := h;
    end;
  end;

  function GetInputCallbackFunction() : sg_input_callbacks;
  begin
    result.do_quit           := @DoQuitCallback;
    result.handle_key_down   := @HandleKeydownEventCallback;
    result.handle_key_up     := @HandleKeyupEventCallback;
    result.handle_mouse_up   := @ProcessMouseEventCallback; // click occurs on up
    result.handle_mouse_down := nil;
    result.handle_input_text := @HandleInputTextCallback;
    result.handle_window_resize := @HandleWindowResize;
  end;
  
end.