unit sgDriverInput;

interface
uses 
  sgTypes, sgDriverInputSDL2;

type

  IsKeyPressedProcedure = function(virtKeyCode: Longint) : Boolean;
  CheckQuitProcedure = function() : Boolean;
  ProcessEventsProcedure = procedure();
  GetRelativeMouseStateProcedure = function(var x : LongInt; var y : LongInt): Byte;
  GetMouseStateProcedure = function(var x : LongInt; var y : LongInt): Byte;
  ShowCursorProcedure = function (toggle : LongInt) : LongInt;
  ButtonProcedure = function(button : LongInt) : LongInt;
  WarpMouseProcedure = procedure(x,y : Word);
  
  InputDriverRecord = Record
    IsKeyPressed : IsKeyPressedProcedure;
    CheckQuit : CheckQuitProcedure;
    ProcessEvents : ProcessEventsProcedure;
    GetRelativeMouseState : GetRelativeMouseStateProcedure;
    GetMouseState : GetMouseStateProcedure;
    ShowCursor : ShowCursorProcedure;
    Button : ButtonProcedure;
    WarpMouse : WarpMouseProcedure;
  end;

var
  InputDriver : InputDriverRecord;

implementation
  procedure LoadDefaultInputDriver; 
  begin
      LoadSDL2InputDriver();
  end;

  function DefaultIsKeyPressedProcedure(virtKeyCode : LongInt) : Boolean;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.IsKeyPressed(virtKeyCode);
  end;
  
  function DefaultCheckQuitProcedure() : Boolean;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.CheckQuit();
  end;
  
  procedure DefaultProcessEventsProcedure();
  begin
    LoadDefaultInputDriver();
    InputDriver.ProcessEvents();
  end;
    
  function DefaultGetRelativeMouseStateProcedure(var x : LongInt; var y : LongInt) : Byte;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.GetRelativeMouseState(x,y);
  end;

  function DefaultGetMouseStateProcedure(var x : LongInt; var y : LongInt): Byte; 
  begin
    LoadDefaultInputDriver();
    result := InputDriver.GetMouseState(x,y);
  end;
  
  function DefaultShowCursorProcedure(toggle : LongInt):LongInt;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.ShowCursor(toggle);
  end;
  
  function DefaultButtonProcedure(button : LongInt) : LongInt;
  begin
    LoadDefaultInputDriver();
    result := InputDriver.Button(button);
  end;
  
  procedure DefaultWarpMouseProcedure(x,y : Word); 
  begin
    LoadDefaultInputDriver();
    InputDriver.WarpMouse(x,y);
  end;
  
initialization
  begin
    InputDriver.IsKeyPressed := @DefaultIsKeyPressedProcedure;
    InputDriver.CheckQuit := @DefaultCheckQuitProcedure;
    InputDriver.ProcessEvents := @DefaultProcessEventsProcedure;
    InputDriver.GetRelativeMouseState := @DefaultGetRelativeMouseStateProcedure;
    InputDriver.GetMouseState := @DefaultGetMouseStateProcedure;
    InputDriver.ShowCursor := @DefaultShowCursorProcedure;
    InputDriver.Button := @DefaultButtonProcedure;
    InputDriver.WarpMouse := @DefaultWarpMouseProcedure;
  end;



end.