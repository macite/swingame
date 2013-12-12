unit sgDriverInputSDL2;

interface
  procedure LoadSDL2InputDriver();

implementation
  uses sgDriverInput, sgDriverSDL2Types;

  function IsKeyPressedProcedure(virtKeyCode : LongInt) : Boolean;
  begin
    result := true;
  end;
  
  function CheckQuitProcedure() : Boolean;
  begin
    result := false;
  end;
  
  procedure ProcessEventsProcedure();
  begin
  end;
  
  procedure DestroyProcedure();
  begin
  end;
  
  function GetKeyStateProcedure() : Byte; 
  begin
    result := 0
  end;
  
  function GetRelativeMouseStateProcedure(var x : LongInt; var y : LongInt) : Byte;
  begin
    result := 0;
  end;

  function GetMouseStateProcedure(var x : LongInt; var y : LongInt): Byte; 
  begin
    result := 0;
  end;
  
  function ShowCursorProcedure(toggle : LongInt):LongInt;
  begin
    result := 0;
  end;
  
  function ButtonProcedure(button : LongInt) : LongInt;
  begin
    result := 0;
  end;
  
  procedure WarpMouseProcedure(x,y : Word); 
  begin
  end;
  
  procedure LoadSDL2InputDriver();
  begin
    InputDriver.IsKeyPressed := @IsKeyPressedProcedure;
    InputDriver.CheckQuit := @CheckQuitProcedure;
    InputDriver.ProcessEvents := @ProcessEventsProcedure;
    InputDriver.Destroy := @DestroyProcedure;
    InputDriver.GetKeyState := @GetKeyStateProcedure;
    InputDriver.GetRelativeMouseState := @GetRelativeMouseStateProcedure;
    InputDriver.GetMouseState := @GetMouseStateProcedure;
    InputDriver.ShowCursor := @ShowCursorProcedure;
    InputDriver.Button := @ButtonProcedure;
    InputDriver.WarpMouse := @WarpMouseProcedure;
  end;

end.