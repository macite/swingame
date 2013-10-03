unit sgDriver;
//=============================================================================
// sgDriver.pas
//=============================================================================
//
// The Driver is responsible for acting as the interface between driver
// code and swingame code. Swingame code uses the Driver to access the 
// current active driver. 
//
// Changing this driver will probably cause graphics drivers to break.
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
  sgTypes;

  type
    GetErrorProcedure = function () : PChar;
    QuitProcedure = procedure();
    InitProcedure = procedure();
    GetKeyCodeProcedure = function(val : LongInt) : LongInt;
    
  	DriverRecord = record
  	  GetError                : GetErrorProcedure;
  	  Quit                    : QuitProcedure;
  	  Init                    : InitProcedure;
      GetKeyCode              : GetKeyCodeProcedure;
	  end;  

	var
		Driver : DriverRecord = ( GetError: nil; Quit: nil; Init: nil ; GetKeyCode: nil);
	
	procedure LoadDefaultDriver();
		
implementation
  	uses sgDriverTimer, {$IFDEF SWINGAME_SDL13}sgDriverSDL13{$ELSE}sgDriverSDL{$ENDIF};

	procedure LoadDefaultDriver();
	begin
	  {$IFDEF SWINGAME_SDL13}
      // WriteLn('Loading 1.3');
		  LoadSDL13Driver();
		{$ELSE}
      // WriteLn('Loading 1.2');
		  LoadSDLDriver();
		{$ENDIF}
	end;

	procedure DefaultInitProcedure();
	begin
    // WriteLn('Default Init');
	 LoadDefaultDriver();
	 Driver.Init();
	end;

	function DefaultGetErrorProcedure () : PChar;
	begin
		LoadDefaultDriver();
		result := Driver.GetError();
	end;
	
	function DefaultGetKeyCodeProcedure (val : LongInt) : LongInt;
	begin
		LoadDefaultDriver();
		result := Driver.GetKeyCode(val);
	end;

	procedure DefaultQuitProcedure();
	begin
	 LoadDefaultDriver();
	 Driver.Quit();
	end;  

initialization
  if not Assigned(Driver.Init) then
  begin
    // WriteLn('Loading driver');
		Driver.GetError               := @DefaultGetErrorProcedure;
		Driver.Quit                   := @DefaultQuitProcedure;
		Driver.GetKeyCode             := @DefaultGetKeyCodeProcedure;
		Driver.Init                   := @DefaultInitProcedure;
	end;
end.
	
