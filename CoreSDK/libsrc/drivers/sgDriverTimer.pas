unit sgDriverTimer;
//=============================================================================
// sgDriverTimer.pas
//=============================================================================
//
// The timer driver is responsible for controling delay and timer functions.
// 
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//
//=============================================================================

interface
	uses sgTypes, {$IFDEF SWINGAME_SDL13}sgDriverTimerSDL13{$ELSE}sgDriverTimerSDL{$ENDIF};
	
	type
		DelayProcedure = procedure (time : LongWord);
		GetTicksProcedure = function () : LongWord;
	
	TimerDriverRecord = record
		Delay                   : DelayProcedure;
		GetTicks                : GetTicksProcedure;
	end;  
	
	
	
	var
		TimerDriver : TimerDriverRecord;
		
implementation
	procedure LoadDefaultTimerDriver();
	begin
	  {$IFDEF SWINGAME_SDL13}
		  LoadSDL13TimerDriver();
		{$ELSE}
		  LoadSDLTimerDriver();
		{$ENDIF}
	end;
	
	procedure DefaultDelayProcedure(time : LongWord);
	begin
		LoadDefaultTimerDriver();
		TimerDriver.Delay(time);
	end;
	
	function DefaultGetTicksProcedure() : LongWord;
	begin
		LoadDefaultTimerDriver();
		result := TimerDriver.GetTicks();
	end;

	
	initialization
	begin
		TimerDriver.Delay := @DefaultDelayProcedure;
		TimerDriver.GetTicks := @DefaultGetTicksProcedure;
	end;
end.
	
	