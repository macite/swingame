unit sgDriverTimerSDL;
//=============================================================================
// sgDriverTimerSDL.pas
//=============================================================================
//
// The timer driver is responsible for controling delay and timer functions 
// between SDL 1.2 and SwinGame.
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//
//=============================================================================

interface 
	uses SDL;
	procedure LoadSDLTimerDriver();
	
implementation
	uses sgDriverTimer, sgShared, sgDriver;
	procedure DelayProcedure(time : LongWord);
	begin
		SDL_Delay(time);
	end;
	
	function GetTicksProcedure() : LongWord;
	begin
		result := SDL_GetTicks();
	end;
	

	
	procedure LoadSDLTimerDriver();
	begin
		TimerDriver.Delay := @DelayProcedure;
		TimerDriver.GetTicks := @GetTicksProcedure;
	end;
end.