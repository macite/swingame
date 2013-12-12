unit sgDriverTimerSDL2;

interface
	procedure LoadSDLTimerDriver();

implementation
	uses sgDriverTimer;

	procedure DelayProcedure(time : LongWord);
	begin
	end;
	
	function GetTicksProcedure() : LongWord;
	begin
		result := 0;
	end;

	procedure LoadSDLTimerDriver();
	begin
		TimerDriver.Delay := @DelayProcedure;
		TimerDriver.GetTicks := @GetTicksProcedure;
	end;
end.
	
	