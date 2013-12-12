unit sgDriverTimerSDL2;

interface
	procedure LoadSDLTimerDriver();

implementation
	procedure DelayProcedure(time : LongWord);
	begin
	end;
	
	function DefaultGetTicksProcedure() : LongWord;
	begin
		result := 0;
	end;

	initialization
	begin
		TimerDriver.Delay := @DelayProcedure;
		TimerDriver.GetTicks := @GetTicksProcedure;
	end;
end.
	
	