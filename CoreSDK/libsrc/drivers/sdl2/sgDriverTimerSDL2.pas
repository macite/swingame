unit sgDriverTimerSDL2;

interface
	procedure LoadSDLTimerDriver();

implementation
	uses sgDriverTimer, sgDriverSDL2Types;

	procedure DelayProcedure(time : LongWord);
	begin
		_sg_functions^.utils.delay(time);
	end;
	
	function GetTicksProcedure() : LongWord;
	begin
		result := _sg_functions^.utils.get_ticks();
	end;

	procedure LoadSDLTimerDriver();
	begin
		TimerDriver.Delay := @DelayProcedure;
		TimerDriver.GetTicks := @GetTicksProcedure;
	end;
end.
	
	