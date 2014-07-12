program TimerTest;
uses sgTypes, sgTimers, sgUtils;

procedure Main();
var
	time1, time2 : LongWord;
	tmr : Timer;
begin
	WriteLn('Testing Timer...');
	time1 := GetTicks();
	Delay(1000);
	time2 := GetTicks();
	WriteLn('complete... Should have taken ~1000, took: ', time2 - time1 );
	WriteLn();
	WriteLn('Creating Timer...');
	tmr := CreateTimer('MyTimer');
	if not Assigned(tmr) then WriteLn('Timer was not assigned!');
	
	
	StartTimer(tmr);
	Delay(1000);
	WriteLn('part way at: ', TimerTicks(tmr));
	Delay(1000);
	PauseTimer(tmr);
	WriteLn('complete... Should have taken ~2000, took: ', TimerTicks(tmr));

	Delay(1000);
	WriteLn('test resume');
	ResumeTimer(tmr);
	Delay(1000);
	PauseTimer(tmr);
	WriteLn('complete... Should have taken ~3000, took: ', TimerTicks(tmr));	
	
	WriteLn('reset...');
	ResetTimer(tmr);
	WriteLn('time is now: ', TimerTicks('MyTimer'));

	ResumeTimer(tmr);

	Delay(1000);
	WriteLn('part way at: ', TimerTicks('MyTimer'));
	Delay(1000);
	PauseTimer('MyTimer');
	WriteLn('complete... Should have taken ~2000, took: ', TimerTicks(tmr));

	WriteLn('restarting...');
	ResetTimer('MyTimer');
	WriteLn('done');

end;

begin
	Main();
end.