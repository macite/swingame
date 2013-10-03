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
	Delay(2000);
	PauseTimer(tmr);
	WriteLn('complete... Should have taken ~2000, took: ', TimerTicks(tmr));
end;

begin
	Main();
end.