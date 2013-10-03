program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgAudio, sgText, sgGraphics, sgResources, sgTimers, sgUtils, sgInput;

procedure Main();
var
  t: Timer;
  time: Integer;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 640, 480);
  
  t := CreateTimer();
  StartTimer(t);
  
  ClearScreen();
  
  repeat // The game loop...
    ProcessEvents();
    
    time := TimerTicks(t);
    
    if time < 1000 then
    begin
      FillEllipse(RandomColor(), Rnd(640), Rnd(480), -Rnd(200), -Rnd(200));
      DrawEllipse(RandomColor(), Rnd(640), Rnd(480), -Rnd(200), -Rnd(200));
    end
    else if time < 2000 then
    begin
      FillEllipse(RandomColor(), Rnd(640), Rnd(480), Rnd(200), Rnd(200));
      DrawEllipse(RandomColor(), Rnd(640), Rnd(480), Rnd(200), Rnd(200));
    end
    else if time < 3000 then
    begin
      DrawCircle(RandomColor(), Rnd(640), Rnd(480), -Rnd(200));
      FillCircle(RandomColor(), Rnd(640), Rnd(480), -Rnd(200));
    end
    else if time < 4000 then
    begin
      DrawCircle(RandomColor(), Rnd(640), Rnd(480), Rnd(200));
      FillCircle(RandomColor(), Rnd(640), Rnd(480), Rnd(200));
    end
    else
      ResetTimer(t);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
