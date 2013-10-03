program OpenGLTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgDriverGraphics,sgGraphics, sgUtils,sgInput,sgGeometry,sgTypes, sgTimers, sgShared, sgImages;
  
const LONG_INT_LIMIT = 2147483647;

procedure Main();
var
  x,y : LongInt;
  stopwatch : Timer;
  bmp : Bitmap;
begin

  x :=0;
  Writeln('opening window');
  OpenGraphicsWindow('Hello World', 960, 640);

  stopwatch := CreateTimer();
  bmp := LoadBitmap('Swinburne.jpg');
  Writeln(bmp^.Width);
  DrawBitmap(bmp,0,0);
  RefreshScreen();
  Delay(1000);
  StartTimer(stopwatch);
  ClearScreen(LONG_INT_LIMIT);
  repeat
    begin
      
      ProcessEvents();
      
      GraphicsDriver.DrawRectangle(Screen, RectangleFrom(rnd(1024),rnd(768),rnd(120),rnd(120)), rnd(LONG_INT_LIMIT));
     
      RefreshScreen();
      x += 1;
    end;
  until x= 100;
  x := 0;
  ClearScreen(LONG_INT_LIMIT);
  repeat
    begin
      
      ProcessEvents();

      GraphicsDriver.FillRectangle(Screen, RectangleFrom(rnd(1024),rnd(768),rnd(120),rnd(120)), rnd(LONG_INT_LIMIT));
      RefreshScreen();
      x += 1;
    end;
  until x= 100;
  x := 0;
  ClearScreen(LONG_INT_LIMIT);
  repeat
    begin
      
      ProcessEvents();
      GraphicsDriver.DrawLine(Screen,rnd(1024),rnd(768),rnd(1024),rnd(768), rnd(LONG_INT_LIMIT));
      RefreshScreen();
      x += 1;
    end;
  until x= 100;
  x := 0;
  ClearScreen(LONG_INT_LIMIT);
  repeat
    begin
      
      ProcessEvents();
      GraphicsDriver.FillTriangle(Screen,rnd(LONG_INT_LIMIT),rnd(1024),rnd(768),rnd(1024),rnd(768),rnd(1024),rnd(768));
      RefreshScreen();
      x += 1;
    end;
  until x= 100;
  x := 0;
  ClearScreen(LONG_INT_LIMIT);
  repeat
    begin
      
      ProcessEvents();
      GraphicsDriver.FillCircle(Screen,rnd(LONG_INT_LIMIT),rnd(1024),rnd(768),rnd(120));
      RefreshScreen();
      x += 1;
    end;
  until x= 100;
  x := 0;
  ClearScreen(LONG_INT_LIMIT);
  repeat
    begin
      
      ProcessEvents();
      GraphicsDriver.DrawCircle(Screen,rnd(LONG_INT_LIMIT),rnd(1024),rnd(768),rnd(120));
      RefreshScreen();
      x += 1;
    end;
  until x= 100;
  x := 0;
  ClearScreen(LONG_INT_LIMIT);
  repeat
    begin
      
      ProcessEvents();
      GraphicsDriver.DrawEllipse(Screen,rnd(LONG_INT_LIMIT),rnd(1024),rnd(768),rnd(120),rnd(120));
      RefreshScreen();
      x += 1;
    end;
  until x= 100;
  x := 0;
  ClearScreen(LONG_INT_LIMIT);
  repeat
    begin
      ProcessEvents();
      GraphicsDriver.FillEllipse(Screen,rnd(LONG_INT_LIMIT),rnd(1024),rnd(768),rnd(120),rnd(120));
      RefreshScreen();
      x += 1;
    end;
  until x= 100;
WriteLn('Time: ', TimerTicks(stopwatch));
FreeTimer(stopwatch);

end;

begin
  Main();
end.
