program DrawPrimitives;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, SwinGame;

procedure Main();
var
  t: Timer;
  time: Integer;
  i: Integer;
  clipped: Boolean;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Draw Primitives', 640, 480);
  
  t := CreateTimer();
  StartTimer(t);

  ClearScreen(ColorWhite);

  clipped := false;
  
  repeat // The game loop...
    ProcessEvents();

    if KeyTyped(vk_F) then
    begin
      ToggleFullscreen();
      WriteLn('Toggle Fullscreen');
    end;

    if KeyTyped(vk_S) then 
      TakeScreenshot('test');
    if KeyTyped(vk_B) then ToggleWindowBorder();
    if KeyTyped(vk_C) then
    begin
      WriteLn('Centre pixel is ', GetPixelFromScreen(320, 240));
      //MoveMouse(320, 240);
    end;
    
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
    else if time < 4000 then
    begin
      DrawCircle(RandomColor(), Rnd(640), Rnd(480), Rnd(200));
      FillCircle(RandomColor(), Rnd(640), Rnd(480), Rnd(200));
    end
    else if time < 5000 then
    begin
      DrawRectangle(RandomColor(), Rnd(640), Rnd(480), Rnd(200), Rnd(200));
      FillRectangle(RandomColor(), Rnd(640), Rnd(480), Rnd(200), Rnd(200));
    end
    else if time < 6000 then
    begin
      DrawRectangle(RandomColor(), Rnd(640), Rnd(480), -Rnd(200), -Rnd(200));
      FillRectangle(RandomColor(), Rnd(640), Rnd(480), -Rnd(200), -Rnd(200));
    end
    else if time < 7500 then
    begin
      DrawThickLine(RandomColor(), Rnd(640), Rnd(480), Rnd(640), Rnd(480), 1 + Rnd(5));
    end
    else if time < 9000 then
    begin
      for i := 0 to 999 do
        DrawPixel(RandomColor(), Rnd(640), Rnd(480));
    end
    else if time < 11000 then
    begin
      DrawTriangle(RandomColor(), Rnd(640), Rnd(480), Rnd(640), Rnd(480), Rnd(640), Rnd(480));
      FillTriangle(RandomColor(), Rnd(640), Rnd(480), Rnd(640), Rnd(480), Rnd(640), Rnd(480));
    end
    else
    begin
      ResetTimer(t);
      if clipped then
      begin
        ResetClip();
        clipped := False;
      end
      else
      begin
        ClearScreen(ColorWhite);
        FillRectangle(ColorGreen, 160, 50, 320, 240);
        SetClip(RectangleFrom(160, 50, 320, 240));
        clipped := True;
      end;
    end;

    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.