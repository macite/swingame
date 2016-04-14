program HelloWorld;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, SwinGame;

procedure TestRectLineCollision();
var
  lns: array [0..3] of LineSegment;
  r: Rectangle;
  i: Integer;
begin
  r := RectangleFrom(50, 50, 50, 50);
  DrawRectangle(ColorWhite, r);

  lns[0] := LineFrom(PointAt(55, 55), MousePosition());

  for i := 0 to 0 do
  begin
    if RectLineCollision(r, lns[i]) then
      DrawLine(ColorRed, lns[i])
    else
      DrawLine(ColorBlue, lns[i]);
  end;
end;

procedure Main();
var
  rect, rect2: Rectangle;
  mvmtLn: LineSegment;
  mvmt, outMvmt: Vector;
  tmr: Timer;
  mat: Matrix2D;
begin
  OpenAudio();

  OpenGraphicsWindow('Collision Test', 800, 600);

  rect := RectangleFrom(303, 453, 5, 5);
  rect2 := RectangleFrom(300, 450, 10, 10);
  mvmt := VectorTo(100, 0);

  tmr := CreateTimer();
  StartTimer(tmr);

  mat := RotationMatrix(10);

  repeat // The game loop...
    ProcessEvents();

    //DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
    ClearScreen(ColorBlack);

    TestRectLineCollision();

    FillRectangle(ColorWhite, rect);
    DrawRectangle(ColorGreen, rect2);

    if TimerTicks(tmr) div 500 > 0 then
    begin
      mvmt := MatrixMultiply(mat, mvmt);
      ResetTimer(tmr);
    end;

    mvmtLn := LineFromVector(rect.x + rect.width / 2, rect.y + rect.height / 2, mvmt);
    DrawLine(ColorRed, mvmtLn);

    outMvmt := VectorOutOfRectFromRect(rect, rect2, mvmt);
    mvmtLn := LineFromVector(rect.x, rect.y, outMvmt);
    DrawLine(ColorBlue, mvmtLn);
    DrawRectangle(ColorBlue, rect.x + outMvmt.x, rect.y + outMvmt.y, rect.width, rect.height);

    DrawFramerate(0,0);

    RefreshScreen();
  until WindowCloseRequested();

  ReleaseAllResources();

  CloseAudio();
end;

begin
  Main();
end.
