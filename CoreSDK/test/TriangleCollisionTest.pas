program HelloWorld;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, sgAudio, sgText, sgGraphics, sgResources,
  sgCamera, sgGeometry, sgImages, sgInput, sgPhysics, 
  sgSprites, sgTimers;

procedure Main();
var
  tri: array [0..5] of Triangle;
  rect: Rectangle;
  
  current: Integer;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Triangle Collision Test', 800, 600);
  
  rect := RectangleFrom(300, 400, 100, 100);
  
  tri[0] := TriangleFrom(
      290, 410,
      320, 490,
      280, 600
    );
  tri[1] := TriangleFrom(
      310, 410,
      310, 490,
      360, 490
    );
  tri[2] := TriangleFrom(
      10, 10,
      800, 600,
      10, 600
    );
  tri[3] := TriangleFrom(
      10, 10,
      20, 10,
      20, 20
    );
  tri[4] := TriangleFrom(
      290, 410,
      299, 410,
      299, 500
    );
  tri[5] := TriangleFrom(
      450, 470,
      350, 530,
      800, 600
    );
  current := 0;
  
  repeat // The game loop...
    ProcessEvents();
    
    //DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
    
    ClearScreen(ColorBlack);
    
    if KeyTyped(vk_1) then current := 0
    else if KeyTyped(vk_2) then current := 1
    else if KeyTyped(vk_3) then current := 2
    else if KeyTyped(vk_4) then current := 3
    else if KeyTyped(vk_5) then current := 4
    else if KeyTyped(vk_6) then current := 5;
    
    if TriangleRectangleIntersect(tri[current], rect) then
      FillTriangle(ColorGreen, tri[current])
    else
      FillTriangle(ColorRed, tri[current]);
    
    DrawRectangle(ColorGreen, rect);
    
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
