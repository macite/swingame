program HowToMoveAShape;
uses
  SwinGame, sgTypes;
  

procedure Main();
var 
  r: Rectangle;	
begin
  r := RectangleFrom(140, 110, 40, 20);

  OpenGraphicsWindow('How To Move A Shape', 320, 240);
  LoadDefaultColors();

  repeat // The game loop...
    // Update the game
    ProcessEvents();
    if KeyDown(vk_UP) then
    begin
      r.y -= 1;
      if r.y < -20 then r.y := 240;
    end;
    if KeyDown(vk_DOWN) then
    begin
      r.y += 1;
      if r.y > 240 then r.y := -20;
    end;
    if KeyDown(vk_LEFT) then
    begin
      r.x -= 1;
      if r.x < -40 then r.x := 320;
    end;
    if KeyDown(vk_RIGHT) then
    begin
      r.x += 1;
      if r.x > 320 then r.x := -40;
    end;
    
    // Draw the game
    ClearScreen(ColorWhite);
    FillRectangle(ColorGreen, r);
    RefreshScreen(60);
  until WindowCloseRequested();

  ReleaseAllResources();
end;

begin
  Main();
end.