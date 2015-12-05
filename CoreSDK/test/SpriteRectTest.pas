program SpriteRecTest;
uses
  sgTypes, sgAudio, sgText, sgGraphics, sgResources, sgSprites, sgInput, sgPhysics, sgCamera, sgImages;

procedure Main();
var
  ball: Bitmap;
  y, x: Single;
  s: Sprite;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Sprite Rect Test', 640, 480);
  
  ball := LoadBitmapNamed('ball', 'ball_small.png');
  s := CreateSprite(ball);
  
  SpriteSetX(s, 10);
  SpriteSetY(s, 20);
  
  x := 50;
  y := 50;
  
  repeat // The game loop...
    ClearScreen();
    ProcessEvents();
    
    //DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
    if SpriteRectCollision(s, x, y, 640 - Round(x) - 10, 10) then
      FillRectangle(ColorRed, x, y, 640 - Round(x) - 10, 10)
    else
      FillRectangle(ColorWhite, x, y, 640 - Round(x) - 10, 10);
    
    
    if KeyDown(UPKey) then y -= 1;
    if KeyDown(DOWNKey) then y += 1;
    if KeyDown(LEFTKey) then x -= 1;
    if KeyDown(RIGHTKey) then x += 1;
    
    if KeyDown(aKey) then SpriteSetX(s, SpriteX(s) - 1);
    if KeyDown(sKey) then SpriteSetY(s, SpriteY(s) + 1);
    if KeyDown(dKey) then SpriteSetX(s, SpriteX(s) + 1);
    if KeyDown(wKey) then SpriteSetY(s, SpriteY(s) - 1);
    
    if x < 0 then x := 0;
    if y < 0 then y := 0;
    
    
    CenterCameraOn(s, -50, +50);
    DrawSprite(s);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
