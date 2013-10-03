program SpriteRecTest;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
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
    
    
    if KeyDown(vk_UP) then y -= 1;
    if KeyDown(vk_DOWN) then y += 1;
    if KeyDown(vk_LEFT) then x -= 1;
    if KeyDown(vk_RIGHT) then x += 1;
    
    if KeyDown(vk_a) then SpriteSetX(s, SpriteX(s) - 1);
    if KeyDown(vk_s) then SpriteSetY(s, SpriteY(s) + 1);
    if KeyDown(vk_d) then SpriteSetX(s, SpriteX(s) + 1);
    if KeyDown(vk_w) then SpriteSetY(s, SpriteY(s) - 1);
    
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
