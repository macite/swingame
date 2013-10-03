program HowToCenterCameraOnASprite;
uses
  SwinGame, sgTypes;
  
procedure Main();
var  
  sSprite : Sprite;  
begin 
  OpenGraphicsWindow('Center Camera On A Sprite', 800, 600);  
  LoadDefaultColors();  
  LoadBitmapNamed('ufo', 'ufo.png');  
  
  sSprite := CreateSprite(BitmapNamed('ufo'));
  SpriteSetX(sSprite, 60);
  SpriteSetY(sSprite, 60);
  
  repeat // The game loop...
    ProcessEvents();  
    
    ClearScreen(ColorWhite);    
    
    if KeyDown(vk_UP) then  SpriteSetY(sSprite, SpriteY(sSprite)-1);          
    if KeyDown(vk_DOWN) then SpriteSetY(sSprite, SpriteY(sSprite)+1);    
    if KeyDown(vk_LEFT) then SpriteSetX(sSprite, SpriteX(sSprite)-1);    
    if KeyDown(vk_RIGHT) then SpriteSetX(sSprite, SpriteX(sSprite)+1);    
    
    FillRectangle(RGBColor(205,201,201), -150, 250, 1150, 20);    
    FillRectangle(RGBColor(205,201,201), -150, 330, 1150, 20);
    
    FillRectangle(RGBColor(255,255,0), -150, 290, 50, 20);
    FillRectangle(RGBColor(124,252,0), -50, 290, 50, 20);
    FillRectangle(RGBColor(184,134,11), 50, 290, 50, 20);
    FillRectangle(RGBColor(0,255,255), 150, 290, 50, 20);
    FillRectangle(RGBColor(255,165,0), 250, 290, 50, 20);
    FillRectangle(RGBColor(255,192,203), 350, 290, 50, 20);
    
    FillRectangle(RGBColor(160,32,240), 450, 290, 50, 20);
    FillRectangle(RGBColor(165,42,42), 550, 290, 50, 20);
    FillRectangle(RGBColor(240,230,140), 650, 290, 50, 20);
    FillRectangle(RGBColor(0,0,128), 750, 290, 50, 20);
    FillRectangle(RGBColor(245,255,250), 850, 290, 50, 20);
    FillRectangle(RGBColor(255,228,225), 950, 290, 50, 20);
    
    FillRectangle(RGBColor(205,201,201), 320, -275, 20, 1150);    
    FillRectangle(RGBColor(205,201,201), 510, -275, 20, 1150);
            
    if KeyTyped(vk_SPACE) then CenterCameraOn(sSprite, 0, 0);    
        
    DrawTextOnScreen(PointToString(CameraPos()), ColorBlack, 0, 0);    
    DrawSprite(sSprite);    
    
    RefreshScreen();    
  until WindowCloseRequested();  
  FreeSprite(sSprite);
  ReleaseAllResources();
end;

begin
  Main();
end.