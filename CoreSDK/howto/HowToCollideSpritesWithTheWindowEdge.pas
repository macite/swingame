program HowToCollideSpritesWithWindowEdge;
uses
    SwinGame, sgTypes;
    
procedure KeepOnScreen(s: Sprite);
begin
  if SpriteX(s) > ScreenWidth() - SpriteWidth(s) then
  begin
    SpriteSetDX(s, -SpriteDX(s));
    SpriteSetX(s, ScreenWidth() - SpriteWidth(s));
  end;
  if SpriteY(s) > ScreenHeight() - SpriteHeight(s) then
  begin
    SpriteSetDY(s, -SpriteDY(s));
    SpriteSetY(s, ScreenHeight() - SpriteHeight(s));
  end;
  
  if SpriteX(s) < 0 then
  begin
    SpriteSetDX(s, -SpriteDX(s));
    SpriteSetX(s, 0);
  end;
  if SpriteY(s) < 0 then
  begin
    SpriteSetDY(s, -SpriteDY(s));
    SpriteSetY(s, 0);
  end;
end;

procedure Main();
var
    asteroid: Sprite;
begin
    OpenGraphicsWindow('Keeping Sprite On Screen', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('asteroid', 'asteroid.png');

    asteroid := CreateSprite(BitmapNamed('asteroid'));
    SpriteSetX(asteroid, 100);
    SpriteSetY(asteroid, 500);
    SpriteSetMass(asteroid, 1);
    SpriteSetVelocity(asteroid, VectorTo(4, -2.4));

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawSprite(asteroid);
        UpdateSprite(asteroid);
        
        KeepOnScreen(asteroid);
        RefreshScreen(60);
    until WindowCloseRequested();
    FreeSprite(asteroid);
    ReleaseAllResources();
end;

begin
    Main();
end.