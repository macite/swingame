program HowToCollideTwoSprites;
uses
    SwinGame, sgTypes;

procedure Main();
var
    earth: Sprite;
    asteroid: Sprite;
begin
    OpenGraphicsWindow('Colliding Sprites', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('earth', 'earth.png');
    LoadBitmapNamed('asteroid', 'asteroid.png');

    earth := CreateSprite(BitmapNamed('earth'));
    SpriteSetX(earth, 700);
    SpriteSetY(earth, 100);
    SpriteSetVelocity(earth, VectorTo(-0.8, 0.6));

    asteroid := CreateSprite(BitmapNamed('asteroid'));
    SpriteSetX(asteroid, 100);
    SpriteSetY(asteroid, 500);
    SpriteSetVelocity(asteroid, VectorTo(1, -0.6));

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawSprite(earth);
        UpdateSprite(earth);
        DrawSprite(asteroid);
        UpdateSprite(asteroid);
        if SpriteCollision(earth, asteroid) then CollideCircles(asteroid, earth);

        RefreshScreen();
    until WindowCloseRequested();
    FreeSprite(earth);
    FreeSprite(asteroid);
    ReleaseAllResources();
end;

begin
    Main();
end.