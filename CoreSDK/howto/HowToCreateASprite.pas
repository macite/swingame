program HowToCreateASprite;
uses
    SwinGame, sgTypes;

procedure Main();
var
    ball: Sprite;
begin
    OpenGraphicsWindow('Create a Sprite', 150, 150);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('ball', 'ball_small.png');

    ball := CreateSprite(BitmapNamed('ball'));
    SpriteSetX(ball, 60);
    SpriteSetY(ball, 60);

    DrawSprite(ball);
    UpdateSprite(ball);
    RefreshScreen();
    Delay(5000);
    FreeSprite(ball);
    ReleaseAllResources();
end;

begin
    Main();
end.