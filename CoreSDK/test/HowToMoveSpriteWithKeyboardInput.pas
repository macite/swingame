program HowToMoveSpriteWithKeyboardInput;
uses
    SwinGame, sgTypes;

procedure Main();
var
    ball: Sprite;
begin
    OpenGraphicsWindow('Move a Sprite with Keyboard Input', 800, 600);

    LoadBitmapNamed('ball', 'ball_small.png');

    ball := CreateSprite(BitmapNamed('ball'));
    SpriteSetX(ball, 385);
    SpriteSetY(ball, 285);

    repeat
    ProcessEvents();
    ClearScreen(ColorWhite);

    if KeyDown(VK_RIGHT) then
    begin
        SpriteSetDx(ball, 1);
        SpriteSetDy(ball, 0);
    end

    else if KeyDown(VK_LEFT) then
    begin
        SpriteSetDx(ball, -1);
        SpriteSetDy(ball, 0);
    end

    else if KeyDown(VK_UP) then
    begin
        SpriteSetDx(ball, 0);
        SpriteSetDy(ball, -1);
    end

    else if KeyDown(VK_DOWN) then
    begin
        SpriteSetDx(ball, 0);
        SpriteSetDy(ball, 1);
    end

    else
    begin
        SpriteSetDx(ball, 0);
        SpriteSetDy(ball, 0);
    end;

    DrawSprite(ball);
    UpdateSprite(ball);
    RefreshScreen();
    until WindowCloseRequested();
    FreeSprite(ball);
    ReleaseAllResources();
end;

begin
    Main();
end.