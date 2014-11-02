program HowToCollideASpriteWithRectangle;
uses
    SwinGame, sgTypes;

procedure Main();
var
    leftRect, rightRect, topRect, bottomRect: Rectangle;
    ball: Sprite;
begin
    OpenGraphicsWindow('Bouncing Ball', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('ball', 'ball_small.png');

    leftRect := RectangleFrom(50, 80, 30, 440);
    rightRect := RectangleFrom(720, 80, 30, 440);
    topRect := RectangleFrom(80, 50, 640, 30);
    bottomRect := RectangleFrom(80, 520, 640, 30);

    ball := CreateSprite(BitmapNamed('ball'));
    SpriteSetX(ball, 200);
    SpriteSetY(ball, 400);
    SpriteSetVelocity(ball, VectorTo(1, -0.6));

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);

        FillRectangle(ColorBlue, leftRect);
        FillRectangle(ColorRed, rightRect);
        FillRectangle(ColorGreen, topRect);
        FillRectangle(ColorYellow, bottomRect);

        DrawSprite(ball);
        UpdateSprite(ball);

        if SpriteRectCollision(ball, leftRect) then
            CollideCircleRectangle(ball, leftRect);

        if SpriteRectCollision(ball, rightRect) then
            CollideCircleRectangle(ball, rightRect);

        if SpriteRectCollision(ball, topRect) then
            CollideCircleRectangle(ball, topRect);

        if SpriteRectCollision(ball, bottomRect) then
            CollideCircleRectangle(ball, bottomRect);

        RefreshScreen();
    until WindowCloseRequested();
    FreeSprite(ball);
    ReleaseAllResources();
end;

begin
    Main();
end.