program HowToCollideSpriteWithLines;
uses
    SwinGame, sgTypes;

procedure Main();
var
    leftLine, rightLine, topLine, bottomLine: LineSegment;
    ball: Sprite;
begin
    OpenGraphicsWindow('Bouncing Ball', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('ball', 'ball_small.png');

    leftLine := LineFrom(10, 10, 10, 590);
    rightLine := LineFrom(790, 10, 790, 590);
    topLine := LineFrom(10, 10, 790, 10);
    bottomLine := LineFrom(10, 590, 790, 590);

    ball := CreateSprite(BitmapNamed('ball'));
    SpriteSetX(ball, 100);
    SpriteSetY(ball, 500);
    SpriteSetVelocity(ball, VectorTo(1, -0.6));

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);

        DrawLine(ColorRed, leftLine);
        DrawLine(ColorRed, rightLine);
        DrawLine(ColorRed, topLine);
        DrawLine(ColorRed, bottomLine);

        DrawSprite(ball);
        UpdateSprite(ball);

        if CircleLineCollision(ball, leftLine) then
            CollideCircleLine(ball, leftLine);

        if CircleLineCollision(ball, rightLine) then
            CollideCircleLine(ball, rightLine);

        if CircleLineCollision(ball, topLine) then
            CollideCircleLine(ball, topLine);

        if CircleLineCollision(ball, bottomLine) then
            CollideCircleLine(ball, bottomLine);

        RefreshScreen();
    until WindowCloseRequested();
    FreeSprite(ball);
    ReleaseAllResources();
end;

begin
    Main();
end.