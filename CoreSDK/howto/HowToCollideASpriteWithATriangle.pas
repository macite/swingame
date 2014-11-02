program HowToCollideASpriteWithATriangle;
uses
    SwinGame, sgTypes;

procedure Main();
var
    leftTri, rightTri, topTri, bottomTri: Triangle;
    leftLines, rightLines, topLines, bottomLines: LinesArray;
    ball: Sprite;
begin
    OpenGraphicsWindow('Bouncing Ball', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('ball', 'ball_small.png');

    leftTri := TriangleFrom(10, 10, 10, 580, 200, 285);
    rightTri := TriangleFrom(790, 10, 790, 580, 600, 285);
    topTri := TriangleFrom(10, 10, 780, 10, 390, 170);
    bottomTri := TriangleFrom(10, 590, 780, 590, 390 ,430);

    leftLines := LinesFrom(leftTri);
    rightLines := LinesFrom(rightTri);
    topLines := LinesFrom(topTri);
    bottomLines := LinesFrom(bottomTri);

    ball := CreateSprite(BitmapNamed('ball'));
    SpriteSetX(ball, 300);
    SpriteSetY(ball, 300);
    SpriteSetVelocity(ball, VectorTo(1, -0.6));

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);

        FillTriangle(ColorRed, leftTri);
        FillTriangle(ColorGreen, rightTri);
        FillTriangle(ColorYellow, topTri);
        FillTriangle(ColorBlue, bottomTri);

        DrawSprite(ball);
        UpdateSprite(ball);

        if CircleTriangleCollision(SpriteCircle(ball), leftTri) then
            CollideCircleLines(ball, leftLines);

        if CircleTriangleCollision(SpriteCircle(ball), rightTri) then
            CollideCircleLines(ball, rightLines);

        if CircleTriangleCollision(SpriteCircle(ball), topTri) then
            CollideCircleLines(ball, topLines);

        if CircleTriangleCollision(SpriteCircle(ball), bottomTri) then
            CollideCircleLines(ball, bottomLines);

        RefreshScreen();
    until WindowCloseRequested();
    FreeSprite(ball);
    ReleaseAllResources();
end;

begin
    Main();
end.