program HowToCreateProgressBar;
uses 
    SwinGame, sgTypes;
    
procedure Main();
var
    partRect: Rectangle;
    fullBar: Bitmap;
    width: LongInt;
    size: rectangle;
begin
    OpenGraphicsWindow('Progress Bar', 400, 400);

    LoadBitmapNamed('empty', 'progress_empty.png');
    fullBar := LoadBitmapNamed('full', 'progress_full.png');
    width := 0;
    partRect := RectangleFrom(0, 0, width, 39);

    repeat
    ProcessEvents();
    ClearScreen(ColorWhite);
    DrawText('Press Space to progress :)', ColorBlack, 10, 10);
    DrawBitmap ('empty', 46, 180);
    if KeyTyped(VK_SPACE) then
    begin
        width := width + 31;
        partRect := RectangleFrom(0, 0, width, 39);
    end;
    DrawBitmapPart (fullBar, partRect, 46, 180);
    RefreshScreen();
    until WindowCloseRequested();

    ReleaseAllResources();
end;

begin
    Main();
end.