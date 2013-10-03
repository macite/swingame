program HowToDrawABitmap;
uses 
    SwinGame, sgTypes;

procedure Main();
begin    
    OpenGraphicsWindow('Draw Bitmap', 800, 600);
    
    ClearScreen(ColorWhite);

    LoadBitmapNamed('rocket image', 'rocket_large.png');
    DrawBitmap ('rocket image', 111, 4);
    RefreshScreen();
    Delay(5000);

    ReleaseAllResources();
end;

begin
    Main();
end.