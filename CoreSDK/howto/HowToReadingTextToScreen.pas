program HowToReadingTextToScreen;
uses
  SwinGame;

procedure Main();
var
  fnt: Font;
  bmp: Bitmap;
begin  
  OpenGraphicsWindow('Read Text To Screen', 240, 160);
  LoadDefaultColors();

  fnt := LoadFont('arial.ttf', 12);
  StartReadingText(ColorRed, 40, fnt, 50, 10);

  bmp := DrawTextTo(fnt, 'Hello', ColorBlack, ColorTransparent);

  repeat // The game loop...
    ProcessEvents();

    ClearScreen(ColorWhite);
    DrawText('Enter: ', ColorBlack, fnt, 10, 10);
    DrawBitmap(bmp, 0, 0);

    if not ReadingText() then
    begin
      WriteLn('Got ', EndReadingText());
      StartReadingText(ColorBlack, 40, fnt, 50, 10);
    end;
    
    RefreshScreen(60);
  until WindowCloseRequested();

  ReleaseAllResources();
end;

begin
  Main();
end.