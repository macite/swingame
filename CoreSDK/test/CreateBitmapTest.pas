program CreateBitmapTest;
uses SwinGame, sgTypes;

procedure Main();
var
  bmp: Bitmap;
begin
  bmp := CreateBitmap(400, 400);

  ClearSurface(bmp, ColorRed);
  DrawRectangle(ColorPink, 1, 1, 398, 398, OptionDrawTo(bmp));
  ClearSurface(bmp, ColorTransparent);
  DrawRectangle(ColorGreen, 0, 0, 400, 400, OptionDrawTo(bmp));

  OpenWindow('Create Bitmap Test', 600, 600);
  ClearScreen(ColorWhite);

  DrawBitmap(bmp, 100, 100);
  RefreshScreen();

  Delay(5000);
end;

begin
  Main();
end.
