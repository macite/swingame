program SaveBitmaps;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sysutils,
  sgTypes, sgAudio, sgText, sgGraphics, sgResources,
  sgCamera, sgGeometry, sgImages, sgInput, sgPhysics, 
  sgSprites, sgTimers;

procedure Main();
var
  bmp, rot, zoom: Bitmap;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Save Bitmap', 640, 480);
  
  bmp := CreateBitmap(100, 100);
  
  ClearSurface(bmp, ColorYellow);
  
  FillRectangle(bmp, ColorWhite,  1, 1, 98, 98);
  FillRectangle(bmp, ColorRed,    2, 2, 96, 96);
  FillRectangle(bmp, ColorGreen,  3, 3, 94, 94);
  FillRectangle(bmp, ColorBlue,   4, 4, 92, 92);
  
  rot := RotateScaleBitmap(bmp, 45, 1);
  zoom := RotateScaleBitmap(bmp, 0, 2);
  
  WriteLn(HexStr(rot), ' = ', HexStr(zoom));
  
  SaveToPNG(bmp, GetUserDir() + PathDelim + 'Desktop/test.png');
  SaveToPNG(rot, GetUserDir() + PathDelim + 'Desktop/test_rot.png');
  SaveToPNG(zoom, GetUserDir() + PathDelim + 'Desktop/test_zoom.png');
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
