program TestSDL13;
uses SwinGame, glu, glut, glext;

procedure Main();
var
  bmp, bmp2 : Bitmap;
begin      
  OpenGraphicsWindow('Herp', 1000, 1000);
  ClearScreen();
  bmp := LoadBitmap('/Users/admin/Documents/SwingameSDK/CoreSDK/bin/Test.app/Contents/Resources/images/ball_small.png');
//  bmp2 := LoadBitmap('/Users/admin/Documents/SwingameSDK/CoreSDK/bin/Test.app/Contents/Resources/images/swinburne.jpg');

  repeat 
    ProcessEvents();
  //  DrawBitmap(bmp2, 0, 0);
    DrawBitmap(bmp, 200, 0);
    RefreshScreen();
  until WindowCloseRequested();
end;

begin
	main();
end.