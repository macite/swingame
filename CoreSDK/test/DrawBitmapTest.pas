program DrawBitmapTest;
uses sgTypes, SwinGame, sgDrawingOptions;

var 
	opts : BitmapDrawOpts;
	i : integer;
begin
	OpenGraphicsWindow('test',1000,1000);
	LoadBitmapNamed('test','Swinburne.jpg');
	ClearScreen(ColorWhite);
	ShowSwinGameSplashScreen();
	// opts:=OptionDefaults();
	// DrawBitmap('test',100,100,opts);
	// RefreshScreen();
	// Delay(1000);
	// ClearScreen(ColorWhite);
	// // opts:=OptionPartBmp(140,140,100,100);
	// DrawBitmapPart(BitmapNamed('test'),140,140,100,100,100,100);
	// RefreshScreen();
	// Delay(1000);
	// ClearScreen(ColorWhite);
	// opts:=OptionFlipXY(opts);
	// DrawBitmap('test',100,100,opts);
	// RefreshScreen();
	// Delay(1000);
end.