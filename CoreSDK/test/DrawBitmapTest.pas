program DrawBitmapTest;
uses sgTypes, SwinGame, sgDrawingOptions;

begin
	OpenGraphicsWindow('test',1000,1000);
	// ShowSwinGameSplashScreen();

	LoadBitmapNamed('test','Swinburne.jpg');
	DrawBitmap('test',0,0, OptionScaleBmp(0.75, 0.75));
	RefreshScreen();
	Delay(1000);
end.