program DrawBitmapTest;
uses sgTypes, SwinGame, sgDrawingOptions;

var 
	opts : BitmapDrawOpts;
	i : integer;
begin
	OpenGraphicsWindow('test',1000,1000);
	// ShowSwinGameSplashScreen();
	LoadBitmapNamed('test','Swinburne.jpg');
	opts:=OptionDefaults();
	for i:=1 to 1000 do
		begin
			opts:=OptionRotateBmp(10,10,90,opts);
			ClearScreen(ColorWhite);
			DrawBitmap('test',100,100,opts);
			RefreshScreen();
		end;
end.