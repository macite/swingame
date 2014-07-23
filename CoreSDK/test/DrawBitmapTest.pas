program DrawBitmapTest;
uses sgTypes, SwinGame, sgDrawingOptions, sgCamera;

var 
	opts : BitmapDrawOpts;
	i : integer;
begin
	OpenGraphicsWindow('test',1000,1000);
	LoadBitmapNamed('test','Swinburne.jpg');
	
	for i:=1 to 400 do
		begin
			ClearScreen(ColorWhite);
			DrawBitmap('test',100,100,OptionToWorld());
			DrawBitmap('test',400,400,OptionToScreen());
			DrawBitmap('test',700,700);
			MoveCameraBy(0,-1);
			RefreshScreen();
		end;
end.