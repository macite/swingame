program HowToDrawBitmap;
uses sgGraphics, sgTypes, sgImages, sgUtils, sgAudio, sgResources;

procedure main();
begin    
    OpenGraphicsWindow('Import Bitmap', 800, 600);
    
	ClearScreen();
	
	LoadBitmapNamed('predator', 'frog.png');
	DrawBitmap ('predator', 350, 100);
	
	RefreshScreen();
    
    delay(5000);

	ReleaseAllResources();
end;

begin
	main();
end.