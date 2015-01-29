program BitmapDrawingTest;
uses SwinGame;

procedure Main();
begin
	OpenGraphicsWindow('BitmapDrawingTest', 600, 600);
	LoadDefaultColors();

	repeat
		ProcessEvents();

		ClearScreen(ColorBlack);
		DrawBitmap('bubble.png', 50, 50);
		RefreshScreen();

	until WindowCloseRequested();
end;

begin
	Main();
end.