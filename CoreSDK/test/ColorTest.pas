program ColorTest;
uses SwinGame;

begin
	OpenGraphicsWindow('Color Test', 400, 400);
	ClearScreen(ColorRed);
	RefreshScreen();
	Delay(1000);

	ClearScreen(ColorGreen);
	RefreshScreen();
	Delay(1000);

	ClearScreen(ColorBlue);
	RefreshScreen();
	Delay(1000);

	ClearScreen(ColorYellow);
	RefreshScreen();
	Delay(1000);

	ClearScreen(ColorWhite);
	FillRectangle(ColorTransparent, 50, 50, 100, 100);
	RefreshScreen();
	Delay(1000);	
	
end.
