program DrawTextTest;
uses SwinGame, sgTypes;

procedure Main();
var
	fnt: Font;
begin
	OpenGraphicsWindow('Draw Text Test', 800, 600);
	fnt := LoadFontNamed('arial:14', 'arial.ttf', 14);
	LoadFontNamed(FontNameFor('arial', 10), 'arial.ttf', 10);

	repeat
		ClearScreen(ColorWhite);
		DrawFramerate(0, 0);
		DrawText('With Font', ColorBlack, fnt, 0, 20);
		DrawText('With Font Name and Size', ColorBlack, 'arial', 10, 0, 40);
		DrawText('With Font File Name and Size', ColorBlack, 'arial.ttf', 14, 0, 60);

		DrawText('Hello' + LineEnding + LineEnding + 'World', ColorBlack, fnt, 0, 80);

		RefreshScreen();

		ProcessEvents();
	until WindowCloseRequested();

end;

begin
	Main();
end.