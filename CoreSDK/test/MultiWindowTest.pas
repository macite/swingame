program MultWindowTest;
uses SwinGame, sgTypes, SysUtils;

procedure Main();
var
	i: Integer;
	clr: Color;
begin
	OpenWindow('Window 1', 300, 300);
	OpenWindow('Window 2', 400, 400);

	SetCurrentWindow('Window 1');
	ClearScreen(ColorAqua);
	RefreshScreen();

	SetCurrentWindow('Window 2');
	ClearScreen(ColorIndigo);
	RefreshScreen();

	clr := ColorIndigo;

	i := 0;

	repeat
 		ProcessEvents();

 		if KeyTyped(Key1) then
 		begin
 			SetCurrentWindow('Window 1');
 			clr := ColorAqua;
 		end;

 		if KeyTyped(Key2) then
 		begin
 			SetCurrentWindow('Window 2');
 			clr := ColorIndigo;
 		end;

 		if WindowCloseRequested(WindowNamed('Window 2')) then
 		begin
 			CloseWindow(WindowNamed('Window 2'));
 		end;

 		ClearScreen(clr);
 		FillRectangle(ColorWhite, 0, 0, 100, 100);
 		DrawText(IntToStr(i), ColorBlack, 0, 0);
		RefreshScreen();

		i := i + 1;
	until QuitRequested();
end;

begin
	Main();
end.
