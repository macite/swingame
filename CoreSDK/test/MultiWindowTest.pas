program MultWindowTest;
uses SwinGame, sgTypes, SysUtils;

procedure Main();
var
	i, active: Integer;
begin
	OpenWindow('Window 1', 300, 300);
	OpenWindow('Window 2', 400, 400);

	SetCurrentWindow('Window 1');
	ClearScreen(ColorAqua);
	RefreshScreen();

	SetCurrentWindow('Window 2');
	ClearScreen(ColorIndigo);
	RefreshScreen();

	i := 0;
	active := 1;

	repeat
 		ProcessEvents();

 		if KeyTyped(Key1) then
 		begin
 			active := 1;
 		end;

 		if KeyTyped(Key2) then
 		begin
 			active := 2;
 		end;

 		SetCurrentWindow('Window 1');
 		ClearScreen(ColorAqua);
 		FillCircle(ColorYellow, MousePosition(), 25);
 		RefreshScreen();

 		SetCurrentWindow('Window 2');
 		ClearScreen(ColorIndigo);
 		FillCircle(ColorYellow, MousePosition(), 25);
 		RefreshScreen();

 		case active of
 		 1: SetCurrentWindow('Window 1');
 		 2: SetCurrentWindow('Window 2');
 		end;

 		if WindowCloseRequested(WindowNamed('Window 2')) then
 		begin
 			CloseWindow(WindowNamed('Window 2'));
 		end;

 		DrawText(IntToStr(i), ColorBlack, 0, 0);
		RefreshScreen();

		i := i + 1;
	until QuitRequested();
end;

begin
	Main();
end.
