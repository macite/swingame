program SimpleMouseClick;
uses SwinGame, sgTypes;

procedure Main();
var
  clr: Color;
begin
  OpenWindow('SimpleMouseClick', 600, 600);

  clr := ColorWhite;

  repeat
    ProcessEvents();
    if MouseClicked(LeftButton) then clr := ColorGreen;
    if MouseClicked(RightButton) then clr := ColorRed;

    ClearScreen(clr);

    if MouseDown(LeftButton) then DrawText('Left Button Down', ColorBlack, 10, 10);
    if MouseUp(LeftButton) then DrawText('Left Button Up', ColorBlack, 10, 10);
    if MouseDown(RightButton) then DrawText('Right Button Down', ColorBlack, 10, 30);
    if MouseUp(RightButton) then DrawText('Right Button Up', ColorBlack, 10, 30);

    RefreshScreen();
  until WindowCloseRequested();
end;

begin
  Main();
end.
