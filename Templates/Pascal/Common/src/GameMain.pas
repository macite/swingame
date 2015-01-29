program GameMain;
uses SwinGame;

procedure Main();
begin
  OpenGraphicsWindow('Hello World', 800, 600);
  ShowSwinGameSplashScreen();
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen(ColorWhite);
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
end;

begin
  Main();
end.
