program GameMain;
uses SwinGame;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  LoadDefaultColors();
  ShowSwinGameSplashScreen();
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen(ColorWhite);
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.
