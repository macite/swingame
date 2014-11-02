program HowToDrawFramesPerSecond;
uses 
    SwinGame, sgTypes;

procedure Main();
begin
    OpenGraphicsWindow('Draw Framerate', 400, 300);

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawFramerate(10,8);
        RefreshScreen();
    until WindowCloseRequested();
  
    ReleaseAllResources();
end;

begin
    Main();
end.