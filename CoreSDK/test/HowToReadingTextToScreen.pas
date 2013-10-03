program HowToReadingTextToScreen;
uses
  SwinGame;

procedure Main();
begin  
  OpenGraphicsWindow('Read Text To Screen', 240, 160);  
  LoadDefaultColors();
  repeat // The game loop...
    ProcessEvents();
  
    ClearScreen(ColorWhite);
    
    if NOT ReadingText() THEN StartReadingText(ColorRed, 40, LoadFont('Arial',12), 10, 10);   
    
    RefreshScreen(60);
  until WindowCloseRequested();

  ReleaseAllResources();
end;

begin
  Main();
end.