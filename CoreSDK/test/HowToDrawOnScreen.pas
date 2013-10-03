program HowToDrawOnScreen;
uses
  SwinGame;

procedure Main();
begin  
  OpenGraphicsWindow('Drawing On Screen', 800, 600);  
  LoadDefaultColors();
  repeat // The game loop...
    ProcessEvents();
  
    ClearScreen(ColorWhite);
    
    DrawTextOnScreen('How To Draw On Screen', ColorRed, 330, 40);        
    
    FillRectangleOnScreen(RGBColor(205,201,201), 400, 300, 100, 100);    
    FillEllipseOnScreen(RandomColor(), 100, 100, 60, 30);    
    DrawCircleOnScreen(RandomColor(), 105, 420, 30);        
    
    RefreshScreen(60);
  until WindowCloseRequested();

  ReleaseAllResources();
end;

begin
  Main();
end.