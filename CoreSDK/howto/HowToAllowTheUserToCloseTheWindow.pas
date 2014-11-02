program HowToAllowTheUserToCloseTheWindow;
uses SwinGame;

procedure Main();
begin
  OpenGraphicsWindow('How To Allow The User To Close The Window', 800, 600);
  LoadDefaultColors();
  
  repeat
    // Call process events to listen to user input
    ProcessEvents();
    
    // Draw...
    ClearScreen(ColorWhite);
    DrawText('Close the window', ColorBlack, 0, 0);
    RefreshScreen();
    
    // Repeat until the user closes the window...
  until WindowCloseRequested(); 
  
end;

begin
  Main();
end.