program HowToDrawSomethingToTheScreen;
uses
    SwinGame, sgTypes;    

procedure Main();
begin
    OpenGraphicsWindow('How To Draw Something To The Screen', 320, 240); 
  
    ClearScreen();    // clear the background canvas (black by default)
    RefreshScreen();  // make it appear on screen...
    Delay(2000);      // wait for 2 seconds  
  
    ClearScreen(ColorYellow);   // clear the background canvas
    RefreshScreen();            // make it appear on screen...
  
    //Start drawing the "next" screen to appear...
    ClearScreen(ColorWhite);    // clear the background canvas
  
    Delay(2000);                // wait for 2 seconds... notice the yellow screen!
    RefreshScreen();            // make the white screen appear...
  
    Delay(2000);
  
    ReleaseAllResources();
end;

begin
    Main();
end.