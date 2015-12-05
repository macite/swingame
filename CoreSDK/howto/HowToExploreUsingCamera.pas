program HowToExploreUsingCamera;
uses
  SwinGame, sgTypes;
  
procedure Main();
begin 
  OpenGraphicsWindow('Explore with camera', 800, 600);
  LoadDefaultColors();
  repeat // The game loop...
    ProcessEvents();      
      
    //Move the camera
    if KeyDown(UPKey) then MoveCameraBy(0, -1);
    if KeyDown(DOWNKey) then MoveCameraBy(0, +1);
    if KeyDown(LEFTKey) then MoveCameraBy(-1, 0);
    if KeyDown(RIGHTKey) then MoveCameraBy(+1, 0);
    
    // Move Camera to a certain point
    if KeyTyped(Key0) then MoveCameraTo(0, 0);     
    
    //Draw the scene
    ClearScreen(ColorWhite);    
    
    FillRectangle(RGBColor(205,201,201), -150, 250, 1150, 20);    
    FillRectangle(RGBColor(205,201,201), -150, 330, 1150, 20);
    
    FillRectangle(RGBColor(255,255,0), -150, 290, 50, 20);
    FillRectangle(RGBColor(124,252,0), -50, 290, 50, 20);
    FillRectangle(RGBColor(184,134,11), 50, 290, 50, 20);
    FillRectangle(RGBColor(0,255,255), 150, 290, 50, 20);
    FillRectangle(RGBColor(255,165,0), 250, 290, 50, 20);
    FillRectangle(RGBColor(255,192,203), 350, 290, 50, 20);
    
    FillRectangle(RGBColor(160,32,240), 450, 290, 50, 20);
    FillRectangle(RGBColor(165,42,42), 550, 290, 50, 20);
    FillRectangle(RGBColor(240,230,140), 650, 290, 50, 20);
    FillRectangle(RGBColor(0,0,128), 750, 290, 50, 20);
    FillRectangle(RGBColor(245,255,250), 850, 290, 50, 20);
    FillRectangle(RGBColor(255,228,225), 950, 290, 50, 20);  
    
    DrawCircle(ColorBlue, 105, 420, 30);
    FillRectangle(ColorBlack, 100, 450, 10, 50);        
    
    FillEllipse(ColorBlue, 300, 50, 60, 30);
    DrawText('Ellipse in blue at position 300,50', ColorRed, 380, 60);        
    
    DrawTextOnScreen(PointToString(CameraPos()), ColorBlack, 0, 0);    
    
    RefreshScreen();    
    
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
  Main();
end.