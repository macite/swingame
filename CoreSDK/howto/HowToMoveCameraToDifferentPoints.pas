program HowToMoveCameraToDifferentPoints;
uses
  SwinGame, sgTypes;  
  
procedure Main();
begin 
  OpenGraphicsWindow('Move Camera To Different Points', 800, 600);
  LoadDefaultColors();
  
  repeat // The game loop...
    ProcessEvents();  
    
    //Move the camera by KeyBoard
    if KeyDown(vk_UP) then MoveCameraBy(0, -1);
    if KeyDown(vk_DOWN) then MoveCameraBy(0, +1);
    if KeyDown(vk_LEFT) then MoveCameraBy(-1, 0);
    if KeyDown(vk_RIGHT) then MoveCameraBy(+1, 0);
        
    
    // Move Camera to a certain point
    if KeyTyped(vk_A) then MoveCameraTo(400, 450); 
    if KeyTyped(vk_B) then MoveCameraTo(1400, 1300);
    if KeyTyped(vk_C) then MoveCameraTo(-500, -300);        
    
    ClearScreen(ColorWhite);
    
    //Camera current coordinate
    DrawTextOnScreen(PointToString(CameraPos()), ColorBlack, 690, 2);
    
    DrawRectangle(ColorGreen, CameraScreenRect()); // Draw rectangle that encompases the area of the game world that is curretnly on the screen
    FillCircle(ColorBlue, 400, 450, 4);
    DrawText('Point A', ColorBlue, 'Arial', 24, 407, 444);    
    
    FillCircle(ColorBlue, 1400, 1300, 4);
    DrawText('Point B', ColorBlue, 'Arial', 24, 1407, 1294);    
    
    FillCircle(ColorBlue, -500, -300, 4);
    DrawText('Point C', ColorBlue, 'Arial', 24, -493, -294);    
    
    
    RefreshScreen(60);    
    
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
  Main();
end.