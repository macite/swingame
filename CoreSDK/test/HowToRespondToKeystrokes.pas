program HowToRespondToKeystrokes;
uses
  SwinGame, sgTypes;  
  
procedure Main();
var
  clr: Color;  
begin  
  OpenGraphicsWindow('Keyboard Input', 240, 180);    
  LoadDefaultColors();  

  clr := RGBAColor(255, 255, 255, 64);
  ClearScreen(ColorWhite);
  repeat // The game loop...
    ProcessEvents();
    
    FillRectangle(clr, 0, 0, 240, 180);
    
    if KeyReleased(vk_a) then DrawText('A Release', ColorBlue, 'Arial', 14, 20, 40);
    
    if KeyTyped(vk_a) then DrawText('A Typed', ColorGreen, 'Arial', 14, 20, 70);         
    
    if KeyDown(vk_a) then DrawText('A Down', ColorRed, 'Arial', 14, 20, 100);      
        
    if KeyUp(vk_a) then DrawText('A Up', ColorTurquoise, 'Arial', 14, 20, 130);      
        
    DrawText('KeyBoard Input', ColorRed, 'Arial', 18, 60, 15);
    
    RefreshScreen(60);    
  until WindowCloseRequested() OR KeyTyped(vk_ESCAPE) OR KeyTyped(VK_Q);  
  
  ReleaseAllResources();
end;

begin
  Main();
end.