program HowToUseRadioButton;
uses 
 SwinGame, sgTypes, sgUserInterface;

procedure Main();
var
  cp : Panel;  
begin    
  OpenGraphicsWindow('How To Radio Button ', 400, 150);
  LoadDefaultColors();  
  
  cp := LoadPanel('radioPanel.txt');  
  ShowPanel(cp);
  PanelSetDraggable(cp, false);
  
  repeat // The game loop...
    ProcessEvents();    
    
    ClearScreen(ColorWhite);        
    DrawInterface();  
    
    UpdateInterface();
    
    RefreshScreen();        
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
    Main();
end.