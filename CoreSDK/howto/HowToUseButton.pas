program HowToUseButton;
uses 
 SwinGame, sgTypes, sgUserInterface;

procedure ChangeColor(c : Color);
begin
  GUISetBackgroundColor(c);
  GUISetForegroundColor(c);
end; 
 
procedure Main();
var
  p1,p2 : Panel;  
begin    
  OpenGraphicsWindow('How To Use Button ', 320, 240);
  LoadDefaultColors();  
  
  p1 := LoadPanel('buttonPanel1.txt');  
  ShowPanel(p1);
  p2 := LoadPanel('buttonPanel2.txt');  
  ShowPanel(p2);
  PanelSetDraggable(p1, false);  
  PanelSetDraggable(p2, false);    
  
  ChangeColor(ColorBlue);  
  
  repeat // The game loop...
    ProcessEvents();    
    
    if RegionClickedId() = 'Button1' then ChangeColor(RandomColor());    
    DrawInterface();      
    UpdateInterface();
    
    RefreshScreen();        
  until WindowCloseRequested() or (RegionClickedId() = 'Button2');  
  ReleaseAllResources();
end;

begin
    Main();
end.