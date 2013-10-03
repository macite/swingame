program HowToDrawAPanel;
uses 
 SwinGame, sgTypes, sgUserInterface;

procedure Main();
var
  p : Panel;  
begin    
  OpenGraphicsWindow('How To Draw A Panel', 800, 600);
  LoadDefaultColors();  
  
  p := LoadPanel('panelwithlabel.txt');  
  ShowPanel(p);
  
  ClearScreen(ColorWhite);
  GUISetBackgroundColor(ColorBlack);
  DrawInterface();
  
  RefreshScreen();
  
  Delay(10000);  
  ReleaseAllResources();
end;

begin
    Main();
end.