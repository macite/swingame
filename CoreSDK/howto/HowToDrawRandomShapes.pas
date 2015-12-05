program HowToDrawRandomShape;
uses
  SwinGame, sgTypes;

procedure Main();
var 
  i : Integer;
begin  
  OpenGraphicsWindow('How To Draw Random Shape', 800, 600);    
  LoadDefaultColors();

  ClearScreen(ColorWhite);

  repeat // The game loop...
    ProcessEvents();    

    i := Rnd(7);    
    case i of
      0: DrawCircle(RandomColor(), Rnd(800), Rnd(600), Rnd(300));
      1: DrawEllipse(RandomColor(), Rnd(800), Rnd(600), Rnd(800), Rnd(600));
      2: DrawHorizontalLine(RandomColor(), Rnd(600), Rnd(800), Rnd(800));
      3: DrawLine(RandomColor(), Rnd(800), Rnd(600), Rnd(800), Rnd(600));
      4: DrawRectangle(RandomColor(), Rnd(800), Rnd(600), Rnd(800), Rnd(600));
      5: DrawTriangle(RandomColor(), Rnd(800), Rnd(600), Rnd(800), Rnd(600), Rnd(800), Rnd(600));
      6: DrawVerticalLine(RandomColor(), Rnd(800), Rnd(600), Rnd(600));
    end;
  
    RefreshScreen(60);
  until WindowCloseRequested() OR KeyTyped(EscapeKey) OR KeyTyped(QKey);

  ReleaseAllResources();
end;

begin
  Main();
end.