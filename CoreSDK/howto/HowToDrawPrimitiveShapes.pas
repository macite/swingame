program HowToDrawPrimitiveShapes;
uses
  SwinGame, sgTypes;

procedure Main();
begin 

  OpenGraphicsWindow('Primitive shapes', 800, 250);
  LoadDefaultColors();

  ClearScreen(ColorWhite);
  DrawCircle(ColorBlack, 50, 50, 30);
  DrawEllipse(ColorBlue, 150, 20, 120, 60);
  DrawLine(ColorGreen, 300, 40, 450, 40);
  DrawRectangle(ColorGrey, 500, 20, 100, 60 );
  DrawTriangle(ColorPink, 650, 80, 750, 80, 700, 20);

  FillCircle(ColorMagenta, 50, 160, 30);
  FillEllipse(ColorRed, 150, 130, 120, 60);
  FillRectangle(ColorTurquoise, 300, 130, 100, 60);
  FillTriangle(ColorYellow, 500, 190, 700, 190, 600, 130);

  RefreshScreen();

  Delay(5000);

  ReleaseAllResources();    
end;

begin
  Main();
end.