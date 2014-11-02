program HowToUseVectorToDetermineAPath;
uses
    SwinGame, sgTypes;
  
procedure Main();
var
    origin: Point2D;
    planetVector, ufoVector, resultant: Vector;
    distance: LineSegment;
begin    
    OpenGraphicsWindow('Determine Vector Path', 800, 600);
    
    ClearScreen(ColorWhite);
  
    origin := PointAt(0, 0);
    planetVector := VectorTo(700, 100);
    ufoVector := VectorTo(150, 520);
  
    LoadBitmapNamed('planet', 'planet.png');
    LoadBitmapNamed('ufo', 'ufo.png');
  
    repeat
    ProcessEvents();
    ClearScreen(ColorWhite);
  
    resultant := SubtractVectors(planetVector, ufoVector);
    distance := LineFromVector(ufoVector, resultant);
  
    DrawBitmap ('planet', planetVector);
    DrawBitmap ('ufo', ufoVector);
    DrawLine(ColorBlue, origin, planetVector);
    DrawLine(ColorRed, origin, ufoVector);
    DrawLine(ColorGreen, distance);
    RefreshScreen();
    until WindowcloseRequested();

    ReleaseAllResources();
end;

begin
    Main();
end.