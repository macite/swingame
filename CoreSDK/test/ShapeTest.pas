program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgAudio, sgText, sgGraphics, sgGeometry, sgResources, sgInput, sgTimers, sgVectorShapes;

procedure Main();
var
  pts: Point2DArray;
  p: ShapePrototype;
  s, s1: Shape;
  t: Timer;
  time: Integer;
  filled: Boolean;
  aabb: Rectangle;
  r: Rectangle;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Shape Test', 640, 480);
  
  SetLength(pts, 9);
  pts[0] := PointAt(0, 0);
  pts[1] := PointAt(0, 50);
  pts[2] := PointAt(50, 0);
  pts[3] := PointAt(50, 50);
  pts[4] := PointAt(100, 0);
  pts[5] := PointAt(100, 50);
  pts[6] := PointAt(150, 0);
  pts[7] := PointAt(150, 50);
  pts[8] := PointAt(200, 0);
  
  p := PrototypeFrom(pts, pkTriangleList);
  
  s := ShapeAtPoint(p, PointAt(200,200));
  ShapeSetAngle(s, 0);
  ShapeSetScale(s, PointAt(1.0, 1.0));
  ShapeSetColor(s, ColorRed);
  
  s1 := ShapeAtPoint(p, PointAt(0,100));
  ShapeSetAngle(s1, 15);
  ShapeSetScale(s1, PointAt(0.75, 0.75));
  ShapeSetColor(s1, RGBAColor(255, 0, 0, 100));
  
  ShapeAddSubShape(s, s1);
  
  t := CreateTimer();
  StartTimer(t);
  
  filled := false;
  
  repeat // The game loop...
    ClearScreen();
    
    ProcessEvents();
    
    r := RectangleFrom(MousePosition(), 10, 10);
    
    time := TimerTicks(t);
    
    if KeyTyped(pKey) then PrototypeSetKind(p, pkPoint);
    if KeyTyped(cKey) then PrototypeSetKind(p, pkCircle);
    // if KeyTyped(eKey) then PrototypeSetKind(p, pkEllipse);
    if KeyTyped(kKey) then PrototypeSetKind(p, pkLine);
    if KeyTyped(tKey) then PrototypeSetKind(p, pkTriangle);
    if KeyTyped(jKey) then PrototypeSetKind(p, pkLineList);
    if KeyTyped(hKey) then PrototypeSetKind(p, pkLineStrip);
    // if KeyTyped(oKey) then PrototypeSetKind(p, pkPolygon);
    if KeyTyped(lKey) then PrototypeSetKind(p, pkTriangleList);
    if KeyTyped(sKey) then PrototypeSetKind(p, pkTriangleStrip);
    if KeyTyped(fKey) then PrototypeSetKind(p, pkTriangleFan);
    
    if KeyTyped(spaceKey) then filled := not filled;
      
    //if KeyDown(leftKey) then 
    
    aabb := ShapeAABB(s);
    FillRectangle(ColorLightGrey, aabb);
    
    if filled then
      FillShape(s)
    else
      DrawShape(s);
    
    if ShapeRectangleIntersect(s, r) then
      FillRectangle(ColorBlue, r)
    else
      DrawRectangle(ColorGreen, r);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  CloseAudio();
  
  FreeShape(s);
  FreePrototype(p);
end;

begin
  Main();
end.
