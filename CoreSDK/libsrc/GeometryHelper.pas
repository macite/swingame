unit GeometryHelper;

interface
uses sgBackendTypes, sgTypes;

	function LineIntersectsLines(const line: LineSegment; const lines: LinesArray): Boolean;
	function LinesFrom(const tri: Triangle): LinesArray; overload;
	function LinesFrom(const rect: Rectangle): LinesArray; overload;
	function VectorOverLinesFromCircle(const c: Circle; const lines: LinesArray; const velocity: Vector; out maxIdx: Longint): Vector;

implementation
uses sgGeometry, sgTrace, sgShared;

  function LineIntersectsLines(const line: LineSegment; const lines: LinesArray): Boolean;
  var
    i: Longint;
    pt: Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineIntersectsLines(const line: LineSegment', '');
    {$ENDIF}
    
    for i := 0 to High(lines) do
    begin
      if LineIntersectionPoint(line, lines[i], pt) and PointOnLine(pt, lines[i]) and PointOnLine(pt, line) then
      begin
        result := true;
        exit;
      end;
    end;
    result := false;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineIntersectsLines(const line: LineSegment', '');
    {$ENDIF}
  end;

  function LinesFrom(const tri: Triangle): LinesArray; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LinesFrom(const tri: Triangle): LinesArray', '');
    {$ENDIF}
    
    SetLength(result, 3);
    result[0] := LineFrom(tri.points[0], tri.points[1]);
    result[1] := LineFrom(tri.points[1], tri.points[2]);
    result[2] := LineFrom(tri.points[2], tri.points[0]);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LinesFrom(const tri: Triangle): LinesArray', '');
    {$ENDIF}
  end;
 
  function LinesFrom(const rect: Rectangle): LinesArray; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LinesFrom(const rect: Rectangle): LinesArray', '');
    {$ENDIF}
    
    SetLength(result, 4);
    
    with rect do
    begin
      result[0] := LineFrom(x, y, x + width, y);
      result[1] := LineFrom(x, y, x, y + height);
      result[2] := LineFrom(x + width, y, x + width, y + height);
      result[3] := LineFrom(x, y + height, x + width, y + height);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LinesFrom(const rect: Rectangle): LinesArray', '');
    {$ENDIF}
  end;

  //
  // This internal function is used to calculate the vector and determine if a hit has occurred...
  //
  function VectorOverLinesFromCircle(const c: Circle; const lines: LinesArray; const velocity: Vector; out maxIdx: Longint): Vector;
  type
    DoublePt = record ptOnCircle, ptOnLine: Point2D; end;
  var
    ptOnLine, ptOnCircle: Point2D;
    tmp: Array [0..3] of Point2D;
    chkPts: Array [0..3] of DoublePt;
    lineVec, normalMvmt, normalLine, toEdge, edge, ray, vOut: Vector;
    i, j, hits: Longint;
    dotProd, dist, maxDist: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', '_VectorOverLinesFromCircle(const c: Circle', '');
    {$ENDIF}
    
    // Cast ray searching for points back from shape
    ray := InvertVector(velocity);
    normalMvmt := VectorNormal(velocity);
    vOut := VectorTo(0,0);
    ptOnCircle := PointAt(0,0);

    maxIdx := -1;
    maxDist := -1;

    // fix for tmp initialized warning
    for i := 0 to high(tmp) do tmp[i] := PointAt(0,0);

    //Search all lines for hit points
    for i := 0 to High(lines) do
    begin
      lineVec := LineAsVector(lines[i]);
      //Get the normal of the line we hit
      normalLine := VectorNormal(lineVec);
      hits := 0;

      //tmp 0 and tmp 1 are the widest points to detect collision with line
      WidestPoints(c, normalMvmt, tmp[0], tmp[1]);
      //tmp 2 and tmp 3 are the closest and furthest points from the line
      WidestPoints(c, normalLine, tmp[2], tmp[3]);
      
      // for both points...
      for j := 0 to 3 do
      begin
        //DrawCircle(ColorWhite, tmp[j], 2);
        
        // Cast a ray back from the test points to find line pts... out on ptOnLine
        if RayIntersectionPoint(tmp[j], ray, lines[i], ptOnLine) then
        begin
          //DrawCircle(ColorRed, ptOnLine, 1);
          //DrawLine(ColorRed, tmp[j], ptOnLine);
          
          chkPts[hits].ptOnLine := ptOnLine;
          chkPts[hits].ptOnCircle := tmp[j];
          hits := hits + 1;
        end;
      end;

      // for each of the hits on this line...
      // search for the longest hit.
      for j := 0 to hits - 1 do
      begin
        //if not maxLine then DrawCircle(ColorWhite, chkPts[j].ptOnCircle, 1);
        toEdge := VectorFromPoints(c.center, chkPts[j].ptOnCircle);
        //if not maxLine then DrawLine(ColorRed, c.center, chkPts[j].ptOnCircle);
        dotProd := DotProduct(toEdge, normalLine);

        // 2d: Most distant edge pt is on a line this distance (dotProd) from the center
        edge := AddVectors(c.center, VectorMultiply(normalLine, dotProd));
        //DrawPixel(ColorWhite, edge); // Draws pt on line to distant pt

        //  Find the point we hit on the line... ptOnLine receives intersection point...
        if not LineIntersectionPoint(LineFromVector(edge, velocity), lines[i], ptOnLine) then continue;
        // Move back onto line segment... linePt -> closest point on line to intersect point
        //if not maxLine then DrawCircle(ColorRed, ptOnLine, 1); // point on line, but not necessarily the line segment
        //if not maxLine then DrawLine(ColorWhite, edge, ptOnLine);

        ptOnLine := ClosestPointOnLine(ptOnLine, lines[i]);
        //if not maxLine then FillCircle(ColorBlue, ptOnLine, 1); // point on segment

        // Find the most distant point on the circle, given the velocity vector
        if not DistantPointOnCircleHeading(ptOnLine, c, velocity, ptOnCircle) then continue;
        // if not maxLine then FillCircle(ColorBlue, ptOnCircle, 2); // point on segment
        // if not maxLine then DrawLine(ColorBlue, ptOnLine, ptOnCircle);

        dist := PointPointDistance(ptOnLine, ptOnCircle);

        if (dist > maxDist) or (maxIdx = -1) then
        begin
          maxDist := dist;
          maxIdx := i;
          vOut := VectorFromPoints(ptOnCircle, ptOnLine);
          vOut := VectorMultiply(UnitVector(vOut), VectorMagnitude(vOut) + 1.42)
        end;
      end;
    end;
    
    result.x := Ceiling(vOut.x);
    result.y := Ceiling(vOut.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', '_VectorOverLinesFromCircle(const c: Circle', '');
    {$ENDIF}
  end;


end.
