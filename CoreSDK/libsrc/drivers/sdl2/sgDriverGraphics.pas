unit sgDriverGraphics;
//=============================================================================
// sgDriverGraphics.pas
//=============================================================================
//
// The Graphics driver is responsible for acting as the interface between driver
// code and swingame code. Swingame code uses the graphics driver to access the 
// current active driver. 
//
// Changing this driver will probably cause graphics drivers to break.
//
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//		- Pascal SmallInt is equivalent to Sint16
//
//=============================================================================

interface
	uses sgTypes, sgBackendTypes;

  procedure LoadGraphicsDriver();

  function RGBAColor(r, g, b, a: Byte)  : Color;
  function GetPixel32 (bmp: BitmapPtr; x, y: Single) : Color;
  procedure FillTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions);
  procedure DrawTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions);
  procedure FillCircle(clr: Color; xc, yc, radius: Single; const opts : DrawingOptions);
  procedure DrawCircle(clr: Color; xc, yc, radius: Single; const opts : DrawingOptions); 
  procedure FillEllipse (clr: Color; x, y, width, height: Single; const opts : DrawingOptions);
  procedure DrawEllipse (clr: Color; x, y, width, height: Single; const opts : DrawingOptions);
  procedure FillRectangle (clr : Color; rect : Rectangle; const opts : DrawingOptions);
  procedure DrawRectangle (clr : Color; rect : Rectangle; const opts : DrawingOptions);
  procedure DrawLine(clr : Color; x1, y1, x2, y2 : Single; const opts : DrawingOptions);
  procedure DrawPixel(clr : Color; x, y : Single; const opts: DrawingOptions);
  procedure SetClipRectangle(dest : BitmapPtr; rect : Rectangle);
  procedure ResetClip(dest : BitmapPtr);
  procedure SetVideoModeFullScreen();
  procedure SetVideoModeNoFrame();
  procedure InitializeGraphicsWindow(const caption : String; screenWidth, screenHeight : LongInt);
  procedure InitializeScreen( screen: BitmapPtr; x, y : LongInt; bgColor, stringColor : Color;const msg : String);
  procedure ResizeGraphicsWindow(newWidth, newHeight : LongInt);
  procedure RefreshScreen(screen : BitmapPtr);
  procedure ColorComponents(c : Color; var r, g, b, a : Byte);
  function ColorFrom(bmp : BitmapPtr; r, g, b, a: Byte)  : Color;
  function GetScreenWidth(): LongInt; 
  function GetScreenHeight(): LongInt; 
  function AvailableResolutions(): ResolutionArray;
  procedure DrawQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;
  procedure FillQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;

implementation
  uses sgDriverSDL2Types, sgShared, sgGeometry;

  function RGBAColor(r, g, b, a: Byte)  : Color;
  begin
    //TODO: standardise and remove from drivers
    result := a shl 24 or r shl 16 or g shl 8 or b ;
  end;

	function GetPixel32 (bmp: BitmapPtr; x, y: Single) : Color;
  var
    clr: sg_color;
	begin
    clr := _sg_functions^.graphics.read_pixel(bmp^.surface, Round(x), Round(y));
    result := RGBAColor(Round(clr.r * 255), Round(clr.g * 255), Round(clr.b * 255), Round(clr.a * 255));
	end;
	
  procedure FillTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions);
  var
    pts: array [0..6] of Single;
  begin
    XYFromOpts(opts, x1, y1);
    XYFromOpts(opts, x2, y2);
    XYFromOpts(opts, x3, y3);

    pts[0] := x1;
    pts[1] := y1;
    pts[2] := x2;
    pts[3] := y2;
    pts[4] := x3;
    pts[5] := y3;

    _sg_functions^.graphics.fill_triangle(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 6);
  end;

  procedure DrawTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions);
  var
    pts: array [0..6] of Single;
  begin
    XYFromOpts(opts, x1, y1);
    XYFromOpts(opts, x2, y2);
    XYFromOpts(opts, x3, y3);

    pts[0] := x1;
    pts[1] := y1;
    pts[2] := x2;
    pts[3] := y2;
    pts[4] := x3;
    pts[5] := y3;

    _sg_functions^.graphics.draw_triangle(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 6);
  end;
  
  procedure FillCircle(clr: Color; xc, yc, radius: Single; const opts : DrawingOptions); 
  var
    pts: array [0..3] of Single;
  begin
    XYFromOpts(opts, xc, yc);

    pts[0] := xc;
    pts[1] := yc;
    pts[2] := radius;

    _sg_functions^.graphics.fill_circle(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 3);
  end;

  procedure DrawCircle(clr: Color; xc, yc, radius: Single; const opts : DrawingOptions); 
  var
    pts: array [0..3] of Single;
  begin
    XYFromOpts(opts, xc, yc);

    pts[0] := xc;
    pts[1] := yc;
    pts[2] := radius;

    _sg_functions^.graphics.draw_circle(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 3);
  end;
	
	procedure FillEllipse (clr: Color; x, y, width, height: Single; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
	begin
    pts[0] := x;
    pts[1] := y;
    pts[2] := width;
    pts[3] := height;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.fill_ellipse(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 4);
	end;
	
	procedure DrawEllipse (clr: Color; x, y, width, height: Single; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := x;
    pts[1] := y;
    pts[2] := width;
    pts[3] := height;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.draw_ellipse(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 4);
  end;
	
  procedure DrawQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;
  var
    pts: array [0..7] of Single;
    i: Integer;
  begin
    for i := 0 to 3 do
    begin
      pts[i * 2] := q.points[i].x;
      pts[i * 2 + 1] := q.points[i].y;
      XYFromOpts(opts, pts[i * 2], pts[i * 2 + 1]);
    end;

    _sg_functions^.graphics.draw_rect(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 8);
  end;

  procedure FillQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;
  var
    pts: array [0..7] of Single;
    i: Integer;
  begin
    for i := 0 to 3 do
    begin
      pts[i * 2] := q.points[i].x;
      pts[i * 2 + 1] := q.points[i].y;
      XYFromOpts(opts, pts[i * 2], pts[i * 2 + 1]);
    end;

    _sg_functions^.graphics.fill_rect(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 8);
  end;

	procedure FillRectangle (clr : Color; rect : Rectangle; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := rect.x;
    pts[1] := rect.y;
    pts[2] := rect.width;
    pts[3] := rect.height;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.fill_aabb_rect(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 4);
	end;

  procedure DrawRectangle (clr : Color; rect : Rectangle; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := rect.x;
    pts[1] := rect.y;
    pts[2] := rect.width;
    pts[3] := rect.height;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.draw_aabb_rect(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 4);
  end;
	
	procedure DrawLine(clr : Color; x1, y1, x2, y2 : Single; const opts : DrawingOptions);
  var
    pts: array [0..5] of Single;
  begin
    pts[0] := x1;
    pts[1] := y1;
    pts[2] := x2;
    pts[3] := y2;
    pts[4] := opts.lineWidth;

    XYFromOpts(opts, pts[0], pts[1]);
    XYFromOpts(opts, pts[2], pts[3]);

    _sg_functions^.graphics.draw_line(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 5);
  end;
	
	procedure DrawPixel(clr : Color; x, y : Single; const opts: DrawingOptions);
  var
    pts: array [0..2] of Single;
  begin
    pts[0] := x;
    pts[1] := y;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.draw_pixel(ToBitmapPtr(opts.dest)^.surface, _ToSGColor(clr), @pts[0], 2);
	end;
  
  procedure SetClipRectangle(dest : BitmapPtr; rect : Rectangle);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := rect.x;
    pts[1] := rect.y;
    pts[2] := rect.width;
    pts[3] := rect.height;

    _sg_functions^.graphics.set_clip_rect(dest^.surface, @pts[0], 4);
  end;
  
  procedure ResetClip(dest : BitmapPtr);
  begin
    _sg_functions^.graphics.clear_clip_rect(dest^.surface);
  end;

  procedure SetVideoModeFullScreen();
  var
    val: Longint = 0;
  begin
    _wind_fullscreen := not _wind_fullscreen;
    if _wind_fullscreen then val := -1;

    _sg_functions^.graphics.show_fullscreen(@wind, val);
  end;

  procedure SetVideoModeNoFrame();
  var
    val: Longint = 0;
  begin
    _wind_border := not _wind_border;
    if _wind_border then val := -1;
    _sg_functions^.graphics.show_border(@wind, val);
  end;
	
  procedure InitializeGraphicsWindow(const caption : String; screenWidth, screenHeight : LongInt);
  begin
    wind := _sg_functions^.graphics.open_window(PChar(caption), screenWidth, screenHeight);
    wind_open := true;

    // Allocate space for the screen variable - TODO: move this out of here!
    New(screen);
    screen^.id := BITMAP_PTR;
    screen^.surface := @wind;
    
    screenRect    := RectangleFrom(0,0, screenWidth, screenHeight);
    screen^.width := screenWidth;
    screen^.height := screenHeight;

    //TODO: remove dependency on this global variable
    _screen := @wind;
  end;

  procedure InitializeScreen( screen: BitmapPtr; x, y : LongInt; bgColor, stringColor : Color;const msg : String);
  var
    clr: sg_color;
  begin
    clr := _ToSGColor(bgColor);
    _sg_functions^.graphics.clear_drawing_surface(psg_drawing_surface(screen^.surface), clr);

    clr := _ToSGColor(stringColor);
    _sg_functions^.text.draw_text( psg_drawing_surface(screen^.surface), nil, x - 30, y, PChar(msg), clr);
  end;
  
  procedure ResizeGraphicsWindow(newWidth, newHeight : LongInt);
  begin
    _sg_functions^.graphics.resize(@wind, newWidth, newHeight);
    screenRect.width := newWidth;
    screenRect.height := newHeight;
    screen^.width := newWidth;
    screen^.height := newHeight;
  end;
  
  procedure RefreshScreen(screen : BitmapPtr);
  begin
    _sg_functions^.graphics.refresh_window(psg_drawing_surface(screen^.surface));
  end;
  
  procedure ColorComponents(c : Color; var r, g, b, a : Byte);
  begin
    //TODO: standardise and remove from drivers
    a := c and $FF000000 shr 24;
    r := c and $00FF0000 shr 16;
    g := c and $0000FF00 shr 8;
    b := c and $000000FF;
  end;
  
  function ColorFrom(bmp : BitmapPtr; r, g, b, a: Byte)  : Color;
  begin
    //TODO: standardise and remove from drivers
    result := a shl 24 or r shl 16 or g shl 8 or b ;
  end;
  
  function GetScreenWidth(): LongInt; 
  begin
    result := Round(screenRect.width);
  end;
  
  function GetScreenHeight(): LongInt; 
  begin
    result := Round(screenRect.height);
  end;

  function AvailableResolutions(): ResolutionArray;
  var
    sysData: ^sg_system_data;
  begin
    sysData := _sg_functions^.read_system_data();

    SetLength(result, 0);

    if sysData^.num_displays > 0 then
    begin
      // Just read display 0
      // TODO: think about how to handle multiple displays
      // TODO: add in other possible modes
      // TODO: check why refresh reate is always 0

      SetLength(result, 1);
      result[0].width := sysData^.displays^.width;
      result[0].height := sysData^.displays^.height;
      result[0].format := sysData^.displays^.format;
      result[0].refreshRate := sysData^.displays^.refresh_rate;
    end;    
  end;

	procedure LoadGraphicsDriver();
	begin
	end;
end.
	
	