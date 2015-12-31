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
	uses sgTypes, sgBackendTypes, sgDriverSDL2Types;

  function RGBAColor(r, g, b, a: Byte)  : Color;
  procedure ColorComponents(c : Color; var r, g, b, a : Byte);

  function GetPixel (src: psg_drawing_surface; x, y: Single) : Color;

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
  procedure DrawQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;
  procedure FillQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;

  procedure SetClipRectangle(dest : psg_drawing_surface; rect : Rectangle);
  procedure ResetClip(dest : psg_drawing_surface);

  function OpenWindow(const caption : String; screenWidth, screenHeight : LongInt): WindowPtr;
  procedure CloseWindow(var wind: WindowPtr);
  procedure ResizeWindow(wind: WindowPtr; newWidth, newHeight : LongInt);
  procedure RefreshWindow(window : WindowPtr);
  procedure SetVideoModeFullScreen(wind: WindowPtr);
  procedure SetVideoModeNoFrame(wind: WindowPtr);

  function AvailableResolutions(): ResolutionArray;

implementation
  uses sgShared, sgGeometry;

  function RGBAColor(r, g, b, a: Byte)  : Color;
  begin
    //TODO: standardise and remove from drivers
    result := a shl 24 or r shl 16 or g shl 8 or b ;
  end;

  procedure ColorComponents(c : Color; var r, g, b, a : Byte);
  begin
    //TODO: standardise and remove from drivers
    a := c and $FF000000 shr 24;
    r := c and $00FF0000 shr 16;
    g := c and $0000FF00 shr 8;
    b := c and $000000FF;
  end;

	function GetPixel (src: psg_drawing_surface; x, y: Single) : Color;
  var
    clr: sg_color;
	begin
    clr := _sg_functions^.graphics.read_pixel(src, Round(x), Round(y));
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

    _sg_functions^.graphics.fill_triangle(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 6);
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

    _sg_functions^.graphics.draw_triangle(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 6);
  end;
  
  procedure FillCircle(clr: Color; xc, yc, radius: Single; const opts : DrawingOptions); 
  var
    pts: array [0..3] of Single;
  begin
    XYFromOpts(opts, xc, yc);

    pts[0] := xc;
    pts[1] := yc;
    pts[2] := radius;

    _sg_functions^.graphics.fill_circle(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 3);
  end;

  procedure DrawCircle(clr: Color; xc, yc, radius: Single; const opts : DrawingOptions); 
  var
    pts: array [0..3] of Single;
  begin
    XYFromOpts(opts, xc, yc);

    pts[0] := xc;
    pts[1] := yc;
    pts[2] := radius;

    _sg_functions^.graphics.draw_circle(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 3);
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

    _sg_functions^.graphics.fill_ellipse(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 4);
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

    _sg_functions^.graphics.draw_ellipse(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 4);
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

    _sg_functions^.graphics.draw_rect(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 8);
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

    _sg_functions^.graphics.fill_rect(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 8);
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

    _sg_functions^.graphics.fill_aabb_rect(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 4);
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

    _sg_functions^.graphics.draw_aabb_rect(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 4);
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

    _sg_functions^.graphics.draw_line(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 5);
  end;
	
	procedure DrawPixel(clr : Color; x, y : Single; const opts: DrawingOptions);
  var
    pts: array [0..2] of Single;
  begin
    pts[0] := x;
    pts[1] := y;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.draw_pixel(ToSurfacePtr(opts.dest), _ToSGColor(clr), @pts[0], 2);
	end;
  
  procedure SetClipRectangle(dest : psg_drawing_surface; rect : Rectangle);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := rect.x;
    pts[1] := rect.y;
    pts[2] := rect.width;
    pts[3] := rect.height;

    _sg_functions^.graphics.set_clip_rect(dest, @pts[0], 4);
  end;
  
  procedure ResetClip(dest : psg_drawing_surface);
  begin
    _sg_functions^.graphics.clear_clip_rect(dest);
  end;

  procedure SetVideoModeFullScreen(wind: WindowPtr);
  var
    val: Longint = 0;
  begin
    wind^.fullscreen := not wind^.fullscreen;
    if wind^.fullscreen then val := -1;

    _sg_functions^.graphics.show_fullscreen(@wind^.image.surface, val);
  end;

  procedure SetVideoModeNoFrame(wind: WindowPtr);
  var
    val: Longint = 0;
  begin
    wind^.border := not wind^.border;
    if wind^.border then val := -1;
    _sg_functions^.graphics.show_border(@wind^.image.surface, val);
  end;
	
  function OpenWindow(const caption : String; screenWidth, screenHeight : LongInt): WindowPtr;
  var
    clr: sg_color;
  begin
    New(result);
    result^.id := WINDOW_PTR;
    result^.caption := caption;

    result^.image.surface := _sg_functions^.graphics.open_window(PChar(caption), screenWidth, screenHeight);
    SetLength(result^.image.clipStack, 0);

    _sg_functions^.input.window_position(@result^.image.surface, @result^.x, @result^.y);

    result^.open := true;
    result^.fullscreen := false;
    result^.border := true;

    result^.eventData.close_requested := 0;
    result^.eventData.has_focus := 0;
    result^.eventData.mouse_over := 0;
    result^.eventData.shown := -1;

    result^.screenRect := RectangleFrom(0,0,screenWidth, screenHeight);

    result^.tempString := '';
    result^.maxStringLen := 0;

    result^.textBitmap := nil;
    result^.cursorBitmap := nil;
    result^.font := nil;
    result^.foreColor := RGBAColor(0, 0, 0, 255);
    result^.backgroundColor := RGBAColor(255, 255, 255, 255);
    result^.area := RectangleFrom(0,0,0,0);

    result^.readingString := false;
    result^.textCancelled := false;

    clr := _ToSGColor(RGBAColor(255,255,255,255));
    _sg_functions^.graphics.clear_drawing_surface(@result^.image.surface, clr);

    clr := _ToSGColor(RGBAColor(128,128,128,255));
    _sg_functions^.text.draw_text( @result^.image.surface, nil, screenWidth div 2 - 60, screenHeight div 2, 'Getting ready to make a Splash!', clr);

    RefreshWindow(result);
  end;

  procedure CloseWindow(var wind: WindowPtr);
  begin
    _sg_functions^.graphics.close_drawing_surface(@wind^.image.surface);
    wind^.id := NONE_PTR;
    Dispose(wind);
    wind := nil;
  end;
  
  procedure ResizeWindow(wind: WindowPtr; newWidth, newHeight : LongInt);
  begin
    _sg_functions^.graphics.resize(@wind^.image.surface, newWidth, newHeight);
    wind^.screenRect := RectangleFrom(0, 0, newWidth, newHeight);
  end;
  
  procedure RefreshWindow(window : WindowPtr);
  begin
    _sg_functions^.graphics.refresh_window(@window^.image.surface);
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
end.
	
	