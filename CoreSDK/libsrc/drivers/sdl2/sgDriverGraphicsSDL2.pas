
unit sgDriverGraphicsSDL2;
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
	uses sgTypes;

  procedure LoadSDL2GraphicsDriver();

implementation
  uses sgDriverSDL2Types, sgDriverGraphics, sgShared, sgGeometry;

  function RGBAColorProcedure(r, g, b, a: Byte)  : Color;
  begin
    //TODO: standardise and remove from drivers
    result := a shl 24 or r shl 16 or g shl 8 or b ;
  end;

	function GetPixel32Procedure (bmp: Bitmap; x, y: Single) : Color;
  var
    clr: sg_color;
	begin
    clr := _sg_functions^.graphics.read_pixel(bmp^.surface, Round(x), Round(y));
    result := RGBAColorProcedure(Round(clr.r * 255), Round(clr.g * 255), Round(clr.b * 255), Round(clr.a * 255));
	end;
	
  procedure FillTriangleProcedure(clr: Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions);
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

    _sg_functions^.graphics.fill_triangle(opts.dest^.surface, _ToSGColor(clr), @pts[0], 6);
  end;

  procedure DrawTriangleProcedure(clr: Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions);
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

    _sg_functions^.graphics.draw_triangle(opts.dest^.surface, _ToSGColor(clr), @pts[0], 6);
  end;
  
  procedure FillCircleProcedure(clr: Color; xc, yc, radius: Single; const opts : DrawingOptions); 
  var
    pts: array [0..3] of Single;
  begin
    XYFromOpts(opts, xc, yc);

    pts[0] := xc;
    pts[1] := yc;
    pts[2] := radius;

    _sg_functions^.graphics.fill_circle(opts.dest^.surface, _ToSGColor(clr), @pts[0], 3);
  end;

  procedure DrawCircleProcedure(clr: Color; xc, yc, radius: Single; const opts : DrawingOptions); 
  var
    pts: array [0..3] of Single;
  begin
    XYFromOpts(opts, xc, yc);

    pts[0] := xc;
    pts[1] := yc;
    pts[2] := radius;

    _sg_functions^.graphics.draw_circle(opts.dest^.surface, _ToSGColor(clr), @pts[0], 3);
  end;
	
	procedure FillEllipseProcedure (clr: Color; x, y, width, height: Single; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
	begin
    pts[0] := x;
    pts[1] := y;
    pts[2] := width;
    pts[3] := height;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.fill_ellipse(opts.dest^.surface, _ToSGColor(clr), @pts[0], 4);
	end;
	
	procedure DrawEllipseProcedure (clr: Color; x, y, width, height: Single; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := x;
    pts[1] := y;
    pts[2] := width;
    pts[3] := height;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.draw_ellipse(opts.dest^.surface, _ToSGColor(clr), @pts[0], 4);
  end;
	
	procedure FillRectangleProcedure (clr : Color; rect : Rectangle; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := rect.x;
    pts[1] := rect.y;
    pts[2] := rect.width;
    pts[3] := rect.height;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.fill_aabb_rect(opts.dest^.surface, _ToSGColor(clr), @pts[0], 4);
	end;

  procedure DrawRectangleProcedure (clr : Color; rect : Rectangle; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := rect.x;
    pts[1] := rect.y;
    pts[2] := rect.width;
    pts[3] := rect.height;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.draw_aabb_rect(opts.dest^.surface, _ToSGColor(clr), @pts[0], 4);
  end;
	
	procedure DrawLineProcedure(clr : Color; x1, y1, x2, y2 : Single; const opts : DrawingOptions);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := x1;
    pts[1] := y1;
    pts[2] := x2;
    pts[3] := y2;

    XYFromOpts(opts, pts[0], pts[1]);
    XYFromOpts(opts, pts[2], pts[3]);

    _sg_functions^.graphics.draw_line(opts.dest^.surface, _ToSGColor(clr), @pts[0], 4);
  end;
	
	procedure DrawPixelProcedure(clr : Color; x, y : Single; const opts: DrawingOptions);
  var
    pts: array [0..2] of Single;
  begin
    pts[0] := x;
    pts[1] := y;

    XYFromOpts(opts, pts[0], pts[1]);

    _sg_functions^.graphics.draw_pixel(opts.dest^.surface, _ToSGColor(clr), @pts[0], 2);
	end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  var
    pts: array [0..4] of Single;
  begin
    pts[0] := rect.x;
    pts[1] := rect.y;
    pts[2] := rect.width;
    pts[3] := rect.height;

    _sg_functions^.graphics.set_clip_rect(dest^.surface, @pts[0], 4);
  end;
  
  procedure ResetClipProcedure(dest : Bitmap);
  begin
    _sg_functions^.graphics.clear_clip_rect(dest^.surface);
  end;

  procedure SetVideoModeFullScreenProcedure();
  var
    val: Longint = 0;
  begin
    _wind_fullscreen := not _wind_fullscreen;
    if _wind_fullscreen then val := -1;

    _sg_functions^.graphics.show_fullscreen(@wind, val);
  end;

  procedure SetVideoModeNoFrameProcedure();
  var
    val: Longint = 0;
  begin
    _wind_border := not _wind_border;
    if _wind_border then val := -1;
    _sg_functions^.graphics.show_border(@wind, val);
  end;
	
  procedure InitializeGraphicsWindowProcedure(caption : String; screenWidth, screenHeight : LongInt);
  begin
    wind := _sg_functions^.graphics.open_window(PChar(caption), screenWidth, screenHeight);
    wind_open := true;

    // Allocate space for the screen variable - TODO: move this out of here!
    New(screen);
    screen^.surface := @wind;
    
    screenRect    := RectangleFrom(0,0, screenWidth, screenHeight);
    screen^.width := screenWidth;
    screen^.height := screenHeight;

    //TODO: remove dependency on this global variable
    _screen := @wind;
  end;

  procedure InitializeScreenProcedure( screen: Bitmap; x, y : LongInt; bgColor, stringColor : Color; msg : String);
  var
    clr: sg_color;
  begin
    clr := _ToSGColor(bgColor);
    _sg_functions^.graphics.clear_drawing_surface(psg_drawing_surface(screen^.surface), clr);

    clr := _ToSGColor(stringColor);
    _sg_functions^.text.draw_text( psg_drawing_surface(screen^.surface), nil, x - 30, y, PChar(msg), clr);
  end;
  
  procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
    _sg_functions^.graphics.resize(@wind, newWidth, newHeight);
    screenRect.width := newWidth;
    screenRect.height := newHeight;
    screen^.width := newWidth;
    screen^.height := newHeight;
  end;
  
  procedure RefreshScreenProcedure(screen : Bitmap);
  begin
    _sg_functions^.graphics.refresh_window(psg_drawing_surface(screen^.surface));
  end;
  
  procedure ColorComponentsProcedure(c : Color; var r, g, b, a : Byte);
  begin
    //TODO: standardise and remove from drivers
    a := c and $FF000000 shr 24;
    r := c and $00FF0000 shr 16;
    g := c and $0000FF00 shr 8;
    b := c and $000000FF;
  end;
  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a: Byte)  : Color;
  begin
    //TODO: standardise and remove from drivers
    result := a shl 24 or r shl 16 or g shl 8 or b ;
  end;
  
  function GetScreenWidthProcedure(): LongInt; 
  begin
    result := Round(screenRect.width);
  end;
  
  function GetScreenHeightProcedure(): LongInt; 
  begin
    result := Round(screenRect.height);
  end;

  function AvailableResolutionsProcedure(): ResolutionArray;
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


	procedure LoadSDL2GraphicsDriver();
	begin
    GraphicsDriver.GetPixel32               := @GetPixel32Procedure;    // # (done)
    GraphicsDriver.FillTriangle             := @FillTriangleProcedure;  // #
    GraphicsDriver.DrawTriangle             := @DrawTriangleProcedure;  // #    
    GraphicsDriver.FillCircle               := @FillCircleProcedure;    // #
    GraphicsDriver.DrawCircle               := @DrawCircleProcedure;    // #
    GraphicsDriver.FillEllipse              := @FillEllipseProcedure;   // #
    GraphicsDriver.DrawEllipse              := @DrawEllipseProcedure;   // #
    GraphicsDriver.FillRectangle            := @FillRectangleProcedure; // #
    GraphicsDriver.DrawLine                 := @DrawLineProcedure;      // #  
    GraphicsDriver.DrawPixel                := @DrawPixelProcedure; // #
    GraphicsDriver.DrawRectangle            := @DrawRectangleProcedure; // #
    GraphicsDriver.SetClipRectangle         := @SetClipRectangleProcedure;  // #
    GraphicsDriver.ResetClip                := @ResetClipProcedure;         // #
    GraphicsDriver.SetVideoModeFullScreen   := @SetVideoModeFullScreenProcedure;    // # show_fullscreen  
    GraphicsDriver.SetVideoModeNoFrame      := @SetVideoModeNoFrameProcedure;       // # show_border 
    GraphicsDriver.InitializeGraphicsWindow := @InitializeGraphicsWindowProcedure;  // #
    GraphicsDriver.InitializeScreen         := @InitializeScreenProcedure;          // #
    GraphicsDriver.ResizeGraphicsWindow     := @ResizeGraphicsWindowProcedure;      // #
    GraphicsDriver.RefreshScreen            := @RefreshScreenProcedure;         // #
    GraphicsDriver.ColorComponents          := @ColorComponentsProcedure;       // -
    GraphicsDriver.ColorFrom                := @ColorFromProcedure;             // -
    GraphicsDriver.RGBAColor                := @RGBAColorProcedure;             // -
    GraphicsDriver.GetScreenWidth           := @GetScreenWidthProcedure;        // -
    GraphicsDriver.GetScreenHeight          := @GetScreenHeightProcedure;       // -
    GraphicsDriver.AvailableResolutions     := @AvailableResolutionsProcedure;  // # 
	end;
end.
	
	