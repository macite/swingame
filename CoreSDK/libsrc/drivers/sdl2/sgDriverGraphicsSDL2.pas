
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

    
	function GetPixel32Procedure (bmp: Bitmap; x, y: Longint) : Color;
	begin
    result := 0;
	end;
	
	procedure PutPixelProcedure (bmp: Bitmap; clr: Color; x, y: Longint);
	begin
	end;

  procedure FillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
  end;

  procedure DrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
  end;
  
  procedure FillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
  begin
  end;

  procedure DrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
  begin
  end;
	
	procedure FillEllipseProcedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
	end;
	
	procedure DrawEllipseProcedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
	end;
	
	procedure FillRectangleProcedure (dest : Bitmap; rect : Rectangle; clr : Color);
	begin
	end;
	
	procedure DrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : Longint; clr : Color);
	begin
	end;
	
	procedure SetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
	begin
	end;
  
  procedure DrawRectangleProcedure (dest : Bitmap; rect : Rectangle; clr : Color);
	begin
	end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  begin
  end;
  
  procedure ResetClipProcedure(dest : Bitmap);
  begin
  end;

  procedure SetVideoModeFullScreenProcedure();
  begin
  end;

  procedure SetVideoModeNoFrameProcedure();
  begin
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

    _screen := @wind;
  end;

  procedure InitializeScreenProcedure( screen: Bitmap; x, y : LongInt; bgColor, stringColor : Color; msg : String);
  var
    clr: sg_color;
  begin
    clr.a := ((bgColor and $ff000000) shr 24) / 255.0;
    clr.r := ((bgColor and $00ff0000) shr 16) / 255.0;
    clr.g := ((bgColor and $0000ff00) shr  8) / 255.0;
    clr.b := ((bgColor and $000000ff)       ) / 255.0;

    _sg_functions^.graphics.clear_drawing_surface(psg_drawing_surface(screen^.surface), clr);

    clr.a := ((stringColor and $ff000000) shr 24) / 255.0;
    clr.r := ((stringColor and $00ff0000) shr 16) / 255.0;
    clr.g := ((stringColor and $0000ff00) shr  8) / 255.0;
    clr.b := ((stringColor and $000000ff)       ) / 255.0;

    _sg_functions^.text.draw_text( psg_drawing_surface(screen^.surface), nil, x - 30, y, PChar(msg), clr);
  end;
  
  procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
  end;
  
  function SaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
    result := false;
  end;
  
  procedure RefreshScreenProcedure(screen : Bitmap);
  begin
    _sg_functions^.graphics.refresh_window(psg_drawing_surface(screen^.surface));
  end;
  
  procedure ColorComponentsProcedure(c : Color; var r, g, b, a : Byte);
  begin
  end;
  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a: Byte)  : Color;
  begin
    result := a shl 24 or r shl 16 or g shl 8 or b ;
  end;
  
  function RGBAColorProcedure(r, g, b, a: Byte)  : Color;
  begin
    result := a shl 24 or r shl 16 or g shl 8 or b ;
  end;

  function SurfaceFormatAssignedProcedure(bmp : Bitmap) : Boolean; 
  begin
    result := true;
  end;
  
  function GetScreenWidthProcedure(): LongInt; 
  begin
    result := screenRect.width;
  end;
  
  function GetScreenHeightProcedure(): LongInt; 
  begin
    result := screenRect.height;
  end;

  function AvailableResolutionsProcedure(): ResolutionArray;
  begin
    SetLength(result, 0);
  end;


	procedure LoadSDL2GraphicsDriver();
	begin
    GraphicsDriver.GetPixel32               := @GetPixel32Procedure;    // # (done)
    GraphicsDriver.PutPixel                 := @PutPixelProcedure;      // #
    GraphicsDriver.FillTriangle             := @FillTriangleProcedure;  // #
    GraphicsDriver.DrawTriangle             := @DrawTriangleProcedure;  // #    
    GraphicsDriver.FillCircle               := @FillCircleProcedure;    // #
    GraphicsDriver.DrawCircle               := @DrawCircleProcedure;    // #
    GraphicsDriver.FillEllipse              := @FillEllipseProcedure;   // #
    GraphicsDriver.DrawEllipse              := @DrawEllipseProcedure;   // #
    GraphicsDriver.FillRectangle            := @FillRectangleProcedure; // #
    GraphicsDriver.DrawLine                 := @DrawLineProcedure;      // #  
    GraphicsDriver.SetPixelColor            := @SetPixelColorProcedure; // - (not needed)
    GraphicsDriver.DrawRectangle            := @DrawRectangleProcedure; // #
    GraphicsDriver.SetClipRectangle         := @SetClipRectangleProcedure;  // #
    GraphicsDriver.ResetClip                := @ResetClipProcedure;         // #
    GraphicsDriver.SetVideoModeFullScreen   := @SetVideoModeFullScreenProcedure;    // # show_fullscreen  
    GraphicsDriver.SetVideoModeNoFrame      := @SetVideoModeNoFrameProcedure;       // # show_border 
    GraphicsDriver.InitializeGraphicsWindow := @InitializeGraphicsWindowProcedure;  // #
    GraphicsDriver.InitializeScreen         := @InitializeScreenProcedure;          // #
    GraphicsDriver.ResizeGraphicsWindow     := @ResizeGraphicsWindowProcedure;      // #
    GraphicsDriver.SaveImage                := @SaveImageProcedure;             // - use to_pixels
    GraphicsDriver.RefreshScreen            := @RefreshScreenProcedure;         // #
    GraphicsDriver.ColorComponents          := @ColorComponentsProcedure;       // -
    GraphicsDriver.ColorFrom                := @ColorFromProcedure;             // -
    GraphicsDriver.RGBAColor                := @RGBAColorProcedure;             // -
    GraphicsDriver.SurfaceFormatAssigned    := @SurfaceFormatAssignedProcedure; // -
    GraphicsDriver.GetScreenWidth           := @GetScreenWidthProcedure;        // -
    GraphicsDriver.GetScreenHeight          := @GetScreenHeightProcedure;       // -
    GraphicsDriver.AvailableResolutions     := @AvailableResolutionsProcedure;  // # 
	end;
end.
	
	