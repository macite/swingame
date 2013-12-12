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
  uses sgDriverSDL2Types, sgDriverGraphics;

  // Currently only a single window... TODO: allow multiple windows
  var
    wind: sg_drawing_surface;

    
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
    wind := _sg_functions^.graphics.open_window('Hello', 800, 600);
  end;

  procedure InitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, stringColor : Color; msg : String);
  begin
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
  end;
  
  procedure ColorComponentsProcedure(c : Color; var r, g, b, a : Byte);
  begin
  end;
  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a: Byte)  : Color;
  begin
    result := 0;
  end;
  
  function RGBAColorProcedure(r, g, b, a: Byte)  : Color;
  begin
    result := 0;
  end;

  function GetSurfaceWidthProcedure(src : Bitmap)  : LongInt;
  begin
    result := 0;
  end;

  function GetSurfaceHeightProcedure(src : Bitmap)  : LongInt;
  begin
    result := 0;
  end;
  
  function SurfaceFormatAssignedProcedure(bmp : Bitmap) : Boolean; 
  begin
    result := false;
  end;
  
  function GetScreenWidthProcedure(): LongInt; 
  begin
    result := 0;
  end;
  
  function GetScreenHeightProcedure(): LongInt; 
  begin
    result := 0;
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
    GraphicsDriver.GetSurfaceWidth          := @GetSurfaceWidthProcedure;       // -
    GraphicsDriver.GetSurfaceHeight         := @GetSurfaceHeightProcedure;      // -
    GraphicsDriver.SurfaceFormatAssigned    := @SurfaceFormatAssignedProcedure; // -
    GraphicsDriver.GetScreenWidth           := @GetScreenWidthProcedure;        // -
    GraphicsDriver.GetScreenHeight          := @GetScreenHeightProcedure;       // -
    GraphicsDriver.AvailableResolutions     := @AvailableResolutionsProcedure;  // # 
	end;
end.
	
	