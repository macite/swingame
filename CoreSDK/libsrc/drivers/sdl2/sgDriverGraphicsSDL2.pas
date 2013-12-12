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
	uses sgTypes;

interface
		
  procedure LoadSDL2GraphicsDriver();

implementation
    
	function GetPixel32Procedure (bmp: Bitmap; x, y: Longint) : Color;
	begin
		LoadGraphicsDriver();
		result := GraphicsDriver.GetPixel32(bmp, x, y);
	end;
	
	procedure PutPixelProcedure (bmp: Bitmap; clr: Color; x, y: Longint);
	begin
		LoadGraphicsDriver();
		GraphicsDriver.PutPixel(bmp, clr, x, y);
	end;

  procedure FillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
  	LoadGraphicsDriver();
  	GraphicsDriver.FillTriangle(dest, clr, x1, y1, x2, y2, x3, y3);
  end;

  procedure DrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
  	LoadGraphicsDriver();
  	GraphicsDriver.DrawTriangle(dest, clr, x1, y1, x2, y2, x3, y3);
  end;
  
  procedure FillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
  begin
  	LoadGraphicsDriver();
  	GraphicsDriver.FillCircle(dest, clr, xc, yc, radius);
  end;

  procedure DrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
  begin
  	LoadGraphicsDriver();
  	GraphicsDriver.DrawCircle(dest, clr, xc, yc, radius);
  end;
	
	procedure FillEllipseProcedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		LoadGraphicsDriver();
		GraphicsDriver.FillEllipse(dest, clr,  xPos, yPos, halfWidth, halfHeight);
	end;
	
	procedure DrawEllipseProcedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		LoadGraphicsDriver();
		GraphicsDriver.DrawEllipse(dest, clr,  xPos, yPos, halfWidth, halfHeight);
	end;
	
	procedure FillRectangleProcedure (dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		LoadGraphicsDriver();
		GraphicsDriver.FillRectangle(dest, rect, clr);
	end;
	
	procedure DrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : Longint; clr : Color);
	begin
		LoadGraphicsDriver();
		GraphicsDriver.DrawLine(dest, x1, y1, x2, y2, clr);
	end;
	
	procedure SetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
	begin
		LoadGraphicsDriver();
		GraphicsDriver.SetPixelColor(dest, x, y, clr);
	end;
  
  procedure DrawRectangleProcedure (dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		LoadGraphicsDriver();
		GraphicsDriver.DrawRectangle(dest, rect, clr);
	end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  begin
    GraphicsDriver.SetClipRectangle(dest, rect);
  end;
  
  procedure ResetClipProcedure(dest : Bitmap);
  begin
    GraphicsDriver.ResetClip(dest);
  end;

  procedure SetVideoModeFullScreenProcedure();
  begin
    GraphicsDriver.SetVideoModeFullScreen();
  end;

  procedure SetVideoModeNoFrameProcedure();
  begin
    GraphicsDriver.SetVideoModeNoFrame();
  end;
	
  procedure InitializeGraphicsWindowProcedure(caption : String; screenWidth, screenHeight : LongInt);
  begin
    GraphicsDriver.InitializeGraphicsWindow(caption, screenWidth, screenHeight);
  end;

  procedure InitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, stringColor : Color; msg : String);
  begin
    GraphicsDriver.InitializeScreen( screen, width, height, bgColor, stringColor, msg);
  end;
  
  procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
    GraphicsDriver.ResizeGraphicsWindow(newWidth, newHeight);
  end;
  
  function SaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
    result := GraphicsDriver.SaveImage(bmptoSave, path);
  end;
  
  procedure RefreshScreenProcedure(screen : Bitmap);
  begin
    GraphicsDriver.RefreshScreen(screen);
  end;
  
  procedure ColorComponentsProcedure(c : Color; var r, g, b, a : Byte);
  begin
    GraphicsDriver.ColorComponents(c, r, g, b, a);
  end;
  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a: Byte)  : Color;
  begin
    result := GraphicsDriver.ColorFrom(bmp, r, g, b, a);
  end;
  
  function RGBAColorProcedure(r, g, b, a: Byte)  : Color;
  begin
    result := GraphicsDriver.RGBAColor(r, g, b, a);
  end;

  function GetSurfaceWidthProcedure(src : Bitmap)  : LongInt;
  begin
    result := GraphicsDriver.GetSurfaceWidth(src);
  end;

  function GetSurfaceHeightProcedure(src : Bitmap)  : LongInt;
  begin
    result := GraphicsDriver.GetSurfaceHeight(src);
  end;
  
  // function ToGfxColorProcedure(val : Color): Color; 
  // begin
  //   LoadGraphicsDriver();
  //   result := GraphicsDriver.ToGfxColor(val);
  // end;
  // 
  procedure GetRGBProcedure(pixel : Byte ; r,g,b : Byte);
  begin
    GraphicsDriver.GetRGB(pixel,r,g,b);
  end;
  
  function SurfaceFormatAssignedProcedure(bmp : Bitmap) : Boolean; 
  begin
    result := GraphicsDriver.SurfaceFormatAssigned(bmp);
  end;
  
  function GetScreenWidthProcedure(): LongInt; 
  begin
    result := GraphicsDriver.GetScreenWidth();
  end;
  
  function GetScreenHeightProcedure(): LongInt; 
  begin
    result := GraphicsDriver.GetScreenHeight();
  end;

	procedure LoadSDL2GraphicsDriver();
	begin
		GraphicsDriver.GetPixel32               := @GetPixel32Procedure;
  	GraphicsDriver.PutPixel                 := @PutPixelProcedure;		
		GraphicsDriver.FillTriangle             := @FillTriangleProcedure;
		GraphicsDriver.DrawTriangle             := @DrawTriangleProcedure;		
		GraphicsDriver.FillCircle               := @FillCircleProcedure;
		GraphicsDriver.DrawCircle               := @DrawCircleProcedure;				
		GraphicsDriver.FillEllipse              := @FillEllipseProcedure;
		GraphicsDriver.DrawEllipse              := @DrawEllipseProcedure;
		GraphicsDriver.FillRectangle            := @FillRectangleProcedure;
		GraphicsDriver.DrawLine                 := @DrawLineProcedure;
		GraphicsDriver.SetPixelColor            := @SetPixelColorProcedure;
    GraphicsDriver.DrawRectangle            := @DrawRectangleProcedure;
    GraphicsDriver.SetClipRectangle         := @SetClipRectangleProcedure;
    GraphicsDriver.ResetClip                := @ResetClipProcedure;
    GraphicsDriver.SetVideoModeFullScreen   := @SetVideoModeFullScreenProcedure;
    GraphicsDriver.SetVideoModeNoFrame      := @SetVideoModeNoFrameProcedure;
    GraphicsDriver.InitializeGraphicsWindow := @InitializeGraphicsWindowProcedure;
    GraphicsDriver.InitializeScreen         := @InitializeScreenProcedure;
    GraphicsDriver.ResizeGraphicsWindow     := @ResizeGraphicsWindowProcedure;
    GraphicsDriver.SaveImage                := @SaveImageProcedure;
    GraphicsDriver.RefreshScreen            := @RefreshScreenProcedure;
    GraphicsDriver.ColorComponents          := @ColorComponentsProcedure;
    GraphicsDriver.ColorFrom                := @ColorFromProcedure;
    GraphicsDriver.RGBAColor                := @RGBAColorProcedure;
    GraphicsDriver.GetSurfaceWidth          := @GetSurfaceWidthProcedure;
    GraphicsDriver.GetSurfaceHeight         := @GetSurfaceHeightProcedure;
    // GraphicsDriver.ToGfxColor               := @ToGfxColorProcedure;
    GraphicsDriver.GetRGB                   := @GetRGBProcedure;
    GraphicsDriver.SurfaceFormatAssigned    := @SurfaceFormatAssignedProcedure;
    GraphicsDriver.GetScreenWidth           := @GetScreenWidthProcedure;
    GraphicsDriver.GetScreenHeight          := @GetScreenHeightProcedure;
	end;
end.
	
	