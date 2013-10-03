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
	
	type
	  GetPixel32Procedure                   = function (bmp: Bitmap; x, y: Longint) : Color;
    PutPixelProcedure                     = procedure (bmp: Bitmap; clr: Color; x, y: Longint);      
    FillTriangleProcedure                 = procedure (dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);  
    DrawTriangleProcedure                 = procedure (dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);      
    FillCircleProcedure                   = procedure (dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
    DrawCircleProcedure                   = procedure (dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);      
	  FillEllipseProcedure                  = procedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	  DrawEllipseProcedure                  = procedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
		FillRectangleProcedure                = procedure (dest : Bitmap; rect : Rectangle; clr : Color);
    DrawRectangleProcedure                = procedure (dest : Bitmap; rect : Rectangle; clr : Color);
		DrawLineProcedure                     = procedure (dest : Bitmap; x1, y1, x2, y2 : Longint; clr : Color);
		SetPixelColorProcedure                = procedure (dest : Bitmap; x, y : Integer; clr : Color);
    SetClipRectangleProcedure             = procedure (dest : Bitmap; rect : Rectangle);      
    ResetClipProcedure                    = procedure (bmp: Bitmap);  
    SetVideoModeFullScreenProcedure       = procedure ();
    SetVideoModeNoFrameProcedure          = procedure ();      
    InitializeGraphicsWindowProcedure     = procedure (caption : String; screenWidth, screenHeight : LongInt);
    InitializeScreenProcedure             = procedure ( screen: Bitmap; width, height : LongInt; bgColor, stringColor : Color; msg : String);      
    ResizeGraphicsWindowProcedure         = procedure (newWidth, newHeight : LongInt);
    SaveImageProcedure                    = function  (bmpToSave : Bitmap; path : String) : Boolean;
    RefreshScreenProcedure                = procedure (screen : Bitmap);
    ColorComponentsProcedure              = procedure (c : Color; var r, g, b, a : Byte); 
    ColorFromProcedure                    = function  (bmp : Bitmap; r, g, b, a: Byte)  : Color;
    RGBAColorProcedure                    = function  (r, g, b, a: Byte)  : Color;
    GetSurfaceWidthProcedure              = function  (src : Bitmap) : LongInt;
    GetSurfaceHeightProcedure             = function  (src : Bitmap) : LongInt;
    // ToGfxColorProcedure                   = function(val: Color): Color;
    GetRGBProcedure                       = procedure (pixel : Byte; r,g,b : Byte);
    SurfaceFormatAssignedProcedure        = function ( bmp : Bitmap) : Boolean;
    GetScreenWidthProcedure               = function():LongInt;
    GetScreenHeightProcedure              = function():LongInt;
  	AvaialbleResolutionsProcedure         = function (): ResolutionArray;

	GraphicsDriverRecord = record
	  GetPixel32                : GetPixel32Procedure;
	  PutPixel                  : PutPixelProcedure;	  
	  FillTriangle              : FillTriangleProcedure;
	  DrawTriangle              : DrawTriangleProcedure;	  
	  FillCircle                : FillCircleProcedure;
	  DrawCircle                : DrawCircleProcedure;	  
    FillEllipse               : FillEllipseProcedure;
	  DrawEllipse               : DrawEllipseProcedure;
	  FillRectangle             : FillRectangleProcedure;
		DrawLine                  : DrawLineProcedure;
		SetPixelColor             : SetPixelColorProcedure;
    DrawRectangle             : DrawRectangleProcedure;
    SetClipRectangle          : SetClipRectangleProcedure;
    ResetClip                 : ResetClipProcedure;
    SetVideoModeFullScreen    : SetVideoModeFullScreenProcedure;
    SetVideoModeNoFrame       : SetVideoModeNoFrameProcedure;
    InitializeGraphicsWindow  : InitializeGraphicsWindowProcedure;
    InitializeScreen          : InitializeScreenProcedure;
    ResizeGraphicsWindow      : ResizeGraphicsWindowProcedure;
    SaveImage                 : SaveImageProcedure;
    RefreshScreen             : RefreshScreenProcedure;
    ColorComponents           : ColorComponentsProcedure;
    ColorFrom                 : ColorFromProcedure;
    RGBAColor                 : RGBAColorProcedure;
    GetSurfaceWidth           : GetSurfaceWidthProcedure;
    GetSurfaceHeight          : GetSurfaceHeightProcedure;
    // ToGfxColor                : ToGfxColorProcedure;
    GetRGB                    : GetRGBProcedure;
    SurfaceFormatAssigned     : SurfaceFormatAssignedProcedure;
    GetScreenWidth            : GetScreenWidthProcedure;
    GetScreenHeight           : GetScreenHeightProcedure;
    AvailableResolutions      : AvaialbleResolutionsProcedure;
	end;
	
	var
		GraphicsDriver : GraphicsDriverRecord;
		
implementation
uses 
   
  {$IFDEF SWINGAME_OPENGL}
    sgDriverGraphicsOpenGL;
  {$ELSE}
      {$IFDEF SWINGAME_SDL13}
        sgDriverGraphicsSDL13;
      {$ELSE}
        sgDriverGraphicsSDL;
      {$ENDIF}
  {$ENDIF}
    
	procedure LoadDefaultGraphicsDriver();
	begin
    {$IFDEF SWINGAME_OPENGL}
      {$INFO Using OpenGL Driver}
      LoadOpenGLGraphicsDriver();
		{$ELSE}
      {$IFDEF SWINGAME_SDL13}
        {$INFO Using SDL 2 Driver}
        LoadSDL13GraphicsDriver();  
      {$ELSE}
        {$INFO Using SDL 1.2 Driver}
		    LoadSDLGraphicsDriver();
      {$ENDIF}
		{$ENDIF}
	end;
	
	function DefaultGetPixel32Procedure (bmp: Bitmap; x, y: Longint) : Color;
	begin
		LoadDefaultGraphicsDriver();
		result := GraphicsDriver.GetPixel32(bmp, x, y);
	end;
	
	procedure DefaultPutPixelProcedure (bmp: Bitmap; clr: Color; x, y: Longint);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.PutPixel(bmp, clr, x, y);
	end;

  procedure DefaultFillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
  	LoadDefaultGraphicsDriver();
  	GraphicsDriver.FillTriangle(dest, clr, x1, y1, x2, y2, x3, y3);
  end;

  procedure DefaultDrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
  	LoadDefaultGraphicsDriver();
  	GraphicsDriver.DrawTriangle(dest, clr, x1, y1, x2, y2, x3, y3);
  end;
  
  procedure DefaultFillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
  begin
  	LoadDefaultGraphicsDriver();
  	GraphicsDriver.FillCircle(dest, clr, xc, yc, radius);
  end;

  procedure DefaultDrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); 
  begin
  	LoadDefaultGraphicsDriver();
  	GraphicsDriver.DrawCircle(dest, clr, xc, yc, radius);
  end;
	
	procedure DefaultFillEllipseProcedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.FillEllipse(dest, clr,  xPos, yPos, halfWidth, halfHeight);
	end;
	
	procedure DefaultDrawEllipseProcedure (dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.DrawEllipse(dest, clr,  xPos, yPos, halfWidth, halfHeight);
	end;
	
	procedure DefaultFillRectangleProcedure (dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.FillRectangle(dest, rect, clr);
	end;
	
	procedure DefaultDrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : Longint; clr : Color);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.DrawLine(dest, x1, y1, x2, y2, clr);
	end;
	
	procedure DefaultSetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.SetPixelColor(dest, x, y, clr);
	end;
  
  procedure DefaultDrawRectangleProcedure (dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		LoadDefaultGraphicsDriver();
		GraphicsDriver.DrawRectangle(dest, rect, clr);
	end;
  
  procedure DefaultSetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.SetClipRectangle(dest, rect);
  end;
  
  procedure DefaultResetClipProcedure(dest : Bitmap);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.ResetClip(dest);
  end;

  procedure DefaultSetVideoModeFullScreenProcedure();
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.SetVideoModeFullScreen();
  end;

  procedure DefaultSetVideoModeNoFrameProcedure();
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.SetVideoModeNoFrame();
  end;
	
  procedure DefaultInitializeGraphicsWindowProcedure(caption : String; screenWidth, screenHeight : LongInt);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.InitializeGraphicsWindow(caption, screenWidth, screenHeight);
  end;

  procedure DefaultInitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, stringColor : Color; msg : String);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.InitializeScreen( screen, width, height, bgColor, stringColor, msg);
  end;
  
  procedure DefaultResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.ResizeGraphicsWindow(newWidth, newHeight);
  end;
  
  function DefaultSaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.SaveImage(bmptoSave, path);
  end;
  
  procedure DefaultRefreshScreenProcedure(screen : Bitmap);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.RefreshScreen(screen);
  end;
  
  procedure DefaultColorComponentsProcedure(c : Color; var r, g, b, a : Byte);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.ColorComponents(c, r, g, b, a);
  end;
  
  function DefaultColorFromProcedure(bmp : Bitmap; r, g, b, a: Byte)  : Color;
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.ColorFrom(bmp, r, g, b, a);
  end;
  
  function DefaultRGBAColorProcedure(r, g, b, a: Byte)  : Color;
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.RGBAColor(r, g, b, a);
  end;

  function DefaultGetSurfaceWidthProcedure(src : Bitmap)  : LongInt;
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.GetSurfaceWidth(src);
  end;

  function DefaultGetSurfaceHeightProcedure(src : Bitmap)  : LongInt;
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.GetSurfaceHeight(src);
  end;
  
  // function DefaultToGfxColorProcedure(val : Color): Color; 
  // begin
  //   LoadDefaultGraphicsDriver();
  //   result := GraphicsDriver.ToGfxColor(val);
  // end;
  // 
  procedure DefaultGetRGBProcedure(pixel : Byte ; r,g,b : Byte);
  begin
    LoadDefaultGraphicsDriver();
    GraphicsDriver.GetRGB(pixel,r,g,b);
  end;
  
  function DefaultSurfaceFormatAssignedProcedure(bmp : Bitmap) : Boolean; 
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.SurfaceFormatAssigned(bmp);
  end;
  
  function DefaultGetScreenWidthProcedure(): LongInt; 
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.GetScreenWidth();
  end;
  
  function DefaultGetScreenHeightProcedure(): LongInt; 
  begin
    LoadDefaultGraphicsDriver();
    result := GraphicsDriver.GetScreenHeight();
  end;

	initialization
	begin
		GraphicsDriver.GetPixel32               := @DefaultGetPixel32Procedure;
  	GraphicsDriver.PutPixel                 := @DefaultPutPixelProcedure;		
		GraphicsDriver.FillTriangle             := @DefaultFillTriangleProcedure;
		GraphicsDriver.DrawTriangle             := @DefaultDrawTriangleProcedure;		
		GraphicsDriver.FillCircle               := @DefaultFillCircleProcedure;
		GraphicsDriver.DrawCircle               := @DefaultDrawCircleProcedure;				
		GraphicsDriver.FillEllipse              := @DefaultFillEllipseProcedure;
		GraphicsDriver.DrawEllipse              := @DefaultDrawEllipseProcedure;
		GraphicsDriver.FillRectangle            := @DefaultFillRectangleProcedure;
		GraphicsDriver.DrawLine                 := @DefaultDrawLineProcedure;
		GraphicsDriver.SetPixelColor            := @DefaultSetPixelColorProcedure;
    GraphicsDriver.DrawRectangle            := @DefaultDrawRectangleProcedure;
    GraphicsDriver.SetClipRectangle         := @DefaultSetClipRectangleProcedure;
    GraphicsDriver.ResetClip                := @DefaultResetClipProcedure;
    GraphicsDriver.SetVideoModeFullScreen   := @DefaultSetVideoModeFullScreenProcedure;
    GraphicsDriver.SetVideoModeNoFrame      := @DefaultSetVideoModeNoFrameProcedure;
    GraphicsDriver.InitializeGraphicsWindow := @DefaultInitializeGraphicsWindowProcedure;
    GraphicsDriver.InitializeScreen         := @DefaultInitializeScreenProcedure;
    GraphicsDriver.ResizeGraphicsWindow     := @DefaultResizeGraphicsWindowProcedure;
    GraphicsDriver.SaveImage                := @DefaultSaveImageProcedure;
    GraphicsDriver.RefreshScreen            := @DefaultRefreshScreenProcedure;
    GraphicsDriver.ColorComponents          := @DefaultColorComponentsProcedure;
    GraphicsDriver.ColorFrom                := @DefaultColorFromProcedure;
    GraphicsDriver.RGBAColor                := @DefaultRGBAColorProcedure;
    GraphicsDriver.GetSurfaceWidth          := @DefaultGetSurfaceWidthProcedure;
    GraphicsDriver.GetSurfaceHeight         := @DefaultGetSurfaceHeightProcedure;
    // GraphicsDriver.ToGfxColor               := @DefaultToGfxColorProcedure;
    GraphicsDriver.GetRGB                   := @DefaultGetRGBProcedure;
    GraphicsDriver.SurfaceFormatAssigned    := @DefaultSurfaceFormatAssignedProcedure;
    GraphicsDriver.GetScreenWidth           := @DefaultGetScreenWidthProcedure;
    GraphicsDriver.GetScreenHeight          := @DefaultGetScreenHeightProcedure;
	end;
end.
	
	