unit sgDriverGraphicsSDL;
//=============================================================================
// sgDriverGraphicsSDL.pas
//=============================================================================
//
//
//
// 
//
// Notes:
//		- Pascal PChar is equivalent to a C-type string
// 		- Pascal Word is equivalent to a Uint16
//		- Pascal LongWord is equivalent to a Uint32
//		- Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
uses sgTypes, SDL;

  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
	
	procedure LoadSDLGraphicsDriver();
	    
  function ToGfxColorProcedure(val : Color): Color;
	
implementation
	uses sgDriverGraphics, sysUtils, sgShared, sgGeometry,
		SDL_gfx, SDL_Image, sgSavePNG, sgInputBackend, sgTrace, sgSDLUtils;
  
	
	
  function GetPixel32Procedure(bmp: Bitmap; x, y: Longint) :Color;
  begin
    result := GetSurfacePixel(bmp^.surface, x, y);
  end;

	// var
 //    pixel, pixels: PUint32;
 //    offset: Longword;
 //  {$IFDEF FPC}
 //    pixelAddress: PUint32;
 //  {$ELSE}
 //    pixelAddress: Longword;
 //  {$ENDIF}
 //    surface : PSDL_Surface;
	// begin
	// 	if not Assigned(bmp) then begin RaiseWarning('SDL1.2 Driver - GetPixel32Procedure recieved empty Bitmap'); exit; end;
		
	//   if (x < 0) or (x >= bmp^.width) or (y < 0) or (y >= bmp^.height) then
 //    begin
 //      result := 0;
 //      exit;
 //    end;  
		 
	// 	surface := bmp^.surface;
		
		
 //    //Convert the pixels to 32 bit
 //    pixels := surface^.pixels;

 //    //Get the requested pixel
 //    offset := (( y * surface^.w ) + x) * surface^.format^.BytesPerPixel;

 //    {$IFDEF FPC}
 //      pixelAddress := pixels + (offset div 4);
 //      pixel := PUint32(pixelAddress);
 //    {$ELSE}
 //      pixelAddress := Longword(pixels) + offset;
 //      pixel := Ptr(pixelAddress);
 //    {$ENDIF}

 //    {$IF SDL_BYTEORDER = SDL_BIG_ENDIAN }
 //    case surface^.format^.BytesPerPixel of
 //      1: result := pixel^ and $000000ff;
 //      2: result := pixel^ and $0000ffff;
 //      3: result := pixel^ and $00ffffff;
 //      4: result := pixel^;
 //    else
 //      RaiseException('Unsuported bit format...');
 //      exit;
 //    end;
 //    {$ELSE}
 //    case surface^.format^.BytesPerPixel of
 //      1: result := pixel^ and $ff000000;
 //      2: result := pixel^ and $ffff0000;
 //      3: result := pixel^ and $ffffff00;
 //      4: result := pixel^;
 //    else
 //      raise Exception.Create('Unsuported bit format...')
 //    end;
 //    {$IFEND}
	// end;
		
  procedure PutPixelProcedure(bmp: Bitmap; clr: Color; x, y: Longint);
  begin
    PutSurfacePixel(bmp^.surface, clr, x, y);
  end;
 //  var
 //      p:    ^Color;
 //      bpp:  Longint;
 //  begin
 //      if not assigned(bmp) then begin RaiseWarning('SDL1.2 Driver - PutPixelProcedure recieved empty Bitmap'); exit; end;
        
 //      bpp := PSDL_Surface(bmp^.surface)^.format^.BytesPerPixel;
 //      // Here p is the address to the pixel we want to set
 //      p := PSDL_Surface(bmp^.surface)^.pixels + y * PSDL_Surface(bmp^.surface)^.pitch + x * bpp;

 //      if bpp <> 4 then RaiseException('PutPixel only supported on 32bit images.');
 //      p^ := clr;
 //  end;
  
  procedure ColorComponentsProcedure(c: Color; var r, g, b, a: byte);
  begin
    SDL_GetRGBA(c, PSDL_surface(screen^.surface)^.Format, @r, @g, @b, @a);
  end;
  
  function ToGfxColorProcedure(val : Color): Color; 
  var
    r, g, b, a: Byte;
  begin
    ColorComponentsProcedure(val, r, g, b, a);
    result := (r shl 24) or (g shl 16) or (b shl 8) or a;
  end;
  
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
  begin
    if w < 0 then
    begin
      x += w;
      w := -w;
    end;
    if h < 0 then
    begin
      y += h;
      h := -h;
    end;
    
    result.x := x;
    result.y := y;
    result.w := Word(w);
    result.h := Word(h);
  end;
  
  
  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  begin
      result := NewSDLRect(Round(r.x), Round(r.y), r.width, r.height);
  end;

  procedure DrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    if not assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawTriangleProcedure recieved empty Bitmap'); exit; end;
    trigonColor(dest^.surface, Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), ToGfxColorProcedure(clr));
  end;

  procedure FillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    if not assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - FillTriangleProcedure recieved empty Bitmap'); exit; end;
    filledTrigonColor(dest^.surface, Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), ToGfxColorProcedure(clr));
  end;
	
  procedure DrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  begin
    if not assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawCircleProcedure recieved empty Bitmap'); exit; end;
    aacircleColor(dest^.surface, Round(xc), Round(yc), Abs(radius), ToGfxColorProcedure(clr));
  end;

  procedure FillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  begin
    if not assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - FillCircleProcedure recieved empty Bitmap'); exit; end;
    filledCircleColor(dest^.surface, Round(xc), Round(yc), Abs(radius), ToGfxColorProcedure(clr));
  end;
  
	// This procedure draws an Ellipse to a bitmap
	procedure FillEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - FillEllipseProcedure recieved empty Bitmap'); exit; end;
		filledEllipseColor(dest^.surface, xPos, yPos, halfWidth, halfHeight, ToGfxColorProcedure(clr));
	end;
	
	// This procedure draws an Ellipse to a bitmap
	procedure DrawEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawEllipseProcedure recieved empty Bitmap'); exit; end;
		aaellipseColor(dest^.surface, xPos, yPos, halfWidth, halfHeight, ToGfxColorProcedure(clr));
	end;
	
	// This procedure draws a filled rectangle to a bitmap
	procedure FillRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - FillRectangleProcedure recieved empty Bitmap'); exit; end;
		boxColor(dest^.surface, 
			RoundInt(rect.x), RoundInt(rect.y), 
			RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), 
			ToGfxColorProcedure(clr));
	end;
	
	procedure DrawRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawRectangleProcedure recieved empty Bitmap'); exit; end;
    rectangleColor(dest^.surface, 
      RoundInt(rect.x), RoundInt(rect.y), 
      RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), 
      ToGfxColorProcedure(clr));
	end;
	
	// This procedure draws a line on a bitmap between two points
	procedure DrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : LongInt; clr : Color);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - DrawLineProcedure recieved empty Bitmap'); exit; end;
		// Add check to ensure points are less than 32,767
		aalineColor(dest^.surface, x1, y1, x2, y2, ToGfxColorProcedure(clr));
	end;
	
	// This procedure sets the color of a pixel on a bitmap
	procedure SetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
	begin
		if not Assigned(dest^.surface) then begin RaiseWarning('SDL1.2 Driver - SetPixelColorProcedure recieved empty Bitmap'); exit; end;
		pixelColor(dest^.surface, x, y, ToGfxColorProcedure(clr));
	end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  var
    SDLrect: SDL_Rect;
  begin
    if dest = nil then begin RaiseWarning('SDL1.2 Driver - SetClipRectangle recieved empty Bitmap'); exit; end;
    
    SDLrect := NewSDLRect(Round(rect.x), Round(rect.y), rect.width, rect.height);
    SDL_SetClipRect(dest^.surface, @SDLrect);
  end;
  
  procedure ResetClipProcedure(bmp : Bitmap);
  begin
    if bmp = nil then begin RaiseWarning('SDL1.2 Driver - ResetClip recieved empty Bitmap'); exit; end;
    
    SetLength(bmp^.clipStack, 0);
    SDL_SetClipRect(bmp^.surface, nil);
  end;

  procedure SetVideoModeFullScreenProcedure();
  var
    oldScr: PSDL_Surface;
  begin    
    oldScr := _screen;
    _screen := SDL_SetVideoMode(oldScr^.w, oldScr^.h, 32, oldScr^.flags xor SDL_FULLSCREEN);
  end;
  
  procedure SetVideoModeNoFrameProcedure();
  var
    oldScr: PSDL_Surface;
  begin    
    oldScr := _screen;
    _screen := SDL_SetVideoMode(oldScr^.w, oldScr^.h, 32, oldScr^.flags xor SDL_NOFRAME);
  end;
  
  // This procedure sets up the global variable (screen)
  procedure _SetupScreen();
  var
    psdlScreen : PSDL_Surface; 
  
  begin
    if screen = nil then New(screen)
    else if (screen^.surface <> nil) then SDL_FreeSurface(screen^.surface);
	  psdlScreen := PSDL_Surface(_screen);
    with psdlScreen^.format^ do
    begin
      screen^.surface := SDL_CreateRGBSurface(SDL_HWSURFACE,
                                             psdlScreen^.w, psdlScreen^.h, 32,
                                             RMask, GMask, BMask, 
                                             $ffffffff and not RMask
                                                and not GMask
                                                and not BMask);
      
      //WriteLn(RMask, ':', GMask, ':', BMask, ':', screen^.surface^.format^.AMask);
      
      //Turn off alpha blending for when scr is blit onto _screen
      SDL_SetAlpha(screen^.surface, 0, 255);
      SDL_FillRect(screen^.surface, @PSDL_Surface(screen^.surface)^.clip_rect, 0);

      screen^.width := psdlScreen^.w;
      screen^.height := psdlScreen^.h;
      screenRect := RectangleFrom(0,0, screen^.width, screen^.height);
    end;
  end;
  
  /// Sets up the graphical window for the specified width and height.
  /// Sets the caption of the window, and the icon if one is specified.
  procedure InitializeGraphicsWindowProcedure(caption: String; screenWidth, screenHeight: Longint);
  var
    icon: PSDL_Surface;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'InitSDL');
    {$ENDIF}

    if (screenWidth < 1) or (screenHeight < 1) then
    begin
      RaiseWarning('Screen Width and Height must be greater then 0 when opening a Graphical Window');
      exit;
    end;

    if Length(iconFile) > 0 then
    begin
      try
        icon := IMG_Load(PChar(iconFile));
        SDL_WM_SetIcon(icon, 0);
        SDL_FreeSurface(icon);
      except
        RaiseWarning('The icon file specified could not be loaded');
        exit;
      end;
    end;

    _screen := SDL_SetVideoMode(screenWidth, screenHeight, 32, SDL_HWSURFACE or SDL_DOUBLEBUF);
    if _screen = nil then
    begin
      RaiseWarning('Unable to create window drawing surface... ' + SDL_GetError());
      exit;
    end;

    _SetupScreen();
    if length(caption) > 0 then
      SDL_WM_SetCaption(PChar(caption), nil);

    {$IFDEF TRACE}
      TraceExit('sgGraphics', '_InitSDL');
    {$ENDIF}
  end;
  
  // This resizes the graphics window used by SwinGame
	procedure InitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, strColor : Color; msg : String);
  begin      
  	if not Assigned(screen^.surface) then begin RaiseWarning('SDL1.2 Driver - SetPixelColorProcedure recieved empty Bitmap'); exit; end;
    SDL_FillRect(screen^.surface, nil, bgColor);
    stringColor(screen^.surface, width div 2 - 30, height div 2, PChar(msg), ToGfxColorProcedure(strColor));
  end;
  
  // This resizes the graphics window used by SwinGame
	procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  var
    oldScr: PSDL_Surface;
  begin
    oldScr := _screen;
    _screen := SDL_SetVideoMode(newWidth, newHeight, 32, oldScr^.flags);
    _SetupScreen();
  end;
  
  // This function saves an image at path.
  // returns true if the save is successful, and false if it is not
  function SaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
		if not Assigned(bmpToSave^.surface) then begin RaiseWarning('SDL1.2 Driver - SaveImageProcedure recieved empty Bitmap'); exit; end;
    result := png_save_surface(path, bmpToSave^.surface);
  end;
  
  procedure RefreshScreenProcedure(screen : Bitmap);
  begin  
  	if not Assigned(screen^.surface) then begin RaiseWarning('SDL1.2 Driver - RefreshScreenProcedure recieved empty Bitmap'); exit; end;
    DrawCollectedText(screen);
    SDL_BlitSurface(screen^.surface, nil, _screen, nil);
    SDL_Flip(_screen);
  end;
  

  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a : byte) : Color;
  begin
		if not Assigned(bmp^.surface) then begin RaiseWarning('SDL1.2 Driver - ColorFromProcedure recieved empty Bitmap'); exit; end;
    result := SDL_MapRGBA(PSDL_Surface(bmp^.surface)^.format, r, g, b, a);
  end;
  
  function RGBAColorProcedure(red, green, blue, alpha: byte) : Color;
  begin
    result := SDL_MapRGBA(PSDL_Surface(screen^.surface)^.format, red, green, blue, alpha);
  end;

  function GetSurfaceWidthProcedure(src : Bitmap) : LongInt;
  begin
    result := PSDL_Surface(src^.surface)^.w;
  end;
  
  function GetSurfaceHeightProcedure(src : Bitmap) : LongInt;
  begin
    result := PSDL_Surface(src^.surface)^.h;
  end;
  
  function SurfaceFormatAssignedProcedure(src: Bitmap) : Boolean;
  begin
    result := Assigned( PSDL_Surface(src^.surface)^.format);
  end; 
  

  

  
  procedure GetRGBProcedure(pixel : Byte; r,g,b : Byte); 
  var
    fmt : PSDL_Surface;
  begin
    fmt := screen^.surface;
    SDL_GetRGB(pixel,fmt^.format, @r,@g,@b);    
  end;

  function GetScreenWidthProcedure(): LongInt; 
  begin
    result := PSDL_Surface(_screen)^.w;
  end;
  
  function GetScreenHeightProcedure(): LongInt;
  begin
    result := PSDL_Surface(_screen)^.h;
  end;

	procedure LoadSDLGraphicsDriver();
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