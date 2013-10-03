unit sgDriverGraphicsSDL13;
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
uses sgTypes, SDL2;
  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
	
	procedure LoadSDL13GraphicsDriver();
	function ToGfxColorProcedure(val : Color): Color;
	
implementation
	uses sgDriverGraphics, sysUtils, sgShared, sgGeometry, 
		SDL13_gfx, SDL13_Image, sgSavePNG, sgInputBackend, sgDriverSDL13, sgDriverImages, sgUtils, sgSDLUtils, sgSDL13Utils;	

  procedure DisposeSurface(surface : Pointer);
  begin
    if Assigned(surface) then
    begin
      if Assigned(PSDL13Surface(surface)^.surface) then
        SDL_FreeSurface(PSDL_Surface(PSDL13Surface(surface)^.surface));
  	  if Assigned(PSDL13Surface(surface)^.texture) then
  	    SDL_DestroyTexture(PSDL_Texture(PSDL13Surface(surface)^.texture));
    	Dispose(PSDL13Surface(surface));
    end;
  end;
  
  function RGBAColorProcedure(red, green, blue, alpha: byte) : Color; 
  begin
    if not Assigned(GetSurface(screen)) then begin RaiseWarning('SDL1.3 Graphics Driver - RGBAColorProcedure Screen is not Initialised'); exit; end;
    result := SDL_MapRGBA(GetSurface(screen)^.format, red, green, blue, alpha);
  end;

  function CreateSurface(width, height : LongInt) : PSDL_Surface;  
  var
    bpp        : integer;
    rMask, gMask, bMask, aMask, format : Uint32;
  begin
    format := SDL_GetWindowPixelFormat(PSDL13Screen(_screen)^.window);
    SDL_PixelFormatEnumToMasks(format, bpp, rMask, gMask, bMask, aMask);
    result := SDL_CreateRGBSurface(SDL_HWSURFACE, width, height, 32, 
                                   rMask, gMask, bMask, 
                                   $ffffffff and not RMask and not GMask and not BMask);
  end;  
  
  // Used to draw Primitive Shapes to a Surface -> Bitmap - > Renderer.
  // procedure DrawSurfaceToRenderer();
  // var
  //   // surf : PSDL_Surface;
  //   offset : Rectangle;
  // begin  
  //   offset := RectangleFrom(0, 0, screen^.width, screen^.height); 
  //   SDL_SetSurfaceAlphaMod(GetSurface(screen), 255);
  //   ImagesDriver.BlitSurface(screen, nil, nil, @offset);
  //   SDL_FillRect(GetSurface(screen), nil, RGBAColorProcedure(0, 0, 0, 0));   
  // end;
	
	function GetPixel32Procedure(bmp: Bitmap; x, y: Longint) :Color;
  begin
    result := GetSurfacePixel(GetSurface(bmp), x, y);
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
	// 	if not Assigned(GetSurface(bmp)) then begin RaiseWarning('SDL1.3 Graphics Driver - GetPixel32Procedure recieved empty Bitmap'); exit; end;
		
	//   if (x < 0) or (x >= bmp^.width) or (y < 0) or (y >= bmp^.height) then
 //    begin
 //      result := 0;
 //      exit;
 //    end;  
		 
	// 	surface := GetSurface(bmp);		
		
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
    PutSurfacePixel(GetSurface(bmp), clr, x, y);
  end;
 //  var
 //      p:    ^Color;
 //      bpp:  Longint;
 //  begin
 //      if not Assigned(GetSurface(bmp)) then begin RaiseWarning('SDL1.3 Graphics Driver - PutPixelProcedure recieved empty Bitmap'); exit; end;
        
 //      bpp := GetSurface(bmp)^.format^.BytesPerPixel;
 //      // Here p is the address to the pixel we want to set
 //      p := GetSurface(bmp)^.pixels + y * GetSurface(bmp)^.pitch + x * bpp;

 //      if bpp <> 4 then RaiseException('PutPixel only supported on 32bit images.');
 //      p^ := clr;
 //  end;
  
  procedure ColorComponentsProcedure(c: Color; var r, g, b, a: byte);
  begin  
    if not Assigned(GetSurface(screen)) then begin RaiseWarning('SDL1.3 Graphics Driver - ColorComponentsProcedure Screen is not Initialised'); exit; end;
    SDL_GetRGBA(c, GetSurface(screen)^.format, r, g, b, a);
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
  
  procedure SetRenderDrawColor(c : Color);
	var
    r, g, b, a : Uint8;
  begin  
    ColorComponentsProcedure(c, r, g, b, a);
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, r, g, b, a);
  end;
  
  procedure DrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
    // var
    //     r, g, b, a : Uint8;
	begin
	  if dest <> screen then
	  begin
	    if not Assigned(GetSurface(dest)) then 
	      RaiseWarning('SDL1.3 Graphics Driver - DrawTriangleProcedure recieved empty Bitmap')
	    else
        trigonColor(GetSurface(dest), Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), clr);      
	  end else begin
      SetRenderDrawColor(clr);
      SDL_RenderDrawLine(PSDL13Screen(_screen)^.renderer, Round(x1), Round(y1), Round(x2), Round(y2));
      SDL_RenderDrawLine(PSDL13Screen(_screen)^.renderer, Round(x2), Round(y2), Round(x3), Round(y3));
      SDL_RenderDrawLine(PSDL13Screen(_screen)^.renderer, Round(x3), Round(y3), Round(x1), Round(y1));
	  end;
  end;
  
  procedure GetLengthFromPoints(pos : array of Single; var lnth : LongInt; var lowest : Single );
  var
    highest : Single;
    i : LongInt;
  begin
    lowest := pos[0];
    highest := pos[0];
    for i := 1 to High(pos) do
    begin
      if (pos[i] < lowest) then lowest := pos[i];
      if (pos[i] > highest) then highest := pos[i];
    end;
    lnth := Round(highest - lowest) + 1;
  end;
  
  procedure PresentAndFreeShapes(surface : PSDL_Surface; srcRect, destRect : SDL_Rect);
  var  
    texture : PSDL_Texture;
  begin  	
    texture := SDL_CreateTextureFromSurface(PSDL13Screen(_screen)^.renderer, surface);
	  SDL_RenderCopy(PSDL13Screen(_screen)^.renderer, texture, @srcRect, @destRect);

	  SDL_DestroyTexture(texture);
	  SDL_FreeSurface(surface);    
  end;

  procedure FillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  var
    width, height : LongInt;  
    leftMost, topMost : Single;
    positions : Array [0..2] of Single;  
  	surf : PSDL_Surface;
    srcRect, destRect : SDL_Rect;
  begin                 
    if dest <> screen then
    begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillTriangleProcedure recieved empty Bitmap'); exit; end;
      filledTrigonColor(GetSurface(dest), Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), ToGfxColorProcedure(clr));      
    end else begin
      leftMost := 0; topMost := 0; width := 0; height := 0;
      
      positions[0] := x1; positions[1] := x2; positions[2] := x3;
      GetLengthFromPoints(positions, width, leftMost);   
       
      positions[0] := y1; positions[1] := y2; positions[2] := y3;
      GetLengthFromPoints(positions, height, topMost);
      srcRect  := NewSDLRect(0, 0, width, height);
      destRect := NewSDLRect(Round(leftMost), round(topMost), width, height);
      surf := CreateSurface(width,height);
      filledTrigonColor(surf, Round(x1 - leftMost), Round(y1 - topMost), Round(x2 - leftMost), Round(y2 - topMost), Round(x3 - leftMost), Round(y3 - topMost), ToGfxColorProcedure(clr));
      PresentAndFreeShapes(surf, srcRect, destRect);
    end;
  end;
	
  procedure DrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  var
    size : LongInt;
  	surf : PSDL_Surface;
    srcRect, destRect : SDL_Rect;
  begin  
    if dest <> screen then
    begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - DrawCircleProcedure recieved empty Bitmap'); exit; end;
      circleColor(GetSurface(dest), Round(xc), Round(yc), Abs(radius), ToGfxColorProcedure(clr));
    end else begin    
      size := radius * 2 + 1;
      surf := CreateSurface(size, size);
      srcRect  := NewSDLRect(0, 0, size, size);
      destRect := NewSDLRect(Round(xc - radius), Round(yc - radius), size, size);
      circleColor(surf, radius, radius, Abs(radius), ToGfxColorProcedure(clr));
      PresentAndFreeShapes(surf, srcRect, destRect);
    end;
  end;

  procedure FillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  var
    size : LongInt;
  	surf : PSDL_Surface;
    srcRect, destRect : SDL_Rect;
  begin  
    if dest <> screen then
    begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillCircleProcedure recieved empty Bitmap'); exit; end;
      filledCircleColor(GetSurface(dest), Round(xc), Round(yc), Abs(radius), ToGfxColorProcedure(clr));
    end else begin    
      size := radius * 2 + 1;
      surf := CreateSurface(size, size);
      srcRect  := NewSDLRect(0, 0, size, size);
      destRect := NewSDLRect(Round(xc - radius), Round(yc - radius), size, size);
      filledCircleColor(surf, radius, radius, Abs(radius), ToGfxColorProcedure(clr));
      PresentAndFreeShapes(surf, srcRect, destRect);
    end;
  end;
  
	// This procedure draws an Ellipse to a bitmap
	procedure FillEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
  var
    width, height : LongInt;
    // size : LongInt;
  	surf : PSDL_Surface;
    srcRect, destRect : SDL_Rect;
  begin  
    if dest <> screen then
    begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillEllipseProcedure recieved empty Bitmap'); exit; end;  
    	filledEllipseColor(GetSurface(dest), xPos , yPos, halfWidth, halfHeight, ToGfxColorProcedure(clr));
  	end else begin
  	  width := halfWidth * 2 + 1;
  	  height := halfHeight * 2 + 1;
      surf := CreateSurface(width, height);
      srcRect  := NewSDLRect(0, 0, width, height);
      destRect := NewSDLRect(Round(xPos - halfWidth), Round(yPos - halfHeight), width, height);
    	filledEllipseColor(surf, halfWidth , halfHeight, halfWidth, halfHeight, ToGfxColorProcedure(clr));
      PresentAndFreeShapes(surf, srcRect, destRect);  	  
  	end;
	end;
	
	// This procedure draws an Ellipse to a bitmap
	procedure DrawEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
   var
    width, height : LongInt;
    // size : LongInt;
  	surf : PSDL_Surface;
    srcRect, destRect : SDL_Rect;
  begin  
    if dest <> screen then
    begin
      if not assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - FillEllipseProcedure recieved empty Bitmap'); exit; end;  
    	ellipseColor(GetSurface(dest), xPos , yPos, halfWidth, halfHeight, ToGfxColorProcedure(clr));
  	end else begin
  	  width := halfWidth * 2 + 1;
  	  height := halfHeight * 2 + 1;
      surf := CreateSurface(width, height);
      srcRect  := NewSDLRect(0, 0, width, height);
      destRect := NewSDLRect(Round(xPos - halfWidth), Round(yPos - halfHeight), width, height);
    	ellipseColor(surf, halfWidth , halfHeight, halfWidth, halfHeight, ToGfxColorProcedure(clr));
      PresentAndFreeShapes(surf, srcRect, destRect);  	  
  	end;
	end;
	
	// This procedure draws a filled rectangle to a bitmap
	procedure FillRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	var
	  sdlrect : SDL_Rect;
	begin	
	  if clr = $00000000 then exit;
	  if dest <> screen then
	  begin
    	if not Assigned(GetSurface(dest)) then 
    	  RaiseWarning('SDL1.3 Graphics Driver - FillRectangleProcedure recieved empty Bitmap')
    	else
	      boxColor(GetSurface(dest),  RoundInt(rect.x), RoundInt(rect.y),  RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), ToGfxColorProcedure(clr));	
    end else begin
      sdlrect := NewSDLRect(rect);
  	  SetRenderDrawColor(clr);
      SDL_RenderFillRect(PSDL13Screen(_screen)^.renderer, sdlrect);
	  end;
	end;
	
	procedure DrawRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
	var
	  sdlrect : SDL_Rect;
	begin
	  if clr = $00000000 then exit;
	  if dest <> screen then
	  begin
    	if not Assigned(GetSurface(dest)) then 
    	  RaiseWarning('SDL1.3 Graphics Driver - DrawRectangleProcedure recieved empty Bitmap')
    	else
    	  rectangleColor(GetSurface(dest),  RoundInt(rect.x), RoundInt(rect.y),  RoundInt(rect.x + rect.width - 1), RoundInt(rect.y + rect.height - 1), ToGfxColorProcedure(clr));	  
    end else begin
  	  sdlrect := NewSDLRect(rect);
  	  SetRenderDrawColor(clr);
  	  SDL_RenderDrawRect(PSDL13Screen(_screen)^.renderer, sdlrect);
	  end;
	end;
	
	// This procedure draws a line on a bitmap between two points
	procedure DrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : LongInt; clr : Color);
	begin
	  if dest <> screen then
	  begin
  		if not Assigned(dest^.surface) then
  		  RaiseWarning('SDL1.3 Graphics Driver - DrawLineProcedure recieved empty Bitmap')
  		else
  		  lineColor(GetSurface(dest), x1, y1, x2, y2, ToGfxColorProcedure(clr));
	  end else begin
    	SetRenderDrawColor(clr);
      SDL_RenderDrawLine(PSDL13Screen(_screen)^.renderer, x1, y1, x2, y2);
	  end;
	end;
	
	// This procedure sets the color of a pixel on a bitmap
	procedure SetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
    // var
    //     r, g, b, a : Uint8;
	begin
	  if dest <> screen then
	  begin
  		if not Assigned(dest^.surface) then
  		  RaiseWarning('SDL1.3 Graphics Driver - SetPixelProcedure recieved empty Bitmap')
  		else
  		  pixelColor(GetSurface(dest), x, y, ToGfxColorProcedure(clr));
	  end else begin
    	SetRenderDrawColor(clr);
      SDL_RenderDrawPoint(PSDL13Screen(_screen)^.renderer, x, y);
	  end;
	end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  var
    SDLrect: SDL_Rect;
  begin
    if not Assigned(GetSurface(dest)) then begin RaiseWarning('SDL1.3 Graphics Driver - SetClipRectangle recieved empty Bitmap'); exit; end;
    
    SDLrect := NewSDLRect(Round(rect.x), Round(rect.y), rect.width, rect.height);
    SDL_SetClipRect(GetSurface(dest), @SDLrect);
  end;
  
  procedure ResetClipProcedure(bmp : Bitmap);
  begin
    if not Assigned(GetSurface(bmp))  then begin RaiseWarning('SDL1.3 Graphics Driver - ResetClip recieved empty Bitmap'); exit; end;
    
    SetLength(bmp^.clipStack, 0);
    SDL_SetClipRect(GetSurface(bmp), nil);
  end;

  procedure SetVideoModeFullScreenProcedure();
  var
    fullScreen : SDL_Bool = true;
    flags : Uint32;
    //noFrame : Boolean;
  begin    
    flags := SDL_GetWindowFlags(PSDL13Screen(_screen)^.window);

    if (flags and Uint32(SDL_WINDOW_FULLSCREEN)) = Uint32(SDL_WINDOW_FULLSCREEN) then
      fullScreen := false;
    SDL_SetWindowFullScreen(PSDL13Screen(_screen)^.window, fullScreen);
  end;
  
  procedure SetVideoModeNoFrameProcedure();
  begin    
    // TODO: need to recreate window to switch to borderless... check further if needed
  end;
  
  // This procedure sets up the global variable (screen)
  procedure _SetupScreen(screenWidth, screenHeight: Longint);
  // var
  //   bpp        : integer;
  //   rMask, gMask, bMask, aMask, format : Uint32; 
  begin
    if screen = nil 
      then New(screen)
    else if (screen^.surface <> nil) then 
      DisposeSurface(screen^.surface);
	  
//	  format := SDL_GetWindowPixelFormat(PSDL13Screen(_screen)^.window);
	  
	//  SDL_PixelFormatEnumToMasks(format, @bpp, @rMask, @gMask, @bMask, @aMask);
	  
	  New(PSDL13Surface(screen^.surface));
    
    PSDL13Surface(screen^.surface)^.surface := CreateSurface(screenWidth, screenHeight);{ SDL_CreateRGBSurface(SDL_HWSURFACE, screenWidth, screenHeight, 32, 
                                                                    rMask, gMask, bMask, 
                                                                    $ffffffff and not RMask and not GMask and not BMask);         }                                                
    screen^.width := screenWidth;
    screen^.height := screenHeight;
    
    screenRect := RectangleFrom(0,0, screenWidth, screenHeight);
    
    if (Assigned(PSDL13Screen(_screen)^.renderer)) then
      SDL_DestroyRenderer(PSDL13Screen(_screen)^.renderer);
      
    PSDL13Screen(_screen)^.renderer := SDL_CreateRenderer(PSDL13Screen(_screen)^.window, -1, LongWord(SDL_RENDERER_ACCELERATED) or LongWord(SDL_RENDERER_PRESENTVSYNC));
    SDL_SetRenderDrawColor(PSDL13Screen(_screen)^.renderer, 0, 0, 0, 255);
    SDL_RenderClear(PSDL13Screen(_screen)^.renderer);
    SDL_RenderPresent(PSDL13Screen(_screen)^.renderer);
  end;
  
  function AvailableResolutionsProcedure() : ResolutionArray;
	var
  	mode : SDL_DisplayMode;
  	i, modes    : LongInt;
  	resolutions : ResolutionArray;
	begin
	  setLength(resolutions,0);
	  //built in screen = 0
		modes := SDL_GetNumDisplayModes(0);
    for i:= 0 to modes do
    begin
      SDL_GetDisplayMode(0, i, @mode);
      setLength(resolutions, length(resolutions)+1);
      resolutions[i].width := mode.w;
      resolutions[i].height := mode.h;
      resolutions[i].refreshRate := mode.refresh_rate;
      resolutions[i].format      := mode.format;
    end;
    result := resolutions;
    
	end;
	
  procedure InitializeGraphicsWindowProcedure(caption: String; screenWidth, screenHeight: Longint);
  // var
  //     sdlScreen : PSDL13Screen;
  begin
    
    // Initialize SDL.
    if (SDL_Init(SDL_INIT_VIDEO) < 0) then exit;
    
    _screen := New(PSDL13Screen);
    
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 4);
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
    
    // Create the window where we will draw.
    {$IFDEF IOS}
      PSDL13Screen(_screen)^.window  := SDL_CreateWindow(PChar(caption), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                                       screenWidth, screenHeight, Uint32(SDL_WINDOW_OPENGL) or Uint32(SDL_WINDOW_SHOWN) or Uint32(SDL_WINDOW_BORDERLESS));
    {$ELSE}
      PSDL13Screen(_screen)^.window  := SDL_CreateWindow(PChar(caption), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                                       screenWidth, screenHeight, Uint32(SDL_WINDOW_OPENGL) or Uint32(SDL_WINDOW_SHOWN) );
    {$ENDIF}
    _SetupScreen(screenWidth, screenHeight);
  end;
  
  // This resizes the graphics window used by SwinGame
	procedure InitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, strColor : Color; msg : String);
  begin      
  	if not Assigned(GetSurface(screen)) then begin RaiseWarning('SDL1.3 Graphics Driver - SetPixelColorProcedure recieved empty Bitmap'); exit; end;
  //	  _SetupScreen(width, height);
 //   stringColor(GetSurface(screen), width div 2 - 30, height div 2, PChar(msg), ToGfxColorProcedure(strColor));
  end;
  
  // This resizes the graphics window used by SwinGame
	procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
    SDL_SetWindowSize(PSDL_Window(PSDL13Screen(_screen)^.window), newWidth, newHeight);
    _SetupScreen(newWidth, newHeight);
  end;
  
  // This function saves an image at path.
  // returns true if the save is successful, and false if it is not
  function SaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
		if not Assigned(GetSurface(bmpToSave)) then begin RaiseWarning('SDL1.3 Graphics Driver - SaveImageProcedure recieved empty Bitmap'); exit; end;
    result := png_save_surface(path, GetSurface(bmpToSave));
  end;
  
  procedure RefreshScreenProcedure(screen : Bitmap);
  begin  
    DrawCollectedText(screen);
    SDL_RenderPresent(PSDL13Screen(_screen)^.renderer);
  end;
  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a : byte) : Color;
  begin
		if not Assigned(GetSurface(bmp)) then begin RaiseWarning('SDL1.3 Graphics Driver - ColorFromProcedure recieved empty Bitmap'); exit; end;
    result := SDL_MapRGBA(GetSurface(screen)^.format, r, g, b, a);
  end;

  function GetSurfaceWidthProcedure(src : Bitmap) : LongInt;
  begin
		if not Assigned(GetSurface(src)) then begin RaiseWarning('SDL1.3 Graphics Driver - GetSurfaceWidthProcedure recieved empty Bitmap'); exit; end;
    result := GetSurface(src)^.w;
  end;
  
  function GetSurfaceHeightProcedure(src : Bitmap) : LongInt;
  begin
		if not Assigned(GetSurface(src)) then begin RaiseWarning('SDL1.3 Graphics Driver - GetSurfaceHeightProcedure recieved empty Bitmap'); exit; end;
    result := GetSurface(src)^.h;
  end;
  
  function SurfaceFormatAssignedProcedure(src: Bitmap) : Boolean;
  begin
		if not Assigned(GetSurface(src)) then begin RaiseWarning('SDL1.3 Graphics Driver - SurfaceFormatAssignedProcedure recieved empty Bitmap'); exit; end;
    result := Assigned(GetSurface(src)^.format );
  end; 
  
  procedure GetRGBProcedure(pixel : Byte; r,g,b : Byte); 
  var
    fmt : PSDL_Surface;
  begin
    fmt := GetSurface(screen);
    SDL_GetRGB(pixel,fmt^.format, r,g,b);    
  end;

  function GetScreenWidthProcedure(): LongInt; 
  var
    w, h : LongInt;
  begin
    SDL_GetWindowSize(PSDL13Screen(_screen)^.window, w, h);
    result := w;
  end;
  
  function GetScreenHeightProcedure(): LongInt;
  var
    w, h : LongInt;
  begin
    SDL_GetWindowSize(PSDL13Screen(_screen)^.window, w, h);
    result := h;
  end;
  

	procedure LoadSDL13GraphicsDriver();
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
    GraphicsDriver.GetRGB                   := @GetRGBProcedure;
    GraphicsDriver.SurfaceFormatAssigned    := @SurfaceFormatAssignedProcedure;
    GraphicsDriver.GetScreenWidth           := @GetScreenWidthProcedure;
    GraphicsDriver.GetScreenHeight          := @GetScreenHeightProcedure;
    GraphicsDriver.AvailableResolutions     := @AvailableResolutionsProcedure;
	end;
end.