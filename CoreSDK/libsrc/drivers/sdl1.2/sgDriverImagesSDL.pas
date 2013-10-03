unit sgDriverImagesSDL;
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
	
	procedure LoadSDLImagesDriver();
		
implementation
	uses sgDriverImages, sgShared, sgTypes, SysUtils, sgGraphics, sgDriver, sgSharedUtils,
	     SDL_gfx, SDL, SDL_Image, sgDriverGraphics, sgDriverGraphicsSDL, sgSDLUtils; // sdl;
		
	procedure InitBitmapColorsProcedure(bmp : Bitmap);
 	begin	  
 	  if not CheckAssigned('SDL1.2 ImagesDriver - InitBitmapColorsProcedure recieved unassigned Bitmap', bmp) then exit;
   	if not CheckAssigned('SDL1.2 ImagesDriver - InitBitmapColorsProcedure recieved empty Bitmap Surface', bmp^.surface) then exit;
    
    SDL_SetAlpha(bmp^.surface, SDL_SRCALPHA, 0);
    SDL_FillRect(bmp^.surface, nil, ColorTransparent);
 	end;
     	
	function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
	begin
	  if not assigned(bmp) then 
	    result := false
	  else 
	    result := Assigned(bmp^.surface);
	end;
  

	procedure CreateBitmapProcedure(bmp : Bitmap; width, height : LongInt);
	begin
 	  if not CheckAssigned('SDL1.2 ImagesDriver - CreateBitmapProcedure recieved unassigned Bitmap', bmp) then exit;
    // if not CheckAssigned('SDL1.2 ImagesDriver - Screen must be loaded to create bitmap', screen) then exit;
    // if not CheckAssigned('SDL1.2 ImagesDriver - Screen must be loaded to create bitmap', screen^.surface) then exit;
    
		if (screen = nil) or (screen^.surface = nil) or (PSDL_Surface(screen^.surface)^.format = nil) then
    begin
      RaiseWarning('Creating ARGB surface as screen format unknown.');
      bmp^.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32, $00FF0000, $0000FF00, $000000FF, $FF000000);
    end
    else
    begin
      with PSDL_Surface(screen^.surface)^.format^ do
      begin
        bmp^.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32, RMask, GMask, BMask, AMask);
      end;
    end;
	end;

  // Sets the non-transparent pixels in a Bitmap. This is then used for
  // collision detection, allowing the original surface to be optimised.
  //
  // @param bmp  A pointer to the Bitmap being set
  // @param surface The surface with pixel data for this Bitmap
  procedure SetNonTransparentPixels(bmp: Bitmap; transparentColor: Color);
  var
    r, c: Longint;
  begin
 	  if not CheckAssigned('SDL1.2 ImagesDriver - SetNonTransparentPixels recieved unassigned Bitmap', bmp) then exit;
    
    SetLength(bmp^.nonTransparentPixels, bmp^.width, bmp^.height);

    for c := 0 to bmp^.width - 1 do
    begin
      for r := 0 to bmp^.height - 1 do
      begin
        bmp^.nonTransparentPixels[c, r] := (GraphicsDriver.GetPixel32(bmp, c, r) <> transparentColor);
      end;
    end;
  end;
  
  procedure SetNonAlphaPixelsProcedure(bmp : Bitmap); 
  begin
    if Assigned(bmp) then SetNonAlphaPixels(bmp, bmp^.surface);
  end;
	
	function DoLoadBitmapProcedure(filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
  var
    loadedImage: PSDL_Surface;
    correctedTransColor: Color;
  begin
    result := nil; //start at nil to exit cleanly on error
    
    //Load the image
    loadedImage := IMG_Load(PChar(filename));

 	  if not CheckAssigned('SDL1.2 ImagesDriver - Error loading image: ' + filename + ': ' + SDL_GetError(), loadedImage) then exit;

    //
    // Image loaded, so create SwinGame bitmap
    //
    new(result);
    result^.surface := nil;

    if not transparent then 
    begin
      // Only move to screen alpha if window is open...
      if (screen = nil) or (screen^.surface = nil) then
        result^.surface := loadedImage
      else
        result^.surface := SDL_DisplayFormatAlpha(loadedImage);
    end
    else 
      result^.surface := SDL_DisplayFormat(loadedImage);

    if not assigned(result^.surface) then
    begin
      SDL_FreeSurface(loadedImage);
      dispose(result);
      result := nil;
      RaiseException('Error loading image.')
    end;

    result^.width     := PSDL_Surface(result^.surface)^.w;
    result^.height    := PSDL_Surface(result^.surface)^.h;

    //Determine pixel level collision data
    if transparent then
    begin
      correctedTransColor := ColorFrom(result, transparentColor);
      SDL_SetColorKey(result^.surface, SDL_RLEACCEL or SDL_SRCCOLORKEY, correctedTransColor);
      WriteLn('Pixels for ', filename, ' ', HexStr(result^.surface));
      SetNonTransparentPixels(result, transparentColor);
      WriteLn('Done ', filename);
    end
    else
    begin
      SetNonAlphaPixelsProcedure(result);
    end;

    // Free the loaded image if its not the result's surface
    if loadedImage <> result^.surface then SDL_FreeSurface(loadedImage);
	end;
	
	procedure FreeSurfaceProcedure(bmp : Bitmap);
	begin
	  //Free the surface
    if Assigned(bmp) and Assigned(bmp^.surface) then
    begin
      SDL_FreeSurface(bmp^.surface);
    end;
    
    bmp^.surface := nil;
	end;
	
	procedure MakeOpaqueProcedure(bmp : Bitmap);
	begin
 	  if not CheckAssigned('SDL1.2 ImagesDriver - MakeOpaqueProcedure recieved unassigned Bitmap', bmp) then exit;
   	if not CheckAssigned('SDL1.2 ImagesDriver - MakeOpaqueProcedure recieved empty Bitmap Surface', bmp^.surface) then exit;
    SDL_SetAlpha(bmp^.surface, 0, 255);
	end;

	procedure SetOpacityProcedure(bmp : Bitmap; pct : Single);
	begin
   	if not CheckAssigned('SDL1.2 ImagesDriver - SetOpacityProcedure received unassigned Bitmap', bmp) then exit;
    if not CheckAssigned('SDL1.2 ImagesDriver - SetOpacityProcedure recieved empty Bitmap Surface', bmp^.surface) then exit;
    SDL_SetAlpha(bmp^.surface, SDL_SRCALPHA, RoundUByte(pct * 255));
	end;

	procedure MakeTransparentProcedure(bmp : Bitmap);
	begin
   	if not CheckAssigned('SDL1.2 ImagesDriver - MakeTransparentProcedure recieved empty Bitmap', bmp) then exit;
    if not CheckAssigned('SDL1.2 ImagesDriver - MakeTransparentProcedure recieved empty Bitmap Surface', bmp^.surface) then exit;
    
    SDL_SetAlpha(bmp^.surface, SDL_SRCALPHA, 0);
	end;

	procedure RotateScaleSurfaceProcedure(resultBmp, src : Bitmap; deg, scale : Single; smooth : LongInt);
	begin
   	if not CheckAssigned('SDL1.2 ImagesDriver - RotateScaleSurfaceProcedure recieved unassigned Result Bitmap', resultBmp) then exit;
   	if not CheckAssigned('SDL1.2 ImagesDriver - RotateScaleSurfaceProcedure recieved unassigned Source Bitmap', src) then exit;
    if not CheckAssigned('SDL1.2 ImagesDriver - RotateScaleSurfaceProcedure recieved empty Source Bitmap Surface', src^.surface) then exit;
        
    resultBmp^.surface := rotozoomSurface(src^.surface, deg, scale, 0);
    resultBmp^.width   := PSDL_Surface(resultBmp^.surface)^.w;
    resultBmp^.height  := PSDL_Surface(resultBmp^.surface)^.h;
	end;

	function SameBitmapProcedure(const bitmap1,bitmap2 : Bitmap) : Boolean;
	begin
   	if not Assigned(bitmap1) or not Assigned(bitmap2) then
   	begin
   	  result := false;
   	end
	  else
	  begin
	    result := (bitmap1^.surface = bitmap2^.surface);
    end;
	end;
	
	procedure BlitSurfaceProcedure(srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr); 
	var
	  sRect, dRect : SDL_Rect;
	  pDRect : ^SDL_Rect = nil;
	  pSRect : ^SDL_Rect = nil;
	   
	begin
   	if not CheckAssigned('SDL1.2 ImagesDriver - BlitSurfaceProcedure recieved unassigned Source Bitmap', srcBmp) then exit;
    if not CheckAssigned('SDL1.2 ImagesDriver - BlitSurfaceProcedure recieved empty Source Bitmap Surface', srcBmp^.surface) then exit;
    if not CheckAssigned('SDL1.2 ImagesDriver - BlitSurfaceProcedure recieved unassigned Destination Bitmap', destBmp) then exit;
    if not CheckAssigned('SDL1.2 ImagesDriver - BlitSurfaceProcedure recieved empty Destination Bitmap Surface', destBmp^.surface) then exit;
	  
	  if assigned(srcRect) then
	  begin
	    sRect := NewSDLRect(srcRect^);
  	  pSRect := @sRect;
	  end;
	  if assigned(destRect) then
	  begin
	    dRect := NewSDLRect(destRect^);
  	  pDRect := @dRect;
	  end;
	  
	  SDL_BlitSurface(srcBmp^.surface, pSRect, destBmp^.surface, pDRect);
	end;
	
	procedure ClearSurfaceProcedure(dest : Bitmap; toColor : Color); 
  begin
   	if not CheckAssigned('SDL1.2 ImagesDriver - ClearSurfaceProcedure recieved empty Bitmap', dest) then exit;
    if not CheckAssigned('SDL1.2 ImagesDriver - ClearSurfaceProcedure recieved empty Bitmap Surface', dest^.surface) then exit;
    
    SDL_FillRect(dest^.surface, @PSDL_Surface(dest^.surface)^.clip_rect, toColor);
  end;
  
	procedure OptimiseBitmapProcedure(surface : Bitmap); 
  var
    oldSurface: PSDL_Surface;
  begin
 	  if not CheckAssigned('SDL1.2 ImagesDriver - OptimiseBitmapProcedure recieved empty Bitmap', surface) then exit;
   	if not CheckAssigned('SDL1.2 ImagesDriver - OptimiseBitmapProcedure recieved empty Bitmap Surface', surface^.surface) then exit;
  
    oldSurface := surface^.surface;
    SetNonAlphaPixelsProcedure(surface);
    surface^.surface := SDL_DisplayFormatAlpha(oldSurface);
    SDL_FreeSurface(oldSurface);
  end;
  
  procedure SaveBitmapProcedure(src : Bitmap; filepath : String);
  begin
 	  if not CheckAssigned('SDL1.2 ImagesDriver - SaveBitmapProcedure recieved empty Bitmap', src) then exit;
   	if not CheckAssigned('SDL1.2 ImagesDriver - SaveBitmapProcedure recieved empty Bitmap Surface', src^.surface) then exit;
    SDL_SaveBMP(src^.surface, PChar(filepath));
  end;
  

  
	procedure LoadSDLImagesDriver();
	begin
		ImagesDriver.InitBitmapColors                         := @InitBitmapColorsProcedure;
		ImagesDriver.SurfaceExists                            := @SurfaceExistsProcedure;
		ImagesDriver.CreateBitmap                             := @CreateBitmapProcedure;
		ImagesDriver.DoLoadBitmap                             := @DoLoadBitmapProcedure;
		ImagesDriver.FreeSurface                              := @FreeSurfaceProcedure;
		ImagesDriver.MakeOpaque                               := @MakeOpaqueProcedure;
		ImagesDriver.SetOpacity                               := @SetOpacityProcedure;
		ImagesDriver.SameBitmap                               := @SameBitmapProcedure;
		ImagesDriver.BlitSurface                              := @BlitSurfaceProcedure;
		ImagesDriver.MakeTransparent                          := @MakeTransparentProcedure;
		ImagesDriver.RotateScaleSurface                       := @RotateScaleSurfaceProcedure;
		ImagesDriver.ClearSurface                             := @ClearSurfaceProcedure;
		ImagesDriver.OptimiseBitmap                           := @OptimiseBitmapProcedure;
		ImagesDriver.SetNonAlphaPixels                        := @SetNonAlphaPixelsProcedure;
	end;
end.