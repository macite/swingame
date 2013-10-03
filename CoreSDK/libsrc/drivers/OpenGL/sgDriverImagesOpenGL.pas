unit sgDriverImagesOpenGL;

//=============================================================================
// sgDriverImagesSDL13.pas
//=============================================================================
//
//
// 
//
// Notes:
//    - Pascal PChar is equivalent to a C-type string
//    - Pascal Word is equivalent to a Uint16
//    - Pascal LongWord is equivalent to a Uint32
//    - Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
  uses {$IFDEF IOS}gles11;{$ELSE}gl, glext;{$ENDIF}
  
  procedure LoadOpenGLImagesDriver();
    
implementation
  uses sgTypes,
       sgDriverGraphics, SDL2, sgShared, sgDriverImages, sdl13_image, SysUtils, sgSharedUtils, 
       GLDriverUtils, sgSDLUtils; // sdl;
  const
    // PixelFormat
    GL_COLOR_INDEX     = $1900;
    GL_STENCIL_INDEX   = $1901;
    GL_DEPTH_COMPONENT = $1902;
    GL_RED             = $1903;
    GL_GREEN           = $1904;
    GL_BLUE            = $1905;
    GL_ALPHA           = $1906;
    GL_RGB             = $1907;
    GL_RGBA            = $1908;
    GL_LUMINANCE       = $1909;
    GL_LUMINANCE_ALPHA = $190A;
    GL_BGR             = $80E0;
    GL_BGRA            = $80E1;

  // Setup a created bitmap, clearing surface and setting alpha blending options
  procedure InitBitmapColorsProcedure(bmp : Bitmap);
  begin   
    // WriteLn('InitBitmapColorsProcedure');
    bmp^.surface := nil;
    exit;
  end;
  
  // Returns true if the bitmap's surface exists
  function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
  begin
    result := Assigned(SurfaceOf(bmp));
  end;
  

  procedure CreateBitmapProcedure(bmp : Bitmap; width, height : LongInt);
  var
    surf: PSDL_Surface;
  begin
    // in case things fail...
    bmp^.surface := nil;
    if not CheckAssigned('OpenGL ImagesDriver - CreateBitmapProcedure received unassigned Bitmap', bmp) then exit;
    
    surf := SDL_CreateRGBSurface(0, width, height, 32, $FF000000, $00FF0000, $0000FF00, $000000FF);
    
    // Transfer ownership of surface to the Bitmap
    bmp^.surface := CreateSurfaceData(surf);
  end;

  // Sets the non-transparent pixels in a Bitmap. This is then used for
  // collision detection, allowing the original surface to be optimised.
  //
  // @param bmp  A pointer to the Bitmap being set
  // @param surface The surface with pixel data for this Bitmap
  procedure SetNonTransparentPixels(bmp: Bitmap; transparentColor: Color);
  begin   
    exit;
  end;
  
  procedure SetNonAlphaPixelsProcedure(bmp : Bitmap); 
  begin   
    ;
  end;

  // Convert a surface to RGBA
  procedure ToRGBA(var srcImg : PSDL_Surface);
  var
    temp: PSDL_Surface;
    pixelFormat : SDL_PIXELFORMAT;
  begin
    pixelFormat := srcImg^.format^;
    
    pixelFormat.format := SDL_PIXELFORMAT_RGBA8888;
    pixelFormat.Rmask  := $000000FF;
    pixelFormat.Gmask  := $0000FF00;
    pixelFormat.Bmask  := $00FF0000;
    pixelFormat.Amask  := $FF000000;
    pixelFormat.Rloss  := 0;
    pixelFormat.Gloss  := 0;
    pixelFormat.Bloss  := 0;
    pixelFormat.Aloss  := 0; 
    pixelFormat.Rshift := 0;
    pixelFormat.Gshift := 8;
    pixelFormat.Bshift := 16;
    pixelFormat.Ashift := 24;

    temp := SDL_ConvertSurface(srcImg, @pixelFormat, 0);
    if Assigned(temp) then
    begin
      SDL_FreeSurface(srcImg);
      srcImg := temp;
    end;
  end;

  procedure ReplaceColors(surf : PSDL_Surface; originalColor, newColor : Color; width, height : LongInt);
  var
    x, y : LongInt;
  begin
    for x := 0 to width - 1  do 
    begin
      for y := 0 to height - 1 do 
      begin
        if GetSurfacePixel(surf, x, y) = originalColor then
          PutSurfacePixel(surf, newColor, x, y);
      end;
    end;
  end;

  procedure CopyOnto(src, dest: PSDL_Surface);
  var
    x, y : LongInt;
  begin
    for x := 0 to src^.w - 1  do 
    begin
      for y := 0 to src^.h - 1 do 
      begin
          PutSurfacePixel(dest, GetSurfacePixel(src, x, y), x, y);
      end;
    end;
  end;

  function DoLoadBitmapProcedure(filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
  var
    loadedImage: PSDL_Surface;
    //offset : Rectangle;
    //lTransparentSurface : PSDL_Surface;
    lColorToSetTo : Color = $00000000;
    optimizeSizedImage : PSDL_Surface;
    dRect: SDL_Rect;
  begin
    result := nil; //start at nil to exit cleanly on error
    
    //Load the image
    loadedImage := IMG_Load(PChar(filename));
    if not CheckAssigned('OpenGL ImagesDriver - Error loading image: ' + filename + ': ' + SDL_GetError(), loadedImage) then
    begin      
      exit;
    end;

    optimizeSizedImage := SDL_CreateRGBSurface(0, Pow2GEQ(loadedImage^.w),
                                          Pow2GEQ(loadedImage^.h),
                                          32,
                                          $000000FF, //loadedImage^.format^.Rmask,
                                          $0000FF00, //loadedImage^.format^.Gmask,
                                          $00FF0000, //loadedImage^.format^.Bmask,
                                          $FF000000 //loadedImage^.format^.Amask
                                           );
    
    //optimizeSizedImage := IMG_Load(PChar(filename));
    dRect.x := 0;               dRect.y := 0;
    dRect.w := loadedImage^.w;  dRect.h := loadedImage^.h;
    ToRGBA(loadedImage);

    CopyOnto(loadedImage, optimizeSizedImage);

    //SDL_UpperBlit(loadedImage, nil, optimizeSizedImage, @dRect);
    //SDL_BlitSurface(loadedImage, nil, optimizeSizedImage, @dRect);
    SDL_FreeSurface(loadedImage);

    if not CheckAssigned('OpenGL ImagesDriver - Error loading image: ' + filename + ': ' + SDL_GetError(), loadedImage) then
    begin      
      exit;
    end;

    // Image loaded, so create SwinGame bitmap    
    new(result);
    result^.width              := dRect.w;
    result^.height             := dRect.h;   
    result^.textureWidthRatio  := result^.width / optimizeSizedImage^.w;
    result^.textureHeightRatio := result^.height / optimizeSizedImage^.h;

     
    //Determine pixel level collision data
    if transparent then
    begin
      //offset.x := 0;
      //offset.y := 0;
      //offset.width := result^.width;
      //offset.height := result^.height;
      ReplaceColors(optimizeSizedImage, transparentColor, lColorToSetTo, result^.width, result^.height);
    end 
    else
    begin    
      SetNonAlphaPixels(result, optimizeSizedImage);
    end;
    
    // Transfer ownership of surface to the Bitmap
    result^.surface := CreateSurfaceData(optimizeSizedImage);
    // Setup the GL texture
    CreateGLTexture(result);
  end; 
  
  procedure RemoveTexture(bmp: Bitmap);
  var
    tex: Cardinal;
  begin
    if not (Assigned(bmp) and Assigned(bmp^.surface)) then exit;
    tex := TextureOf(bmp);
    glDeleteTextures(1, @tex);
    PSGOpenGLSurface(bmp^.surface)^.texture := 0;
  end;
  
  procedure FreeSurfaceProcedure(bmp : Bitmap);
  begin
    if bmp = screen then exit;
    
    if Assigned(bmp) and Assigned(bmp^.surface) then
    begin
      RemoveTexture(bmp);
      
      // Delete texture and surface
      SDL_FreeSurface(SurfaceOf(bmp));
      
      // nil just in case...
      PSGOpenGLSurface(bmp^.surface)^.surface := nil;
      
      // Dispose surface structure
      Dispose(PSGOpenGLSurface(bmp^.surface));
      bmp^.surface := nil;
    end;
  end; 
  
  procedure MakeOpaqueProcedure(bmp : Bitmap);
  begin   
    exit;
  end;

  procedure SetOpacityProcedure(bmp : Bitmap; pct : Single);
  begin   
    exit;
  end;

  procedure MakeTransparentProcedure(bmp : Bitmap);
  begin   
    exit;
  end;

  procedure RotateScaleSurfaceProcedure(resultBmp, src : Bitmap; deg, scale : Single; smooth : LongInt);
  begin   
    exit;
  end;

  function SameBitmapProcedure(const bitmap1, bitmap2 : Bitmap) : Boolean;
  begin
   result := false;
  end;

  function GetCoords(bmpSize : Single; srcRectSize : Single) : Single;
  begin
    result := 0;
    if (bmpSize = 0) or (srcRectSize = 0) then
      exit
    else
      result := srcRectSize / bmpSize;
  end;
  
  procedure BlitSurfaceProcedure(srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr); 
  var
    //lTexture : GLuint;
    // textureCoord : Array[0..3] of Point2D;
    // vertices : Array[0..3] of Point2D;
    lRatioX, lRatioY, lRatioW, lRatioH, lTexWidth, lTexHeight : Single;
    tex: Cardinal;
  begin
    
    if ((srcRect = nil) and (srcBmp^.textureWidthRatio <> 0) and ( srcBmp^.textureHeightRatio <> 0)) then //check for /0 s
    begin
      lRatioX := 0;
      lRatioY := 0;
      lRatioW := srcBmp^.textureWidthRatio;
      lRatioH := srcBmp^.textureHeightRatio;
    end else begin
      lTexWidth  := srcBmp^.width / srcBmp^.textureWidthRatio;
      lTexHeight := srcBmp^.height / srcBmp^.textureHeightRatio;
      
      lRatioX := GetCoords(lTexWidth, srcRect^.x);
      lRatioY := GetCoords(lTexHeight, srcRect^.y);
      
      lRatioW := lRatioX + GetCoords(lTexWidth, srcRect^.width);
      lRatioH := lRatioY + GetCoords(lTexHeight, srcRect^.height);
    end;
    
    //reset color
    glColor4f(1,1,1,1);
    //glLoadIdentity();
    
    tex := TextureOf(srcBmp);
    if tex = 0 then CreateGLTexture(srcBmp);
    RenderTexture(TextureOf(srcBmp), lRatioX, lRatioY, lRatioW, lRatioH, destRect);   
    if tex = 0 then RemoveTexture(srcBmp);
  end;
  
  procedure ClearSurfaceProcedure(dest : Bitmap; toColor : Color); 
  var
    r,g,b,a : Byte;
  begin   
    if dest <> Screen then
    begin
      
    end
    else
    begin
      glBindTexture(GL_TEXTURE_2D, 0);
      GraphicsDriver.ColorComponents(toColor,r,g,b,a);
      glClearColor ( r/255,g/255,b/255,a/255 );
      glClear ( GL_COLOR_BUFFER_BIT );
    end;
  end;
  
  procedure OptimiseBitmapProcedure(surface : Bitmap); 
  begin   
    if TextureOf(surface) = 0 then CreateGLTexture(surface);
  end;
  
  procedure SaveBitmapProcedure(src : Bitmap; filepath : String);
  begin   
    exit;
  end;
    
  procedure LoadOpenGLImagesDriver();
  begin
    ImagesDriver.InitBitmapColors   := @InitBitmapColorsProcedure;
    ImagesDriver.SurfaceExists      := @SurfaceExistsProcedure;
    ImagesDriver.CreateBitmap       := @CreateBitmapProcedure;
    ImagesDriver.DoLoadBitmap       := @DoLoadBitmapProcedure;
    ImagesDriver.FreeSurface        := @FreeSurfaceProcedure;
    ImagesDriver.MakeOpaque         := @MakeOpaqueProcedure;
    ImagesDriver.SetOpacity         := @SetOpacityProcedure;
    ImagesDriver.SameBitmap         := @SameBitmapProcedure;
    ImagesDriver.BlitSurface        := @BlitSurfaceProcedure;
    ImagesDriver.MakeTransparent    := @MakeTransparentProcedure;
    ImagesDriver.RotateScaleSurface := @RotateScaleSurfaceProcedure;
    ImagesDriver.ClearSurface       := @ClearSurfaceProcedure;
    ImagesDriver.OptimiseBitmap     := @OptimiseBitmapProcedure;
    ImagesDriver.SetNonAlphaPixels  := @SetNonAlphaPixelsProcedure;
  end;
end.