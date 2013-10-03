unit sgDriverImages;
//=============================================================================
// sgDriverImages.pas
//=============================================================================
//
// The Graphics driver is responsible for acting as the interface between driver
// code and swingame code. Swingame code uses the images driver to access the 
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
	uses sgTypes, sgShared;
	
	type
	  InitBitmapColorsProcedure            = procedure (bmp : Bitmap);
	  CreateBitmapProcedure                = procedure (bmp : Bitmap; width, height : LongInt);
	  SurfaceExistsProcedure               = function  (bmp : Bitmap) : Boolean;
	  DoLoadBitmapProcedure                = function  (filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
    FreeSurfaceProcedure                 = procedure (bmp : Bitmap);
    MakeOpaqueProcedure                  = procedure (bmp : Bitmap);
    SetOpacityProcedure                  = procedure (bmp : Bitmap; pct : Single);
    MakeTransparentProcedure             = procedure (bmp : Bitmap);
    RotateScaleSurfaceProcedure          = procedure (resultBmp, src : Bitmap; deg, scale : Single; smooth : LongInt);
    SameBitmapProcedure                  = function  (const bitmap1, bitmap2 : Bitmap) : Boolean;
    BlitSurfaceProcedure                 = procedure (srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr);
    ClearSurfaceProcedure                = procedure (dest : Bitmap; toColor : Color);
    OptimiseBitmapProcedure              = procedure (surface : Bitmap);
    SaveBitmapProcedure                  = procedure (src : Bitmap; filepath : String);
    SetNonAlphaPixelsProcedure           = procedure (bmp: Bitmap);
    
	ImagesDriverRecord = record
	  InitBitmapColors            : InitBitmapColorsProcedure;
	  SurfaceExists               : SurfaceExistsProcedure;
	  CreateBitmap                : CreateBitmapProcedure;
	  DoLoadBitmap                : DoLoadBitmapProcedure;
	  FreeSurface                 : FreeSurfaceProcedure;
	  MakeOpaque                  : MakeOpaqueProcedure;
	  SetOpacity                  : SetOpacityProcedure;
	  MakeTransparent             : MakeTransparentProcedure;
	  RotateScaleSurface          : RotateScaleSurfaceProcedure;
	  SameBitmap                  : SameBitmapProcedure;
	  BlitSurface                 : BlitSurfaceProcedure;
	  ClearSurface                : ClearSurfaceProcedure;
	  OptimiseBitmap              : OptimiseBitmapProcedure;
	  SaveBitmap                  : SaveBitmapProcedure;
	  SetNonAlphaPixels           : SetNonAlphaPixelsProcedure;
	end;
	
	var
		ImagesDriver : ImagesDriverRecord;
		
implementation
  uses
    {$IFDEF SWINGAME_OPENGL}
      sgDriverImagesOpenGL
      
    {$ELSE}
      {$IFDEF SWINGAME_SDL13}
        sgDriverImagesSDL13
      {$ELSE}
        sgDriverImagesSDL
      {$ENDIF}
    {$ENDIF};
    
	procedure LoadDefaultImagesDriver();
	begin
	  {$IFDEF SWINGAME_OPENGL }
		  LoadOpenGLImagesDriver();
    {$ELSE}
      {$IFDEF SWINGAME_SDL13}
        LoadSDL13ImagesDriver();
      {$ELSE}
        // WriteLn('SDL 1.3 Not Defined');
        LoadSDLImagesDriver();
      {$ENDIF}
		{$ENDIF}
	end;
	
	procedure DefaultSetNonAlphaPixelsProcedure(bmp : Bitmap);
	begin
	 LoadDefaultImagesDriver();
	 ImagesDriver.SetNonAlphaPixels(bmp);
	end;
	
	procedure DefaultInitBitmapColorsProcedure(bmp : Bitmap);
	begin	  
	  LoadDefaultImagesDriver();
	  ImagesDriver.InitBitmapColors(bmp);
	end;
	
	function DefaultSurfaceExistsProcedure(bmp : Bitmap) : Boolean;
	begin
	  LoadDefaultImagesDriver();
	  result := ImagesDriver.SurfaceExists(bmp);
	end;
	
	procedure DefaultCreateBitmapProcedure (bmp : Bitmap; width, height : LongInt);
	begin
		LoadDefaultImagesDriver();
		ImagesDriver.CreateBitmap(bmp, width, height);
	end;  
	
	function DefaultDoLoadBitmapProcedure (filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
	begin
		LoadDefaultImagesDriver();
		result := ImagesDriver.DoLoadBitmap(filename, transparent, transparentColor);
	end;
	
	function DefaultSameBitmapProcedure(const bitmap1, bitmap2 : Bitmap) : Boolean;
  begin
    LoadDefaultImagesDriver();
    result := ImagesDriver.SameBitmap(Bitmap1, Bitmap2);
  end;
  
  procedure DefaultBlitSurfaceProcedure(srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr);
  begin
    LoadDefaultImagesDriver();
    ImagesDriver.BlitSurface(srcBmp, destBmp, srcRect, destRect);
  end;
  
  procedure DefaultFreeSurfaceProcedure(bmp : Bitmap);
  begin	  
    LoadDefaultImagesDriver();
    ImagesDriver.FreeSurface(bmp);
  end;

  procedure DefaultMakeOpaqueProcedure(bmp : Bitmap);
  begin	  
    LoadDefaultImagesDriver();
    ImagesDriver.MakeOpaque(bmp);
  end;

  procedure DefaultSetOpacityProcedure(bmp : Bitmap; pct : Single);
  begin	  
    LoadDefaultImagesDriver();
    ImagesDriver.SetOpacity(bmp, pct);
  end;

  procedure DefaultMakeTransparentProcedure(bmp : Bitmap);
  begin	  
    LoadDefaultImagesDriver();
    ImagesDriver.MakeTransparent(bmp);
  end;

  procedure DefaultRotateScaleSurfaceProcedure(resultBmp, src : Bitmap; deg, scale : Single; smooth : LongInt);
  begin	  
    LoadDefaultImagesDriver();
    ImagesDriver.RotateScaleSurface(resultBmp, src, deg, scale, smooth);
  end;

  procedure DefaultClearSurfaceProcedure(dest : Bitmap; toColor : Color);
  begin	  
    LoadDefaultImagesDriver();
    ImagesDriver.ClearSurface(dest, toColor);
  end;

  procedure DefaultOptimiseBitmapProcedure(surface : Bitmap);
  begin	  
    LoadDefaultImagesDriver();
    ImagesDriver.OptimiseBitmap(surface);
  end;

  procedure DefaultSaveBitmapProcedure(src : Bitmap; filepath : String);
  begin	  
    LoadDefaultImagesDriver();
    ImagesDriver.SaveBitmap(src, filepath);
  end;

	initialization
	begin
	  ImagesDriver.InitBitmapColors           := @DefaultInitBitmapColorsProcedure;
	  ImagesDriver.SurfaceExists              := @DefaultSurfaceExistsProcedure;
		ImagesDriver.CreateBitmap               := @DefaultCreateBitmapProcedure;
		ImagesDriver.DoLoadBitmap               := @DefaultDoLoadBitmapProcedure;
		ImagesDriver.SameBitmap                 := @DefaultSameBitmapProcedure;
		ImagesDriver.BlitSurface                := @DefaultBlitSurfaceProcedure;
		ImagesDriver.FreeSurface                := @DefaultFreeSurfaceProcedure;
		ImagesDriver.MakeOpaque                 := @DefaultMakeOpaqueProcedure;
		ImagesDriver.SetOpacity                 := @DefaultSetOpacityProcedure;
		ImagesDriver.MakeTransparent            := @DefaultMakeTransparentProcedure;
	  ImagesDriver.RotateScaleSurface         := @DefaultRotateScaleSurfaceProcedure;
	  ImagesDriver.ClearSurface               := @DefaultClearSurfaceProcedure;
	  ImagesDriver.OptimiseBitmap             := @DefaultOptimiseBitmapProcedure;
	  ImagesDriver.SaveBitmap                 := @DefaultSaveBitmapProcedure;
	  ImagesDriver.SetNonAlphaPixels          := @DefaultSetNonAlphaPixelsProcedure;
	end;
end.
	
