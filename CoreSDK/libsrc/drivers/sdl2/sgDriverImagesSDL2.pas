unit sgDriverImagesSDL2;

interface
	
	procedure LoadSDL2ImagesDriver();
		
implementation
  uses sgDriverSDL2Types;

	procedure SetNonAlphaPixelsProcedure(bmp : Bitmap);
	begin
	end;
	
	procedure InitBitmapColorsProcedure(bmp : Bitmap);
	begin	  
	end;
	
	function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
	begin
	  result := false;
	end;
	
	procedure CreateBitmapProcedure (bmp : Bitmap; width, height : LongInt);
	begin
	end;  
	
	function DoLoadBitmapProcedure (filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
	begin
		result := nil;
	end;
	
	function SameBitmapProcedure(const bitmap1, bitmap2 : Bitmap) : Boolean;
  begin
    result := false;
  end;
  
  procedure BlitSurfaceProcedure(srcBmp, destBmp : Bitmap; srcRect, destRect : RectPtr);
  begin
  end;
  
  procedure FreeSurfaceProcedure(bmp : Bitmap);
  begin	  
  end;

  procedure MakeOpaqueProcedure(bmp : Bitmap);
  begin	  
  end;

  procedure SetOpacityProcedure(bmp : Bitmap; pct : Single);
  begin	  
  end;

  procedure MakeTransparentProcedure(bmp : Bitmap);
  begin	  
  end;

  procedure RotateScaleSurfaceProcedure(resultBmp, src : Bitmap; deg, scale : Single; smooth : LongInt);
  begin	  
  end;

  procedure ClearSurfaceProcedure(dest : Bitmap; toColor : Color);
  begin	  
  end;

  procedure OptimiseBitmapProcedure(surface : Bitmap);
  begin	  
  end;

  procedure SaveBitmapProcedure(src : Bitmap; filepath : String);
  begin	  
  end;

	procedure LoadSDL2ImagesDriver()
	begin
	  ImagesDriver.InitBitmapColors           := @InitBitmapColorsProcedure;
	  ImagesDriver.SurfaceExists              := @SurfaceExistsProcedure;
		ImagesDriver.CreateBitmap               := @CreateBitmapProcedure;
		ImagesDriver.DoLoadBitmap               := @DoLoadBitmapProcedure;
		ImagesDriver.SameBitmap                 := @SameBitmapProcedure;
		ImagesDriver.BlitSurface                := @BlitSurfaceProcedure;
		ImagesDriver.FreeSurface                := @FreeSurfaceProcedure;
		ImagesDriver.MakeOpaque                 := @MakeOpaqueProcedure;
		ImagesDriver.SetOpacity                 := @SetOpacityProcedure;
		ImagesDriver.MakeTransparent            := @MakeTransparentProcedure;
	  ImagesDriver.RotateScaleSurface         := @RotateScaleSurfaceProcedure;
	  ImagesDriver.ClearSurface               := @ClearSurfaceProcedure;
	  ImagesDriver.OptimiseBitmap             := @OptimiseBitmapProcedure;
	  ImagesDriver.SaveBitmap                 := @SaveBitmapProcedure;
	  ImagesDriver.SetNonAlphaPixels          := @SetNonAlphaPixelsProcedure;
	end;
end.
	
