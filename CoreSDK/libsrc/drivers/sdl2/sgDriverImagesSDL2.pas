unit sgDriverImagesSDL2;

interface
	
	procedure LoadSDL2ImagesDriver();
		
implementation
  uses sgDriverSDL2Types, sgTypes, sgDriverImages, sgShared, sgSavePNG;

	procedure SetNonAlphaPixelsProcedure(bmp : Bitmap);
	begin
	end;
	
	//TODO: remove this
	procedure InitBitmapColorsProcedure(bmp : Bitmap);
	begin	  
	end;
	
	function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
	begin
	  result := Assigned(bmp) and Assigned(bmp^.surface);
	end;
	
	procedure CreateBitmapProcedure (bmp : Bitmap; width, height : LongInt);
	begin
	end;  
	
	//TODO: move to SwinGame
	procedure SetNonAlphaPixels(bmp : Bitmap; const pixels: array of Longword); 
	var
		r, c: Longint;
	begin
		if not ( Assigned(bmp) ) then exit;

		SetLength(bmp^.nonTransparentPixels, bmp^.width, bmp^.height);

	  for r := 0 to bmp^.height - 1 do
	  begin
			for c := 0 to bmp^.width - 1 do
			begin
		    bmp^.nonTransparentPixels[c, r] := ((pixels[c + r * bmp^.width] and $000000FF) > 0);
		  end;
		end;
	end;

	function DoLoadBitmapProcedure (filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
	var
		surface: ^sg_drawing_surface;
    pixels: array of Longword;
    sz: Longint;
  begin
		result := nil;

		new(surface);
		surface^ := _sg_functions^.image.load_bitmap(PChar(filename));

		if not Assigned(surface) then exit;

		new(result);
		result^.surface := surface;
		result^.width := surface^.width;
		result^.height := surface^.height;

    sz := result^.width * result^.height;
    SetLength(pixels, sz);
    _sg_functions^.graphics.to_pixels(surface, @pixels[0], sz);
		SetNonAlphaPixels(result, pixels);
		SetLength(pixels, 0);
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
  	_sg_functions^.graphics.clear_drawing_surface(dest^.surface, _ToSGColor(toColor));
  end;

  procedure OptimiseBitmapProcedure(surface : Bitmap);
  begin	  
  end;

  procedure SaveBitmapProcedure(bmpToSave: Bitmap; path : String);
  var
    pixels: array of LongInt;
    sz: Longint;
  begin
    sz := bmpToSave^.width * bmpToSave^.height;
    SetLength(pixels, sz);
    _sg_functions^.graphics.to_pixels(bmpToSave^.surface, @pixels[0], sz);
    png_save_pixels(path, @pixels[0], bmpToSave^.width, bmpToSave^.height);
    SetLength(pixels, 0);
  end;

	procedure LoadSDL2ImagesDriver();
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
	
