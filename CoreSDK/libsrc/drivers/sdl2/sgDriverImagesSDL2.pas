unit sgDriverImagesSDL2;

interface
	
	procedure LoadSDL2ImagesDriver();
		
implementation
	uses sgDriverSDL2Types, sgTypes, sgDriverImages, sgShared, sgSavePNG;

	//TODO: remove this
	procedure InitBitmapColorsProcedure(bmp : Bitmap);
	begin   
	end;
	
	function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
	begin
		result := Assigned(bmp) and Assigned(bmp^.surface);
	end;
	
	procedure CreateBitmapProcedure (bmp : Bitmap; width, height : LongInt);
	var
		surface: ^sg_drawing_surface;
	begin
		if Assigned(bmp^.surface) then exit;

		New(surface);
		surface^ := _sg_functions^.image.create_bitmap(width, height);
		bmp^.surface := surface;	// bmp owns surface
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

	procedure SetNonAlphaPixelsProcedure(bmp : Bitmap); 
	var
		surface: ^sg_drawing_surface;
		pixels: array of Longword;
		sz: Longint;
	begin
		surface := bmp^.surface;
		sz := bmp^.width * bmp^.height;
		SetLength(pixels, sz);
		_sg_functions^.graphics.to_pixels(surface, @pixels[0], sz);
		SetNonAlphaPixels(bmp, pixels);
		SetLength(pixels, 0);
	end;

	function DoLoadBitmapProcedure (filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
	var
		surface: ^sg_drawing_surface;
	begin
		result := nil;

		new(surface);
		surface^ := _sg_functions^.image.load_bitmap(PChar(filename));

		if not Assigned(surface) then exit;

		new(result);
		result^.surface := surface;
		result^.width := surface^.width;
		result^.height := surface^.height;

		SetNonAlphaPixelsProcedure(result);
	end;
	
	function SameBitmapProcedure(const bitmap1, bitmap2 : Bitmap) : Boolean;
	begin
		result := bitmap1^.surface = bitmap2^.surface;
	end;
	
	//TODO: rename to DrawImage
	procedure BlitSurfaceProcedure(src: Bitmap; x, y: Single; const opts : DrawingOptions);
	var
		srcData: array [0..3] of Single;
		dstData: array [0..6] of Single;
		flip : sg_renderer_flip;
	begin
		if not (opts.isPart) then
			begin
				srcData[0] := 0;
				srcData[1] := 0;
				srcData[2] := src^.width;
				srcData[3] := src^.height;
			end
		else
			begin
				srcData[0] := opts.part.x;
				srcData[1] := opts.part.y;
				srcData[2] := opts.part.width;
				srcData[3] := opts.part.height;
			end;

		//
		if (opts.flipX) and (opts.flipY)then
			flip := SG_FLIP_BOTH
		else
			if (opts.flipX) then
				flip := SG_FLIP_VERTICAL
			else
				if (opts.flipY) then
					flip := SG_FLIP_HORIZONTAL
				else
					flip := SG_FLIP_NONE;

			// make up dst data
		dstData[0] := x; // X
		dstData[1] := y; // Y
		dstData[2] := opts.angle; // Angle
		dstData[3] := opts.anchoroffsetX; // Centre X
		dstData[4] := opts.anchoroffsetY; // Centre Y
		dstData[5] := opts.scaleX; // Scale X
		dstData[6] := opts.scaleY; // Scale Y
		

		_sg_functions^.image.draw_bitmap(src^.surface, opts.dest^.surface, @srcData[0], Length(srcData), @dstData[0], Length(dstData), flip);
	end;
	
	procedure FreeSurfaceProcedure(bmp : Bitmap);
	begin
		_sg_functions^.graphics.close_drawing_surface(bmp^.surface);
		bmp^.surface := nil;   
	end;

	// TODO: remove
	procedure MakeOpaqueProcedure(bmp : Bitmap);
	begin   
	end;

	// TODO: remove
	procedure SetOpacityProcedure(bmp : Bitmap; pct : Single);
	begin   
	end;

	// TODO: remove
	procedure MakeTransparentProcedure(bmp : Bitmap);
	begin   
	end;

	// TODO: remove - adjust drawing code
	procedure RotateScaleSurfaceProcedure(resultBmp, src : Bitmap; deg, scale : Single; smooth : LongInt);
	begin   
	end;

	procedure ClearSurfaceProcedure(dest : Bitmap; toColor : Color);
	begin   
		_sg_functions^.graphics.clear_drawing_surface(dest^.surface, _ToSGColor(toColor));
	end;

	// TODO: remove
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
	
