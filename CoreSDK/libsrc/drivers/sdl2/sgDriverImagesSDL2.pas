unit sgDriverImagesSDL2;

interface
	
	procedure LoadSDL2ImagesDriver();
		
implementation
	uses sgDriverSDL2Types, sgTypes, sgDriverImages, sgShared, sgBackendTypes;

	//TODO: remove this
	procedure InitBitmapColorsProcedure(bmp : Bitmap);
	begin   
	end;
	
	function SurfaceExistsProcedure(bmp : Bitmap) : Boolean;
	var
		b: BitmapPtr;
	begin
		b := ToBitmapPtr(bmp);

		result := Assigned(b) and Assigned(b^.surface);
	end;
	
	procedure CreateBitmapProcedure (bmp : Bitmap; width, height : LongInt);
	var
		surface: ^sg_drawing_surface;
		b: BitmapPtr;
	begin
		b := ToBitmapPtr(bmp);
		if Assigned(b^.surface) then exit;

		New(surface);
		surface^ := _sg_functions^.image.create_bitmap(width, height);
		b^.surface := surface;	// bmp owns surface
	end;  
	
	//TODO: move to SwinGame
	procedure SetNonAlphaPixels(bmp : Bitmap; const pixels: array of Longword); 
	var
		r, c: Longint;
		b: BitmapPtr;
	begin
		b := ToBitmapPtr(bmp);
		if not ( Assigned(b) ) then exit;

		SetLength(b^.nonTransparentPixels, b^.width, b^.height);

		// WriteLn(b^.name);
		for r := 0 to b^.height - 1 do
		begin
			for c := 0 to b^.width - 1 do
			begin
				b^.nonTransparentPixels[c, r] := ((pixels[c + r * b^.width] and $000000FF) > $0000008F );
				// if b^.nonTransparentPixels[c, r] then Write(1) else Write(0);
			end;
			// WriteLn();
		end;
		// WriteLn();
	end;

	procedure SetNonAlphaPixelsProcedure(bmp : Bitmap); 
	var
		surface: ^sg_drawing_surface;
		pixels: array of Longword;
		sz: Longint;
		b: BitmapPtr;
	begin
		b := ToBitmapPtr(bmp);

		surface := b^.surface;
		sz := b^.width * b^.height;
		SetLength(pixels, sz);
		_sg_functions^.graphics.to_pixels(surface, @pixels[0], sz);
		SetNonAlphaPixels(bmp, pixels);
		SetLength(pixels, 0);
	end;

	function DoLoadBitmapProcedure (const filename: String; transparent: Boolean; transparentColor: Color): Bitmap;
	var
		surface: ^sg_drawing_surface;
		b: BitmapPtr;
	begin
		result := nil;

		new(surface);
		surface^ := _sg_functions^.image.load_bitmap(PChar(filename));

		if not Assigned(surface) then exit;

		new(b);
		b^.id := BITMAP_PTR;
		b^.surface := surface;
		b^.width := surface^.width;
		b^.height := surface^.height;

		result := b;

		SetNonAlphaPixelsProcedure(result);
	end;
	
	function SameBitmapProcedure(const bitmap1, bitmap2 : Bitmap) : Boolean;
	var
		b1, b2: BitmapPtr;
	begin
		b1 := ToBitmapPtr(bitmap1);
		b2 := ToBitmapPtr(bitmap2);

		result := (b1^.surface = b2^.surface);
	end;
	
	//TODO: rename to DrawImage
	procedure _BlitSurfaceProcedure(src: BitmapPtr; x, y: Single; const opts : DrawingOptions);
	var
		srcData: array [0..3] of Single;
		dstData: array [0..6] of Single;
		flip : sg_renderer_flip;
		dest: BitmapPtr;
	begin
		if not Assigned(src) then exit;

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
		
		XYFromOpts(opts, dstData[0], dstData[1]); // Camera?

		dest := ToBitmapPtr(opts.dest);
		_sg_functions^.image.draw_bitmap(src^.surface, dest^.surface, @srcData[0], Length(srcData), @dstData[0], Length(dstData), flip);
	end;

	procedure BlitSurfaceProcedure(src: Bitmap; x, y: Single; const opts : DrawingOptions);
	begin
		_BlitSurfaceProcedure(ToBitmapPtr(src), x, y, opts);
	end;
	
	procedure FreeSurfaceProcedure(bmp : Bitmap);
	var
		b: BitmapPtr;
	begin
		b := ToBitmapPtr(bmp);

		_sg_functions^.graphics.close_drawing_surface(b^.surface);
		
		b^.surface := nil;   
		b^.id := NONE_PTR;
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
	var
		d: BitmapPtr;
	begin
		d := ToBitmapPtr(dest);
		if not Assigned(d) then exit;
		_sg_functions^.graphics.clear_drawing_surface(d^.surface, _ToSGColor(toColor));
	end;

	// TODO: remove
	procedure OptimiseBitmapProcedure(surface : Bitmap);
	begin   
	end;

	procedure SaveBitmapProcedure(bmpToSave: Bitmap; const path: String);
	var
		b: BitmapPtr;
	begin
		b := ToBitmapPtr(bmpToSave);
		if not Assigned(b) then exit;
		_sg_functions^.graphics.save_png(b^.surface, PChar(path));
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
	
