unit sgDriverImages;

interface
uses sgDriverSDL2Types, sgTypes, sgBackendTypes;

  function CreateBitmap (const name: String; width, height : LongInt): BitmapPtr;
  function LoadBitmap (const name, filename: String): BitmapPtr;

  procedure FreeBitmap(bmp : BitmapPtr);

  procedure SetupCollisionMask(bmp : BitmapPtr); 

  procedure DrawBitmap(src: BitmapPtr; x, y: Single; const opts : DrawingOptions);
  
  procedure ClearSurface(dest : psg_drawing_surface; toColor : Color);
  procedure SaveSurface(src: psg_drawing_surface; const path: String);
    
implementation
  uses sgShared;
  
  function CreateBitmap (const name: String; width, height : LongInt): BitmapPtr;
  begin
    New(result);
    result^.image.surface := _sg_functions^.image.create_bitmap(width, height);
    SetLength(result^.image.clipStack, 0);

    result^.id := BITMAP_PTR;

    result^.cellW     := width;
    result^.cellH     := height;
    result^.cellCols  := 1;
    result^.cellRows  := 1;
    result^.cellCount := 1;
    
    result^.name      := name;
    result^.filename  := '';
  end;  
  
  function LoadBitmap (const name, filename: String): BitmapPtr;
  var
    surface: sg_drawing_surface;
  begin
    result := nil;

    surface := _sg_functions^.image.load_bitmap(PChar(filename));
    if not Assigned(surface._data) then exit;

    New(result);
    result^.image.surface := surface;

    result^.id      := BITMAP_PTR;

    result^.cellW     := surface.width;
    result^.cellH     := surface.height;
    result^.cellCols  := 1;
    result^.cellRows  := 1;
    result^.cellCount := 1;

    SetLength(result^.image.clipStack, 0);
    
    result^.name      := name;
    result^.filename  := filename;

    SetupCollisionMask(result);
  end;

  procedure SetupCollisionMask(bmp : BitmapPtr); 
  var
    pixels: array of Longword;
    sz: Longint;
    r, c: Longint;
  begin
    sz := bmp^.image.surface.width * bmp^.image.surface.height;
    SetLength(pixels, sz);

    _sg_functions^.graphics.to_pixels(@bmp^.image.surface, @pixels[0], sz);

    SetLength(bmp^.nonTransparentPixels, bmp^.image.surface.width, bmp^.image.surface.height);

    // WriteLn(bmp^.name);
    for r := 0 to bmp^.image.surface.height - 1 do
    begin
      for c := 0 to bmp^.image.surface.width - 1 do
      begin
        bmp^.nonTransparentPixels[c, r] := ((pixels[c + r * bmp^.image.surface.width] and $000000FF) > $0000007F );
        // if b^.nonTransparentPixels[c, r] then Write(1) else Write(0);
      end;
      // WriteLn();
    end;
    // WriteLn();

    SetLength(pixels, 0);
  end;  
  
  //TODO: rename to DrawImage
  procedure DrawBitmap(src: BitmapPtr; x, y: Single; const opts : DrawingOptions);
  var
    srcData: array [0..3] of Single;
    dstData: array [0..6] of Single;
    flip : sg_renderer_flip;
    dest: psg_drawing_surface;
  begin
    if not Assigned(src) then exit;

    if not (opts.isPart) then
      begin
        srcData[0] := 0;
        srcData[1] := 0;
        srcData[2] := src^.image.surface.width;
        srcData[3] := src^.image.surface.height;
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

    dest := ToSurfacePtr(opts.dest);
    _sg_functions^.image.draw_bitmap(@src^.image.surface, dest, @srcData[0], Length(srcData), @dstData[0], Length(dstData), flip);
  end;
  
  procedure FreeBitmap(bmp : BitmapPtr);
  begin
    _sg_functions^.graphics.close_drawing_surface(@bmp^.image.surface);
    
    bmp^.id := NONE_PTR;
  end;

  procedure ClearSurface(dest : psg_drawing_surface; toColor : Color);
  begin
    _sg_functions^.graphics.clear_drawing_surface(dest, _ToSGColor(toColor));
  end;

  procedure SaveSurface(src: psg_drawing_surface; const path: String);
  begin
    _sg_functions^.graphics.save_png(src, PChar(path));
  end;
end.
  
