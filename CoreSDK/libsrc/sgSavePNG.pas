unit sgSavePNG;

interface
uses {$IFDEF SWINGAME_SDL13}SDL2{$ELSE}SDL{$ENDIF};

function png_save_surface(filename: String; surf: PSDL_Surface): Boolean;

implementation
uses png, sgShared;

type PFile = ^FILE;

{$IFDEF WINDOWS}
function fopen( 
  a: pchar;
  b: pchar
):PFile; cdecl; external 'msvcrt.dll';

procedure fclose(
  a: PFile
); cdecl; external 'msvcrt.dll';
{$ENDIF}

{$IFDEF UNIX}
function fopen( 
  a: pchar;
  b: pchar
):PFile; cdecl; external 'libc';

procedure fclose(
  a: PFile
); cdecl; external 'libc';
{$ENDIF}

const 
  PNG_COLOR_MASK_PALETTE    = 1;
  PNG_COLOR_MASK_COLOR      = 2;
  PNG_COLOR_MASK_ALPHA      = 4;
  
  PNG_COLOR_TYPE_RGB        = PNG_COLOR_MASK_COLOR;
  
  PNG_INTERLACE_NONE        = 0; // Non-interlaced image
  
  PNG_COMPRESSION_TYPE_BASE     = 0; // Deflate method 8, 32K window
  PNG_COMPRESSION_TYPE_DEFAULT  = PNG_COMPRESSION_TYPE_BASE;
  
  PNG_FILTER_TYPE_BASE      = 0; // Single row per-byte filtering
  PNG_FILTER_TYPE_DEFAULT   = PNG_FILTER_TYPE_BASE;
  

// static int png_colortype_from_surface(SDL_Surface *surface)
// {
//  int colortype = PNG_COLOR_MASK_COLOR; /* grayscale not supported */
// 
//  if (surface->format->palette)
//    colortype |= PNG_COLOR_MASK_PALETTE;
//  else if (surface->format->Amask)
//    colortype |= PNG_COLOR_MASK_ALPHA;
//    
//  return colortype;
// }

function png_colortype_from_surface(surface: PSDL_Surface): Longint;
begin
  result := PNG_COLOR_TYPE_RGB; // grayscale not supported
  
  if not assigned(surface) then exit;
  
  if assigned(surface^.format^.palette) then
    result := result or PNG_COLOR_MASK_PALETTE
  else if surface^.format^.Amask <> 0 then
    result := result or PNG_COLOR_MASK_ALPHA;
end;


// void png_user_warn(png_structp ctx, png_const_charp str)
// {
//   fprintf(stderr, "libpng: warning: %s\n", str);
// }

// void png_user_error(png_structp ctx, png_const_charp str)
// {
//   fprintf(stderr, "libpng: error: %s\n", str);
// }


procedure png_user_warn(ctx: png_structp; str: png_const_charp); cdecl;
begin
  WriteLn(stderr, 'libpng: warning: ', str);
end;

procedure png_user_error(ctx: png_structp; str: png_const_charp); cdecl;
begin
  WriteLn(stderr, 'libpng: error: ', str);
end;

// int png_save_surface(char *filename, SDL_Surface *surf)
// {
//   FILE *fp;
//   png_structp png_ptr;
//   png_infop info_ptr;
//   int i, colortype;
//   png_bytep *row_pointers;
// 
//   /* Opening output file */
//   fp = fopen(filename, "wb");
//   if (fp == NULL) {
//     perror("fopen error");
//     return -1;
//   }


function png_save_surface(filename: String; surf: PSDL_Surface): Boolean;
var
  fp:           Pointer;
  png_ptr:      png_structp;
  info_ptr:     png_infop;
  i, colortype: Longint;
  row_pointers: pppng_byte;
begin
  result := false;
  
  try
    // Opening output file
    fp := fopen(PChar(filename), 'wb');
    
    try
      // Initializing png structures and callbacks
      png_ptr := png_create_write_struct(PNG_LIBPNG_VER_STRING, nil, @png_user_error, @png_user_warn);
      if not assigned(png_ptr) then
      begin
        //printf("png_create_write_struct error!\n");
        result := false;
        exit;
      end;
      
      info_ptr := png_create_info_struct(png_ptr);
      if not assigned(info_ptr) then
      begin
        png_destroy_write_struct(@png_ptr, nil);
        //printf("png_create_info_struct error!\n");
        result := false;
        exit;
      end;
      
      if setjmp(png_ptr^.jmpbuf) <> 0 then
      begin
        png_destroy_write_struct(@png_ptr, @info_ptr);
        result := false;
        exit;
      end;
      
      png_init_io(png_ptr, fp);
      
      colortype := png_colortype_from_surface(surf);
      png_set_IHDR( png_ptr, info_ptr, 
                    surf^.w, surf^.h, 8, colortype, 
                    PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
      
      // Writing the image
      png_write_info(png_ptr, info_ptr);
      png_set_packing(png_ptr);
      png_set_bgr(png_ptr);
      
      GetMem(row_pointers, sizeof(png_bytep) * surf^.h);
      
      for i := 0 to surf^.h - 1 do
        row_pointers[i] := png_bytep(PUInt8(surf^.pixels) + i * surf^.pitch);
      
      png_write_image(png_ptr, row_pointers);
      png_write_end(png_ptr, info_ptr);
      
      // Cleaning out...
      Dispose(row_pointers);
      png_destroy_write_struct(@png_ptr, @info_ptr);
    finally
      // Close the file...
      fclose(fp);
    end;
    
    result := true;
  except
    //Errors...
  end;
end;

end.