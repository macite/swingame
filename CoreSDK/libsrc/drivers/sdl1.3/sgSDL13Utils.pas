// Add shared util functions for SDL2 here.
//
unit sgSDL13Utils;

interface
uses sgTypes, SDL2;

  type	  
    SDL13Surface = record
      surface : PSDL_Surface;
      texture : PSDL_Texture;
      optimised  : Boolean;
    end;
    
    SDL13Screen = record
      window : PSDL_Window;
      renderer : PSDL_Renderer;
    end;
    
    PSDL13Screen = ^SDL13Screen;
    PSDL13Surface = ^SDL13Surface;
  
	function  GetSurface(bmp: Bitmap): PSDL_Surface;

	procedure RecreateTexture(bmp: Bitmap);
	procedure RenderTempSurface(bmp: Bitmap; srcRect, destRect: PSDL_Rect);
	function SDL_Swap32(D: Uint32): Uint32;

implementation
uses sgShared, sgDriverSDL13;

  function GetSurface(bmp : Bitmap) : PSDL_Surface;
  begin
    if not Assigned(bmp) or not Assigned(bmp^.surface) then
      result := nil
    else 
      result := PSDL13Surface(bmp^.surface)^.surface;
  end;
  
	procedure RecreateTexture(bmp: Bitmap);
	var
		bmpData: PSDL13Surface;
	begin
		bmpData := PSDL13Surface(bmp^.surface);
		if not Assigned(bmpData) then exit;

		if Assigned(bmpData^.texture) then
		begin
			SDL_DestroyTexture(bmpData^.texture);
		end;
	    
	    bmpData^.texture := SDL_CreateTextureFromSurface(PSDL13Screen(_screen)^.renderer, bmpData^.surface);
	end;

	procedure RenderTempSurface(bmp: Bitmap; srcRect, destRect: PSDL_Rect); 
	var
		texture : PSDL_Texture;
		renderer : PSDL_Renderer;
	begin
		renderer := PSDL13Screen(_screen)^.renderer;
	    texture := SDL_CreateTextureFromSurface(renderer, GetSurface(bmp));
		
		SDL_RenderCopy(renderer, texture, srcRect, destRect);
		
		SDL_DestroyTexture(texture);
	end;

	function SDL_Swap32(D: Uint32): Uint32;
	begin
  	Result := ((D shl 24) or ((D shl 8) and $00FF0000) or ((D shr 8) and $0000FF00) or (D shr 24));
	end;

end.