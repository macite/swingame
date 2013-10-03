unit sdl13Update;

interface
  uses SDL2;

  function SDL_SaveBMP(surface: PSDL_Surface; filename: PChar): Integer;
  function SDL_Swap32(D: Uint32): Uint32;

implementation

  function SDL_SaveBMP(surface: PSDL_Surface; filename: PChar): Integer;
  begin
    Result := SDL_SaveBMP_RW(surface, SDL_RWFromFile(filename, 'wb'), 1);
  end;

  function SDL_Swap32(D: Uint32): Uint32;
  begin
    Result := ((D shl 24) or ((D shl 8) and $00FF0000) or ((D shr 8) and $0000FF00) or (D shr 24));
  end;
end.