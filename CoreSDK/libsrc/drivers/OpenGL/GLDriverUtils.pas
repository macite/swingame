unit GLDriverUtils;

interface
  uses sgTypes, sgShared, SDL2;
  
  type
    SGOpenGLSurface = record
      surface : PSDL_Surface;
      texture : Cardinal;
    end;
    
    PSGOpenGLSurface = ^SGOpenGLSurface;
  
  function  SurfaceOf(bmp: Bitmap): PSDL_Surface;
  function  TextureOf(bmp: Bitmap): Cardinal;
  
  procedure FloatColors(c: Color; var r, g, b, a: Single);
  
  // Return a power of 2 that is greater than or equal to v.
  function Pow2GEQ(v: Longint) : Longint;
  
  procedure RenderTexture(tex: Cardinal; ratioX, ratioY, ratioW, ratioH : Single; const destRect : RectPtr); 
  procedure SetColor(clr : Color);
  
  function  CreateSurfaceData(surf: PSDL_Surface) : Pointer;
  procedure CreateGLTexture(bmp: Bitmap);

implementation
  uses {$IFDEF IOS}gles11{$ELSE}gl, glext{$ENDIF}, sgDriverGraphics;
  
  procedure CreateGLTexture(bmp: Bitmap);
  var
    lLoadedImg: PSDL_Surface;
    tex: Cardinal;
  begin
    lLoadedImg := SurfaceOf(bmp);
    if not Assigned(lLoadedImg) then exit;
    
    if ( lLoadedImg^.w  <= 0 ) or ( lLoadedImg^.h <= 0 ) then
    begin
      RaiseWarning('Loaded bitmap with 0 width and/or height.');
      exit;
    end;
    
    tex := TextureOf(bmp);
    
    if tex = 0 then
    begin
      // Have OpenGL generate a texture object handle for us
      glGenTextures( 1, @tex );
      PSGOpenGLSurface(bmp^.surface)^.texture := tex;
    end;
    
    // Bind the texture object
    glBindTexture( GL_TEXTURE_2D, tex );
    
    //glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    // glBlendFunc( GL_ONE, GL_ZERO );
    
    // Just copy the data to the surface
    glDisable( GL_BLEND );
    
    // Set the texture's stretching properties
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    
    // Edit the texture object's image data using the information SDL_Surface gives us
    glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, lLoadedImg^.w, lLoadedImg^.h, 0, 
                  GL_RGBA, GL_UNSIGNED_BYTE, lLoadedImg^.pixels );
    
    // Switch to the 0 texture
    glBindTexture( GL_TEXTURE_2D, 0 );
    glEnable( GL_BLEND );
  end;
  
  function  CreateSurfaceData(surf: PSDL_Surface) : Pointer;
  var
    data: PSGOpenGLSurface;
  begin
    New(data);
    
    data^.surface := surf;
    data^.texture := 0;
    
    result := data;
  end;
  
  function  SurfaceOf(bmp: Bitmap): PSDL_Surface;
  begin
    if not (Assigned(bmp) and Assigned(bmp^.surface)) then result := nil
    else result := PSGOpenGLSurface(bmp^.surface)^.surface;
  end;
  
  function  TextureOf(bmp: Bitmap): Cardinal;
  begin
    if (not Assigned(bmp)) or (not Assigned(bmp^.surface)) then result := 0
    else
    begin
      result := PSGOpenGLSurface(bmp^.surface)^.texture;
    end;
  end;
  
  procedure SetColor(clr : Color);
  var
    r,g,b,a : Byte;
  begin
    GraphicsDriver.ColorComponents(clr,r,g,b,a);
    glColor4f(r/255, g/255, b/255, a/255);
  end;

  procedure FloatColors(c: Color; var r, g, b, a: Single);
  begin
    // writeln(c, ' = ', IntToHex(c, 8));
    // writeln(IntToHex(c and $FF000000, 8), ' -> ', IntToHex((c and $FF000000) shr 24, 8));
    a := (c and $FF000000 shr 24) / 255;
    r := (c and $00FF0000 shr 16) / 255;
    g := (c and $0000FF00 shr 8) / 255;
    b := (c and $000000FF) / 255;
  end;

  function Pow2GEQ(v: Longint) : Longint;
  begin
    result := 1;
    while result < v do
    begin
      result := result shl 1; // result *= 2
    end;
  end;

  // Render the srcRect part of the tex texture, to the dest rectangle
  procedure RenderTexture(tex: Cardinal; ratioX, ratioY, ratioW, ratioH : Single; const destRect : RectPtr); 
  var
    //lTexture : GLuint;
    textureCoord : Array[0..3] of Point2D;
    vertices : Array[0..3] of Point2D;
  begin
    //set up texture co-ords
    textureCoord[0].x := ratioX;      textureCoord[0].y := ratioY;
    textureCoord[1].x := ratioX;      textureCoord[1].y := ratioH;
    textureCoord[2].x := ratioW;      textureCoord[2].y := ratioY;
    textureCoord[3].x := ratioW;      textureCoord[3].y := ratioH;

    //set up vertices co-ords
    vertices[0].x := destRect^.x;                         vertices[0].y := destRect^.y;
    vertices[1].x := destRect^.x;                         vertices[1].y := destRect^.y + destRect^.height;
    vertices[2].x := destRect^.x + destRect^.width;       vertices[2].y := destRect^.y;
    vertices[3].x := destRect^.x + destRect^.width;       vertices[3].y := destRect^.y + destRect^.height;
    
    //enable vertex and texture array
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    glBindTexture( GL_TEXTURE_2D, tex );
    
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glTexCoordPointer(2, GL_FLOAT, 0, @textureCoord[0]);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, Length(vertices));
    
    // Finish with texture
    glBindTexture( GL_TEXTURE_2D, 0);
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  end;

end.