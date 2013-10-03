unit sgDriverGraphicsOpenGL;
//=============================================================================
// sgDriverGraphicsSDL.pas
//=============================================================================
//
//
//
// 
//
// Notes:
//    - Pascal PChar is equivalent to a C-type string
//    - Pascal Word is equivalent to a Uint16
//    - Pascal LongWord is equivalent to a Uint32
//    - Pascal SmallInt is equivalent to Sint16
//
//=============================================================================
interface
uses sgTypes, SDL2, SysUtils {$IFDEF IOS},gles11;{$ELSE},gl, glext;{$ENDIF}

  type
    OpenGLWindow = record
        window  : PSDL_window;
        context : PSDL_GLContext;  
    end;
    
    POpenGLWindow = ^OpenGLWindow;



  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
  procedure LoadOpenGLGraphicsDriver();
  
implementation
  uses sgDriverGraphics, sgShared, Math, sgDriverImages, sgGraphics, GLDriverUtils;  

  var
    _screenWidth, _screenHeight : LongInt;
  
  
  function RGBAColorProcedure(red, green, blue, alpha: byte) : Color; 
  begin
    result := 0;
  end;

  function GetPixel32Procedure(bmp: Bitmap; x, y: Longint) :Color;
  begin
    result := 0;
  end;
    
  procedure PutPixelProcedure(bmp: Bitmap; clr: Color; x, y: Longint);
  begin
    exit;
  end;
  
  procedure ColorComponentsProcedure(c: Color; var r, g, b, a: byte);
  begin
    // writeln(c, ' = ', IntToHex(c, 8));
    // writeln(IntToHex(c and $FF000000, 8), ' -> ', IntToHex((c and $FF000000) shr 24, 8));
    a := c and $FF000000 shr 24;
    r := c and $00FF0000 shr 16;
    g := c and $0000FF00 shr 8;
    b := c and $000000FF;
  end;
  
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
  begin
    result.x := 0;
    result.y := 0;
    result.w := Word(0);
    result.h := Word(0);
  end;  
  
  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  begin
      result := NewSDLRect(Round(r.x), Round(r.y), r.width, r.height);
  end;
  
  procedure SetRenderDrawColor(c : Color);
  begin
    exit;
  end;
  



  
  
  procedure GetLengthFromPoints(pos : array of Single; var lnth : LongInt; var lowest : Single );
  begin
    exit;
  end;




  procedure PresentAndFreeShapes(surface : PSDL_Surface; srcRect, destRect : SDL_Rect);
  begin
    exit;
  end;




  procedure DrawTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  var
    
    vertices : Array[0..2] of Point2D;
  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;
    vertices[0].x := x1;  vertices[0].y := y1;
    vertices[1].x := x2;  vertices[1].y := y2;
    vertices[2].x := x3;  vertices[2].y := y3;
    
    SetColor(clr);

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_LINE_LOOP, 0, Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;


  procedure FillTriangleProcedure(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single);
  var
    vertices : Array[0..2] of Point2D;

  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;


    
    vertices[0].x := x1;  vertices[0].y := y1;
    vertices[1].x := x2;  vertices[1].y := y2;
    vertices[2].x := x3;  vertices[2].y := y3;
    
    SetColor(clr);
    glShadeModel(GL_SMOOTH);
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_TRIANGLES, 0, Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;
  
  procedure DrawCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  var
    angle : LongInt;
    vertices : Array[0..360] of Point2D;
    rad : Single;

  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;

    SetColor(clr);
    for angle := 0 to High(vertices) do
    begin
      rad               := DegToRad(angle);
      vertices[angle].x := (xc + sin(rad) * radius);
      vertices[angle].y := (yc + cos(rad) * radius);
    end;
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_LINE_LOOP,0,Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;

  procedure FillCircleProcedure(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  var
    angle : LongInt;
    vertices : Array[0..360] of Point2D;
    rad : Single;
  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;

    SetColor(clr);
    
    for angle := 0 to High(vertices) do
    begin
      rad := DegToRad(angle);
      vertices[angle].x := (xc + sin(rad) * radius);
      vertices[angle].y := (yc + cos(rad) * radius);
    end;
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_TRIANGLE_FAN,0,Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;
  
  // // This procedure draws an Ellipse to a bitmap
  procedure FillEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
  var
    angle, width, height : LongInt;

    vertices : Array[0..360] of Point2D;
    rad : Single;
  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;

    width := halfWidth*2;
    height := halfHeight*2;

    SetColor(clr);
    
    for angle := 0 to High(vertices) do
    begin
      rad := DegToRad(angle);
      vertices[angle].x := (xPos + cos(rad)*width);
      vertices[angle].y := (yPos + sin(rad)*height);
    end;
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_TRIANGLE_FAN,0,Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;

  
  // This procedure draws an Ellipse to a bitmap
  procedure DrawEllipseProcedure(dest: Bitmap; clr: Color;  xPos, yPos, halfWidth, halfHeight: Longint);
  var
    angle, width, height : LongInt;

    vertices : Array[0..360] of Point2D;
    rad : Single;
  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;

    width := halfWidth*2;
    height := halfHeight*2;

    SetColor(clr);
    
    for angle := 0 to High(vertices) do
    begin
      rad               := DegToRad(angle);
      vertices[angle].x := (xPos + cos(rad)*width);
      vertices[angle].y := (yPos + sin(rad)*height);
    end;
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_LINE_LOOP,0,Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;
  
  // // This procedure draws a filled rectangle to a bitmap
  procedure FillRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
  var
    x1,x2,y1,y2 : LongInt;
    vertices : Array[0..3] of Point2D;
  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;
    x1 := Round(rect.x);
    x2 := x1 + rect.width;
    y1 := Round(rect.y);
    y2 := y1 + rect.height;
    vertices[0].x := x1;  vertices[0].y := y1;
    vertices[1].x := x1;  vertices[1].y := y2;
    vertices[2].x := x2;  vertices[2].y := y1;
    vertices[3].x := x2;  vertices[3].y := y2;

    SetColor(clr);

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;
  
  procedure DrawRectangleProcedure(dest : Bitmap; rect : Rectangle; clr : Color);
  var
    x1,x2,y1,y2 : LongInt;
    vertices : Array[0..3] of Point2D;
  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;
    x1 := Round(rect.x);
    x2 := x1 + rect.width;
    y1 := Round(rect.y);
    y2 := y1 + rect.height;

    vertices[0].x := x1;  vertices[0].y := y1;
    vertices[1].x := x2;  vertices[1].y := y1;
    vertices[2].x := x2;  vertices[2].y := y2;
    vertices[3].x := x1;  vertices[3].y := y2;

    SetColor(clr);

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_LINE_LOOP, 0, Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;
  
  // This procedure draws a line on a bitmap between two points
  procedure DrawLineProcedure(dest : Bitmap; x1, y1, x2, y2 : LongInt; clr : Color);
  var
    
    vertices : Array[0..1] of Point2D;
  begin
    if(dest <> Screen) then
    begin
      RaiseWarning('Drawing primitives to bitmap is currently not supported with the OpenGL driver');
      exit;
    end;
    vertices[0].x := x1;  vertices[0].y := y1;
    vertices[1].x := x2;  vertices[1].y := y2;
    
    SetColor(clr);

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
    glDrawArrays(GL_LINES, 0, Length(vertices));
    glDisableClientState(GL_VERTEX_ARRAY);
  end;
  
  // This procedure sets the color of a pixel on a bitmap
  procedure SetPixelColorProcedure(dest : Bitmap; x, y : Integer; clr : Color);
  begin
    exit;
  end;
  
  procedure SetClipRectangleProcedure(dest : Bitmap; rect : Rectangle);
  begin
    exit;
  end;
  
  procedure ResetClipProcedure(bmp : Bitmap);
  begin
    exit;
  end;

  procedure SetVideoModeFullScreenProcedure();
  begin
    exit;
  end;

  procedure SetVideoModeNoFrameProcedure();
  begin    
    // TODO: need to recreate window to switch to borderless... check further if needed
  end;
  
  // This procedure sets up the global variable (screen)
  procedure _SetupScreen(screenWidth, screenHeight: Longint);
  begin
  end;
  
  function AvailableResolutionsProcedure() : ResolutionArray;
  var
    mode : SDL_DisplayMode;
    i, modes    : LongInt;
    resolutions : ResolutionArray;
  begin
    setLength(resolutions,0);
    //built in screen = 0
    modes := SDL_GetNumDisplayModes(0);
    for i:= 0 to modes do
    begin
      SDL_GetDisplayMode(0, i, @mode);
      setLength(resolutions, length(resolutions)+1);
      resolutions[i].width       := mode.w;
      resolutions[i].height      := mode.h;
      resolutions[i].refreshRate := mode.refresh_rate;
      resolutions[i].format      := mode.format;
    end;
    result := resolutions;
    
  end;


  procedure RefreshScreenProcedure(screen : Bitmap);
  begin
    SDL_GL_SwapWindow(POpenGLWindow(_screen)^.window);
  end;

  procedure InitializeGraphicsWindowProcedure(caption: String; screenWidth, screenHeight: Longint);
  {$IFDEF IOS}
  var
    deviceResolution : ResolutionArray;
  {$ENDIF}
  begin
    _screenWidth := screenWidth;
    _screenHeight := screenHeight;
    // Initialize SDL.
    if (SDL_Init(SDL_INIT_VIDEO) < 0) then exit;
    
    New(POpenGLWindow(_screen));
    
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 1); // was 4
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
    SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
    
    // Create the window where we will draw.
    {$IFDEF IOS}
      SDL_SetHint( 'SDL_IOS_ORIENTATIONS', 'LandscapeLeft' ); 
      POpenGLWindow(_screen)^.window  := SDL_CreateWindow(PChar(caption), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                                       screenWidth, screenHeight, Uint32(SDL_WINDOW_OPENGL) or Uint32(SDL_WINDOW_SHOWN) or Uint32(SDL_WINDOW_BORDERLESS));
    {$ELSE}
      POpenGLWindow(_screen)^.window  := SDL_CreateWindow(PChar(caption), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                                       screenWidth, screenHeight, Uint32(SDL_WINDOW_OPENGL) or Uint32(SDL_WINDOW_SHOWN) );
    {$ENDIF}

      POpenGLWindow(_screen)^.context := SDL_GL_CreateContext(POpenGLWindow(_screen)^.window);

    //VSYNC
    SDL_GL_SetSwapInterval(1);
    
    // {$ifndef IOS}
    // Load_GL_version_2_0();
    // {$endif}
    
    glDisable( GL_MULTISAMPLE );
    // glEnable( GL_MULTISAMPLE );
    
    if screen = nil then 
      New(screen);
    if (screen^.surface = nil) then 
      screen^.surface := POpenGLWindow(_screen)^.window;
      
    ImagesDriver.ClearSurface(screen,0);
    RefreshScreenProcedure(nil);

    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    // left and top has to be -1 otherwise 0,0 is offscreen.
    {$IFDEF IOS}
      glOrthof (0, screenWidth, screenHeight-1, 0, 0, 1);
    {$ELSE}
    //         l   right          bottom    top n  f
      glOrtho (0, screenWidth, screenHeight-1, -1, 0, 1);
    {$ENDIF}
    
    glMatrixMode (GL_MODELVIEW);
    glLoadIdentity ();

    {$IFDEF IOS}
      deviceResolution := AvailableResolutionsProcedure();
      if (Length (deviceResolution) > 0) then
      begin
        glViewport(0,0,deviceResolution[0].width, deviceResolution[0].height);
      end
      else
        glViewport(0,0,screenWidth,screenHeight);
    {$ELSE}
      glViewport(0,0,screenWidth,screenHeight);
    {$ENDIF}
    _SetupScreen(screenWidth, screenHeight);

    glDisable( GL_DEPTH_TEST );
    glDepthFunc( GL_ALWAYS );
    
    glLineWidth(1);
    
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    glEnable( GL_ALPHA_TEST );
    glEnable( GL_TEXTURE_2D );
    glEnable( GL_BLEND );  
    glEnable( GL_LINE_SMOOTH );
    glEnable( GL_POINT_SMOOTH );
    glHint( GL_LINE_SMOOTH_HINT, GL_FASTEST );
    glHint( GL_POINT_SMOOTH_HINT, GL_FASTEST );
    {$ifndef IOS}
    glEnable( GL_POLYGON_SMOOTH );
    glHint( GL_POLYGON_SMOOTH_HINT, GL_FASTEST );
    {$endif}
  end;
  
  // This resizes the graphics window used by SwinGame
  procedure InitializeScreenProcedure( screen: Bitmap; width, height : LongInt; bgColor, strColor : Color; msg : String);
  begin
    exit;
  end;
  
  // This resizes the graphics window used by SwinGame
  procedure ResizeGraphicsWindowProcedure(newWidth, newHeight : LongInt);
  begin
    exit;
  end;
  
  // This function saves an image at path.
  // returns true if the save is successful, and false if it is not
  function SaveImageProcedure(bmpToSave : Bitmap; path : String) : Boolean;
  begin
    result := false;
  end;
  

  
  function ColorFromProcedure(bmp : Bitmap; r, g, b, a : byte) : Color;
  begin
    result := 0;
  end;

  function GetSurfaceWidthProcedure(src : Bitmap) : LongInt;
  begin
    result := 0;
  end;
  
  function GetSurfaceHeightProcedure(src : Bitmap) : LongInt;
  begin
    result := 0;
  end;
  
  function SurfaceFormatAssignedProcedure(src: Bitmap) : Boolean;
  begin
    result := false;
  end;
  
  procedure GetRGBProcedure(pixel : Byte; r,g,b : Byte); 
    begin
    exit;
  end;

  function GetScreenWidthProcedure(): LongInt; 
  begin
    result := _screenWidth;
  end;
  
  function GetScreenHeightProcedure(): LongInt;
  begin
    result := _screenHeight;
  end;
  

  procedure LoadOpenGLGraphicsDriver();
  begin
    GraphicsDriver.GetPixel32               := @GetPixel32Procedure;
    GraphicsDriver.PutPixel                 := @PutPixelProcedure;    
    GraphicsDriver.FillTriangle             := @FillTriangleProcedure;
    GraphicsDriver.DrawTriangle             := @DrawTriangleProcedure;    
    GraphicsDriver.FillCircle               := @FillCircleProcedure;
    GraphicsDriver.DrawCircle               := @DrawCircleProcedure;    
    GraphicsDriver.FillEllipse              := @FillEllipseProcedure;
    GraphicsDriver.DrawEllipse              := @DrawEllipseProcedure;   
    GraphicsDriver.FillRectangle            := @FillRectangleProcedure;
    GraphicsDriver.DrawLine                 := @DrawLineProcedure;
    GraphicsDriver.SetPixelColor            := @SetPixelColorProcedure;
    GraphicsDriver.DrawRectangle            := @DrawRectangleProcedure;
    GraphicsDriver.SetClipRectangle         := @SetClipRectangleProcedure;
    GraphicsDriver.ResetClip                := @ResetClipProcedure;
    GraphicsDriver.SetVideoModeFullScreen   := @SetVideoModeFullScreenProcedure;
    GraphicsDriver.SetVideoModeNoFrame      := @SetVideoModeNoFrameProcedure;    
    GraphicsDriver.InitializeGraphicsWindow := @InitializeGraphicsWindowProcedure;
    GraphicsDriver.InitializeScreen         := @InitializeScreenProcedure;
    GraphicsDriver.ResizeGraphicsWindow     := @ResizeGraphicsWindowProcedure;
    GraphicsDriver.SaveImage                := @SaveImageProcedure;
    GraphicsDriver.RefreshScreen            := @RefreshScreenProcedure;
    GraphicsDriver.ColorComponents          := @ColorComponentsProcedure;
    GraphicsDriver.ColorFrom                := @ColorFromProcedure;
    GraphicsDriver.RGBAColor                := @RGBAColorProcedure;
    GraphicsDriver.GetSurfaceWidth          := @GetSurfaceWidthProcedure;
    GraphicsDriver.GetSurfaceHeight         := @GetSurfaceHeightProcedure;
    GraphicsDriver.GetRGB                   := @GetRGBProcedure;
    GraphicsDriver.SurfaceFormatAssigned    := @SurfaceFormatAssignedProcedure;
    GraphicsDriver.GetScreenWidth           := @GetScreenWidthProcedure;
    GraphicsDriver.GetScreenHeight          := @GetScreenHeightProcedure;
    GraphicsDriver.AvailableResolutions     := @AvailableResolutionsProcedure;
  end;
end.