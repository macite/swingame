//=============================================================================
// sgCamera.pas
//=============================================================================
//
// The Camera unit is used to change the view port (ie the camera location.)
//
// Change History:
//
// Version 3:
// - 2009-12-18: Andrew : Added in new on screen tests.
// - 2009-11-10: Andrew : Added sn and csn tags and tracing to code
// - 2009-11-06: Andrew : Added comments
// - 2009-10-21: Andrew : Fixed camera center on sprite
// - 2009-07-10: Andrew : Fixed missing const modifier on struct types
// - 2009-06-17: Clinton: Added CameraPos, reordered methods,
//                        Renamed ToScreen/ToWorld (removed "Coordinates")
// - 2009-06-16: Clinton: Renaming for consistent World/Camera/Screen use
//                        Comment cleanup/formatting + new comments
//                        Added ToScreenCoordinates
// - 2009-06-15: Andrew: Added meta tags
//
// Version 2:
// - 2008-12-17: Andrew: Moved all integers to Longint
//
// Version 1.1:
// - 2008-01-23: Andrew: Changed ToGameCoordinates to use Point2D
//                 Added Point2D overload for SetScreenOffset
// - 2008-01-21: Andrew: Added const to vector parameters.
// - 2008-01-17: Aki + Andrew: Refactor
//
// Version 1.0:
// - Various
//=============================================================================



/// SwinGame's Camera functionality can be used to create scrolling games where
/// the camera moves around a virtual game world. Using the camera allows you to
/// position and draw game elements using game world coordinates, and then to
/// move the camera around within this game world.
///
///@module Camera
///@static
unit sgCamera;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  
//---------------------------------------------------------------------------
// Camera - position
//---------------------------------------------------------------------------
  
  /// Returns the x location of the camera in game coordinates. This represents
  /// the left most x value shown on the screen, with the right of the screen
  /// being at `CameraX` + `ScreenWidth`.
  ///
  /// @lib
  ///
  /// @class Camera
  /// @getter X
  /// @static
  function CameraX(): Single;

  /// Returns the y location of the camera in game coordinates. This represents
  /// the stop most y value shown on the screen, with bottom of screen being
  /// at `CameraY` + `ScreenHeight`.
  /// 
  /// @lib
  /// 
  /// @class Camera
  /// @getter Y
  /// @static
  function CameraY(): Single;
  
  /// Returns the current camera position in world coordinates. This is the top
  /// left hand corner of the screen.
  ///
  /// @lib
  /// 
  /// @class Camera
  /// @getter Position
  /// @static
  function CameraPos(): Point2D;

  /// Returns the rectangle that encompases the area of the game world
  /// that is currently on the screen.
  ///
  /// @lib
  ///
  /// @class Camera
  /// @getter ScreenRect
  /// @static
  function CameraScreenRect(): Rectangle;
  
  /// Change the X position of the camera to a specified world coordinate. This
  /// will then be the new left most position of the screen within the world.
  ///
  /// @lib
  ///
  /// @class Camera
  /// @setter X
  /// @static
  procedure SetCameraX(x: Single);
  
  /// Change the Y position of the camera to a specified world coordinate. This
  /// will then be the new top most position of the screen within the world.
  ///
  /// @lib
  ///
  /// @class Camera
  /// @setter Y
  /// @static
  procedure SetCameraY(y: Single);
  
  /// Change the position of the camera to a specified world coordinate. This
  /// will then be the new top left most position of the screen within the world.
  ///
  /// @lib
  ///
  /// @class Camera
  /// @setter Position
  /// @static
  procedure SetCameraPos(const pt: Point2D);
  
  
  
//---------------------------------------------------------------------------
// Camera - movement
//---------------------------------------------------------------------------
  
  /// Move the camera (offset its world x and y values) using the specified 
  /// vector. For example, if you move the camera by the same speed vector of 
  /// a sprite the camera will "track" (be locked on to) the sprite as it moves.
  ///
  /// @param offset The offset vector to move the camera world position by.
  /// @lib
  procedure MoveCameraBy(const offset: Vector); overload;
  
  /// Move the camera (offset its world x and y values) using the specified 
  /// dx (change in x) and dy (change in x) values. 
  ///
  /// @param dx the amount of x axis offset to apply 
  /// @param dy the amount of x axis offset to apply 
  ///
  /// @lib MoveCameraByXY
  /// @sn moveCameraByX:%s y:%s
  procedure MoveCameraBy(dx, dy: Single); overload;
  
  /// Move the camera view to a world location specified by the x and y values.
  /// This will be the new top left corner of the screen.
  ///
  /// @param x The world x axis value to move the camera to.
  /// @param y The world y axis value to move the camera to
  ///
  /// @lib MoveCameraToXY
  /// @sn moveCaneraToX:%s y:%s
  procedure MoveCameraTo(x, y: Single); overload;

  /// Move the camera view to a world location specified as a Point2D.
  /// This will be the new top left corner of the screen.
  ///
  /// @param pt The point to move the camera view to.
  /// @lib
  /// @sn moveCameraTo:%s
  procedure MoveCameraTo(const pt: Point2D); overload;
  
  
  //---------------------------------------------------------------------------
  // Camera - sprite tracking
  //---------------------------------------------------------------------------  
  
  /// Set the camera view to be centered over the specified sprite, with an 
  /// offset from the center of the sprite if needed. The sprites size (width
  /// and height) are taken into account. Use x and y offset of 0.0 if you want 
  /// the camera to be exaclty over the center of the sprite.
  ///
  /// @param s The sprite to center the camera on 
  /// @param offsetX The amount of x axis offset for the camaera to use 
  /// @param offsetY The amount of y axis offset for the camaera to use 
  /// @lib CenterCameraOnWithXYOffset
  ///
  /// @sn centerCameraOnSprite:%s offsetX:%s offsetY:%s
  ///
  /// @class Sprite
  /// @overload CenterCamera CenterCameraOffsetXY
  /// @csn centerCameraOffsetX:%s offsetY:%s
  procedure CenterCameraOn(s: Sprite; offsetX, offsetY: Longint); overload;
  
  /// Set the camera view to be centered over the specific sprite. The offset
  /// vector allows you to move the sprite from the direct center of the screen.
  ///
  /// @param offset The amount of offset from sprite center for the camera to use.  
  /// @lib
  ///
  /// @sn centerCameraOnSprite:%s offset:%s
  ///  
  /// @class Sprite
  /// @method CenterCamera
  /// @csn centerCameraOffset:%s
  procedure CenterCameraOn(s: Sprite; const offset: Vector); overload;
  /// Set the camera view to be centered over the specific Character. The offset
  /// vector allows you to move the sprite from the direct center of the screen.
  ///
  /// @lib CenterCameraOnCharacter
  ///
  /// @sn centerCameraOnCharacter:%s offset:%s
  ///  
  /// @class Character
  /// @method CenterCamera
  /// @csn centerCameraOffset:%s
  procedure CenterCameraOn(c: Character; const offset: Vector); overload;
  
  
  
//---------------------------------------------------------------------------
// World-To-Screen Translation
//---------------------------------------------------------------------------
  
  /// Translate a world x value to the current screen x value which is based on
  /// the camera position.
  ///
  /// @param worldX The world x value to be translated
  /// @returns The translated screen x value
  /// @lib
  function ToScreenX(worldX: Single): Longint;

  /// Translate a world y value to the current screen y value set by the camera.
  ///
  /// @param worldY The world y value to be converted
  /// @returns A screen y value
  /// @lib
  function ToScreenY(worldY: Single): Longint;

  /// Translate a Point2D from world coordinates to screen coordinates.
  ///
  /// @param worldPoint The screen coordinate to translate
  /// @returns A screen coordinate point
  /// @lib ToScreen
  function ToScreen(const worldPoint: Point2D): Point2D; overload;
  
  /// Translate the points in a rectangle to screen coordinates. This can 
  /// be used to indicate the screen area used by a rectangle in game
  /// coordinates.
  ///
  /// @lib ToScreenRect
  function ToScreen(const rect: Rectangle): Rectangle; overload;
  
  
  //---------------------------------------------------------------------------
  // Screen-To-World Translation
  //---------------------------------------------------------------------------  
  
  /// Translate a screen x value (based on the camera) to a world x value
  ///
  /// @param screenX The current screen x value to be converted
  /// @returns A world x value 
  /// @lib
  function ToWorldX(screenX: Longint) : Single;

  /// Translate a screen y value (based on the camera) to a world y value
  ///
  /// @param screenY The current screen y value to be converted
  /// @returns A world y value 
  /// @lib
  function ToWorldY(screenY: Longint) : Single;

  /// Translate a Point2D from screen coordinates to world coordinates.
  ///
  /// @param screenPoint The screen coordinate to translate
  /// @returns A world coordinate point
  /// @lib
  function ToWorld(const screenPoint: Point2D): Point2D;  
  
  
  
//---------------------------------------------------------------------------
// Screen tests
//---------------------------------------------------------------------------
  
  /// Tests if the point pt is on the screen.
  /// 
  /// @lib PointOnScreen
  function PointOnScreen(const pt: Point2D): Boolean; overload;
  
  /// Tests if the rectangle rect is on the screen.
  /// 
  /// @lib RectOnScreen
  function RectOnScreen(const rect: Rectangle): Boolean; overload;
  
//=============================================================================
implementation
  uses 
    sgTrace, SysUtils, 
    sgGraphics, sgGeometry, sgSprites, sgShared, sgImages;
//=============================================================================

  ///
  /// The screen offset variables
  ///
  var 
    _cameraX : Single = 0.0;
    _cameraY : Single = 0.0;
  
//---------------------------------------------------------------------------
// Camera - position
//---------------------------------------------------------------------------
  
  function CameraX(): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'CameraX(): Single', '');
    {$ENDIF}
    
    result := _cameraX;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'CameraX(): Single', '');
    {$ENDIF}
  end;
  
  function CameraY(): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'CameraY(): Single', '');
    {$ENDIF}
    
    result := _cameraY;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'CameraY(): Single', '');
    {$ENDIF}
  end;
  
  function CameraPos(): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'CameraPos(): Point2D', '');
    {$ENDIF}
    
    result.x := _cameraX;
    result.y := _cameraY;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'CameraPos(): Point2D', '');
    {$ENDIF}
  end;

  function CameraScreenRect(): Rectangle;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'CameraScreenRect(): Rectangle', '');
    {$ENDIF}
    
    result := BitmapRectangle(_cameraX, _cameraY, screen);
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'CameraScreenRect(): Rectangle', '');
    {$ENDIF}
  end;
  
  procedure SetCameraX(x: Single);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'SetCameraX(x: Single)', '');
    {$ENDIF}
    
    _cameraX := x;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'SetCameraX(x: Single)', '');
    {$ENDIF}
  end;
  
  procedure SetCameraY(y: Single);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'SetCameraY(y: Single)', '');
    {$ENDIF}
    
    _cameraY := y;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'SetCameraY(y: Single)', '');
    {$ENDIF}
  end;
  
  procedure SetCameraPos(const pt: Point2D);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'SetCameraPos(const pt: Point2D)', '');
    {$ENDIF}
    
    _cameraX := pt.x;
    _cameraY := pt.y;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'SetCameraPos(const pt: Point2D)', '');
    {$ENDIF}
  end;
  
  
  
//---------------------------------------------------------------------------
// Camera - movement
//---------------------------------------------------------------------------
  
  procedure MoveCameraBy(const offset: Vector); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'MoveCameraBy(const offset: Vector)', '');
    {$ENDIF}
    
    _cameraX := _cameraX + offset.x;
    _cameraY := _cameraY + offset.y;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'MoveCameraBy(const offset: Vector)', '');
    {$ENDIF}
  end;

  procedure MoveCameraBy(dx, dy: Single); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'MoveCameraBy(dx, dy: Single)', '');
    {$ENDIF}
    
    _cameraX := _cameraX + dx;
    _cameraY := _cameraY + dy;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'MoveCameraBy(dx, dy: Single)', '');
    {$ENDIF}
  end;
  
  procedure MoveCameraTo(x, y: Single); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'MoveCameraTo(x, y: Single)', '');
    {$ENDIF}
    
    _cameraX := x;
    _cameraY := y;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'MoveCameraTo(x, y: Single)', '');
    {$ENDIF}
  end;

  procedure MoveCameraTo(const pt: Point2D); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'MoveCameraTo(const pt: Point2D)', '');
    {$ENDIF}
    
    _cameraX := pt.x;
    _cameraY := pt.y;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'MoveCameraTo(const pt: Point2D)', '');
    {$ENDIF}
  end;

//---------------------------------------------------------------------------
// Camera - Sprite tracking
//---------------------------------------------------------------------------
  
  procedure CenterCameraOn(s: Sprite; offsetX, offsetY: Longint);
  var
    center: Point2D;
    scX, scY: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'CenterCameraOn(s: Sprite', '');
    {$ENDIF}
    
    if s = nil then begin
      RaiseException('CenterCameraOn requires a target sprite. No sprite was provided (nil supplied)');
      exit;
    end;
    
    center := CenterPoint(s);
    scX := center.x + offsetX - (ScreenWidth() / 2);
    scY := center.y + offsetY - (ScreenHeight() / 2);
    
    MoveCameraTo(scX, scY);
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'CenterCameraOn(s: Sprite', '');
    {$ENDIF}
  end;
  
  procedure CenterCameraOn(c: Character; const offset: Vector); overload;
  begin
    if Assigned(c) then 
      CenterCameraOn(c^.CharSprite, Round(offset.x), Round(offset.y));
  end;
  
  procedure CenterCameraOn(s: Sprite; const offset: Vector); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'CenterCameraOn(s: Sprite', '');
    {$ENDIF}
    
    CenterCameraOn(s, Round(offset.x), Round(offset.y));
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'CenterCameraOn(s: Sprite', '');
    {$ENDIF}
  end;

//---------------------------------------------------------------------------
// World-To-Screen Translation
//---------------------------------------------------------------------------

  function ToScreenX(worldX: Single): Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'ToScreenX(worldX: Single): Longint', '');
    {$ENDIF}
    
    result := Round(worldX - _cameraX);
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'ToScreenX(worldX: Single): Longint', '');
    {$ENDIF}
  end;

  function ToScreenY(worldY: Single): Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'ToScreenY(worldY: Single): Longint', '');
    {$ENDIF}
    
    result := Round(worldY - _cameraY);
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'ToScreenY(worldY: Single): Longint', '');
    {$ENDIF}
  end;

  function ToScreen(const worldPoint: Point2D): Point2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'ToScreen(const worldPoint: Point2D): Point2D', '');
    {$ENDIF}
    
    result.x := _cameraX - worldPoint.x;
    result.y := _cameraY - worldPoint.y;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'ToScreen(const worldPoint: Point2D): Point2D', '');
    {$ENDIF}
  end;
  
  function ToScreen(const rect: Rectangle): Rectangle; overload;
  begin
    result.x := ToScreenX(rect.x);
    result.y := ToScreenY(rect.y);
    result.width := rect.width;
    result.height := rect.height;
  end;
  
  
//---------------------------------------------------------------------------
// Screen-To-World Translation
//---------------------------------------------------------------------------
  
  function ToWorldX(screenX: Longint) : Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'ToWorldX(screenX: Longint) : Single', '');
    {$ENDIF}
    
    result := screenX + _cameraX;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'ToWorldX(screenX: Longint) : Single', '');
    {$ENDIF}
  end;

  function ToWorldY(screenY: Longint) : Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'ToWorldY(screenY: Longint) : Single', '');
    {$ENDIF}
    
    result := screenY + _cameraY;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'ToWorldY(screenY: Longint) : Single', '');
    {$ENDIF}
  end;

  function ToWorld(const screenPoint: Point2D): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCamera', 'ToWorld(const screenPoint: Point2D): Point2D', '');
    {$ENDIF}
    
    result.x := screenPoint.x + _cameraX;
    result.y := screenPoint.y + _cameraY;
    
    {$IFDEF TRACE}
      TraceExit('sgCamera', 'ToWorld(const screenPoint: Point2D): Point2D', '');
    {$ENDIF}
  end;
  
  
//---------------------------------------------------------------------------
// Screen tests
//---------------------------------------------------------------------------
  
  function PointOnScreen(const pt: Point2D): Boolean; overload;
  var
    scrPt: Point2D;
  begin
    scrPt := ToScreen(pt);
    result := not ((scrPt.x < 0) or (scrPt.y < 0) or (scrPt.x >= ScreenWidth()) or (scrPt.y >= ScreenHeight()));
  end;
  
  function RectOnScreen(const rect: Rectangle): Boolean; overload;
  begin
    result := RectanglesIntersect(ToScreen(rect), screenRect);
  end;
  
  
//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
  end;

end.
