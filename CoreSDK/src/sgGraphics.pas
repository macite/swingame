//=============================================================================
// sgGraphics.pas
//=============================================================================
//
// The Graphics unit is responsible for all of the drawing of anything to the
// screen or other surfaces. The ...OnScreen routines draw directly onto the
// screen ignoring the camera settings. The standard draw routines draw to the
// screen using the camera settings. Finally the overloaded drawing methods
// with a destination Bitmap will draw onto the supplied bitmap.
//
// Change History:
//
// Version 3.0:
// - 2010-02-02: Aaron  : Added PushClip,PopClip,SetClip,ResetClip
// - 2010-01-13: Aaron  : Made all Draw Shapes  draw with an offset and made those that does not have a destination Bitmap have  an offset of cameraX and cameraY
// - 2010-01-04: Andrew : Added PutPixel
// - 2009-12-10: Andrew : Moved out remaining bitmap function
// - 2009-11-06: Andrew : Moved out bitmap function
// - 2009-10-16: Andrew : Added shapes and shape prototypes
// - 2009-07-14: Andrew : Removed loading and freeing code.
// - 2009-07-10: Andrew : Fixed missing const modifier on struct parameters
// - 2009-06-29: Andrew : Using circle
// - 2009-06-24: Andrew : Moved Sprite routines to Sprites.
// - 2009-06-23: Clinton: Comment format/cleanup
// - 2009-06-05: Andrew : Using sgShared
//
// Version 2.0:
// - 2008-12-17: Andrew : Moved all integers to Longint
// - 2008-12-10: Andrew : Moved primitive drawing to SDL_gfx
//                      : Added rotation and zoom to Sprite + Sprite Drawing
//                      : Added RotateZoomBitmap
//                      : Added MakeOpaque and MakeTransparent to allow multiple blending
//                      : Added extra triangle drawing code
// - 2008-12-09: Andrew : Started transition to SDL_gfx
//
// Version 1.1:
// - 2008-04-08: Stephen: Added DrawTriangle()
// - 2008-04-02: Andrew : Fixed issues related to freeing images
//                      : Fixed transparent pixels for non 32bit images
// - 2008-03-09: Andrew : Fixed DrawSprite with Offset
// - 2008-02-16: Andrew : Added GetPixel and GetPixelFromScreen
// - 2008-01-31: Andrew : Fixed Line Drawing Issue
// - 2008-01-30: Andrew : Fixed DrawRectangle
// - 2008-01-25: Andrew : Fixed compiler hints for pointer use
// - 2008-01-24: Andrew : Added Clipping
// - 2008-01-24: James  : Version 1.1 overloads
// - 2008-01-21: Aki    : 40 overloads added for Point2D and
// - 2008-01-17: Aki + Andrew: Refactor Rectangle support
//
// Version 1.0:
// - Various
//=============================================================================



/// The graphics code of SwinGame is used to draw primitive shapes to the screen
/// or onto bitmaps.
/// 
/// @module Graphics
/// @static
unit sgGraphics;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  
//----------------------------------------------------------------------------
// Window management
//----------------------------------------------------------------------------
  
  /// Sets the icon for the window. This must be called before openning the
  /// graphics window. The icon is loaded as a bitmap, though this can be from
  /// any kind of bitmap file.
  ///
  /// @param filename The name of the file to load as the images icon
  ///
  ///Side Effects
  /// - The icon will be loaded and used as the windows icon when the window
  /// is opened.
  ///
  /// @lib
  procedure SetIcon(filename: String);

  /// Returns the number of resolutions in the list of available resolutions.
  ///
  /// @lib
  function NumberOfResolutions(): Longint;

  /// Returns a list of the available resolutions.
  ///
  /// @lib
  /// @length NumberOfResolutions
  function AvailableResolutions(): ResolutionArray;
  
  /// Opens the graphical window so that it can be drawn onto. You can set the
  /// icon for this window using `SetIcon`. The window itself is only drawn when
  /// you call `RefreshScreen`. All windows are opened at 32 bits per pixel. You
  /// can toggle fullscreen using `ToggleFullScreen`. The window is closed when
  /// the application terminates.
  ///
  /// @param caption The caption for the window
  /// @param width The width of the window
  /// @param height The height of the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  ///
  /// @lib
  /// @uname OpenGraphicsWindow
  /// @sn openGraphicsWindow:%s width:%s height:%s
  procedure OpenGraphicsWindow(caption: String; width, height: Longint); overload;

  /// Opens the graphical window as an 800 x 600 window. See OpenGramhicsWinddow
  /// for more options.
  /// @param caption: The caption for the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  ///
  /// @lib OpenGraphicsWindow(caption, 800, 600)
  /// @uname OpenGraphicsWindow800x600
  /// @sn openGraphicsWindow:%s
  procedure OpenGraphicsWindow(caption: String); overload;

  /// Shows the SwinGame intro splash screen.
  /// It would be great if you could include this at the start of
  /// your game to help us promote the SwinGame API.
  ///
  /// @lib
  procedure ShowSwinGameSplashScreen();

  /// Changes the size of the screen.
  ///
  /// @param width, height: The new width and height of the screen
  ///
  /// Side Effects:
  /// - The screen changes to the specified size
  ///
  /// @lib
  /// @sn changeScreenSizeToWidth:%s height:%s
  procedure ChangeScreenSize(width, height: Longint);

  /// Switches the application to full screen or back from full screen to
  /// windowed.
  ///
  /// Side Effects:
  /// - The window switched between fullscreen and windowed
  ///
  /// @lib
  procedure ToggleFullScreen();
  
  /// Toggle the Window border mode. This enables you to toggle from a bordered
  /// window to a borderless window.
  ///
  /// @lib
  procedure ToggleWindowBorder();
  
  /// Returns the width of the screen currently displayed.
  ///
  /// @returns: The screen's width
  ///
  /// @lib
  ///
  /// @class Graphics
  /// @static
  /// @getter ScreenWidth
  function ScreenWidth(): Longint;

  /// Returns the height of the screen currently displayed.
  ///
  /// @returns: The screen's height
  ///
  /// @lib
  ///
  /// @class Graphics
  /// @static
  /// @getter ScreenHeight
  function ScreenHeight(): Longint;

  /// Saves the current screen a bitmap file. The file will be saved into the
  /// current directory.
  ///
  /// @param basename   The base name for the screen shot. e.g. "GemCollector"
  ///
  /// Side Effects:
  /// - Saves the current screen image to a bitmap file.
  ///
  /// @lib TakeScreenshot
  procedure TakeScreenshot(basename: String);
  
  
  
//----------------------------------------------------------------------------
// Refreshing the screen
//----------------------------------------------------------------------------
  
  /// Draws the current drawing to the screen. This must be called to display
  /// anything to the screen. This will draw all drawing operations, as well
  /// as the text being entered by the user.
  ///
  /// Side Effects:
  /// - The current drawing is shown on the screen.
  ///
  /// @lib RefreshScreen
  procedure RefreshScreen(); overload;
  
  /// Refresh with a target FPS. This will delay a period of time that will 
  /// approximately meet the targetted frames per second.
  ///
  /// @lib RefreshScreenRestrictFPS
  procedure RefreshScreen(TargetFPS: LongInt); overload;
  
  
  
//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

  /// Maps a color from a given bitmap. This is used when determining color
  /// keys for transparent images.
  ///
  /// @param bmp:   the bitmap to get the color for
  /// @param apiColor:     The api color to match
  /// @returns:           The color matched to the bitmaps pixel format
  ///
  /// @lib ColorFromBitmap
  /// @sn colorFrom:%s apiColor:%s
  ///
  /// @doc_group colors
  function ColorFrom(bmp: Bitmap; apiColor: Color): Color;

  /// Creates and returns a random color where R, G, B and A are all randomised.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function RandomColor(): Color;

  /// Creates and returns a random color where R, G, and B are all randomised, and A is set
  /// to the passed in value.
  ///
  /// @param alpha: the opacity of the random color
  ///
  /// @lib
  ///
  /// @doc_group colors
  function RandomRGBColor(alpha: Byte): Color;

  /// Gets a color given its RGBA components.
  ///
  /// @param red, green, blue, alpha:  Components of the color
  /// @returns: The matching colour
  ///
  /// @lib
  /// @sn rgbaColorRed:%s green:%s blue:%s alpha:%s
  ///
  /// @doc_group colors
  function RGBAColor(red, green, blue, alpha: Byte): Color;

  /// Gets a color given its RGB components.
  ///
  /// @param red, green, blue:   Components of the color
  /// @returns:                 The matching colour
  ///
  /// @lib RGBAColor(red, green, blue, 255)
  /// @uname RGBColor
  /// @sn rgbColorRed:%s green:%s blue:%s
  ///
  /// @doc_group colors
  function RGBColor(red, green, blue: Byte): Color;

  /// Gets a color given its RGBA components.
  ///
  /// @lib
  /// @sn colorComponentsOf:%s red:%s green:%s blue:%s alpha:%s
  ///
  /// @doc_group colors
  procedure ColorComponents(c: Color; out r, g, b, a: Byte);


  /// returns color to string.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function  ColorToString(c: Color): String;

  /// Returns a color from a floating point RBG value set.
  ///
  /// @param r,g,b: Components for color 0 = none 1 = full
  ///
  /// @lib
  /// @sn rgbFloatColorRed:%s green:%s blue:%s
  ///
  /// @doc_group colors
  function RGBFloatColor(r,g,b: Single): Color;

  /// Returns a color from a floating point RBGA value set.
  ///
  /// @param r,g,b,a: Components for color 0 = none 1 = full
  ///
  /// @lib
  /// @sn rgbaFloatColorRed:%s green:%s blue:%s alpha:%s
  ///
  /// @doc_group colors
  function RGBAFloatColor(r,g,b, a: Single): Color;

  /// Returs a color from the HSB input.
  ///
  /// @param hue, saturation, brightness: Values between 0 and 1
  /// @returns The matching color
  ///
  /// @lib
  /// @sn hsbColorHue:%s sat:%s bri:%s
  ///
  /// @doc_group colors
  function HSBColor(hue, saturation, brightness: Single): Color;

  /// Get the transpareny value of ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function TransparencyOf(c: Color): byte;

  /// Get the red value of ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function RedOf(c: Color): byte;

  /// Get the green value of ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function GreenOf(c: Color): byte;

  /// Get the blue value of ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function BlueOf(c: Color): byte;

  /// Gets the hue ``h``, saturation ``s``, and brightness ``b`` values from
  /// the color.
  ///
  /// @lib
  /// @sn hsbValueOf:%s hue:%s sat:%s bri:%s
  ///
  /// @doc_group colors
  procedure HSBValuesOf(c: Color; out h, s, b: Single);

  /// Get the hue of the ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function HueOf(c: Color): Single;

  /// Get the saturation of the ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function SaturationOf(c: Color) : Single;

  /// Get the brightness of the ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function BrightnessOf(c: Color) : Single;
  
  
  
//---------------------------------------------------------------------------
// Circle drawing code
//---------------------------------------------------------------------------
  
  /// Draw a circle onto a destination bitmap.
  /// 
  /// @lib DrawCircleOpts
  /// @sn drawCircleColor:%s atX:%s y:%s width:%s height:%s opts:%s
  procedure DrawCircle(clr : Color; x, y, radius: Single; const opts : DrawingOptions); overload;
  
  /// Draw a circle in the game.
  ///
  /// @lib 
  /// @sn drawCircleColor:%s atX:%s y:%s width:%s height:%s
  ///
  /// @doc_idx 0
  procedure DrawCircle(clr : Color; x, y, radius : Single);

  /// Draw a circle onto a destination bitmap.
  /// 
  /// @lib DrawCircleStructOpts
  /// @sn drawCircleColor:%s data:%s opts:%s
  procedure DrawCircle(clr : Color; const c: Circle; const opts : DrawingOptions); overload;
  
  /// Draw a circle in the game.
  ///
  /// @lib DrawCircleStruct
  /// @sn drawCircleColor:%s data:%s
  procedure DrawCircle(clr : Color; const c: Circle);


  /// Fill a circle onto a destination bitmap.
  /// 
  /// @lib FillCircleOpts
  /// @sn fillCircleColor:%s atX:%s y:%s width:%s height:%s opts:%s
  procedure FillCircle(clr : Color; x, y, radius: Single; const opts : DrawingOptions); overload;
  
  /// Fill a circle in the game.
  ///
  /// @lib 
  /// @sn FillCircleColor:%s atX:%s y:%s width:%s height:%s
  ///
  /// @doc_idx 0
  procedure FillCircle(clr : Color; x, y, radius : Single);

  /// Fill a circle onto a destination bitmap.
  /// 
  /// @lib FillCircleStructOpts
  /// @sn fillCircleColor:%s data:%s opts:%s
  procedure FillCircle(clr : Color; const c: Circle; const opts : DrawingOptions); overload;
  
  /// Fill a circle in the game.
  ///
  /// @lib FillCircleStruct
  /// @sn fillCircleColor:%s data:%s
  procedure FillCircle(clr : Color; const c: Circle);
  
  
  
//---------------------------------------------------------------------------
// Triangle drawing code
//---------------------------------------------------------------------------
  
  /// Draw a triangle onto a destination bitmap.
  /// 
  /// @lib DrawTriangleOpts
  /// @sn drawTriangleColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s opts:%s
  procedure DrawTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions); overload;
  
  /// Draw a triangle in the game.
  ///
  /// @lib 
  /// @sn drawTriangleColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
  ///
  /// @doc_idx 0
  procedure DrawTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single);

  /// Draw a triangle onto a destination bitmap.
  /// 
  /// @lib DrawTriangleStructOpts
  /// @sn drawTriangleColor:%s data:%s opts:%s
  procedure DrawTriangle(clr : Color; const tri: Triangle; const opts : DrawingOptions); overload;
  
  /// Draw a triangle in the game.
  ///
  /// @lib DrawTriangleStruct
  /// @sn drawTriangleColor:%s data:%s
  procedure DrawTriangle(clr : Color; const tri: Triangle);


  /// Fill a triangle onto a destination bitmap.
  /// 
  /// @lib FillTriangleOpts
  /// @sn fillTriangleColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s opts:%s
  procedure FillTriangle(clr : Color;  x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions); overload;
  
  /// Fill a triangle in the game.
  ///
  /// @lib 
  /// @sn fillTriangleColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s opts:%s
  ///
  /// @doc_idx 0
  procedure FillTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single);

  /// Fill a triangle onto a destination bitmap.
  /// 
  /// @lib FillTriangleStructOpts
  /// @sn fillTriangleColor:%s data:%s opts:%s
  procedure FillTriangle(clr : Color; const tri: Triangle; const opts : DrawingOptions); overload;
  
  /// Fill a triangle in the game.
  ///
  /// @lib FillTriangleStruct
  /// @sn fillTriangleColor:%s data:%s
  procedure FillTriangle(clr : Color; const tri: Triangle);

//---------------------------------------------------------------------------
// Screen clearing routines
//---------------------------------------------------------------------------
  
  /// Clear the screen black.
  /// 
  /// @lib ClearScreen
  /// @sn clearScreen
  procedure ClearScreen(); overload;
  
  /// Clear the screen to a specified color.
  /// 
  /// @lib ClearScreenTo
  /// @sn clearScreen:%s
  procedure ClearScreen(toColor : Color); overload;
  
  
  
//---------------------------------------------------------------------------
// Pixel drawing
//---------------------------------------------------------------------------
  
  /// Sets the color of the pixel to the specified value.
  ///
  /// @lib
  /// @sn bitmap:%s putPixelX:%s y:%s color:%s
  procedure PutPixel(bmp: Bitmap; value: Color; x, y: Longint);
  
  /// Draw a pixel onto a destination.
  /// 
  /// @lib DrawPixelOnto
  /// @sn drawOnto:%s color:%s pixelX:%s y:%s
  procedure DrawPixel(dest: Bitmap; clr: Color; x, y: Longint); overload;
  
  /// Draw a pixel onto a destination.
  /// 
  /// @lib DrawPixelAtPointOnto
  /// @sn drawOnto:%s color:%s pixel:%s
  procedure DrawPixel(dest: Bitmap; clr: Color; const position: Point2D); overload;
  
  /// Draw a pixel in the game.
  ///
  /// @lib
  /// @sn draw:%s pixelX:%s y:%s
  procedure DrawPixel(clr: Color; x, y: Single); overload;
  
  /// Draw a pixel in the game.
  ///
  /// @lib DrawPixelAtPoint
  /// @sn draw:%s pixelAt:%s
  procedure DrawPixel(clr: Color; const position: Point2D); overload;
  
  /// Draw a pixel on the screen.
  /// 
  /// @lib
  /// @sn draw:%s pixelOnScreenX:%s y:%s
  procedure DrawPixelOnScreen(clr: Color; x, y: Longint); overload;
  
  /// Draw a pixel on the screen.
  ///
  /// @lib DrawPixelAtPointOnScreen
  /// @sn draw:%s pixelOnScreenAt:%s
  procedure DrawPixelOnScreen(clr: Color; const position: Point2D); overload;
  
  
  
//---------------------------------------------------------------------------
// Rectangle drawing
//---------------------------------------------------------------------------
  
  /// Draw a rectangle onto a destination bitmap.
  /// 
  /// @lib DrawRectangleOpts
  /// @sn drawRectangleColor:%s atX:%s y:%s width:%s height:%s opts:%s
  procedure DrawRectangle(clr : Color; xPos, yPos, width, height: Single; const opts : DrawingOptions); overload;
  
  /// Draw a rectangle in the game.
  ///
  /// @lib 
  /// @sn drawRectangleColor:%s atX:%s y:%s width:%s height:%s
  ///
  /// @doc_idx 0
  procedure DrawRectangle(clr : Color; x, y, width, height : Single);

  /// Draw a rectangle onto a destination bitmap.
  /// 
  /// @lib DrawRectangleStructOpts
  /// @sn drawRectangleColor:%s data:%s opts:%s
  procedure DrawRectangle(clr : Color; const rect: Rectangle; const opts : DrawingOptions); overload;
  
  /// Draw a rectangle in the game.
  ///
  /// @lib DrawRectangleStruct
  /// @sn drawRectangleColor:%s data:%s
  procedure DrawRectangle(clr : Color; const rect: Rectangle);


  /// Fill a rectangle onto a destination bitmap.
  /// 
  /// @lib FillRectangleOpts
  /// @sn fillRectangleColor:%s atX:%s y:%s width:%s height:%s opts:%s
  procedure FillRectangle(clr : Color; xPos, yPos, width, height: Single; const opts : DrawingOptions); overload;
  
  /// Fill a rectangle in the game.
  ///
  /// @lib 
  /// @sn FillRectangleColor:%s atX:%s y:%s width:%s height:%s
  ///
  /// @doc_idx 0
  procedure FillRectangle(clr : Color; x, y, width, height : Single);

  /// Fill a rectangle onto a destination bitmap.
  /// 
  /// @lib FillRectangleStructOpts
  /// @sn fillRectangleColor:%s data:%s opts:%s
  procedure FillRectangle(clr : Color; const rect: Rectangle; const opts : DrawingOptions); overload;
  
  /// Fill a rectangle in the game.
  ///
  /// @lib FillRectangleStruct
  /// @sn fillRectangleColor:%s data:%s
  procedure FillRectangle(clr : Color; const rect: Rectangle);
  
  
  
//---------------------------------------------------------------------------
// Line drawing
//---------------------------------------------------------------------------
  
  /// Draw a line onto a destination bitmap.
  /// 
  /// @lib DrawLineOpts
  /// @sn drawLineColor:%s atX:%s y:%s width:%s height:%s opts:%s
  procedure DrawLine(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single; const opts : DrawingOptions);
  
  /// Draw a line in the game.
  ///
  /// @lib 
  /// @sn drawLineColor:%s atX:%s y:%s width:%s height:%s
  ///
  /// @doc_idx 0
  procedure DrawLine(clr : Color; x1, y1, x2, y2: Single);

  /// Draw a line onto a destination bitmap.
  /// 
  /// @lib DrawLineStructOpts
  /// @sn drawLineColor:%s data:%s opts:%s
  procedure DrawLine(clr : Color; const l : LineSegment; const opts : DrawingOptions); overload;
  
  /// Draw a line in the game.
  ///
  /// @lib DrawLineStruct
  /// @sn drawLineColor:%s data:%s
  procedure DrawLine(clr : Color; const l : LineSegment);
  
  
  
//---------------------------------------------------------------------------
// Ellipse drawing
//---------------------------------------------------------------------------

  /// Draw a ellipse onto a destination bitmap.
  /// 
  /// @lib DrawEllipseOpts
  /// @sn drawEllipseColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s opts:%s
  procedure DrawEllipse(clr : Color; xPos, yPos, width, height: Single; const opts : DrawingOptions); overload;
  
  /// Draw a ellipse in the game.
  ///
  /// @lib 
  /// @sn drawEllipseColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
  ///
  /// @doc_idx 0
  procedure DrawEllipse(clr : Color; xPos, yPos, width, height: Single);

  /// Draw a ellipse onto a destination bitmap.
  /// 
  /// @lib DrawEllipseStructOpts
  /// @sn drawEllipseColor:%s data:%s opts:%s
  procedure DrawEllipse(clr : Color; const rec: Rectangle; const opts : DrawingOptions); overload;
  
  /// Draw a ellipse in the game.
  ///
  /// @lib DrawEllipseStruct
  /// @sn drawEllipseColor:%s data:%s
  procedure DrawEllipse(clr : Color; const rec: Rectangle);


  /// Fill a ellipse onto a destination bitmap.
  /// 
  /// @lib FillEllipseOpts
  /// @sn fillEllipseColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s opts:%s
  procedure FillEllipse(clr : Color; xPos, yPos, width, height: Single; const opts : DrawingOptions); overload;
  
  /// Fill a ellipse in the game.
  ///
  /// @lib 
  /// @sn fillEllipseColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s opts:%s
  ///
  /// @doc_idx 0
  procedure FillEllipse(clr : Color; xPos, yPos, width, height: Single);

  /// Fill a ellipse onto a destination bitmap.
  /// 
  /// @lib FillEllipseStructOpts
  /// @sn fillEllipseColor:%s data:%s opts:%s
  procedure FillEllipse(clr : Color; const rec: Rectangle; const opts : DrawingOptions); overload;
  
  /// Fill a ellipse in the game.
  ///
  /// @lib FillEllipseStruct
  /// @sn fillEllipseColor:%s data:%s
  procedure FillEllipse(clr : Color; const rec: Rectangle);
  
  
  
//---------------------------------------------------------------------------
// Clipping
//---------------------------------------------------------------------------
  
  /// Push a clip rectangle to the screen. This can be undone using PopClip.
  ///
  /// @lib PushClipXY
  /// @sn pushClipX:%s y:%s width:%s height:%s
  procedure PushClip(x, y, w, h: Longint); overload;
  
  /// Push a clip rectangle to the screen. This can be undone using PopClip.
  ///
  /// @lib PushClipRect
  /// @sn pushClip:%s
  procedure PushClip(const r: Rectangle); overload;

  /// Add the clipping rectangle of a bitmap and uses the intersect between the new rectangle and previous clip.
  /// 
  /// @lib PushClipRectForBitmap
  /// @sn bitmap:%s PushClipRect:%s
  ///
  /// @class Bitmap
  /// @overload PushClip PushClipRect
  /// @csn pushClip:%s
  procedure PushClip(bmp: Bitmap; const r: Rectangle); overload;  
  
  /// Reset the clipping rectangle of the screen.
  /// 
  /// @lib
  procedure ResetClip(); overload;
  
  /// Reset the clipping rectangle on a bitmap.
  ///
  /// @lib ResetClipForBitmap
  ///
  /// @class Bitmap
  /// @method ResetClip
  procedure ResetClip(bmp: Bitmap); overload;

  /// Set the clip rectangle of the bitmap.
  ///
  /// @lib SetBmpClip
  /// @sn bitmap:%s setClip:%s
  ///
  /// @class Bitmap
  /// @method SetClip
  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;

  /// Set the clip rectangle of the screen.
  ///
  /// @lib SetClip
  /// @sn setClip:%s
  procedure SetClip(const r: Rectangle); overload;
  
  /// Set the clip rectangle of the screen.
  ///
  /// @lib SetClipXY
  /// @sn setClipX:%s y:%s width:%s height:%s
  procedure SetClip(x, y, w, h: Longint); overload;

  /// Set the clip rectangle of the bitmap.
  ///
  /// @lib SetBmpClipXY
  /// @sn bitmap:%s setClipX:%s y:%s width:%s height:%s
  ///
  /// @class Bitmap
  /// @overload SetClip SetClipXY
  /// @csn setClipX:%s y:%s width:%s height:%s
  procedure SetClip(bmp: Bitmap; x, y, w, h: Longint); overload;
  
  /// Pop the clip rectangle of the screen.
  ///
  /// @lib PopClipScreen
  procedure PopClip(); overload;

  /// Pop the clipping rectangle of a bitmap.
  /// 
  /// @lib PopClipBmp
  /// @sn popClipBitmap:%s
  ///
  /// @class Bitmap
  /// @method PopClip
  procedure PopClip(bmp: Bitmap); overload;

  /// Returns the rectangle of the currentl clip of bitmap
  ///
  /// @lib CurrentBmpClip
  /// @sn currentClip:%s
  ///
  /// @class Bitmap
  /// @getter CurrentClip
  function CurrentClip(bmp: Bitmap): Rectangle; overload;

  /// Returns the rectangle of the currentl clip of bitmap
  ///
  /// @lib CurrentScreenClip
  ///
  function CurrentClip(): Rectangle; overload;
  
  
  
//---------------------------------------------------------------------------
// Pixel reading functions
//---------------------------------------------------------------------------
  
  /// Returns the color of the pixel at the x,y location on
  /// the supplied bitmap.
  /// 
  /// @lib
  /// @sn bitmap:%s colorAtX:%s y:%s
  ///
  /// @class Bitmap
  /// @method GetPixel
  /// @csn colorAtX:%s y:%s
  function GetPixel(bmp: Bitmap; x, y: Longint): Color;
  
  /// Returns the color of the pixel at the given x,y location.
  ///
  /// @lib
  /// @sn colorOnScreenAtX:%s y:%s
  function GetPixelFromScreen(x, y: Longint): Color;
  
  
  var
    //Preset colours, do not change these values.
    ColorBlue, ColorGreen, ColorRed, ColorWhite, ColorBlack, ColorYellow,
    ColorPink, ColorTurquoise, ColorGrey, ColorMagenta, ColorTransparent,
    ColorLightGrey: Color;
  
//=============================================================================
implementation
//=============================================================================

  uses Math, Classes, SysUtils, // system
       sgSavePNG, 
       sgTrace, 
       sgCamera, sgShared, sgGeometry, sgResources, sgImages, sgUtils, sgDriverGraphics, sgDriver, sgDriverImages, sgInput, sgAudio, sgText, sgAnimations, sgDrawingOptions,
       sgInputBackend;

  /// Clears the surface of the screen to the passed in color.
  ///
  /// @param toColor: The colour to clear the bitmap to
  ///
  /// Side Effects:
  /// - Screen's surface is set to the toColor
  procedure ClearScreen(toColor : Color); overload;
  begin
    ClearSurface(screen, toColor);
  end;

  /// Clears the screen to Black.
  ///
  /// Side Effects:
  /// - screen's surface is set to black
  procedure ClearScreen(); overload;
  begin
    ClearScreen(ColorBlack);
  end;

  function GetPixel(bmp: Bitmap; x, y: Longint): Color;
  begin
    result := GraphicsDriver.GetPixel32(bmp, x, y);
  end;

  function GetPixelFromScreen(x, y: Longint): Color;
  begin
    result := GetPixel(screen, x, y);
  end;

  procedure PutPixel(bmp: Bitmap; value: Color; x, y: Longint);
  begin
    DrawPixel(bmp, value, x, y);
  end;
  
  /// Draws a pixel onto the screen.
  ///
  /// @param clr:     The color to draw the pixel
  /// @param x,y:         The x,y location to draw the pixel at
  ///
  /// Side Effects:
  /// - Sets one pixel on the screen
  procedure DrawPixelOnScreen(clr: Color; x, y: Longint);
  begin
    DrawPixel(screen, clr, x, y);
  end;

  procedure DrawPixel(clr: Color; x, y: Single); overload;
  begin
    DrawPixelOnScreen(clr, sgCamera.ToScreenX(x), sgCamera.ToScreenY(y));
  end;

//=============================================================================

  procedure DrawRectangle(clr : Color; x, y, width, height : Single);
  begin
    DrawRectangle(clr, x, y, width, height, OptionDefaults());
  end;

  procedure DrawRectangle(clr : Color; const rect : Rectangle);
  begin
    DrawRectangle(clr, rect.x, rect.y, rect.width, rect.height, OptionDefaults());
  end;

  procedure DrawRectangle(clr : Color; const rect : Rectangle; const opts : DrawingOptions);
  begin
    DrawRectangle(clr, rect, opts);
  end;

  procedure FillRectangle(clr : Color; x, y, width, height : Single);
  begin
    FillRectangle(clr, x, y, width, height, OptionDefaults());
  end;

  procedure FillRectangle(clr : Color; const rect : Rectangle);
  begin
    FillRectangle(clr, rect, OptionDefaults());
  end;

  procedure FillRectangle(clr : Color; const rect : Rectangle; const opts : DrawingOptions);
  begin
    FillRectangle(clr, rect, opts);
  end;

//=============================================================================

  procedure DrawTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    DrawTriangle(clr, x1, y1, x2, y2, x3, y3, OptionDefaults());
  end;

  procedure DrawTriangle(clr : Color; const tri: Triangle; const opts : DrawingOptions); overload;
  begin
    DrawTriangle(clr,
                 tri.points[0].x,
                 tri.points[0].y, 
                 tri.points[1].x, 
                 tri.points[1].y, 
                 tri.points[2].x, 
                 tri.points[2].y,
                 opts);
  end;

  procedure DrawTriangle(clr : Color; const tri: Triangle);
  begin
    DrawTriangle(clr,
                 tri.points[0].x,
                 tri.points[0].y, 
                 tri.points[1].x, 
                 tri.points[1].y, 
                 tri.points[2].x, 
                 tri.points[2].y,
                 OptionDefaults());
  end;

  procedure FillTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    DrawTriangle(clr, x1, y1, x2, y2, x3, y3, OptionDefaults());
  end;

  procedure FillTriangle(clr : Color; const tri: Triangle; const opts : DrawingOptions); overload;
  begin
    DrawTriangle(clr,
                 tri.points[0].x,
                 tri.points[0].y, 
                 tri.points[1].x, 
                 tri.points[1].y, 
                 tri.points[2].x, 
                 tri.points[2].y,
                 opts);
  end;

  procedure FillTriangle(clr : Color; const tri: Triangle);
  begin
    DrawTriangle(clr,
                 tri.points[0].x,
                 tri.points[0].y, 
                 tri.points[1].x, 
                 tri.points[1].y, 
                 tri.points[2].x, 
                 tri.points[2].y,
                 OptionDefaults());
  end;

  //=============================================================================
  
  procedure DrawLine(clr : Color; x1, y1, x2, y2: Single);
  begin
    DrawLine(clr,x1,y1,x2,y2,OptionDefaults());
  end;

  procedure DrawLine(clr : Color; const l : LineSegment; const opts : DrawingOptions); overload;
  begin
    DrawLine(clr,l.startPoint.x,l.startPoint.y,l.endPoint.x,l.endPoint.y,opts);
  end;

  procedure DrawLine(clr : Color; const l : LineSegment);
  begin
    DrawLine(clr,l.startPoint.x,l.startPoint.y,l.endPoint.x,l.endPoint.y,OptionDefaults());
  end;

  //=============================================================================

  procedure DrawCircle(clr : Color; x, y, radius : Single);
  begin
    DrawCircle(clr, x, y, radius, OptionDefaults());
  end;

  procedure DrawCircle(clr : Color; const c: Circle; const opts : DrawingOptions); overload;
  begin
    DrawCircle(clr, c.center.x, c.center.y, c.radius, opts);
  end;

  procedure DrawCircle(clr : Color; const c: Circle);
  begin
    DrawCircle(clr, c.center.x, c.center.y, c.radius, OptionDefaults());
  end;

  procedure FillCircle(clr : Color; x, y, radius : Single);
  begin
    FillCircle(clr, x, y, radius, OptionDefaults());
  end;

  procedure FillCircle(clr : Color; const c: Circle; const opts : DrawingOptions); overload;
  begin
    FillCircle(clr, c.center.x, c.center.y, c.radius, opts);
  end;

  procedure FillCircle(clr : Color; const c: Circle);
  begin
    FillCircle(clr, c.center.x, c.center.y, c.radius, OptionDefaults());
  end;

   //=============================================================================

  procedure DrawEllipse(clr : Color; xPos, yPos, width, height: Single);
    begin
      DrawEllipse(clr, xPos, yPos, width, height, OptionDefaults());
    end;

  procedure DrawEllipse(clr : Color; const rec: Rectangle; const opts : DrawingOptions); overload;
    begin
      DrawEllipse(clr, rec.x, rec.y, rec.width, rec.height, opts);
    end;
    
  procedure DrawEllipse(clr : Color; const rec: Rectangle);
    begin
      DrawEllipse(clr, rec.x, rec.y, rec.width, rec.height, OptionDefaults());
    end;
       
  procedure FillEllipse(clr : Color; xPos, yPos, width, height: Single);
    begin
      FillEllipse(clr, xPos, yPos, width, height, OptionDefaults());
    end;
    
  procedure FillEllipse(clr : Color; const rec: Rectangle; const opts : DrawingOptions); overload;
    begin
      FillEllipse(clr,rec.x, rec.y, rec.width, rec.height, opts);
    end;
    
  procedure FillEllipse(clr : Color; const rec: Rectangle);
    begin
      FillEllipse(clr, rec.x, rec.y, rec.width, rec.height, OptionDefaults());
    end;
    
  //=============================================================================
  
  procedure DrawRectangle(clr : Color; xPos, yPos, width, height : Single; const opts : DrawingOptions); overload;
  var
    rect: Rectangle;
  begin

    if opts.dest = nil then begin RaiseWarning('DrawRectangle - No destination bitmap supplied'); exit; end;
    
    if width < 0 then
    begin
      rect.x := xPos + width; //move back by width
      width := -width;
    end else rect.x := xPos;
    
    if height < 0 then
    begin
      rect.y := yPos + height; //move up by height
      height := -height;
    end else rect.y := yPos;
    
    rect.width := Round(width);
    rect.height := Round(height);
    
    GraphicsDriver.DrawRectangle(rect, clr, opts);
  end;

  procedure FillRectangle(clr : Color;  xPos, yPos, width, height : Single; const opts : DrawingOptions);
  var
    rect: Rectangle;
  begin

    if opts.dest = nil then begin RaiseWarning('FillRectangle - No destination bitmap supplied'); exit; end;
    
    if width < 0 then
    begin
      rect.x := xPos + width; //move back by width
      width := -width;
    end else rect.x := xPos;
    
    if height < 0 then
    begin
      rect.y := yPos + height; //move up by height
      height := -height;
    end else rect.y := yPos;
    
    rect.width := Round(width);
    rect.height := Round(height);
    
    //SDL_FillRect(dest^.surface, @rect, clr);
    GraphicsDriver.FillRectangle(rect, clr, opts);

  end;

  procedure DrawTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions);
  begin
    GraphicsDriver.DrawTriangle(clr, x1, y1, x2, y2, x3, y3, opts);
  end;

  procedure FillTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions); overload;
  begin
    GraphicsDriver.FillTriangle(clr, x1, y1, x2, y2, x3, y3, opts);
  end;
  
  procedure DrawCircle(clr: Color; x, y, radius: Single; const opts : DrawingOptions); overload;
  begin
    GraphicsDriver.DrawCircle(clr, x, y, radius, opts);
  end;

  procedure FillCircle(clr: Color; x, y, radius: Single; const opts : DrawingOptions);
  begin
    GraphicsDriver.FillCircle(clr, x, y, radius, opts);
  end;

  procedure DrawLine(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single; const opts : DrawingOptions);
  begin
    if opts.dest = nil then 
      begin 
        RaiseWarning('DrawLine - No destination bitmap supplied');
        exit; 
      end;
    GraphicsDriver.DrawLine(clr, xPosStart, yPosStart, xPosEnd, yPosEnd, opts);
  end;

  procedure DrawEllipse(clr: Color; xPos, yPos, width, height: Single; const opts : DrawingOptions); overload;
  var
    halfWidth, halfHeight: Single;
  begin
    if width < 0 then
    begin
      xPos += width;
      width := -width;
    end;
    if height < 0 then
    begin
      yPos += height;
      height := -height;
    end;

    halfWidth := width / 2;
    halfHeight := height / 2;
    
    GraphicsDriver.DrawEllipse(clr, xPos + halfWidth, yPos + halfHeight, halfWidth, halfHeight, opts);
  end;

  procedure FillEllipse(clr: Color; xPos, yPos, width, height: Single; const opts : DrawingOptions);
  var
    halfWidth, halfHeight: Single;
  begin
    if width < 0 then
    begin
      xPos += width;
      width := -width;
    end;
    if height < 0 then
    begin
      yPos += height;
      height := -height;
    end;

    halfWidth := width / 2;
    halfHeight := height / 2;
    
    GraphicsDriver.FillEllipse(clr, xPos + halfWidth, yPos + halfHeight, halfWidth, halfHeight, opts);
  end;


  /// Draws a pixel onto the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param clr:     The color to draw the pixel
  /// @param x,y:         The x,y location to draw the pixel at
  ///
  /// Side Effects:
  /// - Sets one pixel on the destination bitmap
  procedure DrawPixel(dest: Bitmap; clr: Color; x, y: Longint); overload;
  begin
    if dest = nil then begin RaiseWarning('DrawPixel - No destination bitmap supplied'); exit; end;
    
    if (x < 0) or (x >= BitmapWidth(dest)) or (y < 0) or (y >= BitmapHeight(dest)) then 
    begin 
      // RaiseWarning('DrawPixel - Coordinate out of range of Bitmap supplied'); 
      exit; 
    end;
    GraphicsDriver.SetPixelColor(dest, x, y, clr);
  end;
  
  
  procedure DrawPixel(dest: Bitmap; clr: Color; const position : Point2D); overload;
  begin
    DrawPixel(dest, clr, Round(position.x), Round(position.y));
  end;

  procedure DrawPixel(clr: Color; const position: Point2D); overload;
  begin
    DrawPixel(clr, Round(position.x), Round(position.y));
  end;

  procedure DrawPixelOnScreen(clr: Color; const position: Point2D); overload;
  begin
    DrawPixelOnScreen(clr, Round(position.x), Round(position.y));
  end;
  
  procedure ResetClip(bmp: Bitmap); overload;
  begin
    GraphicsDriver.ResetClip(bmp);
  end;

  procedure ResetClip(); overload;
  begin
    ResetClip(screen);
  end;
  
  procedure DoSetClip(bmp: Bitmap; const r: Rectangle); overload;
  begin
    GraphicsDriver.SetClipRectangle(bmp, r);
  end;
  
  procedure PushClip(bmp: Bitmap; const r: Rectangle); overload;
  begin
    if bmp = nil then begin RaiseWarning('PushClip recieved empty Bitmap'); exit; end;

    SetLength(bmp^.clipStack, Length(bmp^.clipStack) + 1);

    if Length(bmp^.clipStack) > 1 then
    begin
      bmp^.clipStack[high(bmp^.clipStack)] := Intersection(r, bmp^.clipStack[High(bmp^.clipStack) - 1]);
    end
    else
      bmp^.clipStack[high(bmp^.clipStack)] := r;

    DoSetClip(bmp, bmp^.clipStack[high(bmp^.clipStack)]);
  end;

  procedure SetClip(x, y, w, h: Longint); overload;
  begin
    SetClip(screen, RectangleFrom(x, y, w, h));
  end;
  
  procedure SetClip(bmp: Bitmap; x, y, w, h: Longint); overload;
  begin
    SetClip(bmp, RectangleFrom(x, y, w, h));
  end;

  procedure PushClip(x, y, w, h: Longint); overload;
  begin
    PushClip(screen, RectangleFrom(x, y, w, h));
  end;
  
  procedure PushClip(const r: Rectangle); overload;
  begin
    PushClip(screen, r);
  end;

  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;
  begin
    if assigned(bmp) then
    begin
      SetLength(bmp^.clipStack, 0);
      PushClip(bmp, r);
    end;
  end;

  procedure SetClip(const r: Rectangle); overload;
  begin
    SetClip(screen, r);
  end;

  procedure PopClip(); overload;
  begin
    PopClip(screen);
  end;
  
  procedure PopClip(bmp: Bitmap); overload;
  begin
    if not Assigned(bmp) then exit;

    Setlength(bmp^.clipStack, Length(bmp^.clipStack)-1);
    if Length(bmp^.clipStack) > 0 then
      DoSetClip(bmp, bmp^.clipStack[High(bmp^.clipStack)])
    else
      ResetClip(bmp);
  end;

  function CurrentClip(bmp: Bitmap): Rectangle; overload;
  begin
    if not Assigned(bmp) then exit;
    
    if Length(bmp^.clipStack) <> 0 then result:= bmp^.clipStack[high(bmp^.clipStack)]
    else
      result:=BitmapRectangle(0, 0, bmp);
  end;

  function CurrentClip(): Rectangle; overload;
  begin
    result := CurrentClip(screen);
  end;

//----------------------------------------------------------------------------
// Set Icon / Window Open / Screen Size / Resize
//----------------------------------------------------------------------------

  procedure SetIcon(filename: String);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'SetIcon');
    {$ENDIF}
    iconFile := filename;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'SetIcon');
    {$ENDIF}
  end;
  
  procedure OpenGraphicsWindow(caption: String; width: Longint; height: Longint); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'OpenGraphicsWindow', caption + ': W' + IntToStr(width) + ': H' + IntToStr(height));
    {$ENDIF}

    if screen <> nil then
    begin
      RaiseWarning('Screen has been created. Cannot create multiple windows.');
      exit;
    end;

    try         
      GraphicsDriver.InitializeGraphicsWindow(caption, width, height);
      
      //Init the colors
      ColorWhite := RGBAColor(255, 255, 255, 255);
      ColorGreen := RGBAColor(0, 255, 0, 255);
      ColorBlue := RGBAColor(0, 0, 255, 255);
      ColorBlack := RGBAColor(0, 0, 0, 255);
      ColorRed := RGBAColor(255, 0, 0, 255);
      ColorYellow := RGBAColor(255, 255, 0, 255);
      ColorPink := RGBAColor(255, 20, 147, 255);
      ColorTurquoise := RGBAColor(0, 206, 209, 255);
      ColorGrey := RGBAColor(128, 128, 128, 255);
      ColorMagenta := RGBAColor(255, 0, 255, 255);
      ColorTransparent := RGBAColor(0, 0, 0, 0);
      ColorLightGrey := RGBAColor(200, 200, 200, 255);
      
      GraphicsDriver.InitializeScreen(screen, screenWidth div 2 - 30, screenHeight div 2, ColorWhite, ColorGrey, 'Loading ...');
      RefreshScreen();
    except on e: Exception do
      begin
        RaiseException('Error in OpenGraphicsWindow: ' + e.Message);
        exit;
      end;
    end;
    
    {$IFDEF TRACE}
      TraceIf(tlInfo, 'sgGraphics', 'Info', 'OpenGraphicsWindow', 'Window is open (' + caption + ' ' + IntToStr(width) + 'x' + IntToStr(height) + ')');
    {$ENDIF}
    
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'OpenGraphicsWindow');
    {$ENDIF}
  end;
  

  procedure OpenGraphicsWindow(caption: String); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'OpenGraphicsWindow');
    {$ENDIF}
    OpenGraphicsWindow(caption, 800,600);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'OpenGraphicsWindow');
    {$ENDIF}
  end;

  procedure ToggleFullScreen();
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ToggleFullScreen');
    {$ENDIF}

    try
      //Remember... _screen is a pointer to screen buffer, not a "surface"!
      GraphicsDriver.SetVideoModeFullScreen();
    except on exc: Exception do
    end;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ToggleFullScreen');
    {$ENDIF}
  end;
  
  procedure ToggleWindowBorder();
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ToggleWindowBorder');
    {$ENDIF}
    
    try
      //Remember... _screen is a pointer to screen buffer, not a "surface"!
      GraphicsDriver.SetVideoModeNoFrame();
    except on exc: Exception do
    end;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ToggleWindowBorder');
    {$ENDIF}
  end;

  procedure ChangeScreenSize(width, height: Longint);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ChangeScreenSize');
    {$ENDIF}
    if (screen = nil) then
    begin
      RaiseWarning('Screen has not been created. Unable to get screen width.');
      exit;
    end;

    if (width < 1) or (height < 1) then
    begin
      RaiseWarning('Screen Width and Height must be greater then 0 when resizing a Graphical Window');
      exit; 
    end;
    if (width = ScreenWidth()) and (height = ScreenHeight()) then exit;
    GraphicsDriver.ResizeGraphicsWindow(width, height);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ChangeScreenSize');
    {$ENDIF}
  end;

  function ScreenWidth(): Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ScreenWidth');
    {$ENDIF}
    if (_screen = nil) then
    begin
      RaiseWarning('Screen has not been created. Unable to get screen width.');
      exit;
    end;
    
    result := GraphicsDriver.GetScreenWidth();
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ScreenWidth');
    {$ENDIF}
  end;

  function ScreenHeight(): Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ScreenHeight');
    {$ENDIF}
    if (_screen = nil) then
    begin
      RaiseWarning('Screen has not been created. Unable to get screen height.');
      exit;
    end;
    result := GraphicsDriver.GetScreenHeight();
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ScreenHeight');
    {$ENDIF}
  end;

  procedure TakeScreenShot(basename: String);
  var
    path: String;
    filename: String;
    i: Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'TakeScreenShot');
    {$ENDIF}
    
    path := IncludeTrailingPathDelimiter(GetUserDir()) + 'Desktop' + PathDelim;
    if not DirectoryExists(path) then 
      path := IncludeTrailingPathDelimiter(GetUserDir());
    
    filename := basename + '.png';
    
    i := 1;
    
    while FileExists(path + filename) do
    begin
      filename := basename + IntToStr(i) + '.png';
      i := i + 1;
    end;
    
    ImagesDriver.SaveBitmap(screen, path + filename);
    
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'TakeScreenShot');
    {$ENDIF}
  end;

  procedure RefreshScreen(); overload;
  begin
    RefreshScreen(-1);
  end;

  procedure RefreshScreen(targetFPS: Longint); overload;
  var
    nowTime: Longword;
    delta, delayTime: Longword;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RefreshScreen');
    {$ENDIF}
    DrawCollectedText(screen);
    GraphicsDriver.RefreshScreen(screen);
    
    nowTime := GetTicks();
    delta := nowTime - _lastUpdateTime;
    
    //dont sleep if 5ms remaining...
    while (targetFPS > 0) and ((delta + 5) * targetFPS < 1000) do
    begin
      delayTime := (1000 div targetFPS) - delta;
      Delay(delayTime);
      nowTime := GetTicks();
      delta := nowTime - _lastUpdateTime;
    end;  

    _UpdateFPSData(delta);
    _lastUpdateTime := nowTime;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RefreshScreen');
    {$ENDIF}
  end;
  
  
  
//----------------------------------------------------------------------------
// Colour
//----------------------------------------------------------------------------
  function  ColorToString(c: Color): string;
  var
    r,g,b,a : byte;
  begin
    ColorComponents(c,r,g,b,a);
    result:=IntToStr(r)+','+IntToStr(g)+','+IntToStr(b)+','+IntToStr(a);
  end;

  procedure ColorComponents(c: Color; out r, g, b, a: byte);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ColorComponents');
    {$ENDIF}

    {$IFNDEF SWINGAME_OPENGL}
    if not ImagesDriver.SurfaceExists(screen) then
    begin
      RaiseWarning('Estimating color, screen not available.');
    {$ENDIF}
      a := (c and $FF000000) shr 24;
      r := (c and $00FF0000) shr 16;
      g := (c and $0000FF00) shr 8;
      b := (c and $000000FF);
    {$IFNDEF SWINGAME_OPENGL}
    end;
    GraphicsDriver.ColorComponents(c, r, g, b, a);
    {$ENDIF}

    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ColorComponents');
    {$ENDIF}
  end;

  function RedOf(c: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RedOf');
    {$ENDIF}
    ColorComponents(c, r, g, b, a);
    result := r;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RedOf');
    {$ENDIF}
  end;

  function GreenOf(c: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'GreenOf');
    {$ENDIF}
    ColorComponents(c, r, g, b, a);
    result := g;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'GreenOf');
    {$ENDIF}
  end;

  function BlueOf(c: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'BlueOf');
    {$ENDIF}
    ColorComponents(c, r, g, b, a);
    result := b;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'BlueOf');
    {$ENDIF}
  end;

  function TransparencyOf(c: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'TransparencyOf');
    {$ENDIF}
    ColorComponents(c, r, g, b, a);
    result := a;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'TransparencyOf');
    {$ENDIF}
  end;
  
  procedure HSBValuesOf(c: Color; out h, s, b: Single);
  var
    red, green, blue, alpha: byte;
    rf, gf, bf: Single;
    minRGB, maxRGB, delta: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'HSBValuesOf');
    {$ENDIF}
     H := 0.0 ;
     
     ColorComponents(c, red, green, blue, alpha);
     
     rf := red / 255;
     gf := green / 255;
     bf := blue / 255;
     
     minRGB := Min(Min(rf, gf), bf);
     maxRGB := Max(Max(rf, gf), bf);
     delta := (maxRGB - minRGB);
     
     b := maxRGB;
     if (maxRGB <> 0.0) then s := delta / maxRGB
     else s := 0.0;
      
     if (s <> 0.0) then
     begin
       if rf = maxRGB then h := (gf - bf) / Delta
       else
         if gf = maxRGB then h := 2.0 + (bf - rf) / Delta
         else
           if bf = maxRGB then h := 4.0 + (rf - gf) / Delta
     end
     else h := -1.0;
     h := h * 60 ;
     if h < 0.0 then h := h + 360.0;
       
     h := h / 360.0;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'HSBValuesOf');
    {$ENDIF}
  end;
  
  function HueOf(c: Color) : Single;
  var
    s, b: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'HueOf');
    {$ENDIF}
    HSBValuesOf(c, result, s, b);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'HueOf');
    {$ENDIF}
  end;
  
  function SaturationOf(c: Color) : Single;
  var
    h, b: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'SaturationOf');
    {$ENDIF}
    HSBValuesOf(c, h, result, b);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'SaturationOf');
    {$ENDIF}
  end;
  
  function BrightnessOf(c: Color) : Single;
  var
    h, s: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'BrightnessOf');
    {$ENDIF}
    HSBValuesOf(c, h, s, result);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'BrightnessOf');
    {$ENDIF}
  end;
  
  function ColorFrom(bmp: Bitmap; apiColor: Color): Color;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ColorFrom');
    {$ENDIF}
    if (bmp = nil) or not ImagesDriver.SurfaceExists(screen) then
    begin
      RaiseException('Unable to get color as bitmap not specified');
      exit;
    end;
    
    ColorComponents(apiColor, r, g, b, a);
    
    result := GraphicsDriver.ColorFrom(bmp, r, g, b, a);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ColorFrom');
    {$ENDIF}
  end;

  function RGBAColor(red, green, blue, alpha: Byte): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBAColor');
    {$ENDIF}

    {$IFNDEF SWINGAME_OPENGL}
    if not ImagesDriver.SurfaceExists(screen) then
    begin
    //   RaiseWarning('Estimating RGBAColor as the window is not open');
    {$ENDIF}
      result := (alpha shl 24) or (red shl 16) or (green shl 8) or (blue);
      exit;
    {$IFNDEF SWINGAME_OPENGL}
    end;

    try
      result := GraphicsDriver.RGBAColor(red, green, blue, alpha);
    except
      RaiseException('Error occured while trying to get a color from RGBA components');
      exit;
    end;
    {$ENDIF}
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RGBAColor');
    {$ENDIF}
  end;

  function RGBColor(red, green, blue: Byte): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBColor');
    {$ENDIF}
    result := RGBAColor(red, green, blue, 255);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RGBColor');
    {$ENDIF}
  end;

  function RGBFloatColor(r,g,b: Single): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBFloatColor');
    {$ENDIF}
    result := RGBColor(Round(r * 255), Round(g * 255), Round(b * 255));
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RGBFloatColor');
    {$ENDIF}
  end;
  
  function RGBAFloatColor(r,g,b, a: Single): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBAFloatColor');
    {$ENDIF}
    result := RGBAColor(Round(r * 255), Round(g * 255), Round(b * 255), Round(a * 255));
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RGBAFloatColor');
    {$ENDIF}
  end;

  function HSBColor(hue, saturation, brightness: Single): Color;
  var
    domainOffset: Single;
    red, green, blue: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'HSBColor');
    {$ENDIF}
    if brightness = 0 then
    begin
      result := ColorBlack;
      exit;
    end;

    if saturation = 0 then
    begin
      result := RGBFloatColor(brightness, brightness, brightness);
      exit;
    end;

    if hue < 1.0 / 6 then
    begin // red domain... green ascends
      domainOffset := hue;
      red   := brightness;
      blue  := brightness * (1.0 - saturation);
      green := blue + (brightness - blue) * domainOffset * 6;
    end
    else if hue < 2.0 / 6 then
    begin // yellow domain; red descends
      domainOffset := hue - 1.0 / 6;
      green := brightness;
      blue  := brightness * (1.0 - saturation);
      red   := green - (brightness - blue) * domainOffset * 6;
    end
    else if hue < 3.0 / 6 then
    begin // green domain; blue ascends
      domainOffset := hue - 2.0 / 6;
      green := brightness;
      red   := brightness * (1.0 - saturation);
      blue  := red + (brightness - red) * domainOffset * 6;
    end
    else if hue < 4.0 / 6 then
    begin // cyan domain; green descends
      domainOffset := hue - 3.0 / 6;
      blue  := brightness;
      red   := brightness * (1.0 - saturation);
      green := blue - (brightness - red) * domainOffset * 6;
    end
    else if hue < 5.0 / 6 then
    begin // blue domain; red ascends
      domainOffset := hue - 4.0 / 6;
      blue  := brightness;
      green := brightness * (1.0 - saturation);
      red   := green + (brightness - green) * domainOffset * 6;
    end
    else
    begin // magenta domain; blue descends
      domainOffset := hue - 5.0 / 6;
      red   := brightness;
      green := brightness * (1.0 - saturation);
      blue  := red - (brightness - green) * domainOffset * 6;
    end;

    result := RGBFloatColor(red, green, blue);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'HSBColor');
    {$ENDIF}
  end;
  
  function RandomColor(): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RandomColor');
    {$ENDIF}
    result := RGBAFloatColor(Rnd(), Rnd(), Rnd(), Rnd());
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RandomColor');
    {$ENDIF}
  end;
  
  function RandomRGBColor(alpha: Byte): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RandomRGBColor');
    {$ENDIF}
    result := RGBAColor(Byte(Rnd(256)), Byte(Rnd(256)), Byte(Rnd(256)), alpha);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RandomRGBColor');
    {$ENDIF}
  end;


  function AvailableResolutions(): ResolutionArray;
  begin
    result := GraphicsDriver.AvailableResolutions();
  end;

  function NumberOfResolutions(): Longint;
  begin
    result := Length(GraphicsDriver.AvailableResolutions());
  end;

//-----------------------------------------------------------------------------

    procedure ShowSwinGameSplashScreen();
    var
        aniX, aniY, txtX, txtY : LongInt;
        i: Longint;
        f: Font;
        txt: String;
        //oldW, oldH: Longint;
        //isStep: Boolean;
        isPaused: Boolean;
        isSkip: Boolean;
        startAni: Animation;
        aniBmp: sgTypes.Bitmap;
        
        procedure InnerProcessEvents();
        begin
            ProcessEvents();
            if (KeyDown(sgTypes.vk_LSUPER) or KeyDown(sgTypes.vk_LCTRL)) and KeyTyped(sgTypes.vk_p) then
            begin
                isPaused := not isPaused;
            end;
            if WindowCloseRequested() or KeyDown(sgTypes.vk_Escape) then isSkip := true;
        end;
    begin

        isPaused := false;
        isSkip := false;
        
        {$IFDEF TRACE}
            TraceEnter('sgGraphics', 'ShowSwinGameSplashScreen');
        {$ENDIF}
        try
            ClearScreen(ColorWhite);
            RefreshScreen();
            try
                //oldW := ScreenWidth();
                //oldH := ScreenHeight();
                //if (oldW <> 800) or (oldH <> 600) then ChangeScreenSize(800, 600);
                // ToggleWindowBorder();
                
                LoadResourceBundle('splash.txt', False);
                
                i := 1;
                while isPaused or (i < 120) do
                begin
                    aniBmp := BitmapNamed('Swinburne');
                    aniX := (ScreenWidth() - BitmapCellWidth(aniBmp)) div 2;
                    aniY := (ScreenHeight() - BitmapCellHeight(aniBmp)) div 2;
                    
                    ClearScreen(ColorWhite);
                    DrawBitmap(aniBmp, aniX, aniY);

                    f := FontNamed('SwinGameText');
                    txt := 'SwinGame API by Swinburne University of Technology';
                    txtX := (ScreenWidth() - TextWidth(f, txt)) div 2;
                    txtY := aniY + (ScreenHeight() - aniY + BitmapCellHeight(aniBmp)) div 2;

                    if txtY > aniY+ BitmapCellHeight(aniBmp) then DrawText(txt, ColorBlack, f, txtX, txtY );

                    f := FontNamed('LoadingFont');
                    DrawText(DLL_VERSION, ColorLightGrey, f, 5, ScreenHeight() - TextHeight(f, DLL_VERSION) - 2);
                    
                    i += 1;
                    InnerProcessEvents();
                    RefreshScreen(60);
                    if isSkip then break;
                end;
                
                aniBmp := BitmapNamed('SwinGameAni');
                aniX := (ScreenWidth() - BitmapCellWidth(aniBmp)) div 2;
                aniY := (ScreenHeight() - BitmapCellHeight(aniBmp)) div 2;
                
                {$IFDEF TRACE}
                    startAni := CreateAnimation('splash-debug', AnimationScriptNamed('Startup'));
                {$ELSE}
                    startAni := CreateAnimation('splash', AnimationScriptNamed('Startup'));
                {$ENDIF}
                while not AnimationEnded(startAni) do
                begin
                    ClearScreen(ColorWhite);
                    
                    DrawAnimation(startAni, aniBmp, aniX, aniY);
                    UpdateAnimation(startAni);
                    
                    RefreshScreen();
                    InnerProcessEvents();
                    if isSkip then break;
                    Delay(15);                  
                end;
                ClearScreen(ColorWhite);
                RefreshScreen();
                
                while SoundEffectPlaying(SoundEffectNamed('SwinGameStart')) or isPaused do
                begin
                    InnerProcessEvents();
                    if isSkip then break;
                end;
                
                StopSoundEffect('SwinGameStart');

                // i := 1;
                // while isPaused or (i < 30) do
                // begin
                //     i += 1;
                    
                //     InnerProcessEvents();
                //     RefreshScreen(60);
                //     if isSkip then break;
                // end;
            except on e:Exception do
                {$IFDEF TRACE}
                begin
                    Trace('sgGraphics', 'Error', 'ShowSwinGameSplashScreen', 'Error loading and drawing splash.');
                    Trace('sgGraphics', 'Error', 'ShowSwinGameSplashScreen', e.Message);
                end;
                {$ENDIF}
            end;
        finally
            try
                ReleaseResourceBundle('splash.txt');
            except on e1: Exception do
                begin
                    RaiseWarning('Error releating splash resources.');
                    {$IFDEF TRACE}
                        Trace('sgGraphics', 'Error', 'ShowSwinGameSplashScreen', 'Error freeing splash.');
                        Trace('sgGraphics', 'Error', 'ShowSwinGameSplashScreen', e1.Message);
                    {$ENDIF}
                 end;
            end;
            // ToggleWindowBorder();
            
            //if (oldW <> 800) or (oldH <> 600) then ChangeScreenSize(oldW, oldH);
        end;
        
        {$IFDEF TRACE}
            TraceExit('sgGraphics', 'ShowSwinGameSplashScreen');
        {$ENDIF}
    end;



//=============================================================================
  
  initialization
  begin
    InitialiseSwinGame();
  end;
end.
