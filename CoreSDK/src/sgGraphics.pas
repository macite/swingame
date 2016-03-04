//=============================================================================
// sgGraphics.pas
//=============================================================================
//
// The Graphics unit is responsible for all of the drawing of anything to the
// screen or other surfaces. The standard draw routines draw to the
// screen using the camera settings. Finally the overloaded drawing methods
// with a destination Bitmap will draw onto the supplied bitmap.
//
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
  procedure SetIcon(const filename: String);

  /// Returns the number of resolutions in the list of available resolutions.
  ///
  /// @lib
  function NumberOfResolutions(): Longint;

  /// Returns the details of one of the available resolutions. Use idx from 0 to
  /// `NumberOfResolutions` - 1 to access all of the available resolutions.
  ///
  /// @lib
  function AvailableResolution(idx: LongInt): Resolution;
  
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
  ///
  /// @deprecated
  procedure OpenGraphicsWindow(const caption: String; width, height: Longint); overload;

  /// Shows the SwinGame intro splash screen.
  /// It would be great if you could include this at the start of
  /// your game to help us promote the SwinGame API.
  ///
  /// @lib
  ///
  /// @deprecated
  procedure ShowSwinGameSplashScreen();

  /// Saves the current screen a bitmap file. The file will be saved into the
  /// current directory.
  ///
  /// @param basename   The base name for the screen shot. e.g. "GemCollector"
  ///
  /// Side Effects:
  /// - Saves the current screen image to a bitmap file.
  ///
  /// @lib TakeScreenshot
  procedure TakeScreenshot(const basename: String);
  
  
  
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
  
  /// Refresh the display on the passed in window.
  ///
  /// @lib RefreshScrenWindowFPS
  procedure RefreshScreen(wnd: Window; targetFPS: Longint); overload;
  
  
//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

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
  /// @sn drawCircleColor:%s atX:%s y:%s radius:%s opts:%s
  procedure DrawCircle(clr : Color; x, y, radius: Single; const opts : DrawingOptions); overload;
  
  /// Draw a circle in the game.
  ///
  /// @lib
  /// @sn drawCircleColor:%s atX:%s y:%s radius:%s
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
  /// @sn fillCircleColor:%s atX:%s y:%s radius:%s opts:%s
  procedure FillCircle(clr : Color; x, y, radius: Single; const opts : DrawingOptions); overload;
  
  /// Fill a circle in the game.
  ///
  /// @lib
  /// @sn fillCircleColor:%s atX:%s y:%s radius:%s
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
  
  /// Fill a circle at a given point using the passed in drawing options.
  ///
  /// @lib FillCircleAtPointWithOpts
  /// @sn fillCircleColor:%s at:%s radius:%s opts:%s
  procedure FillCircle(clr : Color; const pt: Point2D; radius: Longint; const opts : DrawingOptions); overload;
  
  /// Fill a circle in the game.
  ///
  /// @lib FillCircleAtPoint
  /// @sn fillCircleColor:%s at:%s radius:%s
  procedure FillCircle(clr : Color; const pt: Point2D; radius: Longint);
  
  
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
  procedure FillTriangle(clr: Color;  x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions); overload;
  
  /// Fill a triangle in the game.
  ///
  /// @lib
  /// @sn fillTriangleColor:%s atX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
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
  
  /// Draw a pixel in the game.
  ///
  /// @lib
  /// @sn drawPixel:%s atX:%s y:%s
  procedure DrawPixel(clr: Color; x, y: Single); overload;
  
  /// Draw a pixel in the game.
  ///
  /// @lib DrawPixelAtPoint
  /// @sn drawPixel:%s At:%s
  /// @doc_details
  procedure DrawPixel(clr: Color; const position: Point2D); overload;

  /// Draw a pixel with options.
  ///
  /// @lib DrawPixelOpts
  /// @sn drawPixel:%s atX:%s y:%s opts:%s
  /// @doc_details
  procedure DrawPixel(clr: Color; x, y: Single; const opts: DrawingOptions); overload;
  
  /// Draw a pixel with options.
  ///
  /// @lib DrawPixelAtPointOpts
  /// @sn drawPixel:%s at:%s opts:%s
  /// @doc_details
  procedure DrawPixel(clr: Color; const position: Point2D; const opts: DrawingOptions); overload;


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

  /// Draw a quad in the game.
  ///
  /// @lib DrawQuadStruct
  /// @sn drawQuadColor:%s data:%s
  procedure DrawQuad(clr : Color; const q: Quad); overload;

  /// Fill a quad in the game.
  ///
  /// @lib FillQuadStruct
  /// @sn fillQuadColor:%s data:%s
  procedure FillQuad(clr : Color; const q: Quad); overload;

  /// Draw a quad in the game.
  ///
  /// @lib DrawQuadStructOpts
  /// @sn drawQuadColor:%s data:%s opts:%s
  procedure DrawQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;

  /// Fill a quad in the game.
  ///
  /// @lib FillQuadStructOpts
  /// @sn fillQuadColor:%s data:%s opts:%s
  procedure FillQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;


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
  
  /// Draw a line with the provided DrawingOptions.
  ///
  /// @lib DrawLineOpts
  /// @sn drawLineColor:%s fromX:%s y:%s toX:%s y:%s opts:%s
  procedure DrawLine(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single; const opts : DrawingOptions);
  
  /// Draw a line in the game.
  ///
  /// @lib
  /// @sn drawLineColor:%s fromX:%s y:%s toX:%s y:%s
  ///
  /// @doc_idx 0
  procedure DrawLine(clr : Color; x1, y1, x2, y2: Single);

  /// Draw a line in the game from one point to another point.
  ///
  /// @lib DrawLinePt2PtOpts
  /// @sn drawLineColor:%s fromPt:%s toPt:%s opts:%s
  procedure DrawLine(clr: Color; const fromPt, toPt: Point2D; const opts : DrawingOptions);
  
  /// Draw a line in the game.
  ///
  /// @lib DrawLinePt2Pt
  /// @sn drawLineColor:%s fromPt:%s toPt:%s
  ///
  /// @doc_idx 0
  procedure DrawLine(clr : Color; const fromPt, toPt: Point2D);

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
  /// @sn drawEllipseColor:%s atX:%s y:%s width:%s height:%s opts:%s
  procedure DrawEllipse(clr : Color; xPos, yPos, width, height: Single; const opts : DrawingOptions); overload;
  
  /// Draw a ellipse in the game.
  ///
  /// @lib
  /// @sn drawEllipseColor:%s atX:%s y:%s width:%s height:%s
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
  /// @sn fillEllipseColor:%s atX:%s y:%s width:%s height:%s opts:%s
  procedure FillEllipse(clr : Color; xPos, yPos, width, height: Single; const opts : DrawingOptions); overload;
  
  /// Fill a ellipse in the game.
  ///
  /// @lib
  /// @sn fillEllipseColor:%s atX:%s y:%s width:%s height:%s
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
  
  /// Push a clip rectangle to the current window. This can be undone using PopClip.
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

  /// Add the clipping rectangle of a window and uses the intersect between the new rectangle and previous clip.
  ///
  /// @lib PushClipRectForWindow
  /// @sn window:%s PushClipRect:%s
  ///
  /// @class Window
  /// @overload PushClip PushClipRect
  /// @csn pushClip:%s
  procedure PushClip(wnd: Window; const r: Rectangle); overload;
  
  /// Reset the clipping rectangle of the current window.
  ///
  /// @lib
  procedure ResetClip(); overload;

  /// Reset the clipping rectangle on a window.
  ///
  /// @lib ResetClipForWindow
  ///
  /// @class Window
  /// @method ResetClip
  procedure ResetClip(wnd: Window); overload;
  
  /// Reset the clipping rectangle on a bitmap.
  ///
  /// @lib ResetClipForBitmap
  ///
  /// @class Bitmap
  /// @method ResetClip
  procedure ResetClip(bmp: Bitmap); overload;

  /// Set the clip rectangle of the bitmap.
  ///
  /// @lib SetClipForBitmap
  /// @sn bitmap:%s setClip:%s
  ///
  /// @class Bitmap
  /// @method SetClip
  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;

  /// Set the clip rectangle of a window.
  ///
  /// @lib SetClipForWindow
  /// @sn window:%s setClip:%s
  ///
  /// @class Window
  /// @method SetClip
  procedure SetClip(wnd: Window; const r: Rectangle); overload;

  /// Set the clip rectangle of the current window.
  ///
  /// @lib SetClip
  /// @sn setClip:%s
  procedure SetClip(const r: Rectangle); overload;
  
  /// Pop the clip rectangle of the screen.
  ///
  /// @lib PopClipScreen
  procedure PopClip(); overload;

  /// Pop the clipping rectangle of a bitmap.
  ///
  /// @lib PopClipForBitmap
  /// @sn popClipForBitmap:%s
  ///
  /// @class Bitmap
  /// @method PopClip
  procedure PopClip(bmp: Bitmap); overload;

  /// Pop the clipping rectangle of a bitmap.
  ///
  /// @lib PopClipForWindow
  /// @sn popClipForWindow:%s
  ///
  /// @class Window
  /// @method PopClip
  procedure PopClip(wnd: Window); overload;

  /// Returns the rectangle of the current clip area for a bitmap
  ///
  /// @lib CurrentClipForBitmap
  /// @sn currentClipForBitmap:%s
  ///
  /// @class Bitmap
  /// @getter CurrentClip
  function CurrentClip(bmp: Bitmap): Rectangle; overload;

  /// Returns the rectangle of the clip area for a window
  ///
  /// @lib CurrentClipForWindow
  /// @sn currentClipForWindow:%s
  ///
  /// @class Window
  /// @getter CurrentClip
  function CurrentClip(wnd: Window): Rectangle; overload;

  /// Returns the rectangle of the clip area of the current window
  ///
  /// @lib CurrentWindowClip
  function CurrentClip(): Rectangle; overload;
  
  
  
//---------------------------------------------------------------------------
// Pixel reading functions
//---------------------------------------------------------------------------
  
  /// Returns the color of the pixel at the x,y location on
  /// the supplied bitmap.
  ///
  /// @lib GetPixelFromBitmap
  /// @sn bitmap:%s colorAtX:%s y:%s
  ///
  /// @class Bitmap
  /// @method GetPixel
  /// @csn colorAtX:%s y:%s
  function GetPixel(bmp: Bitmap; x, y: Single): Color;

  /// Returns the color of the pixel at the x,y location on
  /// the supplied window.
  ///
  /// @lib GetPixelFromWindow
  /// @sn WindowPixelColor:%s x:%s y:%s
  ///
  /// @class Window
  /// @method GetPixel
  /// @csn colorAtX:%s y:%s
  function GetPixel(wnd: Window; x, y: Single): Color;
  
  /// Returns the color of the pixel at the given x,y location.
  ///
  /// @lib
  /// @sn colorOnScreenAtX:%s y:%s
  function GetPixelFromScreen(x, y: Single): Color;
  
  
  
//---------------------------------------------------------------------------
// Color Functions
//---------------------------------------------------------------------------

  /// The color Swinburne Red
  ///
  /// @lib
  function ColorSwinburneRed(): Color;

  /// The color Grey
  ///
  /// @lib
  function ColorGrey(): Color;

  /// The color Transparent
  ///
  /// @lib
  function ColorLightGrey(): Color;

  
  /// The color Transparent
  ///
  /// @lib
  function ColorTransparent(): Color;

  /// The color AliceBlue
  ///
  /// @lib
  function ColorAliceBlue(): Color;

  /// The color AntiqueWhite
  ///
  /// @lib
  function ColorAntiqueWhite(): Color;

  /// The color Aqua
  ///
  /// @lib
  function ColorAqua(): Color;

  /// The color Aquamarine
  ///
  /// @lib
  function ColorAquamarine(): Color;

  /// The color Azure
  ///
  /// @lib
  function ColorAzure(): Color;

  /// The color Beige
  ///
  /// @lib
  function ColorBeige(): Color;

  /// The color Bisque
  ///
  /// @lib
  function ColorBisque(): Color;

  /// The color Black
  ///
  /// @lib
  function ColorBlack(): Color;

  /// The color BlanchedAlmond
  ///
  /// @lib
  function ColorBlanchedAlmond(): Color;

  /// The color Blue
  ///
  /// @lib
  function ColorBlue(): Color;

  /// The color BlueViolet
  ///
  /// @lib
  function ColorBlueViolet(): Color;

  /// The color Brown
  ///
  /// @lib
  function ColorBrown(): Color;

  /// The color BurlyWood
  ///
  /// @lib
  function ColorBurlyWood(): Color;

  /// The color CadetBlue
  ///
  /// @lib
  function ColorCadetBlue(): Color;

  /// The color Chartreuse
  ///
  /// @lib
  function ColorChartreuse(): Color;

  /// The color Chocolate
  ///
  /// @lib
  function ColorChocolate(): Color;

  /// The color Coral
  ///
  /// @lib
  function ColorCoral(): Color;

  /// The color CornflowerBlue
  ///
  /// @lib
  function ColorCornflowerBlue(): Color;

  /// The color Cornsilk
  ///
  /// @lib
  function ColorCornsilk(): Color;

  /// The color Crimson
  ///
  /// @lib
  function ColorCrimson(): Color;

  /// The color Cyan
  ///
  /// @lib
  function ColorCyan(): Color;

  /// The color DarkBlue
  ///
  /// @lib
  function ColorDarkBlue(): Color;

  /// The color DarkCyan
  ///
  /// @lib
  function ColorDarkCyan(): Color;

  /// The color DarkGoldenrod
  ///
  /// @lib
  function ColorDarkGoldenrod(): Color;

  /// The color DarkGray
  ///
  /// @lib
  function ColorDarkGray(): Color;

  /// The color DarkGreen
  ///
  /// @lib
  function ColorDarkGreen(): Color;

  /// The color DarkKhaki
  ///
  /// @lib
  function ColorDarkKhaki(): Color;

  /// The color DarkMagenta
  ///
  /// @lib
  function ColorDarkMagenta(): Color;

  /// The color DarkOliveGreen
  ///
  /// @lib
  function ColorDarkOliveGreen(): Color;

  /// The color DarkOrange
  ///
  /// @lib
  function ColorDarkOrange(): Color;

  /// The color DarkOrchid
  ///
  /// @lib
  function ColorDarkOrchid(): Color;

  /// The color DarkRed
  ///
  /// @lib
  function ColorDarkRed(): Color;

  /// The color DarkSalmon
  ///
  /// @lib
  function ColorDarkSalmon(): Color;

  /// The color DarkSeaGreen
  ///
  /// @lib
  function ColorDarkSeaGreen(): Color;

  /// The color DarkSlateBlue
  ///
  /// @lib
  function ColorDarkSlateBlue(): Color;

  /// The color DarkSlateGray
  ///
  /// @lib
  function ColorDarkSlateGray(): Color;

  /// The color DarkTurquoise
  ///
  /// @lib
  function ColorDarkTurquoise(): Color;

  /// The color DarkViolet
  ///
  /// @lib
  function ColorDarkViolet(): Color;

  /// The color DeepPink
  ///
  /// @lib
  function ColorDeepPink(): Color;

  /// The color DeepSkyBlue
  ///
  /// @lib
  function ColorDeepSkyBlue(): Color;

  /// The color DimGray
  ///
  /// @lib
  function ColorDimGray(): Color;

  /// The color DodgerBlue
  ///
  /// @lib
  function ColorDodgerBlue(): Color;

  /// The color Firebrick
  ///
  /// @lib
  function ColorFirebrick(): Color;

  /// The color FloralWhite
  ///
  /// @lib
  function ColorFloralWhite(): Color;

  /// The color ForestGreen
  ///
  /// @lib
  function ColorForestGreen(): Color;

  /// The color Fuchsia
  ///
  /// @lib
  function ColorFuchsia(): Color;

  /// The color Gainsboro
  ///
  /// @lib
  function ColorGainsboro(): Color;

  /// The color GhostWhite
  ///
  /// @lib
  function ColorGhostWhite(): Color;

  /// The color Gold
  ///
  /// @lib
  function ColorGold(): Color;

  /// The color Goldenrod
  ///
  /// @lib
  function ColorGoldenrod(): Color;

  /// The color Gray
  ///
  /// @lib
  function ColorGray(): Color;

  /// The color Green
  ///
  /// @lib
  function ColorGreen(): Color;

  /// The color Green
  ///
  /// @lib
  function ColorBrightGreen(): Color;

  /// The color GreenYellow
  ///
  /// @lib
  function ColorGreenYellow(): Color;

  /// The color Honeydew
  ///
  /// @lib
  function ColorHoneydew(): Color;

  /// The color HotPink
  ///
  /// @lib
  function ColorHotPink(): Color;

  /// The color IndianRed
  ///
  /// @lib
  function ColorIndianRed(): Color;

  /// The color Indigo
  ///
  /// @lib
  function ColorIndigo(): Color;

  /// The color Ivory
  ///
  /// @lib
  function ColorIvory(): Color;

  /// The color Khaki
  ///
  /// @lib
  function ColorKhaki(): Color;

  /// The color Lavender
  ///
  /// @lib
  function ColorLavender(): Color;

  /// The color LavenderBlush
  ///
  /// @lib
  function ColorLavenderBlush(): Color;

  /// The color LawnGreen
  ///
  /// @lib
  function ColorLawnGreen(): Color;

  /// The color LemonChiffon
  ///
  /// @lib
  function ColorLemonChiffon(): Color;

  /// The color LightBlue
  ///
  /// @lib
  function ColorLightBlue(): Color;

  /// The color LightCoral
  ///
  /// @lib
  function ColorLightCoral(): Color;

  /// The color LightCyan
  ///
  /// @lib
  function ColorLightCyan(): Color;

  /// The color LightGoldenrodYellow
  ///
  /// @lib
  function ColorLightGoldenrodYellow(): Color;

  /// The color LightGreen
  ///
  /// @lib
  function ColorLightGreen(): Color;

  /// The color LightGray
  ///
  /// @lib
  function ColorLightGray(): Color;

  /// The color LightPink
  ///
  /// @lib
  function ColorLightPink(): Color;

  /// The color LightSalmon
  ///
  /// @lib
  function ColorLightSalmon(): Color;

  /// The color LightSeaGreen
  ///
  /// @lib
  function ColorLightSeaGreen(): Color;

  /// The color LightSkyBlue
  ///
  /// @lib
  function ColorLightSkyBlue(): Color;

  /// The color LightSlateGray
  ///
  /// @lib
  function ColorLightSlateGray(): Color;

  /// The color LightSteelBlue
  ///
  /// @lib
  function ColorLightSteelBlue(): Color;

  /// The color LightYellow
  ///
  /// @lib
  function ColorLightYellow(): Color;

  /// The color Lime
  ///
  /// @lib
  function ColorLime(): Color;

  /// The color LimeGreen
  ///
  /// @lib
  function ColorLimeGreen(): Color;

  /// The color Linen
  ///
  /// @lib
  function ColorLinen(): Color;

  /// The color Magenta
  ///
  /// @lib
  function ColorMagenta(): Color;

  /// The color Maroon
  ///
  /// @lib
  function ColorMaroon(): Color;

  /// The color MediumAquamarine
  ///
  /// @lib
  function ColorMediumAquamarine(): Color;

  /// The color MediumBlue
  ///
  /// @lib
  function ColorMediumBlue(): Color;

  /// The color MediumOrchid
  ///
  /// @lib
  function ColorMediumOrchid(): Color;

  /// The color MediumPurple
  ///
  /// @lib
  function ColorMediumPurple(): Color;

  /// The color MediumSeaGreen
  ///
  /// @lib
  function ColorMediumSeaGreen(): Color;

  /// The color MediumSlateBlue
  ///
  /// @lib
  function ColorMediumSlateBlue(): Color;

  /// The color MediumSpringGreen
  ///
  /// @lib
  function ColorMediumSpringGreen(): Color;

  /// The color MediumTurquoise
  ///
  /// @lib
  function ColorMediumTurquoise(): Color;

  /// The color MediumVioletRed
  ///
  /// @lib
  function ColorMediumVioletRed(): Color;

  /// The color MidnightBlue
  ///
  /// @lib
  function ColorMidnightBlue(): Color;

  /// The color MintCream
  ///
  /// @lib
  function ColorMintCream(): Color;

  /// The color MistyRose
  ///
  /// @lib
  function ColorMistyRose(): Color;

  /// The color Moccasin
  ///
  /// @lib
  function ColorMoccasin(): Color;

  /// The color NavajoWhite
  ///
  /// @lib
  function ColorNavajoWhite(): Color;

  /// The color Navy
  ///
  /// @lib
  function ColorNavy(): Color;

  /// The color OldLace
  ///
  /// @lib
  function ColorOldLace(): Color;

  /// The color Olive
  ///
  /// @lib
  function ColorOlive(): Color;

  /// The color OliveDrab
  ///
  /// @lib
  function ColorOliveDrab(): Color;

  /// The color Orange
  ///
  /// @lib
  function ColorOrange(): Color;

  /// The color OrangeRed
  ///
  /// @lib
  function ColorOrangeRed(): Color;

  /// The color Orchid
  ///
  /// @lib
  function ColorOrchid(): Color;

  /// The color PaleGoldenrod
  ///
  /// @lib
  function ColorPaleGoldenrod(): Color;

  /// The color PaleGreen
  ///
  /// @lib
  function ColorPaleGreen(): Color;

  /// The color PaleTurquoise
  ///
  /// @lib
  function ColorPaleTurquoise(): Color;

  /// The color PaleVioletRed
  ///
  /// @lib
  function ColorPaleVioletRed(): Color;

  /// The color PapayaWhip
  ///
  /// @lib
  function ColorPapayaWhip(): Color;

  /// The color PeachPuff
  ///
  /// @lib
  function ColorPeachPuff(): Color;

  /// The color Peru
  ///
  /// @lib
  function ColorPeru(): Color;

  /// The color Pink
  ///
  /// @lib
  function ColorPink(): Color;

  /// The color Plum
  ///
  /// @lib
  function ColorPlum(): Color;

  /// The color PowderBlue
  ///
  /// @lib
  function ColorPowderBlue(): Color;

  /// The color Purple
  ///
  /// @lib
  function ColorPurple(): Color;

  /// The color Red
  ///
  /// @lib
  function ColorRed(): Color;

  /// The color RosyBrown
  ///
  /// @lib
  function ColorRosyBrown(): Color;

  /// The color RoyalBlue
  ///
  /// @lib
  function ColorRoyalBlue(): Color;

  /// The color SaddleBrown
  ///
  /// @lib
  function ColorSaddleBrown(): Color;

  /// The color Salmon
  ///
  /// @lib
  function ColorSalmon(): Color;

  /// The color SandyBrown
  ///
  /// @lib
  function ColorSandyBrown(): Color;

  /// The color SeaGreen
  ///
  /// @lib
  function ColorSeaGreen(): Color;

  /// The color SeaShell
  ///
  /// @lib
  function ColorSeaShell(): Color;

  /// The color Sienna
  ///
  /// @lib
  function ColorSienna(): Color;

  /// The color Silver
  ///
  /// @lib
  function ColorSilver(): Color;

  /// The color SkyBlue
  ///
  /// @lib
  function ColorSkyBlue(): Color;

  /// The color SlateBlue
  ///
  /// @lib
  function ColorSlateBlue(): Color;

  /// The color SlateGray
  ///
  /// @lib
  function ColorSlateGray(): Color;

  /// The color Snow
  ///
  /// @lib
  function ColorSnow(): Color;

  /// The color SpringGreen
  ///
  /// @lib
  function ColorSpringGreen(): Color;

  /// The color SteelBlue
  ///
  /// @lib
  function ColorSteelBlue(): Color;

  /// The color Tan
  ///
  /// @lib
  function ColorTan(): Color;

  /// The color Teal
  ///
  /// @lib
  function ColorTeal(): Color;

  /// The color Thistle
  ///
  /// @lib
  function ColorThistle(): Color;

  /// The color Tomato
  ///
  /// @lib
  function ColorTomato(): Color;

  /// The color Turquoise
  ///
  /// @lib
  function ColorTurquoise(): Color;

  /// The color Violet
  ///
  /// @lib
  function ColorViolet(): Color;

  /// The color Wheat
  ///
  /// @lib
  function ColorWheat(): Color;

  /// The color White
  ///
  /// @lib
  function ColorWhite(): Color;

  /// The color WhiteSmoke
  ///
  /// @lib
  function ColorWhiteSmoke(): Color;

  /// The color Yellow
  ///
  /// @lib
  function ColorYellow(): Color;

  /// The color YellowGreen
  ///
  /// @lib
  function ColorYellowGreen(): Color;
  
//=============================================================================
implementation
//=============================================================================

  uses Math, Classes, SysUtils, // system
       sgTrace,
       sgCamera, sgShared, sgGeometry, sgResources, sgImages, sgUtils, sgDriverGraphics, sgDriver, sgDriverImages, sgInput, sgAudio, sgText, sgAnimations, sgDrawingOptions,
       sgInputBackend, sgBackendTypes, sgWindowManager, sgDriverSDL2Types;

  /// Clears the surface of the screen to the passed in color.
  ///
  /// @param toColor: The colour to clear the bitmap to
  ///
  /// Side Effects:
  /// - Screen's surface is set to the toColor
  procedure ClearScreen(toColor : Color); overload;
  begin
    ClearSurface(ToSurfacePtr(_CurrentWindow), toColor);
  end;

  /// Clears the screen to Black.
  ///
  /// Side Effects:
  /// - screen's surface is set to black
  procedure ClearScreen(); overload;
  begin
    ClearScreen(ColorWhite);
  end;

  function GetPixel(bmp: Bitmap; x, y: Single): Color;
  begin
    result := sgDriverGraphics.GetPixel(ToSurfacePtr(bmp), x, y);
  end;

  function GetPixel(wnd: Window; x, y: Single): Color;
  begin
    result := sgDriverGraphics.GetPixel(ToSurfacePtr(wnd), x, y);
  end;

  function GetPixelFromScreen(x, y: Single): Color;
  begin
    result := sgDriverGraphics.GetPixel(ToSurfacePtr(_CurrentWindow), x, y);
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
    DrawRectangle(clr, rect.x, rect.y, rect.width, rect.height, opts);
  end;

  procedure FillRectangle(clr : Color; x, y, width, height : Single);
  begin
    FillRectangle(clr, x, y, width, height, OptionDefaults());
  end;

  procedure FillRectangle(clr : Color; const rect : Rectangle);
  begin
    FillRectangle(clr, rect.x, rect.y, rect.width, rect.height, OptionDefaults());
  end;

  procedure FillRectangle(clr : Color; const rect : Rectangle; const opts : DrawingOptions);
  begin
    FillRectangle(clr, rect.x, rect.y, rect.width, rect.height, opts);
  end;

//=============================================================================

  procedure DrawTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    DrawTriangle(clr, x1, y1, x2, y2, x3, y3, OptionDefaults());
  end;

  procedure DrawTriangle(clr : Color; const tri: Triangle; const opts : DrawingOptions); overload;
  begin
    DrawTriangle(clr,
                 tri.points[0].x, tri.points[0].y,
                 tri.points[1].x, tri.points[1].y,
                 tri.points[2].x, tri.points[2].y,
                 opts);
  end;

  procedure DrawTriangle(clr : Color; const tri: Triangle);
  begin
    DrawTriangle(clr,
                 tri.points[0].x, tri.points[0].y,
                 tri.points[1].x, tri.points[1].y,
                 tri.points[2].x, tri.points[2].y,
                 OptionDefaults());
  end;

  procedure FillTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single);
  begin
    FillTriangle(clr, x1, y1, x2, y2, x3, y3, OptionDefaults());
  end;

  procedure FillTriangle(clr : Color; const tri: Triangle; const opts : DrawingOptions); overload;
  begin
    FillTriangle(clr,
                 tri.points[0].x, tri.points[0].y,
                 tri.points[1].x, tri.points[1].y,
                 tri.points[2].x, tri.points[2].y,
                 opts);
  end;

  procedure FillTriangle(clr : Color; const tri: Triangle);
  begin
    FillTriangle(clr,
                 tri.points[0].x, tri.points[0].y,
                 tri.points[1].x, tri.points[1].y,
                 tri.points[2].x, tri.points[2].y,
                 OptionDefaults());
  end;

  //=============================================================================
  
  procedure DrawLine(clr : Color; x1, y1, x2, y2: Single);
  begin
    DrawLine(clr,x1,y1,x2,y2,OptionDefaults());
  end;

  procedure DrawLine(clr: Color; const fromPt, toPt: Point2D; const opts : DrawingOptions);
  begin
    DrawLine(clr, fromPt.x, fromPt.y, toPt.x, toPt.y, opts);
  end;

  procedure DrawLine(clr : Color; const fromPt, toPt: Point2D);
  begin
    DrawLine(clr, fromPt.x, fromPt.y, toPt.x, toPt.y, OptionDefaults());
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

  procedure FillCircle(clr : Color; const pt: Point2D; radius: Longint; const opts : DrawingOptions); overload;
  begin
    FillCircle(clr, pt.x, pt.y, radius, opts);
  end;

  procedure FillCircle(clr : Color; const pt: Point2D; radius: Longint);
  begin
    FillCircle(clr, pt.x, pt.y, radius, OptionDefaults());
  end;

   //=============================================================================

  procedure DrawEllipse(clr: Color; xPos, yPos, width, height: Single; const opts : DrawingOptions); overload;
  begin
    sgDriverGraphics.DrawEllipse(clr, xPos, yPos, width, height, opts);
  end;

  procedure DrawEllipse(clr : Color; xPos, yPos, width, height: Single);
  begin
    sgDriverGraphics.DrawEllipse(clr, xPos, yPos, width, height, OptionDefaults());
  end;

  procedure DrawEllipse(clr : Color; const rec: Rectangle; const opts : DrawingOptions); overload;
  begin
    sgDriverGraphics.DrawEllipse(clr, rec.x, rec.y, rec.width, rec.height, opts);
  end;
    
  procedure DrawEllipse(clr : Color; const rec: Rectangle);
  begin
    sgDriverGraphics.DrawEllipse(clr, rec.x, rec.y, rec.width, rec.height, OptionDefaults());
  end;

  //=============================================================================
       
  procedure FillEllipse(clr: Color; xPos, yPos, width, height: Single; const opts : DrawingOptions);
  begin
    sgDriverGraphics.FillEllipse(clr, xPos, yPos, width, height, opts);
  end;

  procedure FillEllipse(clr : Color; xPos, yPos, width, height: Single);
  begin
    sgDriverGraphics.FillEllipse(clr, xPos, yPos, width, height, OptionDefaults());
  end;
    
  procedure FillEllipse(clr : Color; const rec: Rectangle; const opts : DrawingOptions); overload;
  begin
    sgDriverGraphics.FillEllipse(clr, rec.x, rec.y, rec.width, rec.height, opts);
  end;
    
  procedure FillEllipse(clr : Color; const rec: Rectangle);
  begin
    sgDriverGraphics.FillEllipse(clr, rec.x, rec.y, rec.width, rec.height, OptionDefaults());
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
    
    sgDriverGraphics.DrawRectangle(clr, rect, opts);
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
    
    sgDriverGraphics.FillRectangle(clr, rect, opts);
  end;

  procedure DrawTriangle(clr : Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions);
  begin
      if opts.dest = nil then
      begin
        RaiseWarning('DrawTriangle - No destination bitmap supplied');
        exit;
      end;
    sgDriverGraphics.DrawTriangle(clr, x1, y1, x2, y2, x3, y3, opts);
  end;

  procedure FillTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single; const opts : DrawingOptions); overload;
  begin
      if opts.dest = nil then
      begin
        RaiseWarning('FillTriangle - No destination bitmap supplied');
        exit;
      end;
    sgDriverGraphics.FillTriangle(clr, x1, y1, x2, y2, x3, y3, opts);
  end;
  
  procedure DrawCircle(clr: Color; x, y, radius: Single; const opts : DrawingOptions); overload;
  begin
      if opts.dest = nil then
      begin
        RaiseWarning('DrawCircle - No destination bitmap supplied');
        exit;
      end;
    sgDriverGraphics.DrawCircle(clr, x, y, radius, opts);
  end;

  procedure FillCircle(clr: Color; x, y, radius: Single; const opts : DrawingOptions);
  begin
     if opts.dest = nil then
      begin
        RaiseWarning('FillCircle - No destination bitmap supplied');
        exit;
      end;
    sgDriverGraphics.FillCircle(clr, x, y, radius, opts);
  end;

  procedure DrawLine(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single; const opts : DrawingOptions);
  begin
    if opts.dest = nil then
      begin
        RaiseWarning('DrawLine - No destination bitmap supplied');
        exit;
      end;
    sgDriverGraphics.DrawLine(clr, xPosStart, yPosStart, xPosEnd, yPosEnd, opts);
  end;


  //=============================================================================

  procedure DrawPixel(clr: Color; x, y: Single; const opts: DrawingOptions); overload;
  begin
    if opts.dest = nil then begin RaiseWarning('DrawPixel - No destination supplied'); exit; end;
    
    sgDriverGraphics.DrawPixel(clr, x, y, opts);
  end;

  procedure DrawPixel(clr: Color; x, y: Single); overload;
  begin
    DrawPixel(clr, x, y, OptionDefaults());
  end;

  procedure DrawPixel(clr: Color; const position : Point2D; const opts: DrawingOptions); overload;
  begin
    DrawPixel(clr, position.x, position.y, opts);
  end;

  procedure DrawPixel(clr: Color; const position: Point2D); overload;
  begin
    DrawPixel(clr, position.x, position.y, OptionDefaults());
  end;

  //=============================================================================


  procedure ResetClip(var img: ImageData); overload;
  begin
    SetLength(img.clipStack, 0);
    sgDriverGraphics.ResetClip(@img.surface);
  end;
  
  procedure ResetClip(bmp: Bitmap); overload;
  var
    b: BitmapPtr;
  begin
    b := ToBitmapPtr(bmp);
    if Assigned(b) then ResetClip(b^.image);
  end;

  procedure ResetClip(wnd: Window); overload;
  var
    w: WindowPtr;
  begin
    w := ToWindowPtr(wnd);
    if Assigned(w) then ResetClip(w^.image);
  end;

  procedure ResetClip(); overload;
  var
    surf: psg_drawing_surface;
  begin
    surf := ToSurfacePtr(_CurrentWindow);
    if Assigned(surf) then
      ResetClip(surf);
  end;
  
  procedure DoSetClip(surf: psg_drawing_surface; const r: Rectangle); overload;
  begin
    sgDriverGraphics.SetClipRectangle(surf, r);
  end;
  
  procedure PushClip(var img: ImageData; const r: Rectangle); overload;
  begin
    SetLength(img.clipStack, Length(img.clipStack) + 1);

    if Length(img.clipStack) > 1 then
    begin
      img.clipStack[high(img.clipStack)] := Intersection(r, img.clipStack[High(img.clipStack) - 1]);
    end
    else
      img.clipStack[high(img.clipStack)] := r;

    DoSetClip(@img.surface, img.clipStack[high(img.clipStack)]);
  end;

  procedure PushClip(bmp: Bitmap; const r: Rectangle); overload;
  var
    b: BitmapPtr;
  begin
    b := ToBitmapPtr(bmp);

    if b = nil then begin exit; end;

    PushClip(b^.image, r);
  end;

  procedure PushClip(wnd: Window; const r: Rectangle); overload;
  var
    w: WindowPtr;
  begin
    w := ToWindowPtr(wnd);

    if w = nil then begin exit; end;

    PushClip(w^.image, r);
  end;

  procedure PushClip(const r: Rectangle); overload;
  begin
    PushClip(Window(_CurrentWindow), r);
  end;

  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;
  var
    b: BitmapPtr;
  begin
    b := ToBitmapPtr(bmp);

    if assigned(b) then
    begin
      SetLength(b^.image.clipStack, 0);
      PushClip(bmp, r);
    end;
  end;

  procedure SetClip(wnd: Window; const r: Rectangle); overload;
  var
    w: WindowPtr;
  begin
    w := ToWindowPtr(wnd);

    if assigned(w) then
    begin
      SetLength(w^.image.clipStack, 0);
      PushClip(wnd, r);
    end;
  end;

  procedure SetClip(const r: Rectangle); overload;
  begin
    SetClip(Window(_CurrentWindow), r);
  end;

  procedure PopClip(); overload;
  begin
    PopClip(Window(_CurrentWindow));
  end;
  
  procedure PopClip(var img: ImageData); overload;
  begin
    Setlength(img.clipStack, Length(img.clipStack)-1);
    if Length(img.clipStack) > 0 then
      DoSetClip(@img.surface, img.clipStack[High(img.clipStack)])
    else
      ResetClip(img);
  end;

  procedure PopClip(bmp: Bitmap); overload;
  var
    b: BitmapPtr;
  begin
    b := ToBitmapPtr(bmp);
    if not Assigned(b) then exit;

    PopClip(b^.image);
  end;

  procedure PopClip(wnd: Window); overload;
  var
    w: WindowPtr;
  begin
    w := ToWindowPtr(wnd);
    if not Assigned(w) then exit;

    PopClip(w^.image);
  end;

  function CurrentClip(const img: ImageData): Rectangle; overload;
  begin
    if Length(img.clipStack) <> 0 then
      result := img.clipStack[high(img.clipStack)]
    else
      result := RectangleFrom(0, 0, img.surface.width, img.surface.height);
  end;

  function CurrentClip(bmp: Bitmap): Rectangle; overload;
  var
    b: BitmapPtr;
  begin
    b := ToBitmapPtr(bmp);

    if not Assigned(b) then exit;
    
    result := CurrentClip(b^.image);
  end;

  function CurrentClip(wnd: Window): Rectangle; overload;
  var
    w: WindowPtr;
  begin
    w := ToWindowPtr(wnd);

    if not Assigned(w) then exit;
    
    result := CurrentClip(w^.image);
  end;

  function CurrentClip(): Rectangle; overload;
  begin
    result := CurrentClip(Window(_CurrentWindow));
  end;

 //=============================================================================
  
  procedure DrawLines(clr: Color; const lines: LinesArray); //TODO: overload;
  var
    i: Longint;
  begin
    for i := 0 to High(lines) do
    begin
      DrawLine(clr, lines[i]);
    end;
  end;
  
  procedure DrawQuad(clr : Color; const q: Quad); overload;
  begin
    DrawQuad(clr, q, OptionDefaults());
  end;

  procedure FillQuad(clr : Color; const q: Quad); overload;
  begin
    FillQuad(clr, q, OptionDefaults());
  end;

  procedure DrawQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;
  begin
    sgDriverGraphics.DrawQuad(clr, q, opts);
  end;

  procedure FillQuad(clr : Color; const q: Quad; const opts: DrawingOptions); overload;
  begin
    sgDriverGraphics.FillQuad(clr, q, opts);
  end;


//----------------------------------------------------------------------------
// Set Icon / Window Open / Screen Size / Resize
//----------------------------------------------------------------------------

  procedure SetIcon(const filename: String);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'SetIcon');
    {$ENDIF}
    iconFile := filename;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'SetIcon');
    {$ENDIF}
  end;
  
  procedure OpenGraphicsWindow(const caption: String; width: Longint; height: Longint); overload;
  begin
    OpenWindow(caption, width, height);
  end;

  procedure SaveSurface(const image: ImageData; const basename: String);
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
    
    sgDriverImages.SaveSurface(@image.surface, path + filename);
    
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'TakeScreenShot');
    {$ENDIF}
  end;

  procedure TakeScreenShot(wnd: Window; const basename: String);
  var
    w: WindowPtr;
  begin
    w := ToWindowPtr(wnd);
    if Assigned(wnd) then SaveSurface(w^.image, basename);
  end;

  procedure TakeScreenShot(const basename: String);
  begin
    TakeScreenshot(Window(_CurrentWindow), basename);
  end;

  procedure RefreshScreen(); overload;
  begin
    RefreshScreen(-1);
  end;

  procedure RefreshScreen(wnd: Window; targetFPS: Longint); overload;
  var
    nowTime: Longword;
    delta, delayTime: Longword;
    w: WindowPtr;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RefreshScreen');
    {$ENDIF}

    w := ToWindowPtr(wnd);

    if not Assigned(w) then exit;

    DrawCollectedText(w);
    sgDriverGraphics.RefreshWindow(w);
    
    nowTime := GetTicks();
    delta := nowTime - _lastUpdateTime;
    
    //dont sleep if 5ms remaining...
    while (targetFPS > 0) and ((delta + 8) * targetFPS < 1000) do
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

  procedure RefreshScreen(targetFPS: Longint); overload;
  begin
    RefreshScreen(Window(_CurrentWindow), targetFPS);
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

    sgDriverGraphics.ColorComponents(c, r, g, b, a);

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
  
  function RGBAColor(red, green, blue, alpha: Byte): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBAColor');
    {$ENDIF}

    result := sgDriverGraphics.RGBAColor(red, green, blue, alpha);

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


  function AvailableResolution(idx: LongInt): Resolution;
  var
    temp: ResolutionArray;
  begin
    temp := sgDriverGraphics.AvailableResolutions();
    if (idx >= 0) and (idx <= High(temp)) then
      result := temp[idx]
    else
    begin
      result.format := 0;
      result.refreshRate := 0;
      result.width := 0;
      result.height := 0;
    end;
  end;

  function NumberOfResolutions(): Longint;
  begin
    result := Length(sgDriverGraphics.AvailableResolutions());
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
            if (KeyDown(sgTypes.SuperKey) or KeyDown(sgTypes.CtrlKey)) and KeyTyped(PKey) then
            begin
                isPaused := not isPaused;
            end;
            if WindowCloseRequested() or KeyDown(sgTypes.EscapeKey) then isSkip := true;
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
                    sgImages.DrawBitmap(aniBmp, aniX, aniY);

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

    function ColorGrey(): Color;
    begin
      result := RGBAColor(128, 128, 128, 255);
    end;

    function ColorLightGrey(): Color;
    begin
      result := RGBAColor(200, 200, 200, 255);
    end;

    function ColorTransparent(): Color;
    begin
      result := RGBAColor(0, 0, 0, 0);
    end;

    function ColorAliceBlue(): Color;
    begin
      result := RGBAColor(240, 248, 255, 255);
    end;

    function ColorAntiqueWhite(): Color;
    begin
      result := RGBAColor(250, 235, 215, 255);
    end;

    function ColorAqua(): Color;
    begin
      result := RGBAColor(0, 255, 255, 255);
    end;

    function ColorAquamarine(): Color;
    begin
      result := RGBAColor(127, 255, 212, 255);
    end;

    function ColorAzure(): Color;
    begin
      result := RGBAColor(240, 255, 255, 255);
    end;

    function ColorBeige(): Color;
    begin
      result := RGBAColor(245, 245, 220, 255);
    end;

    function ColorBisque(): Color;
    begin
      result := RGBAColor(255, 228, 196, 255);
    end;

    function ColorBlack(): Color;
    begin
      result := RGBAColor(0, 0, 0, 255);
    end;

    function ColorBlanchedAlmond(): Color;
    begin
      result := RGBAColor(255, 235, 205, 255);
    end;

    function ColorBlue(): Color;
    begin
      result := RGBAColor(0, 0, 255, 255);
    end;

    function ColorBlueViolet(): Color;
    begin
      result := RGBAColor(138, 43, 226, 255);
    end;

    function ColorBrown(): Color;
    begin
      result := RGBAColor(165, 42, 42, 255);
    end;

    function ColorBurlyWood(): Color;
    begin
      result := RGBAColor(222, 184, 135, 255);
    end;

    function ColorCadetBlue(): Color;
    begin
      result := RGBAColor(95, 158, 160, 255);
    end;

    function ColorChartreuse(): Color;
    begin
      result := RGBAColor(127, 255, 0, 255);
    end;

    function ColorChocolate(): Color;
    begin
      result := RGBAColor(210, 105, 30, 255);
    end;

    function ColorCoral(): Color;
    begin
      result := RGBAColor(255, 127, 80, 255);
    end;

    function ColorCornflowerBlue(): Color;
    begin
      result := RGBAColor(100, 149, 237, 255);
    end;

    function ColorCornsilk(): Color;
    begin
      result := RGBAColor(255, 248, 220, 255);
    end;

    function ColorCrimson(): Color;
    begin
      result := RGBAColor(220, 20, 60, 255);
    end;

    function ColorCyan(): Color;
    begin
      result := RGBAColor(0, 255, 255, 255);
    end;

    function ColorDarkBlue(): Color;
    begin
      result := RGBAColor(0, 0, 139, 255);
    end;

    function ColorDarkCyan(): Color;
    begin
      result := RGBAColor(0, 139, 139, 255);
    end;

    function ColorDarkGoldenrod(): Color;
    begin
      result := RGBAColor(184, 134, 11, 255);
    end;

    function ColorDarkGray(): Color;
    begin
      result := RGBAColor(169, 169, 169, 255);
    end;

    function ColorDarkGreen(): Color;
    begin
      result := RGBAColor(0, 100, 0, 255);
    end;

    function ColorDarkKhaki(): Color;
    begin
      result := RGBAColor(189, 183, 107, 255);
    end;

    function ColorDarkMagenta(): Color;
    begin
      result := RGBAColor(139, 0, 139, 255);
    end;

    function ColorDarkOliveGreen(): Color;
    begin
      result := RGBAColor(85, 107, 47, 255);
    end;

    function ColorDarkOrange(): Color;
    begin
      result := RGBAColor(255, 140, 0, 255);
    end;

    function ColorDarkOrchid(): Color;
    begin
      result := RGBAColor(153, 50, 204, 255);
    end;

    function ColorDarkRed(): Color;
    begin
      result := RGBAColor(139, 0, 0, 255);
    end;

    function ColorDarkSalmon(): Color;
    begin
      result := RGBAColor(233, 150, 122, 255);
    end;

    function ColorDarkSeaGreen(): Color;
    begin
      result := RGBAColor(143, 188, 139, 255);
    end;

    function ColorDarkSlateBlue(): Color;
    begin
      result := RGBAColor(72, 61, 139, 255);
    end;

    function ColorDarkSlateGray(): Color;
    begin
      result := RGBAColor(47, 79, 79, 255);
    end;

    function ColorDarkTurquoise(): Color;
    begin
      result := RGBAColor(0, 206, 209, 255);
    end;

    function ColorDarkViolet(): Color;
    begin
      result := RGBAColor(148, 0, 211, 255);
    end;

    function ColorDeepPink(): Color;
    begin
      result := RGBAColor(255, 20, 147, 255);
    end;

    function ColorDeepSkyBlue(): Color;
    begin
      result := RGBAColor(0, 191, 255, 255);
    end;

    function ColorDimGray(): Color;
    begin
      result := RGBAColor(105, 105, 105, 255);
    end;

    function ColorDodgerBlue(): Color;
    begin
      result := RGBAColor(30, 144, 255, 255);
    end;

    function ColorFirebrick(): Color;
    begin
      result := RGBAColor(178, 34, 34, 255);
    end;

    function ColorFloralWhite(): Color;
    begin
      result := RGBAColor(255, 250, 240, 255);
    end;

    function ColorForestGreen(): Color;
    begin
      result := RGBAColor(34, 139, 34, 255);
    end;

    function ColorFuchsia(): Color;
    begin
      result := RGBAColor(255, 0, 255, 255);
    end;

    function ColorGainsboro(): Color;
    begin
      result := RGBAColor(220, 220, 220, 255);
    end;

    function ColorGhostWhite(): Color;
    begin
      result := RGBAColor(248, 248, 255, 255);
    end;

    function ColorGold(): Color;
    begin
      result := RGBAColor(255, 215, 0, 255);
    end;

    function ColorGoldenrod(): Color;
    begin
      result := RGBAColor(218, 165, 32, 255);
    end;

    function ColorGray(): Color;
    begin
      result := RGBAColor(128, 128, 128, 255);
    end;

    function ColorGreen(): Color;
    begin
      result := RGBAColor(0, 128, 0, 255);
    end;

    function ColorBrightGreen(): Color;
    begin
      result := RGBAColor(0, 255, 0, 255);
    end;

    function ColorGreenYellow(): Color;
    begin
      result := RGBAColor(173, 255, 47, 255);
    end;

    function ColorHoneydew(): Color;
    begin
      result := RGBAColor(240, 255, 240, 255);
    end;

    function ColorHotPink(): Color;
    begin
      result := RGBAColor(255, 105, 180, 255);
    end;

    function ColorIndianRed(): Color;
    begin
      result := RGBAColor(205, 92, 92, 255);
    end;

    function ColorIndigo(): Color;
    begin
      result := RGBAColor(75, 0, 130, 255);
    end;

    function ColorIvory(): Color;
    begin
      result := RGBAColor(255, 255, 240, 255);
    end;

    function ColorKhaki(): Color;
    begin
      result := RGBAColor(240, 230, 140, 255);
    end;

    function ColorLavender(): Color;
    begin
      result := RGBAColor(230, 230, 250, 255);
    end;

    function ColorLavenderBlush(): Color;
    begin
      result := RGBAColor(255, 240, 245, 255);
    end;

    function ColorLawnGreen(): Color;
    begin
      result := RGBAColor(124, 252, 0, 255);
    end;

    function ColorLemonChiffon(): Color;
    begin
      result := RGBAColor(255, 250, 205, 255);
    end;

    function ColorLightBlue(): Color;
    begin
      result := RGBAColor(173, 216, 230, 255);
    end;

    function ColorLightCoral(): Color;
    begin
      result := RGBAColor(240, 128, 128, 255);
    end;

    function ColorLightCyan(): Color;
    begin
      result := RGBAColor(224, 255, 255, 255);
    end;

    function ColorLightGoldenrodYellow(): Color;
    begin
      result := RGBAColor(250, 250, 210, 255);
    end;

    function ColorLightGreen(): Color;
    begin
      result := RGBAColor(144, 238, 144, 255);
    end;

    function ColorLightGray(): Color;
    begin
      result := RGBAColor(211, 211, 211, 255);
    end;

    function ColorLightPink(): Color;
    begin
      result := RGBAColor(255, 182, 193, 255);
    end;

    function ColorLightSalmon(): Color;
    begin
      result := RGBAColor(255, 160, 122, 255);
    end;

    function ColorLightSeaGreen(): Color;
    begin
      result := RGBAColor(32, 178, 170, 255);
    end;

    function ColorLightSkyBlue(): Color;
    begin
      result := RGBAColor(135, 206, 250, 255);
    end;

    function ColorLightSlateGray(): Color;
    begin
      result := RGBAColor(119, 136, 153, 255);
    end;

    function ColorLightSteelBlue(): Color;
    begin
      result := RGBAColor(176, 196, 222, 255);
    end;

    function ColorLightYellow(): Color;
    begin
      result := RGBAColor(255, 255, 224, 255);
    end;

    function ColorLime(): Color;
    begin
      result := RGBAColor(0, 255, 0, 255);
    end;

    function ColorLimeGreen(): Color;
    begin
      result := RGBAColor(50, 205, 50, 255);
    end;

    function ColorLinen(): Color;
    begin
      result := RGBAColor(250, 240, 230, 255);
    end;

    function ColorMagenta(): Color;
    begin
      result := RGBAColor(255, 0, 255, 255);
    end;

    function ColorMaroon(): Color;
    begin
      result := RGBAColor(128, 0, 0, 255);
    end;

    function ColorMediumAquamarine(): Color;
    begin
      result := RGBAColor(102, 205, 170, 255);
    end;

    function ColorMediumBlue(): Color;
    begin
      result := RGBAColor(0, 0, 205, 255);
    end;

    function ColorMediumOrchid(): Color;
    begin
      result := RGBAColor(186, 85, 211, 255);
    end;

    function ColorMediumPurple(): Color;
    begin
      result := RGBAColor(147, 112, 219, 255);
    end;

    function ColorMediumSeaGreen(): Color;
    begin
      result := RGBAColor(60, 179, 113, 255);
    end;

    function ColorMediumSlateBlue(): Color;
    begin
      result := RGBAColor(123, 104, 238, 255);
    end;

    function ColorMediumSpringGreen(): Color;
    begin
      result := RGBAColor(0, 250, 154, 255);
    end;

    function ColorMediumTurquoise(): Color;
    begin
      result := RGBAColor(72, 209, 204, 255);
    end;

    function ColorMediumVioletRed(): Color;
    begin
      result := RGBAColor(199, 21, 133, 255);
    end;

    function ColorMidnightBlue(): Color;
    begin
      result := RGBAColor(25, 25, 112, 255);
    end;

    function ColorMintCream(): Color;
    begin
      result := RGBAColor(245, 255, 250, 255);
    end;

    function ColorMistyRose(): Color;
    begin
      result := RGBAColor(255, 228, 225, 255);
    end;

    function ColorMoccasin(): Color;
    begin
      result := RGBAColor(255, 228, 181, 255);
    end;

    function ColorNavajoWhite(): Color;
    begin
      result := RGBAColor(255, 222, 173, 255);
    end;

    function ColorNavy(): Color;
    begin
      result := RGBAColor(0, 0, 128, 255);
    end;

    function ColorOldLace(): Color;
    begin
      result := RGBAColor(253, 245, 230, 255);
    end;

    function ColorOlive(): Color;
    begin
      result := RGBAColor(128, 128, 0, 255);
    end;

    function ColorOliveDrab(): Color;
    begin
      result := RGBAColor(107, 142, 35, 255);
    end;

    function ColorOrange(): Color;
    begin
      result := RGBAColor(255, 165, 0, 255);
    end;

    function ColorOrangeRed(): Color;
    begin
      result := RGBAColor(255, 69, 0, 255);
    end;

    function ColorOrchid(): Color;
    begin
      result := RGBAColor(218, 112, 214, 255);
    end;

    function ColorPaleGoldenrod(): Color;
    begin
      result := RGBAColor(238, 232, 170, 255);
    end;

    function ColorPaleGreen(): Color;
    begin
      result := RGBAColor(152, 251, 152, 255);
    end;

    function ColorPaleTurquoise(): Color;
    begin
      result := RGBAColor(175, 238, 238, 255);
    end;

    function ColorPaleVioletRed(): Color;
    begin
      result := RGBAColor(219, 112, 147, 255);
    end;

    function ColorPapayaWhip(): Color;
    begin
      result := RGBAColor(255, 239, 213, 255);
    end;

    function ColorPeachPuff(): Color;
    begin
      result := RGBAColor(255, 218, 185, 255);
    end;

    function ColorPeru(): Color;
    begin
      result := RGBAColor(205, 133, 63, 255);
    end;

    function ColorPink(): Color;
    begin
      result := RGBAColor(255, 192, 203, 255);
    end;

    function ColorPlum(): Color;
    begin
      result := RGBAColor(221, 160, 221, 255);
    end;

    function ColorPowderBlue(): Color;
    begin
      result := RGBAColor(176, 224, 230, 255);
    end;

    function ColorPurple(): Color;
    begin
      result := RGBAColor(128, 0, 128, 255);
    end;

    function ColorRed(): Color;
    begin
      result := RGBAColor(255, 0, 0, 255);
    end;

    function ColorRosyBrown(): Color;
    begin
      result := RGBAColor(188, 143, 143, 255);
    end;

    function ColorRoyalBlue(): Color;
    begin
      result := RGBAColor(65, 105, 225, 255);
    end;

    function ColorSaddleBrown(): Color;
    begin
      result := RGBAColor(139, 69, 19, 255);
    end;

    function ColorSalmon(): Color;
    begin
      result := RGBAColor(250, 128, 114, 255);
    end;

    function ColorSandyBrown(): Color;
    begin
      result := RGBAColor(244, 164, 96, 255);
    end;

    function ColorSeaGreen(): Color;
    begin
      result := RGBAColor(46, 139, 87, 255);
    end;

    function ColorSeaShell(): Color;
    begin
      result := RGBAColor(255, 245, 238, 255);
    end;

    function ColorSienna(): Color;
    begin
      result := RGBAColor(160, 82, 45, 255);
    end;

    function ColorSilver(): Color;
    begin
      result := RGBAColor(192, 192, 192, 255);
    end;

    function ColorSkyBlue(): Color;
    begin
      result := RGBAColor(135, 206, 235, 255);
    end;

    function ColorSlateBlue(): Color;
    begin
      result := RGBAColor(106, 90, 205, 255);
    end;

    function ColorSlateGray(): Color;
    begin
      result := RGBAColor(112, 128, 144, 255);
    end;

    function ColorSnow(): Color;
    begin
      result := RGBAColor(255, 250, 250, 255);
    end;

    function ColorSpringGreen(): Color;
    begin
      result := RGBAColor(0, 255, 127, 255);
    end;

    function ColorSteelBlue(): Color;
    begin
      result := RGBAColor(70, 130, 180, 255);
    end;

    function ColorTan(): Color;
    begin
      result := RGBAColor(210, 180, 140, 255);
    end;

    function ColorTeal(): Color;
    begin
      result := RGBAColor(0, 128, 128, 255);
    end;

    function ColorThistle(): Color;
    begin
      result := RGBAColor(216, 191, 216, 255);
    end;

    function ColorTomato(): Color;
    begin
      result := RGBAColor(255, 99, 71, 255);
    end;

    function ColorTurquoise(): Color;
    begin
      result := RGBAColor(64, 224, 208, 255);
    end;

    function ColorViolet(): Color;
    begin
      result := RGBAColor(238, 130, 238, 255);
    end;

    function ColorWheat(): Color;
    begin
      result := RGBAColor(245, 222, 179, 255);
    end;

    function ColorWhite(): Color;
    begin
      result := RGBAColor(255, 255, 255, 255);
    end;

    function ColorWhiteSmoke(): Color;
    begin
      result := RGBAColor(245, 245, 245, 255);
    end;

    function ColorYellow(): Color;
    begin
      result := RGBAColor(255, 255, 0, 255);
    end;

    function ColorYellowGreen(): Color;
    begin
      result := RGBAColor(154, 205, 50, 255);
    end;

    function ColorSwinburneRed(): Color;
    begin
      result := RGBAColor(237, 36, 25, 255);
    end;



//=============================================================================
  
  initialization
  begin
    InitialiseSwinGame();
  end;
end.
