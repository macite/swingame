//=============================================================================
// sgText.pas
//=============================================================================
//
// The Font unit relates to writing text to the screen,
// and to loading and styling the associated fonts.
//
// Change History:
//
// Version 3.0:
// - 2012-01-05: Aaron : changed the code to support using text drivers.
// - 2010-03-18: Andtew : Altered font loading to provide mappings for all fonts.
// - 2010-02-02: Andrew : Added string to FontAlignment
// - 2009-12-07: Andrew : Added font loading and freeing code..
// - 2009-06-05: Andrew : Using sgShared
//
// Version 2.0:
// - 2009-07-14: Andrew : Removed loading and freeing code.
// - 2009-01-05: Andrew : Added Unicode rendering
// - 2008-12-17: Andrew : Moved all integers to Longint
// - 2008-12-12: Andrew : Added simple string printing
// - 2008-12-10: Andrew : Fixed printing of string
//
// Version 1.1:
// - 2008-03-09: Andrew : Added extra exception handling
// - 2008-01-30: Andrew : Fixed Print strings for EOL as last char
// - 2008-01-25: Andrew : Fixed compiler hints
// - 2008-01-21: Andrew : Added Point/Rectangle overloads
// - 2008-01-17: Aki + Andrew: Refactor
//
// Version 1.0:
// - Various
//=============================================================================



/// Supports the presentation (drawing) of text to screen using loaded `Font`s 
/// to style the text. Load a different ``Font`` for each unique text 
/// presentation style (colour and size) you need in your game.
///
///@module Text
///@static
unit sgText;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================


//----------------------------------------------------------------------------
// Font loading routines
//----------------------------------------------------------------------------
  
  /// Loads a font from file with the specified side. Fonts must be freed using
  /// the FreeFont routine once finished with. Once the font is loaded you
  /// can set its style using SetFontStyle. Fonts are then used to draw and
  /// measure text in your programs.
  /// 
  /// @lib
  /// @sn loadFontFile:%s size:%s
  ///
  /// @class Font
  /// @constructor
  /// @csn initWithFontName:%s andSize:%s
  function LoadFont(fontName: String; size: Longint): Font;
  
  /// Frees the resources used by the loaded Font.
  /// 
  /// @lib
  ///
  /// @class Font
  /// @dispose
  procedure FreeFont(var fontToFree: Font);
  
  /// Loads and returns a font that can be used to draw text. The supplied
  /// ``filename`` is used to locate the font to load. The supplied ``name`` indicates the 
  /// name to use to refer to this Font in SwinGame. The `Font` can then be
  /// retrieved by passing this ``name`` to the `FontNamed` function.
  ///
  /// @lib
  /// @sn loadFontNamed:%s fromFile:%s size:%s
  ///
  /// @class Font
  /// @constructor
  /// @csn initWithName:%s fromFile:%s size:%s
  function LoadFontNamed(name, filename: String; size: Longint): Font;
  
  /// Determines if SwinGame has a font loaded for the supplied name.
  /// This checks against all fonts loaded, those loaded without a name
  /// are assigned the filename as a default.
  /// 
  /// @lib
  function HasFont(name: String): Boolean;
  
  /// Determines the name that will be used for a font loaded with
  /// the indicated fontName and size.
  /// 
  /// @lib
  /// @sn fontName:%s size:%s
  function FontNameFor(fontName: String; size: Longint): String;
  
  /// Returns the `Font` that has been loaded with the specified name,
  /// and font size using `LoadFont`.
  ///
  /// @lib FontNamedWithSize
  /// @sn fontNamed:%s withSize:%s
  function FontNamed(name: String; size: Longint): Font; overload;
  
  /// Returns the `Font` that has been loaded with the specified name,
  /// see `LoadFontNamed`.
  ///
  /// @lib
  function FontNamed(name: String): Font; overload;
  
  /// Releases the SwinGame resources associated with the font of the
  /// specified ``name``.
  ///
  /// @lib
  procedure ReleaseFont(name: String);
  
  /// Releases all of the fonts that have been loaded.
  ///
  /// @lib
  procedure ReleaseAllFonts();
  
  
  
//---------------------------------------------------------------------------
// Font properties
//---------------------------------------------------------------------------
  
  /// Alters the style of the font. This is time consuming, so load
  /// fonts multiple times and set the style for each if needed.
  ///
  /// @lib
  /// @sn font:%s setStyle:%s
  /// 
  /// @class Font
  /// @setter FontStyle
  procedure FontSetStyle(font: Font; value: FontStyle);
  
  /// Returns the style settings for the font.
  ///
  /// @lib
  ///
  /// @class Font
  /// @getter FontStyle
  function FontFontStyle(font: Font): FontStyle;
  
  /// Returns the width (in pixels) of the passed in text and the font it will be drawn with.
  ///
  /// @lib
  /// @sn font:%s widthOf:%s
  ///
  /// @class Font
  /// @method TextWidth
  function TextWidth(theFont: Font; theText: String): Longint; overload;

  /// Returns the height (in pixels) of the passed in text and the font it will be drawn with.
  ///
  /// @lib
  /// @sn font:%s heightOf:%s
  ///
  /// @class Font
  /// @method TextHeight
  function TextHeight(theFont: Font; theText: String): Longint; overload;

{  
  function TextWidth(theText: WideString; theFont: Font): Longint; overload;
  function TextHeight(theText: WideString; theFont: Font): Longint; overload;
}  

  /// Returns the font alignment for the passed in character (l = left. r = right, c = center).
  /// 
  /// @lib
  function TextAlignmentFrom(str: String): FontAlignment;
  
  
  
//---------------------------------------------------------------------------
// Draw Text - using font
//---------------------------------------------------------------------------
  
  /// Draws the text at the specified point using the color and font indicated.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib
  /// @sn drawText:%s color:%s font:%s x:%s y:%s
  procedure DrawText(theText: String; textColor: Color; theFont: Font; x, y: Single); overload;
  
  /// Draws the text at the specified point using the color and font indicated.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextWithFontNamed
  /// @sn drawText:%s color:%s fontNamed:%s x:%s y:%s
  procedure DrawText(theText: String; textColor: Color; name: String; x, y: Single); overload;
  
  /// Draws theText at the specified point using the color and font indicated.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextWithFontNamedSize
  /// @sn drawText:%s color:%s fontNamed:%s size:%s x:%s y:%s
  procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; x, y: Single); overload;
  
  
  /// Draws the text at the specified point using the color and font indicated.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  /// 
  /// @lib DrawTextAtPoint
  /// @sn drawText:%s color:%s font:%s pt:%s
  procedure DrawText(theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  
  /// Draws the text at the specified point using the color and font indicated.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  /// 
  /// @lib DrawTextAtPointWithFontNamed
  /// @sn drawText:%s color:%s fontNamed:%s pt:%s
  procedure DrawText(theText: String; textColor: Color; name: String; const pt: Point2D); overload;
    
  // Draws the text at the specified point using the color and font indicated.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  /// 
  /// @lib DrawTextAtPointWithFontNamedAndSize
  /// @sn drawText:%s color:%s fontNamed:%s size:%s pt:%s
  procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  
  
  /// Draws the text onto the bitmap at the specified x,y location using the color and font indicated.
  /// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  /// good idea if the text does not change frequently.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextOnBitmap
  /// @sn bitmap:%s drawText:%s color:%s font:%s atX:%s y:%s
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; theFont: Font; x, y: Longint); overload;
  
  /// Draws the text onto the bitmap at the specified x,y location using the color and font indicated.
  /// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  /// good idea if the text does not change frequently.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextOnBitmapWithFontNamed
  /// @sn bitmap:%s drawText:%s color:%s fontNamed:%s atX:%s y:%s
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; x, y: Longint); overload;
  
  /// Draws the text onto the bitmap at the specified x,y location using the color and font indicated.
  /// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  /// good idea if the text does not change frequently.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextOnBitmapWithFontNamedAndSize
  /// @sn bitmap:%s drawText:%s color:%s fontNamed:%s size:%s atX:%s y:%s
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; size: Longint; x, y: Longint); overload;
  
  
  /// Draws the text onto the bitmap at the specified point using the color and font indicated.
  /// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  /// good idea if the text does not change frequently.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextOnBitmapAtPoint
  /// @sn bitmap:%s drawText:%s color:%s font:%s atPt:%s
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  
  /// Draws the text onto the bitmap at the specified point using the color and font indicated.
  /// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  /// good idea if the text does not change frequently.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextOnBitmapAtPointWithFontNamed
  /// @sn bitmap:%s drawText:%s color:%s fontNamed:%s atPt:%s
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; const pt: Point2D); overload;
  
  /// Draws the text onto the bitmap at the specified point using the color and font indicated.
  /// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  /// good idea if the text does not change frequently.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextOnBitmapAtPointWithFontNamedAndSize
  /// @sn bitmap:%s drawText:%s color:%s fontNamed:%s size:%s atPt:%s
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  
  /// Draws the text onto the bitmap at the specified point using the color and font indicated.
  /// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  /// good idea if the text does not change frequently.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLines` procedure.
  ///
  /// @lib DrawTextToBitmapAtPointWithFontNamedAndSize
  /// @sn drawTextFont:%s string:%s textColor:%s backgroundColor:%s   
  function DrawTextTo(font: Font; str: String; clrFg, backgroundColor : Color) : Bitmap;
  
  /// Draws the text onto the screen at the specified x,y location using the color and font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLinesOnScreen` procedure.
  ///
  /// @lib
  /// @sn drawText:%s color:%s font:%s onScreenAtX:%s y:%s
  procedure DrawTextOnScreen(theText: String; textColor: Color; theFont: Font; x, y: Longint); overload;
  
  /// Draws the text onto the screen at the specified x,y location using the color and font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLinesOnScreen` procedure.
  ///
  /// @lib DrawTextOnScreenWithFontNamed
  /// @sn drawText:%s color:%s fontNamed:%s onScreenAtX:%s y:%s
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; x, y: Longint);
  
  /// Draws the text onto the screen at the specified x,y location using the color and font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLinesOnScreen` procedure.
  ///
  /// @lib DrawTextOnScreenWithFontNamedAndSize
  /// @sn drawText:%s color:%s fontNamed:%s size:%s onScreenAtX:%s y:%s
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; size: Longint; x, y: Longint);
  
  
  /// Draws theText onto the screen at the specified point using the color and font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLinesOnScreen` procedure.
  ///
  /// @lib DrawTextOnScreenAtPoint
  /// @sn drawText:%s color:%s font:%s onScreenAtPt:%s
  procedure DrawTextOnScreen(theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  
  /// Draws theText onto the screen at the specified point using the color and font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLinesOnScreen` procedure.
  ///
  /// @lib DrawTextOnScreenAtPointWithFontNamed
  /// @sn drawText:%s color:%s fontNamed:%s onScreenAtPt:%s
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; const pt: Point2D); overload;
  
  /// Draws theText onto the screen at the specified point using the color and font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version only draws a single line of text, to draw text that contains line breaks use the
  /// `DrawTextLinesOnScreen` procedure.
  ///
  /// @lib DrawTextOnScreenAtPointWithFontNamedAndSize
  /// @sn drawText:%s color:%s fontNamed:%s size:%s onScreenAtPt:%s
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  
  
{  procedure DrawUnicodeOnScreen(theText: WideString; textColor: Color; theFont: Font; x, y: Longint); overload;
  procedure DrawUnicodeOnScreen(theText: WideString; textColor: Color; theFont: Font; const pt: Point2D); overload;
  procedure DrawUnicode(theText: WideString; textColor: Color; theFont: Font; x, y: Single); overload;
  procedure DrawUnicode(theText: WideString; textColor: Color; theFont: Font; const pt: Point2D); overload; }
  
  
  
//---------------------------------------------------------------------------
// Draw Text Lines
//---------------------------------------------------------------------------
  
  /// Draws the text at the specified x,y location using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib
  /// @sn drawTextLines:%s textColor:%s backColor:%s font:%s align:%s atX:%s y:%s w:%s h:%s
  procedure DrawTextLines(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; x, y: Single; w, h: Longint); overload;
  
  /// Draws the text at the specified x,y location using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib DrawTextLinesWithFontNamed
  /// @sn drawTextLines:%s textColor:%s backColor:%s fontNamed:%s align:%s atX:%s y:%s w:%s h:%s
  procedure DrawTextLines(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; x, y:Single; w, h: Longint); overload;
  
  /// Draws the text at the specified x,y location using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib DrawTextLinesWithFontNamedAndSize
  /// @sn drawTextLines:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s atX:%s y:%s w:%s h:%s
  procedure DrawTextLines(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; x, y:Single; w, h: Longint); overload;
  
  
  /// Draws the text in the specified rectangle using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib DrawTextLinesInRect
  /// @sn drawTextLines:%s textColor:%s backColor:%s font:%s align:%s inRect:%s
  procedure DrawTextLines(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  
  /// Draws the text in the specified rectangle using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib DrawTextLinesInRectWithFontNamed
  /// @sn drawTextLines:%s textColor:%s backColor:%s fontNamed:%s align:%s inRect:%s
  procedure DrawTextLines(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  
  /// Draws theText in the specified rectangle using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib DrawTextLinesInRectWithFontNamedAndSize
  /// @sn drawTextLines:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s inRect:%s
  procedure DrawTextLines(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  
  
  /// Draws the text onto the bitmap at the specified x,y location using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib DrawTextLinesOnBitmap
  /// @sn bitmap:%s drawTextLines:%s textColor:%s backColor:%s font:%s align:%s onScreenAtX:%s y:%s w:%s h:%s
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; x, y, w, h: Longint); overload;
  
  /// Draws the text onto the bitmap at the specified x,y location using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib DrawTextLinesOnBitmapWithFontNamed
  /// @sn bitmap:%s drawTextLines:%s textColor:%s backColor:%s fontNamed:%s align:%s onScreenAtX:%s y:%s w:%s h:%s
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; name: String; align: FontAlignment; x, y, w, h: Longint); overload;
  
  /// Draws theText onto the bitmap at the specified x,y location using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib DrawTextLinesOnBitmapWithFontNamedAndSize
  /// @sn bitmap:%s drawTextLines:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s onScreenAtX:%s y:%s w:%s h:%s
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; x, y, w, h: Longint); overload;
  
  
  /// Draws the text onto the bitmap in the rectangle using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib DrawTextLinesInRectOnBitmap
  /// @sn bitmap:%s drawTextLines:%s textColor:%s backColor:%s font:%s align:%s in:%s
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  
  /// Draws the text onto the bitmap in the rectangle using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib DrawTextLinesInRectOnBitmapWithFontNamed
  /// @sn bitmap:%s drawTextLines:%s textColor:%s backColor:%s fontNamed:%s align:%s in:%s
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  
  /// Draws the text onto the bitmap in the rectangle using the fore and back colors, and the font indicated.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawText` procedure.
  ///
  /// @lib DrawTextLinesInRectOnBitmapWithFontNamedAndSize
  /// @sn bitmap:%s drawTextLines:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s in:%s
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  
  
  /// Draws the text onto the screen at the specified x,y location using the fore and back colors, and the font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib
  /// @sn drawTextLines:%s textColor:%s backColor:%s font:%s align:%s onScreenAtX:%s y:%s w:%s h:%s
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; x, y, w, h: Longint); overload;
  
  /// Draws the text onto the screen at the specified x,y location using the fore and back colors, and the font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib DrawTextLinesOnScreenWithFontNamed
  /// @sn drawTextLines:%s textColor:%s backColor:%s fontNamed:%s align:%s onScreenAtX:%s y:%s w:%s h:%s
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; x, y, w, h: Longint); overload;
  
  /// Draws the text onto the screen at the specified x,y location using the fore and back colors, and the font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib DrawTextLinesOnScreenWithFontNamedWithSize
  /// @sn drawTextLines:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s onScreenAtX:%s y:%s w:%s h:%s
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; x, y, w, h: Longint); overload;
  
  
  /// Draws the text onto the screen in the specified rectangle using the fore and back colors, and the font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib DrawTextLinesInRectOnScreen
  /// @sn drawTextLines:%s textColor:%s backColor:%s font:%s align:%s onScreenInRect:%s
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  
  /// Draws the text onto the screen in the specified rectangle using the fore and back colors, and the font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib DrawTextLinesInRectOnScreenWithFontNamed
  /// @sn drawTextLines:%s textColor:%s backColor:%s fontNamed:%s align:%s onScreenInRect:%s
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  
  /// Draws the text onto the screen in the specified rectangle using the fore and back colors, and the font indicated.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// This version should be used to draw text that contains multiple lines, to draw a single line of text
  /// use the `DrawTextOnScreen` procedure.
  ///
  /// @lib DrawTextLinesInRectOnScreenWithFontNamedAndSize
  /// @sn drawTextLines:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s onScreenInRect:%s
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  
  
  
//---------------------------------------------------------------------------
// Draw Text - without font
//---------------------------------------------------------------------------
  
  /// Draws text using a simple bitmap font that is built into SwinGame.
  ///
  /// @lib DrawSimpleText
  /// @sn drawText:%s color:%s x:%s y:%s
  procedure DrawText(theText: String; textColor: Color; x, y: Single); overload;
  
  /// Draws text using a simple bitmap font that is built into SwinGame.
  ///
  /// @lib DrawSimpleTextPt
  /// @sn drawText:%s color:%s pt:%s
  procedure DrawText(theText: String; textColor: Color; const pt: Point2D); overload;
  
  /// Draws text using a simple bitmap font that is built into SwinGame.
  /// As the text is draw directly onto the screen the camera location does not effect its position.
  /// This is useful for drawing text on a HUD or similar actions.
  ///
  /// @lib DrawSimpleTextOnScreen
  /// @sn drawText:%s color:%s onScreenX:%s y:%s
  procedure DrawTextOnScreen(theText: String; textColor: Color; x, y: Single); overload;
  
  /// Draws text using a simple bitmap font that is built into SwinGame.
  ///
  /// @lib DrawSimpleTextOnBitmap
  /// @sn bitmap:%s drawText:%s color:%s onScreenAtX:%s y:%s
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; x, y: Single); overload;
  
  /// Draws the framerate to the screen using the supplied font.
  /// 
  /// @lib
  /// @sn drawFramerateAtX:%s y:%s font:%s
  procedure DrawFramerate(x, y: Longint; font: Font); overload;
  
  /// Draws the framerate to the screen using the supplied font.
  /// 
  /// @lib DrawFramerateFontNamed
  /// @sn drawFramerateAtX:%s y:%s fontNamed:%s
  procedure DrawFramerate(x, y: Longint; name: String); overload;
  
  /// Draws the framerate to the screen using the supplied font.
  /// 
  /// @lib DrawFramerateFontNamedSize
  /// @sn drawFramerateAtX:%s y:%s fontNamed:%s size:%s
  procedure DrawFramerate(x, y: Longint; name: String; size: Longint); overload;
  
  /// Draws the framerate to the screen using a simple font built into SwinGame.
  /// 
  /// @lib DrawFramerateWithSimpleFont
  /// @sn drawFramerateAtX:%s y:%s
  procedure DrawFramerate(x, y: Longint); overload;
  
  
//=============================================================================
implementation
  uses SysUtils, Classes, 
       stringhash, sgTrace,         // libsrc
       sgUtils, sgGeometry, sgGraphics, sgCamera, sgShared, sgResources, sgImages, sgDriverText;
//=============================================================================

  const EOL = LineEnding; // from sgShared

  var
    _Fonts: TStringHash;

//----------------------------------------------------------------------------
  
  function LoadFont(fontName: String; size: Longint): Font;
  begin
    result := LoadFontNamed(FontNameFor(fontName, size), fontName, size);
  end;
  
  procedure _DoFreeFont(var fontToFree: Font);
  begin
    if Assigned(fontToFree) then
    begin
      {$IFDEF TRACE}
        Trace('Resources', 'IN', 'FreeFont', 'After calling free notifier');
      {$ENDIF}
      try
        {$IFDEF TRACE}
            Trace('Resources', 'IN', 'FreeFont', 'Before calling close font');
        {$ENDIF}
        
        CallFreeNotifier(fontToFree);
		TextDriver.CloseFont(fontToFree);
        Dispose(fontToFree);
        fontToFree := nil;
        {$IFDEF TRACE}
            Trace('Resources', 'IN', 'FreeFont', 'At end of free font');
        {$ENDIF}
      except
        RaiseException('Unable to free the specified font');
        exit;
      end;
    end;
  end;
  
  procedure FreeFont(var fontToFree: Font);
  begin
    if Assigned(fontToFree) then ReleaseFont(fontToFree^.name);
      
    fontToFree := nil;
  end;

//----------------------------------------------------------------------------

  function LoadFontNamed(name, filename: String; size: Longint): Font;
  var
    obj: tResourceContainer;
    fnt: Font;
    
    function _DoLoadFont(fontName: String; size: Longint): Font;
    var
      filename: String;
    begin
      {$IFDEF TRACE}
        TraceEnter('sgText', '_DoLoadFont(', fontName + ' ' + IntToStr(size));
      {$ENDIF}
      
      filename := fontName;
      if not FileExists(filename) then
      begin
        filename := PathToResource(filename, FontResource);
        
        if not FileExists(filename) then
        begin
          filename := filename + '.ttf';
          
          if not FileExists(filename) then
          begin
            RaiseWarning('Unable to locate font ' + fontName);
            result := nil;
            exit;
          end;
        end;
      end;
      {$IFDEF TRACE}
        TraceIf(tlInfo, 'sgText', 'Info', '_DoLoadFont', 'About to load font from driver');
      {$ENDIF}      
      
      result := TextDriver.LoadFont(name, filename, size);
      {$IFDEF TRACE}
        TraceExit('sgText', '_DoLoadFont = ' + HexStr(result) );
      {$ENDIF}      
    end;

  begin
    {$IFDEF TRACE}
      TraceEnter('sgText', 'LoadFontNamed(', name + ' ' + IntToStr(size));
    {$ENDIF}
    
    if _Fonts.containsKey(name) then
    begin
      result := FontNamed(name);
      
      {$IFDEF TRACE}
        TraceExit('sgText', 'Exit LoadFontNamed = ' + HexStr(result));
      {$ENDIF}
      
      exit;
    end;
    
    fnt := _DoLoadFont(filename, size);
    if Assigned(fnt) then
    begin
      obj := tResourceContainer.Create(fnt);
      if not _Fonts.setValue(name, obj) then raise Exception.create('Error loaded Font resource - ' + name);
    end;
    result := fnt;
    
    {$IFDEF TRACE}
      TraceExit('sgText', 'LoadFontNamed = ' + HexStr(result));
    {$ENDIF}
    
  end;

  function HasFont(name: String): Boolean;
  begin
    result := _Fonts.containsKey(name);
  end;
  
  function FontNameFor(fontName: String; size: Longint): String;
  begin
    result := fontName + '|' + IntToStr(size);
  end;
  
  function FontNamed(name: String; size: Longint): Font;
  begin
    result := FontNamed(FontNameFor(name, size));
  end;
  
  function FontNamed(name: String): Font;
  var
    tmp : TObject;
  begin
    tmp := _Fonts.values[name];
    if assigned(tmp) then result := Font(tResourceContainer(tmp).Resource)
    else result := nil;
  end;
  
  procedure ReleaseFont(name: String);
  var
    fnt: Font;
  begin
    fnt := FontNamed(name);
    if Assigned(fnt) then
    begin
      _Fonts.remove(name).free();
      _DoFreeFont(fnt);
    end;
  end;
  
  procedure ReleaseAllFonts();
  begin
    ReleaseAll(_Fonts, @ReleaseFont);
  end;
  
  
  
  //----------------------------------------------------------------------------
  
  procedure FontSetStyle(font: Font; value: FontStyle);
  begin
    if not Assigned(font) then begin RaiseWarning('No font supplied to FontSetStyle'); exit; end;
    //TTF_SetFontStyle(font^.fptr, Longint(value));
	TextDriver.SetFontStyle(font,value);
  end;
  
  function FontFontStyle(font: Font): FontStyle;
  begin
    result := NormalFont;
    if not Assigned(font) then begin RaiseWarning('No font supplied to FontFontStyle'); exit; end;
    result := TextDriver.GetFontStyle(font);
  end;

  function IsSet(toCheck, checkFor: FontAlignment): Boolean; overload;
  begin
    result := (Longint(toCheck) and Longint(checkFor)) = Longint(checkFor);
  end;
  
  /// This function prints "str" with font "font" and color "clrFg"
  ///  * onto a rectangle of color "clrBg".
  ///  * It does not pad the text.
  procedure PrintStrings(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment) ;
  begin
    // If there's nothing to draw, return NULL
    if (Length(str) = 0) or (font = nil) then exit;
    if (rc.width <= 0) or (rc.height <= 0) then exit;

  	TextDriver.PrintStrings(dest,font,str,rc,clrFg, clrBg,flags)
  end;
  /// This function prints "str" with font "font" and color "clrFg"
  ///  * onto a rectangle of color "clrBg".
  ///  * It does not pad the text.
  procedure PrintWideStrings(dest: Bitmap; font: Font; str: WideString; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
  begin
    // If there's nothing to draw, return NULL
    if (Length(str) = 0) or (font = nil) then exit;
    if (rc.width <= 0) or (rc.height <= 0) then exit;

	  TextDriver.PrintWideStrings(dest,font,str,rc,clrFg,clrBg,flags);
  end;
  
  function DrawTextTo(font: Font; str: String; clrFg, backgroundColor : Color) : Bitmap;
  var
    resultBitmap : Bitmap;
    bitmapSize : Rectangle;
  begin
    result := nil;
    // If there's nothing to draw, return NULL
    if (Length(str) = 0) or (font = nil) then exit;

    bitmapSize.x := 0;
    bitmapSize.y := 0;
    bitmapSize.width := TextWidth(font, str) + 2;
    bitmapSize.height := TextHeight(font,str) + 2;
    resultBitmap := CreateBitmap(bitmapSize.width, bitmapSize.height);
    PrintStrings(resultBitmap,font,str,bitmapSize,clrFg, ColorTransparent, AlignLeft);
  
    result := resultBitmap;
  end;
  
  
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; x, y: Longint); overload;
  begin
    DrawText(dest, theText, textColor, FontNamed(name), x, y);
  end;
  
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; size: Longint; x, y: Longint); overload;
  begin
    DrawText(dest, theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), x, y);
  end;
  
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; theFont: Font; x, y: Longint); overload;
  var
    rect: Rectangle;
  begin
    if not Assigned(theFont) then exit;
    if dest = nil then begin RaiseWarning('Cannot draw text, as no destination was supplied'); exit; end;
    if Length(theText) <= 0 then exit;
      
    rect.x := x; // + 1;
   	rect.y := y; // + 1;
   	rect.width := TextWidth(theFont, theText); // + 2;
   	rect.height := TextHeight(theFont, theText); // + 2;
    PrintStrings(dest, theFont, theText, rect, textColor, ColorTransparent, AlignLeft);
  end;

  procedure DrawUnicode(dest: Bitmap; theText: WideString; textColor: Color; theFont: Font; x, y: Longint); overload;
  var
    rect: Rectangle;
  begin
    if theFont = nil then begin RaiseWarning('The specified font is nil'); exit; end;
    if dest = nil then begin RaiseWarning('Cannot draw text, as no destination was supplied'); exit; end;
    if Length(theText) <= 0 then exit;
      
    rect.x := x;
 	  rect.y := y;
	  rect.width := TextWidth(theFont, theText);
	  rect.height := TextHeight(theFont, theText);    
    PrintWideStrings(dest, theFont, theText, rect, textColor, ColorTransparent, AlignLeft);
  end;
  
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; const pt: Point2D); overload;
  begin
    DrawText(dest, theText, textColor, FontNamed(name), pt);
  end;
  
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  begin
    DrawText(dest, theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), pt);
  end;
  
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    DrawText(dest, theText, textColor, theFont, RoundInt(pt.x), RoundInt(pt.y));
  end;

  procedure DrawUnicode(dest: Bitmap; theText: WideString; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    DrawUnicode(dest, theText, textColor, theFont, RoundInt(pt.x), RoundInt(pt.y));
  end;
  
  
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; name: String; align: FontAlignment; x, y, w, h: Longint); overload;
  begin
    DrawTextLines(dest, theText, textColor, backColor, FontNamed(name), align, x, y, w, h);
  end;
  
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; x, y, w, h: Longint); overload;
  begin
    DrawTextLines(dest, theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, x, y, w, h);
  end;
  
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; x, y, w, h: Longint); overload;
  var
    rect: Rectangle;
  begin
    if not Assigned(theFont) then exit;
    if Length(theText) <= 0 then exit;
    if (w <= 0) or (h <= 0) then exit;

    rect.x := x; // + 1;
 	  rect.y := y; // + 1;
 	  rect.width := w; // - 2;
 	  rect.height := h; // - 2;
    PrintStrings(dest, theFont, theText, rect, textColor, backColor, align);
  end;
  
  
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(dest, theText, textColor, backColor, FontNamed(name), align, withinRect);
  end;
  
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(dest, theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, withinRect);
  end;
  
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(dest, theText, textColor, backColor, theFont, align, RoundInt(withinRect.x), RoundInt(withinRect.y), withinRect.width, withinRect.height);
  end;
  
  
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; x, y, w, h: Longint); overload;
  begin
    DrawTextLines(screen, theText, textColor, backColor, FontNamed(name), align, x, y, w, h);
  end;
  
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; x, y, w, h: Longint); overload;
  begin
    DrawTextLines(screen, theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, x, y, w, h);
  end;
  
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; x, y, w, h: Longint); overload;
  begin
    DrawTextLines(screen, theText, textColor, backColor, theFont, align, x, y, w, h);
  end;
  
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(screen, theText, textColor, backColor, FontNamed(name), align, RoundInt(withinRect.x), RoundInt(withinRect.y), withinRect.width, withinRect.height);
  end;
  
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(screen, theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, RoundInt(withinRect.x), RoundInt(withinRect.y), withinRect.width, withinRect.height);
  end;
  
  procedure DrawTextLinesOnScreen(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(screen, theText, textColor, backColor, theFont, align, RoundInt(withinRect.x), RoundInt(withinRect.y), withinRect.width, withinRect.height);
  end;
  
  procedure DrawTextLines(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; x, y:Single; w, h: Longint); overload;
  begin
    DrawTextLines(theText, textColor, backColor, FontNamed(name), align, x, y, w, h);
  end;
  
  procedure DrawTextLines(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; x, y:Single; w, h: Longint); overload;
  begin
    DrawTextLines(theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, x, y, w, h);
  end;
  
  procedure DrawTextLines(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; x, y:Single; w, h: Longint); overload;
  begin
    DrawTextLines(screen, theText, textColor, backColor, theFont, align, ToScreenX(x), ToScreenY(y), w, h);
  end;
  
  procedure DrawTextLines(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(theText, textColor, backColor, FontNamed(name), align, withinRect);
  end;
  
  procedure DrawTextLines(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, withinRect);
  end;
  
  procedure DrawTextLines(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    DrawTextLines(screen, theText, textColor, backColor, theFont, align, ToScreenX(withinRect.x), ToScreenY(withinRect.y), withinRect.width, withinRect.height);
  end;
  
  
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; x, y: Longint);
  begin
    DrawTextOnScreen(theText, textColor, FontNamed(name), x, y);
  end;
  
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; size: Longint; x, y: Longint);
  begin
    DrawTextOnScreen(theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), x, y);
  end;
  
  procedure DrawTextOnScreen(theText: String; textColor: Color; theFont: Font; x, y: Longint);
  begin
    DrawText(screen, theText, textColor, theFont, x, y);
  end;
  
  
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; const pt: Point2D); overload;
  begin
    DrawTextOnScreen(theText, textColor, FontNamed(name), pt);
  end;
  
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  begin
    DrawTextOnScreen(theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), pt);
  end;
  
  procedure DrawTextOnScreen(theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    DrawText(screen, theText, textColor, theFont, RoundInt(pt.x), RoundInt(pt.y));
  end;
  
  
  procedure DrawUnicodeOnScreen(theText: WideString; textColor: Color; theFont: Font; x, y: Longint); overload;
  begin
    DrawUnicode(screen, theText, textColor, theFont, x, y);
  end;
  
  procedure DrawUnicodeOnScreen(theText: WideString; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    DrawUnicode(screen, theText, textColor, theFont, RoundInt(pt.x), RoundInt(pt.y));
  end;
  
  
  procedure DrawText(theText: String; textColor: Color; name: String; x, y: Single); overload;
  begin
    DrawText(theText, textColor, FontNamed(name), x, y);
  end;
  
  procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; x, y: Single); overload;
  begin
    DrawText(theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), x, y);
  end;
  
  procedure DrawText(theText: String; textColor: Color; theFont: Font; x, y: Single);
  begin
    DrawText(screen, theText, textColor, theFont, ToScreenX(x), ToScreenY(y));
  end;
  
  
  procedure DrawText(theText: String; textColor: Color; name: String; const pt: Point2D); overload;
  begin
    DrawText(theText, textColor, FontNamed(name), pt);
  end;
  
  procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  begin
    DrawText(theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), pt);
  end;
  
  procedure DrawText(theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    DrawText(screen, theText, textColor, theFont, ToScreenX(pt.x), ToScreenY(pt.y));
  end;
  
  
  procedure DrawUnicode(theText: WideString; textColor: Color; theFont: Font; x, y: Single); overload;
  begin
    DrawUnicode(screen, theText, textColor, theFont, ToScreenX(x), ToScreenY(y));
  end;

  procedure DrawUnicode(theText: WideString; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    DrawUnicode(screen, theText, textColor, theFont, ToScreenX(pt.x), ToScreenY(pt.y));
  end;
  
  /// Calculates the width of a string when drawn with a given font.
  ///
  /// @param theText:   The text to measure
  /// @param theFont:   The font used to draw the text
  /// @returns           The width of the drawing in pixels
  function TextWidth(theFont: Font; theText: String): Longint; overload;
  var
    y: Longint; //SizeText returns both... store and ignore y
  begin
    result := 0;
    if not Assigned(theFont) then begin RaiseWarning('No font supplied to TextWidth'); exit; end;
    try
      y := 0;
      if length(theText) = 0 then result := 0 
	  else TextDriver.SizeOfText(theFont, theText, result, y);
    except
      begin RaiseException('Unable to get the text width'); exit; end;
    end;
  end;

  function TextWidth(theText: WideString; theFont: Font): Longint; overload;
  var
    y: Longint; //SizeText returns both... store and ignore y
  begin
    result := 0;
    if not Assigned(theFont) then begin RaiseWarning('No font supplied to TextWidth'); exit; end;
    try
      y := 0; 
      if length(theText) = 0 then result := 0
      else TextDriver.SizeOfUnicode(theFont, theText, result, y);
    except
      begin RaiseException('Unable to get the text width'); exit; end;
    end;
  end;


  /// Calculates the height of a string when drawn with a given font.
  ///
  /// @param theText:   The text to measure
  /// @param theFont:   The font used to draw the text
  /// @returns           The height of the drawing in pixels
  function TextHeight(theFont: Font; theText: String): Longint; overload;
  var
    w: Longint; //SizeText returns both... store and ignore w
  begin
    result :=  0;
    
    if not Assigned(theFont) then begin RaiseWarning('No font supplied to TextHeight'); exit; end;
    try
        w := 0;
		TextDriver.SizeOfText(theFont, theText, w, result);
    except
      begin RaiseException('Unable to get the text height'); exit; end;
    end;
  end;

  function TextHeight(theText: WideString; theFont: Font): Longint; overload;
  var
    w: Longint; //SizeText returns both... store and ignore w
  begin
    result :=  0;
    
    if not Assigned(theFont) then begin RaiseWarning('No font supplied to TextHeight'); exit; end;
    try
      w := 0;
	  TextDriver.SizeOfUnicode(theFont,theText,w,result);
    except
      begin RaiseException('Unable to get the text height'); exit; end;
    end;
  end;
  
  procedure DrawFramerate(x, y: Longint; name: String); overload;
  begin
    DrawFramerate(x, y, FontNamed(name));
  end;
  
  procedure DrawFramerate(x, y: Longint; name: String; size: Longint); overload;
  begin
    DrawFramerate(x, y, LoadFontNamed(FontNameFor(name, size), name, size));
  end;
  
  procedure DrawFramerate(x, y: Longint; font: Font); overload;
  var
    textColor : Color;
    average, highest, lowest : String;
  begin
    //Draw framerates
    CalculateFramerate(average, highest, lowest, textColor);

    if not Assigned(font) then
      DrawTextOnScreen('FPS: (' + highest + ', ' + lowest + ') ' + average, textColor, x + 2, y + 2)
    else
      DrawTextOnScreen('FPS: (' + highest + ', ' + lowest + ') ' + average, textColor, font, x + 2, y + 2);
  end;
  
  procedure DrawFramerate(x, y: Longint); overload;
  begin
    DrawFramerate(x, y, Font(nil));
  end;
  
  procedure DrawText(theText: String; textColor: Color; x, y: Single); overload;
  begin
    DrawText(screen, theText, textColor, ToScreenX(x), ToScreenY(y));
  end;

  procedure DrawText(theText: String; textColor: Color; const pt: Point2D);
  begin
    DrawText(theText, textColor, pt.x, pt.y);
  end;

  procedure DrawTextOnScreen(theText: String; textColor: Color; x, y: Single); overload;
  begin
    DrawText(screen, theText, textColor, x, y);
  end;
  
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; x, y: Single); overload;
  begin
    TextDriver.stringColor(dest, x, y, theText,textColor);
  end;
  
  function TextAlignmentFrom(str: String): FontAlignment;
  var ch: Char;
  begin
    str := trim(str);
    if length(str) > 0 then ch := str[1] else ch := 'l';
    
    case ch of
      'c', 'C': result := AlignCenter;
      'r', 'R': result := AlignRight;
      else result := AlignLeft;
    end;
  end;

//=============================================================================

//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
    
    _Fonts := TStringHash.Create(False, 1024);
    
    if TextDriver.Init() = -1 then
    begin
      begin RaiseException('Error opening font library. ' + TextDriver.GetError()); exit; end;
    end;
  end;

//=============================================================================

  finalization
  begin
    ReleaseAllFonts();
    FreeAndNil(_Fonts);
    TextDriver.Quit();
  end;

end.
