//=============================================================================
// sgText.pas
//=============================================================================
//
// The Font unit relates to writing text to the screen,
// and to loading and styling the associated fonts.
//
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
	function LoadFont(const fontName: String; size: Longint): Font;
	
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
	function LoadFontNamed(const name, filename: String; size: Longint): Font;
	
	/// Determines if SwinGame has a font loaded for the supplied name.
	/// This checks against all fonts loaded, those loaded without a name
	/// are assigned the filename as a default.
	/// 
	/// @lib
	function HasFont(const name: String): Boolean;
	
	/// Determines the name that will be used for a font loaded with
	/// the indicated fontName and size.
	/// 
	/// @lib
	/// @sn fontName:%s size:%s
	function FontNameFor(const fontName: String; size: Longint): String;
	
	/// Returns the `Font` that has been loaded with the specified name,
	/// and font size using `LoadFont`.
	///
	/// @lib FontNamedWithSize
	/// @sn fontNamed:%s withSize:%s
	function FontNamed(const name: String; size: Longint): Font; overload;
	
	/// Returns the `Font` that has been loaded with the specified name,
	/// see `LoadFontNamed`.
	///
	/// @lib
	function FontNamed(const name: String): Font; overload;
	
	/// Releases the SwinGame resources associated with the font of the
	/// specified ``name``.
	///
	/// @lib
	procedure ReleaseFont(const name: String);
	
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
	function TextWidth(theFont: Font; const theText: String): Longint; overload;

	/// Returns the height (in pixels) of the passed in text and the font it will be drawn with.
	///
	/// @lib
	/// @sn font:%s heightOf:%s
	///
	/// @class Font
	/// @method TextHeight
	function TextHeight(theFont: Font; const theText: String): Longint; overload;

	/// Returns the font alignment for the passed in character (l = left. r = right, c = center).
	/// 
	/// @lib
	function TextAlignmentFrom(const str: String): FontAlignment;
	
	
	
//---------------------------------------------------------------------------
// Draw Text - using font
//---------------------------------------------------------------------------
	
	/// Draws the text at the specified point using the color and font indicated.
	///
	/// @lib
	/// @sn drawText:%s color:%s font:%s x:%s y:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor: Color; theFont: Font; x, y: Single); overload;
	
	/// Draws the text at the specified point using the color and font indicated.
	///
	/// @lib DrawTextWithFontNamed
	/// @sn drawText:%s color:%s fontNamed:%s x:%s y:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor: Color; const name: String; x, y: Single); overload;
	
	/// Draws theText at the specified point using the color and font indicated.
	///
	/// @lib DrawTextWithFontNamedSize
	/// @sn drawText:%s color:%s fontNamed:%s size:%s x:%s y:%s
	procedure DrawText(const theText: String; textColor: Color; const name: String; size: Longint; x, y: Single); overload;
	
	/// Draws the text at the specified x,y location using the color, font, and options indicated.
	///
	/// @lib DrawTextOpts
	/// @sn drawText:%s color:%s font:%s atX:%s y:%s opts:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor: Color; theFont: Font; x, y: Single; const opts: DrawingOptions); overload;
	
	/// Draws the text at the specified x,y location using the color, font, and options indicated.
	///
	/// @lib DrawTextWithFontNamedOpts
	/// @sn drawText:%s color:%s fontNamed:%s atX:%s y:%s opts:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor: Color; const name: String; x, y: Single; const opts: DrawingOptions); overload;
	
	/// Draws the text at the specified x,y location using the color, font, and options indicated.
	///
	/// @lib DrawTextWithFontNamedAndSizeOpts
	/// @sn drawText:%s color:%s fontNamed:%s size:%s atX:%s y:%s opts:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor: Color; const name: String; size: Longint; x, y: Single; const opts: DrawingOptions); overload;
	
	/// Draws the text onto the bitmap using the color and font indicated, then returns the bitmap created.
	/// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
	/// good idea if the text does not change frequently.
	///
	/// @lib DrawTextToBitmapAtPointWithFontNamedAndSize
	/// @sn drawTextFont:%s string:%s textColor:%s backgroundColor:%s   
	/// @doc_details
	function DrawTextToBitmap(font: Font; const str: String; clrFg, backgroundColor : Color) : Bitmap;
	
	
//---------------------------------------------------------------------------
// Draw Text in an area
//---------------------------------------------------------------------------
	
	/// Draws the text in the specified rectangle using the fore and back colors, and the font indicated.
	///
	/// @lib DrawTextInRect
	/// @sn drawText:%s textColor:%s backColor:%s font:%s align:%s inRect:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const area: Rectangle); overload;
	
	/// Draws the text in the specified rectangle using the fore and back colors, and the font indicated.
	///
	/// @lib DrawTextInRectWithFontNamed
	/// @sn drawText:%s textColor:%s backColor:%s fontNamed:%s align:%s inRect:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor, backColor: Color; const name: String; align: FontAlignment; const area: Rectangle); overload;
	
	/// Draws theText in the specified rectangle using the fore and back colors, and the font indicated.
	///
	/// @lib DrawTextInRectWithFontNamedAndSize
	/// @sn drawText:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s inRect:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor, backColor: Color; const name: String; size: Longint; align: FontAlignment; const area: Rectangle); overload;
	
	/// Draws the text in the rectangle using the fore and back colors, font and options indicated.
	///
	/// @lib DrawTextInRectOpts
	/// @sn drawText:%s textColor:%s backColor:%s font:%s align:%s in:%s opts:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	
	/// Draws the text in the rectangle using the fore and back colors, font and options indicated.
	///
	/// @lib DrawTextInRectWithFontNamedOpts
	/// @sn drawText:%s textColor:%s backColor:%s fontNamed:%s align:%s in:%s opts:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor, backColor: Color; const name: String; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	
	/// Draws the text in the rectangle using the fore and back colors, font and options indicated.
	///
	/// @lib DrawTextInRectWithFontNamedAndSizeOpts
	/// @sn drawText:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s in:%s opts:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor, backColor: Color; const name: String; size: Longint; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	
	
//---------------------------------------------------------------------------
// Draw Text - without font
//---------------------------------------------------------------------------
	
	/// Draws text using a simple bitmap font that is built into SwinGame.
	///
	/// @lib DrawSimpleText
	/// @sn drawText:%s color:%s x:%s y:%s
	procedure DrawText(const theText: String; textColor: Color; x, y: Single); overload;
	
	/// Draws text using a simple bitmap font that is built into SwinGame.
	///
	/// @lib DrawSimpleTextOpts
	/// @sn drawText:%s color:%s atX:%s y:%s opts:%s
	/// @doc_details
	procedure DrawText(const theText: String; textColor: Color; x, y: Single; const opts: DrawingOptions); overload;
	
	/// 
	/// @lib DrawFramerateWithSimpleFont
	/// @sn drawFramerateAtX:%s y:%s
	procedure DrawFramerate(x, y: Single); overload;
	
	
//=============================================================================
implementation
	uses SysUtils, Classes, 
			 stringhash, sgTrace, sgBackendTypes,         // libsrc
			 sgUtils, sgGeometry, sgGraphics, sgCamera, sgShared, sgResources, sgImages, sgDriverText, sgDrawingOptions;
//=============================================================================

	const EOL = LineEnding; // from sgShared

	var
		_Fonts: TStringHash;

//----------------------------------------------------------------------------
	
	function LoadFont(const fontName: String; size: Longint): Font;
	begin
		result := LoadFontNamed(FontNameFor(fontName, size), fontName, size);
	end;
	
	procedure _DoFreeFont(var fontToFree: Font);
	var
		fp: FontPtr;
	begin
		fp := ToFontPtr(fontToFree);

		if Assigned(fp) then
		begin
			{$IFDEF TRACE}
				Trace('Resources', 'IN', 'FreeFont', 'After calling free notifier');
			{$ENDIF}
			try
				{$IFDEF TRACE}
						Trace('Resources', 'IN', 'FreeFont', 'Before calling close font');
				{$ENDIF}
				
				CallFreeNotifier(fontToFree);
				TextDriver.CloseFont(fp);
				fp^.id := NONE_PTR;
				Dispose(fp);
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
	var
		fp: FontPtr;
	begin
		fp := ToFontPtr(fontToFree);

		if Assigned(fp) then ReleaseFont(fp^.name);
			
		fontToFree := nil;
	end;

//----------------------------------------------------------------------------

	function LoadFontNamed(const name, filename: String; size: Longint): Font;
	var
		obj: tResourceContainer;
		fnt: FontPtr;
		
		function _DoLoadFont(const fontName: String; size: Longint): Font;
		var
			filename: String;
			originalFilename: String; 
		begin
			originalFilename := ''; 
			{$IFDEF TRACE}
				TraceEnter('sgText', '_DoLoadFont(', fontName + ' ' + IntToStr(size));
			{$ENDIF}
			
			filename := fontName;
			if not FileExists(filename) then
			begin
				filename := PathToResource(filename, FontResource);
				
				if not FileExists(filename) then
				begin
					originalFilename := filename; 
					filename := filename + '.ttf';
					
					if not FileExists(filename) then
					begin
						RaiseWarning('Unable to locate font ' + fontName + ' at ' + originalFilename);
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

	function HasFont(const name: String): Boolean;
	begin
		result := _Fonts.containsKey(name);
	end;
	
	function FontNameFor(const fontName: String; size: Longint): String;
	begin
		result := fontName + '|' + IntToStr(size);
	end;
	
  function FontNamed(const name: String; size: Longint): Font;
  var
    filename: String;
  begin
    result := FontNamed(FontNameFor(name, size));
    if (result = nil) then
    begin
      filename := PathToResource(name, FontResource);

      if FileExists(name) or FileExists(filename) then
      begin
        result := LoadFontNamed(name, name, size);
      end
    end;
  end;
	
	function FontNamed(const name: String): Font;
	var
		tmp : TObject;
	begin
		tmp := _Fonts.values[name];
		if assigned(tmp) then result := Font(tResourceContainer(tmp).Resource)
		else result := nil;
	end;
	
	procedure ReleaseFont(const name: String);
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
	var
		fp: FontPtr;
	begin
		fp := ToFontPtr(font);

		if not Assigned(fp) then begin RaiseWarning('No font supplied to FontSetStyle'); exit; end;
		//TTF_SetFontStyle(font^.fptr, Longint(value));
		TextDriver.SetFontStyle(fp, value);
	end;
	
	function FontFontStyle(font: Font): FontStyle;
	var
		fp: FontPtr;
	begin
		fp := ToFontPtr(font);

		result := NormalFont;
		if not Assigned(fp) then begin RaiseWarning('No font supplied to FontFontStyle'); exit; end;
		result := TextDriver.GetFontStyle(fp);
	end;

	function IsSet(toCheck, checkFor: FontAlignment): Boolean; overload;
	begin
		result := (Longint(toCheck) and Longint(checkFor)) = Longint(checkFor);
	end;
	
	//
	// Converts a string into an array of lines.
	// Updates width and height so that the values are sufficient to create
	// a bitmap that will surround these with the given font.
	// -- note initial values for width and height need to be supplied.
	//
	function ToLineArray(str: String; font: FontPtr; var width, height: Longint): StringArray;
	var
		n, i, w, h, newHeight, baseHeight: Longint;
		subStr: String;
	begin
		// Break the String into its lines:
		SetLength(result, 0);
		n := -1;
		i := 0;
		newHeight := 0;

		// get a height value to use for each empty line
		TextDriver.SizeOfText(font, 'I', w, baseHeight);

		while n <> 0 do // n = position in string
		begin
			// Get until either "\n" or "\0":
			n := Pos(eol, str);

			//Copy all except EOL
			if n = 0 then subStr := str 		// no newlines
			else if n = 1 then subStr := ''		// no text then new line
			else subStr := Copy(str, 1, n - 1); // a new line... copy to new string

			if n <> 0 then // there was some substr copied...
			begin
				//Remove the line from the original string
				str := Copy( str, n + Length(eol), Length(str) );
			end;

			//Store in the lines array
			i := i + 1;
			SetLength(result, i);
			result[i - 1] := subStr;

			w := 0;
			// Get the size of the rendered text.
			if Length(subStr) > 0 then 
			begin
				TextDriver.SizeOfText(font, subStr, w, h);
				newHeight += h;
			end
			else
			begin
				newHeight += baseHeight;
			end;

			if w > width then width := w;
		end;

		// Length(result) = Number of Lines.
		// we assume that height is the same for all lines.
		// newHeight += (Length(result) - 1) * TextDriver.LineSkip( font );
		if newHeight > height then height := newHeight;
	end;

	/// This function prints "str" with font "font" and color "clrFg"
	///  * onto a rectangle of color "clrBg".
	///  * It does not pad the text.
	procedure PrintStrings(dest: Pointer; font: FontPtr; const str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment) ;
	var
		lineSkip, width, height: Longint;
		lines: StringArray;
		i, w, h: Longint;
		x, y: Single;
		isWindow: Boolean;
	begin
		// If there's nothing to draw, return NULL
		if (Length(str) = 0) or (font = nil) then exit;

		if PtrKind(dest) = WINDOW_PTR then isWindow := true
		else isWindow := false;

		// Get basic metrics
		lineSkip  := TextDriver.LineSkip( font );

		width     := Round(rc.width);
		height    := 0;

		lines := ToLineArray(str, font, width, height);

		if (width <= 0) or (height <= 0) then exit;

		if (rc.width < 0) or (rc.height < 0) then
		begin
			rc.width := width;
			rc.height := height;
		end;

		// Clip bitmap
		if isWindow then
			PushClip(Window(dest), rc)
		else
			PushClip(Bitmap(dest), rc);

		x := rc.x;
		y := 0;

		// Actually render the text:
		for i := 0 to High(lines) do
		begin
			// Skip empty lines
			if (Length(lines[i]) = 0) or (lines[i] = ' ') then continue;
		
			// This lines metrics
			w := 0;
			h := 0;
			TextDriver.SizeOfText(font, lines[i], w, h);

			y := rc.y + i * lineSkip;
			if y - rc.y > rc.height then break; // drawing lines outside box

			// Determine position for line
			if IsSet(flags, AlignCenter) then
			begin
				x := rc.x + rc.width / 2 - w div 2;
			end
			else if IsSet(flags, AlignRight) then
			begin
				x := rc.x + rc.width - w;
			end;
			// if its left there is nothing to change...

			// Render the current line. 
			TextDriver.PrintStrings(dest, font, lines[i], RectangleFrom(x, y, w, h), clrFg, clrBg, flags);
		end;

		if isWindow then
			PopClip(Window(dest))
		else
			PopClip(Bitmap(dest));
	end;

	function DrawTextToBitmap(font: Font; const str: String; clrFg, backgroundColor : Color) : Bitmap;
	var
		resultBitmap : Bitmap;
		bitmapSize : Rectangle;
		w, h: Longint;
		fp: FontPtr;
	begin
		fp := ToFontPtr(font);

		result := nil;
		// If there's nothing to draw, return NULL
		if (Length(str) = 0) or (fp = nil) then exit;

		bitmapSize.x := 0;
		bitmapSize.y := 0;
		
		w := 0;
		h := 0;
		ToLineArray(str, fp, w, h);

		bitmapSize.width := w;
		bitmapSize.height := h;

		//WriteLn(bitmapSize.width, 'x', bitmapSize.height);

		resultBitmap := CreateBitmap(Round(bitmapSize.width), Round(bitmapSize.height));
		ClearSurface(resultBitmap, backgroundColor);
		PrintStrings(resultBitmap, fp, str, bitmapSize, clrFg, ColorTransparent, AlignLeft);
	
		result := resultBitmap;
	end;
	
//----------------------------------------------------------------------------
// Draw Text
//----------------------------------------------------------------------------
	
	procedure DrawText(const theText: String; textColor: Color; x, y: Single; const opts: DrawingOptions); overload;
	begin
		if not Assigned(opts.dest) then exit;

		XYFromOpts(opts, x, y);
		TextDriver.stringColor(opts.dest, x, y, theText, textColor);
	end;

	procedure DrawText(const theText: String; textColor: Color; x, y: Single); overload;
	begin
		DrawText(theText, textColor, x, y, OptionDefaults());
	end;

//----------------------------------------------------------------------------
// Draw Text using font
//----------------------------------------------------------------------------

	procedure DrawText(const theText: String; textColor: Color; theFont: Font; x, y: Single; const opts: DrawingOptions); overload;
	var
		rect: Rectangle;
		fp: FontPtr;
	begin
		fp := ToFontPtr(theFont);

		if not Assigned(fp) then exit;
		if not Assigned(opts.dest) then begin RaiseWarning('Cannot draw text, as no destination was supplied'); exit; end;
		if Length(theText) <= 0 then exit;
		
		XYFromOpts(opts, x, y);
		rect.x := x;
		rect.y := y;

		rect.width := -1; //TextWidth(theFont, theText); // + 2;
		rect.height := -1; //TextHeight(theFont, theText); // + 2;
		PrintStrings(opts.dest, fp, theText, rect, textColor, ColorTransparent, AlignLeft);
	end;

	procedure DrawText(const theText: String; textColor: Color; theFont: Font; x, y: Single); overload;
	begin
		DrawText(theText, textColor, theFont, x, y, OptionDefaults());
	end;
	
	procedure DrawText(const theText: String; textColor: Color; const name: String; x, y: Single); overload;
	begin
		DrawText(theText, textColor, FontNamed(name), x, y, OptionDefaults());
	end;
	
	procedure DrawText(const theText: String; textColor: Color; const name: String; size: Longint; x, y: Single); overload;
	begin
		DrawText(theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), x, y, OptionDefaults());
	end;

	procedure DrawText(const theText: String; textColor: Color; const name: String; x, y: Single; const opts: DrawingOptions); overload;
	begin
		DrawText(theText, textColor, FontNamed(name), x, y, opts);
	end;
	
	procedure DrawText(const theText: String; textColor: Color; const name: String; size: Longint; x, y: Single; const opts: DrawingOptions); overload;
	begin
		DrawText(theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), x, y, opts);
	end;

//----------------------------------------------------------------------------
// Draw Text in Area
//----------------------------------------------------------------------------

	procedure DrawText(const theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	var
		fp: FontPtr;
	begin
		fp := ToFontPtr(theFont);

		if not Assigned(fp) then exit;
		if not Assigned(opts.dest) then begin RaiseWarning('Cannot draw text, as no destination was supplied'); exit; end;
		if Length(theText) <= 0 then exit;
		if (area.width <= 0) or (area.height <= 0) then exit;

		PrintStrings(opts.dest, fp, theText, area, textColor, backColor, align);
	end;
	
	procedure DrawText(const theText: String; textColor, backColor: Color; const name: String; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	begin
		DrawText(theText, textColor, backColor, FontNamed(name), align, area, opts);
	end;
	
	procedure DrawText(const theText: String; textColor, backColor: Color; const name: String; size: Longint; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	begin
		DrawText(theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, area, opts);
	end;
		
	procedure DrawText(const theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const area: Rectangle); overload;
	begin
		DrawText(theText, textColor, backColor, theFont, align, area, OptionDefaults());
	end;
	
	procedure DrawText(const theText: String; textColor, backColor: Color; const name: String; align: FontAlignment; const area: Rectangle); overload;
	begin
		DrawText(theText, textColor, backColor, FontNamed(name), align, area, OptionDefaults());
	end;
	
	procedure DrawText(const theText: String; textColor, backColor: Color; const name: String; size: Longint; align: FontAlignment; const area: Rectangle); overload;
	begin
		DrawText(theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, area, OptionDefaults());
	end;
	
//----------------------------------------------------------------------------
// Text metrics
//----------------------------------------------------------------------------
	
	/// Calculates the width of a string when drawn with a given font.
	function TextWidth(theFont: Font; const theText: String): Longint; overload;
	var
		height: Longint; //SizeText returns both... store and ignore height
		fp: FontPtr;
	begin
		fp := ToFontPtr(theFont);

		result := 0;
		height := 0;
		if length(theText) = 0 then exit;
		if not Assigned(fp) then exit;

		ToLineArray(theText, fp, result, height);
	end;

	/// Calculates the height of a string when drawn with a given font.
	function TextHeight(theFont: Font; const theText: String): Longint; overload;
	var
		width: Longint; //SizeText returns both... store and ignore w
		fp: FontPtr;
	begin
		fp := ToFontPtr(theFont);

		result := 0;
		width := 0;
		if length(theText) = 0 then exit;
		if not Assigned(fp) then exit;

		ToLineArray(theText, fp, width, result);
	end;
	
	procedure DrawFramerate(x, y: Single); overload;
	var
		textColor : Color;
		average, highest, lowest : String;
	begin
		//Draw framerates
		CalculateFramerate(average, highest, lowest, textColor);

		DrawText('FPS: (' + highest + ', ' + lowest + ') ' + average, textColor, x + 2, y + 2, OptionToScreen())
	end;
	
	function TextAlignmentFrom(const str: String): FontAlignment;
	var 
		ch: Char;
		tstr: String;
	begin
		tstr := trim(str);
		if length(tstr) > 0 then ch := tstr[1] else ch := 'l';
		
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
