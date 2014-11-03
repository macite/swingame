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

	/// Returns the font alignment for the passed in character (l = left. r = right, c = center).
	/// 
	/// @lib
	function TextAlignmentFrom(str: String): FontAlignment;
	
	
	
//---------------------------------------------------------------------------
// Draw Text - using font
//---------------------------------------------------------------------------
	
	/// Draws the text at the specified point using the color and font indicated.
	///
	/// @lib
	/// @sn drawText:%s color:%s font:%s x:%s y:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor: Color; theFont: Font; x, y: Single); overload;
	
	/// Draws the text at the specified point using the color and font indicated.
	///
	/// @lib DrawTextWithFontNamed
	/// @sn drawText:%s color:%s fontNamed:%s x:%s y:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor: Color; name: String; x, y: Single); overload;
	
	/// Draws theText at the specified point using the color and font indicated.
	///
	/// @lib DrawTextWithFontNamedSize
	/// @sn drawText:%s color:%s fontNamed:%s size:%s x:%s y:%s
	procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; x, y: Single); overload;
	
	/// Draws the text at the specified x,y location using the color, font, and options indicated.
	///
	/// @lib DrawTextOpts
	/// @sn drawText:%s color:%s font:%s atX:%s y:%s opts:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor: Color; theFont: Font; x, y: Single; const opts: DrawingOptions); overload;
	
	/// Draws the text at the specified x,y location using the color, font, and options indicated.
	///
	/// @lib DrawTextWithFontNamedOpts
	/// @sn drawText:%s color:%s fontNamed:%s atX:%s y:%s opts:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor: Color; name: String; x, y: Single; const opts: DrawingOptions); overload;
	
	/// Draws the text at the specified x,y location using the color, font, and options indicated.
	///
	/// @lib DrawTextWithFontNamedAndSizeOpts
	/// @sn drawText:%s color:%s fontNamed:%s size:%s atX:%s y:%s opts:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; x, y: Single; const opts: DrawingOptions); overload;
	
	/// Draws the text onto the bitmap using the color and font indicated, then returns the bitmap created.
	/// Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
	/// good idea if the text does not change frequently.
	///
	/// @lib DrawTextToBitmapAtPointWithFontNamedAndSize
	/// @sn drawTextFont:%s string:%s textColor:%s backgroundColor:%s   
	/// @doc_details
	function DrawTextTo(font: Font; str: String; clrFg, backgroundColor : Color) : Bitmap;
	
	
//---------------------------------------------------------------------------
// Draw Text in an area
//---------------------------------------------------------------------------
	
	/// Draws the text in the specified rectangle using the fore and back colors, and the font indicated.
	///
	/// @lib DrawTextInRect
	/// @sn drawText:%s textColor:%s backColor:%s font:%s align:%s inRect:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const area: Rectangle); overload;
	
	/// Draws the text in the specified rectangle using the fore and back colors, and the font indicated.
	///
	/// @lib DrawTextInRectWithFontNamed
	/// @sn drawText:%s textColor:%s backColor:%s fontNamed:%s align:%s inRect:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const area: Rectangle); overload;
	
	/// Draws theText in the specified rectangle using the fore and back colors, and the font indicated.
	///
	/// @lib DrawTextInRectWithFontNamedAndSize
	/// @sn drawText:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s inRect:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const area: Rectangle); overload;
	
	/// Draws the text in the rectangle using the fore and back colors, font and options indicated.
	///
	/// @lib DrawTextInRectOpts
	/// @sn bitmap:%s drawText:%s textColor:%s backColor:%s font:%s align:%s in:%s opts:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	
	/// Draws the text in the rectangle using the fore and back colors, font and options indicated.
	///
	/// @lib DrawTextInRectOnBitmapWithFontNamed
	/// @sn drawText:%s textColor:%s backColor:%s fontNamed:%s align:%s in:%s opts:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	
	/// Draws the text in the rectangle using the fore and back colors, font and options indicated.
	///
	/// @lib DrawTextInRectOnBitmapWithFontNamedAndSize
	/// @sn drawText:%s textColor:%s backColor:%s fontNamed:%s size:%s align:%s in:%s opts:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	
	
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
	/// @lib DrawSimpleTextOpts
	/// @sn drawText:%s color:%s atX:%s y:%s opts:%s
	/// @doc_details
	procedure DrawText(theText: String; textColor: Color; x, y: Single; const opts: DrawingOptions); overload;
	
	/// 
	/// @lib DrawFramerateWithSimpleFont
	/// @sn drawFramerateAtX:%s y:%s
	procedure DrawFramerate(x, y: Single); overload;
	
	
//=============================================================================
implementation
	uses SysUtils, Classes, 
			 stringhash, sgTrace,         // libsrc
			 sgUtils, sgGeometry, sgGraphics, sgCamera, sgShared, sgResources, sgImages, sgDriverText, sgDrawingOptions;
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
	var
		lineSkip, width, height: Longint;
		lines: Array of String;
		subStr: String;
		n, i, w, h: Longint;
		x, y: Single;
	begin
		// If there's nothing to draw, return NULL
		if (Length(str) = 0) or (font = nil) then exit;

		// Get basic metrics
		lineSkip  := TextDriver.LineSkip( font );
		width     := Round(rc.width);
		height    := 0;

		// Break the String into its lines:
		SetLength(lines, 1);
		n := -1; i := 0;

		while n <> 0 do // n = position in string
		begin
			// Get until either "\n" or "\0":
			n := Pos(eol, str);

			//Copy all except EOL
			if n = 0 then subStr := str
			else if n = 1 then subStr := ''
			else subStr := Copy(str, 1, n - 1); // copy to new string

			//Remove the line from the original string
			if n <> 0 then
			begin
				str := Copy( str, n + Length(eol), Length(str) );
			end;

			//Store in the lines array
			i := i + 1;
			SetLength(lines, i);
			lines[i - 1] := subStr;

			w := 0;
			// Get the size of the rendered text.
			if Length(subStr) > 0 then TextDriver.SizeOfText(font, subStr, w, height);

			if w > width then width := w;
		end;

		if (width <= 0) or (height <= 0) then exit;

		// Length(lines) = Number of Lines.
		// we assume that height is the same for all lines.
		height := (Length(lines) - 1) * lineSkip + (height * Length(lines));

		if (rc.width < 0) or (rc.height < 0) then
		begin
			rc.width := width;
			rc.height := height;
		end;

		// Clip bitmap
		PushClip(dest, rc);

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

		PopClip(dest);
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

		//WriteLn(bitmapSize.width, 'x', bitmapSize.height);

		resultBitmap := CreateBitmap(Round(bitmapSize.width), Round(bitmapSize.height));
		ClearSurface(resultBitmap, backgroundColor);
		PrintStrings(resultBitmap, font, str, bitmapSize, clrFg, ColorTransparent, AlignLeft);
	
		result := resultBitmap;
	end;
	
//----------------------------------------------------------------------------
// Draw Text
//----------------------------------------------------------------------------
	
	procedure DrawText(theText: String; textColor: Color; x, y: Single; const opts: DrawingOptions); overload;
	begin
		if not Assigned(opts.dest) then exit;

		XYFromOpts(opts, x, y);
		TextDriver.stringColor(opts.dest, x, y, theText, textColor);
	end;

	procedure DrawText(theText: String; textColor: Color; x, y: Single); overload;
	begin
		DrawText(theText, textColor, x, y, OptionDefaults());
	end;

//----------------------------------------------------------------------------
// Draw Text using font
//----------------------------------------------------------------------------

	procedure DrawText(theText: String; textColor: Color; theFont: Font; x, y: Single; const opts: DrawingOptions); overload;
	var
		rect: Rectangle;
	begin
		if not Assigned(theFont) then exit;
		if not Assigned(opts.dest) then begin RaiseWarning('Cannot draw text, as no destination was supplied'); exit; end;
		if Length(theText) <= 0 then exit;
		
		XYFromOpts(opts, x, y);
		rect.x := x;
		rect.y := y;

		rect.width := -1; //TextWidth(theFont, theText); // + 2;
		rect.height := -1; //TextHeight(theFont, theText); // + 2;
		PrintStrings(opts.dest, theFont, theText, rect, textColor, ColorTransparent, AlignLeft);
	end;

	procedure DrawText(theText: String; textColor: Color; theFont: Font; x, y: Single); overload;
	begin
		DrawText(theText, textColor, theFont, x, y, OptionDefaults());
	end;
	
	procedure DrawText(theText: String; textColor: Color; name: String; x, y: Single); overload;
	begin
		DrawText(theText, textColor, FontNamed(name), x, y, OptionDefaults());
	end;
	
	procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; x, y: Single); overload;
	begin
		DrawText(theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), x, y, OptionDefaults());
	end;

	procedure DrawText(theText: String; textColor: Color; name: String; x, y: Single; const opts: DrawingOptions); overload;
	begin
		DrawText(theText, textColor, FontNamed(name), x, y, opts);
	end;
	
	procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; x, y: Single; const opts: DrawingOptions); overload;
	begin
		DrawText(theText, textColor, LoadFontNamed(FontNameFor(name, size), name, size), x, y, opts);
	end;

//----------------------------------------------------------------------------
// Draw Text in Area
//----------------------------------------------------------------------------

	procedure DrawText(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	begin
		if not Assigned(theFont) then exit;
		if not Assigned(opts.dest) then begin RaiseWarning('Cannot draw text, as no destination was supplied'); exit; end;
		if Length(theText) <= 0 then exit;
		if (area.width <= 0) or (area.height <= 0) then exit;

		PrintStrings(opts.dest, theFont, theText, area, textColor, backColor, align);
	end;
	
	procedure DrawText(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	begin
		DrawText(theText, textColor, backColor, FontNamed(name), align, area, opts);
	end;
	
	procedure DrawText(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const area: Rectangle; const opts: DrawingOptions); overload;
	begin
		DrawText(theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, area, opts);
	end;
		
	procedure DrawText(theText: String; textColor, backColor: Color; theFont: Font; align: FontAlignment; const area: Rectangle); overload;
	begin
		DrawText(theText, textColor, backColor, theFont, align, area, OptionDefaults());
	end;
	
	procedure DrawText(theText: String; textColor, backColor: Color; name: String; align: FontAlignment; const area: Rectangle); overload;
	begin
		DrawText(theText, textColor, backColor, FontNamed(name), align, area, OptionDefaults());
	end;
	
	procedure DrawText(theText: String; textColor, backColor: Color; name: String; size: Longint; align: FontAlignment; const area: Rectangle); overload;
	begin
		DrawText(theText, textColor, backColor, LoadFontNamed(FontNameFor(name, size), name, size), align, area, OptionDefaults());
	end;
	
//----------------------------------------------------------------------------
// Text metrics
//----------------------------------------------------------------------------
	
	/// Calculates the width of a string when drawn with a given font.
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

	/// Calculates the height of a string when drawn with a given font.
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
	
	procedure DrawFramerate(x, y: Single); overload;
	var
		textColor : Color;
		average, highest, lowest : String;
	begin
		//Draw framerates
		CalculateFramerate(average, highest, lowest, textColor);

		DrawText('FPS: (' + highest + ', ' + lowest + ') ' + average, textColor, x + 2, y + 2, OptionToScreen())
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
