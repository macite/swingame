unit sgDriverText;
//=============================================================================
// sgDriverText.pas
//=============================================================================
//
// The TextDriver is responsible for providing an interface between SwinGame
// code and the drivers. It can interface between any Text driver provided
//
// Change History:
//	2012-01-05: Aaron : Created File. 
//
// Notes:
//		-
// TODO:
// 		- 
//=============================================================================

interface
	uses sgTypes,
		
		{$IFDEF SWINGAME_OPENGL}
			sgDriverTextOpenGL
		{$ELSE}
			{$IFDEF SWINGAME_SDL13}
				sgDriverTextSDL13
			{$ELSE}
				sgDriverTextSDL
		{$ENDIF}
     
   {$ENDIF};
	
	type
		// These function and procedure pointers are required by the TextDriverRecord
			
		//loads a TTF font with a font name and size. Return a swingame Font 
		LoadFontProcedure = function(fontName, fileName : String; size : Longint) : font;
		
		// closes a font.		
		CloseFontProcedure = procedure(fontToClose : font);
		/// This function prints "str" with font "font" and color "clrFg"
	  ///  * onto a rectangle of color "clrBg".
	  ///  * It does not pad the text.
		PrintStringsProcedure = procedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
    /// This function prints "str" with font "font" and color "clrFg"
    ///  * onto a rectangle of color "clrBg".
    ///  * It does not pad the text.
		PrintWideStringsProcedure = procedure(dest: Bitmap; font: Font; str: WideString; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
    // sets the current font and font style.
		SetFontStyleProcedure = procedure(fontToSet : Font; value: Fontstyle);
    // gets the current fontstyle of the given font
		GetFontStyleProcedure = function(font : Font) : FontStyle;	
    //returns the size of a given text using a given font.
		SizeOfTextProcedure = function(font : Font ;  theText : String ; var w : Longint; var h : Longint) : Integer;
    //returns the size of a given text using a given font.
		SizeOfUnicodeProcedure = function(font : Font; theText : WideString; var w : Longint; var h : Longint) : Integer;
    //closes the font module.
		QuitProcedure = procedure();
    // shows error messages.
		GetErrorProcedure = function() : string;
    //checks if font library is initialiszed.
		InitProcedure = function() : integer;
		StringColorProcedure = procedure (dest : Bitmap; x,y : Single; theText : String; theColor : Color);
		
		
		TextDriverRecord = record
			LoadFont : LoadFontProcedure;
			CloseFont : CloseFontProcedure;
			PrintStrings : PrintStringsProcedure;
			PrintWideStrings : PrintWideStringsProcedure;
			SetFontStyle : SetFontStyleProcedure;
			GetFontStyle : GetFontStyleProcedure;
			SizeOfText : SizeOfTextProcedure;
			SizeOfUnicode : SizeOfUnicodeProcedure;
			Quit : QuitProcedure;
			GetError : GetErrorProcedure;
			Init : InitProcedure;
			StringColor : StringColorProcedure;
		end;
		
		
	var
	// Global variable used to allow SwinGame to access the functions and procedures
	// of the audio driver.
		TextDriver : TextDriverRecord;
		
//=============================================================================
		
implementation
	procedure LoadDefaultTextDriver();
	begin

	{$IFDEF SWINGAME_OPENGL}
		LoadOpenGLTextDriver()
	{$ELSE}
	{$IFDEF SWINGAME_SDL13}
		LoadSDL13TextDriver()
		{$ELSE}
	 		LoadSDLTextDriver()
	{$ENDIF}

   {$ENDIF};
	end;
	
	function DefaultLoadFontProcedure(fontName, fileName : String; size : Longint) : font;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.LoadFont(fontName, fileName, size);
	end;
	
	procedure DefaultCloseFontProcedure(fontToClose : font);
	begin
		LoadDefaultTextDriver();
		TextDriver.CloseFont(fontToClose);		
	end;
	
	procedure DefaultPrintStringsProcedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
	begin
		LoadDefaultTextDriver();
		TextDriver.PrintStrings(dest,font,str,rc,clrFg,clrBg,flags)
	end;
	
	procedure DefaultPrintWideStringsProcedure(dest: Bitmap; font: Font; str: WideString; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment) ;
	begin
		LoadDefaultTextDriver();
		TextDriver.PrintWideStrings(dest,font,str,rc,clrFg,clrBg,flags)
	end;
	
	procedure DefaultSetFontStyleProcedure(fontToSet : font; value : FontStyle);
	begin
		LoadDefaultTextDriver();
		TextDriver.SetFontStyle(fontToSet , value);
	end;
	
	function DefaultGetFontStyleProcedure(font : Font) : FontStyle;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.GetFontStyle(font);
	end;
	
	function DefaultSizeOfTextProcedure(font : Font; theText : String; var w : Longint ; var h : Longint) : Integer;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.SizeOfText(font, theText, w, h);
	end;
	
	function DefaultSizeOfUnicodeProcedure(font : Font; theText : WideString; var w : Longint; var h : Longint) : Integer;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.SizeOfUnicode(font, theText, w, h);
	end;
	
	procedure DefaultQuitProcedure();
	begin
		LoadDefaultTextDriver();
		TextDriver.Quit();
	end;
	
	function DefaultGetErrorProcedure() : String;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.GetError();
	end;
	
	function DefaultInitProcedure(): integer;
	begin
		LoadDefaultTextDriver();
		result := TextDriver.Init();
	end;
	
	procedure DefaultStringColorProcedure(dest : Bitmap; x,y : Single; theText : String; theColor : Color); 
	begin
	  LoadDefaultTextDriver();
	  TextDriver.StringColor(dest,x,y,theText,theColor);
	end;
	
//=============================================================================
	
	
	
	
	initialization
	begin
		TextDriver.LoadFont := @DefaultLoadFontProcedure;
		TextDriver.CloseFont := @DefaultCloseFontProcedure;
		TextDriver.PrintStrings := @DefaultPrintStringsProcedure;
		TextDriver.PrintWideStrings := @DefaultPrintWideStringsProcedure;
		TextDriver.SetFontStyle := @DefaultSetFontStyleProcedure;
		TextDriver.GetFontStyle := @DefaultGetFontStyleProcedure;
		TextDriver.SizeOfText := @DefaultSizeOfTextProcedure;
		TextDriver.SizeOfUnicode := @DefaultSizeOfUnicodeProcedure;
		TextDriver.Quit := @DefaultQuitProcedure;
		TextDriver.GetError := @DefaultGetErrorProcedure;
		TextDriver.Init := @DefaultInitProcedure;
		TextDriver.StringColor := @DefaultStringColorProcedure;
	end;

end.