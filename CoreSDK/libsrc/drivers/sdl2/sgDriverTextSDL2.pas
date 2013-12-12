unit sgDriverTextSDL2;

interface
	procedure LoadSDL2TextDriver();
		
implementation
	function LoadFontProcedure(fontName, fileName : String; size : Longint) : font;
	begin
		result := nil;
	end;
	
	procedure CloseFontProcedure(fontToClose : font);
	begin
	end;
	
	procedure PrintStringsProcedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
	begin
	end;
	
	procedure PrintWideStringsProcedure(dest: Bitmap; font: Font; str: WideString; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment) ;
	begin
	end;
	
	procedure SetFontStyleProcedure(fontToSet : font; value : FontStyle);
	begin
	end;
	
	function GetFontStyleProcedure(font : Font) : FontStyle;
	begin
		result := NormalFont;
	end;
	
	function SizeOfTextProcedure(font : Font; theText : String; var w : Longint ; var h : Longint) : Integer;
	begin
		result := 0;
	end;
	
	function SizeOfUnicodeProcedure(font : Font; theText : WideString; var w : Longint; var h : Longint) : Integer;
	begin
		result := 0;
	end;
	
	procedure QuitProcedure();
	begin
	end;
	
	function GetErrorProcedure() : String;
	begin
		result := 'TODO: ERRORS';
	end;
	
	function InitProcedure(): integer;
	begin
	end;
	
	procedure StringColorProcedure(dest : Bitmap; x,y : Single; theText : String; theColor : Color); 
	begin
	end;
	
//=============================================================================	
	
	procedure LoadSDL2TextDriver();
	begin
		TextDriver.LoadFont := @LoadFontProcedure;
		TextDriver.CloseFont := @CloseFontProcedure;
		TextDriver.PrintStrings := @PrintStringsProcedure;
		TextDriver.PrintWideStrings := @PrintWideStringsProcedure;
		TextDriver.SetFontStyle := @SetFontStyleProcedure;
		TextDriver.GetFontStyle := @GetFontStyleProcedure;
		TextDriver.SizeOfText := @SizeOfTextProcedure;
		TextDriver.SizeOfUnicode := @SizeOfUnicodeProcedure;
		TextDriver.Quit := @QuitProcedure;
		TextDriver.GetError := @GetErrorProcedure;
		TextDriver.Init := @InitProcedure;
		TextDriver.StringColor := @StringColorProcedure;
	end;

end.