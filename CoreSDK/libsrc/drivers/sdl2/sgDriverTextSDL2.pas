unit sgDriverTextSDL2;

interface
	procedure LoadSDL2TextDriver();
		
implementation
	uses sgTypes, sgDriverSDL2Types, sgDriverText, sgShared;

	function LoadFontProcedure(fontName, fileName : String; size : Longint) : font;
	var
		fdata: psg_font_data;
	begin
		New(result);
		New(fdata);
		if (result = nil) or (fdata = nil) then
		begin
			RaiseException('LoadFont to allocate space.');
			exit;
		end;

		fdata^ := _sg_functions^.text.load_font(PChar(fileName), size);
		result^.fptr := fdata;

		if result^.fptr = nil then
		begin
			Dispose(result);
			result := nil;
			RaiseWarning('LoadFont failed: ' + fontName + ' (' + fileName + ')');
			exit;
		end;

		result^.name := fontName;
	end;
	
	procedure CloseFontProcedure(fontToClose : font);
	var
		fdata: psg_font_data;   
	begin
		fdata := fontToClose^.fptr;
		_sg_functions^.text.close_font(fdata);
		Dispose(fdata);
		fontToClose^.fptr := nil;
	end;
	
	//TODO: move most of this to sgText
	procedure PrintStringsProcedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
	begin
		_sg_functions^.text.draw_text(dest^.surface, font^.fptr, rc.x, rc.y, PChar(str), _ToSGColor(clrFg));
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
		result := _sg_functions^.text.text_size(font^.fptr, PChar(theText), @w, @h);
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
		result := 0;
	end;
	
	procedure StringColorProcedure(dest : Bitmap; x,y : Single; theText : String; theColor : Color); 
	begin
		_sg_functions^.text.draw_text(dest^.surface, nil, x, y, PChar(theText), _ToSGColor(theColor));
	end;

	function LineSkipFunction(fnt: Font): Integer;
	begin
		result := _sg_functions^.text.text_line_skip(fnt^.fptr);
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

		TextDriver.LineSkip := @LineSkipFunction;
	end;

end.