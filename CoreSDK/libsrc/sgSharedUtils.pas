//=============================================================================
// sgUtils.pas
//=============================================================================
//
// Extra String Utilities used for processing parts of SwinGame.
// 
//
// Change History:
//
// Version 3.0:
// - 2010-02-17: David  : Added ExtractAfterFirstDelim
// - 2010-02-04: Andrew : Renamed unit, added code to process lines of a text file
// - 2010-02-03: Andrew : Added ExtractFileAndPath
// - 2010-02-03: Aaron  : Added SingleArrayToRange
//                        Added ZeroArray
// - 2010-02-02: Aaron  : Added LongintArrayToRange
// - Earlier   : Andrew : Various changes
//=============================================================================
unit sgSharedUtils;

interface
uses sgTypes, sgBackendTypes;

  {$ifndef FPC} // Delphi land
  function ExtractDelimited(index: integer; const value: string; delim: TSysCharSet): string;
  {$endif}
  
  function ExtractAfterFirstDelim(index: integer; const value: string; delim: Char): string;

  function CountDelimiter(const value: String; delim: Char): Longint;
  function CountDelimiterWithRanges(const value: String; delim: Char): Longint;
  
  function ExtractDelimitedWithRanges(index: Longint; const value: String): String;
  function ProcessRange(const valueIn: String): LongintArray;
  function ProcessFloatRange(const valueIn: String): SingleArray;

  function MyStrToInt(const str: String): Longint; overload;
  function MyStrToInt(const str: String; allowEmpty: Boolean) : Longint; overload;
  
  function MyStrToFloat( const str: String): Extended; overload;
  function MyStrToFloat(const str: String; allowEmpty: Boolean) : Extended; overload;

  function LongintArrayToRange(ints: LongintArray):String;
  function SingleArrayToRange(singles: SingleArray):String;

  function ExtractFileAndPath(const fullnameIn: String; out path, filename: String; allowNewFile: Boolean): Boolean;

  procedure ZeroArray (var ints : LongintArray); overload;
  procedure ZeroArray (var singles : SingleArray); overload;
  procedure ZeroArray (var Ints : array of LongintArray); overload;
  procedure ZeroArray (var singles : array of SingleArray); overload;
  
  procedure MakeFalse (var visited: Array of Boolean);
  function WithinRange(arrayLength : Integer; currentIndex : Integer) : Boolean;
    
  //Checks if pointer is assigned, then raises a warning and exits
  function CheckAssigned(const msg : String; ptr : Pointer): Boolean;
    
  type
    LineData = record
      filename: String;
      data:     String;
      lineNo:   Longint;
    end;
    LineProcessor = procedure(const data: LineData; ptr: Pointer);
  
  // This reads the lines of the passed in file, calling proc for each non comment line.
  // the ptr is then passed to each line to allow custom data to be passed into the line processing
  // procedure.
  procedure ProcessLinesInFile(const filename: String; kind: ResourceKind; proc: LineProcessor; ptr: Pointer);
  
  
implementation
  uses 
    SysUtils, Math, Classes, StrUtils,
    sgShared, sgResources, sgTrace;

  function WithinRange(arrayLength : Integer; currentIndex : Integer) : Boolean;
  begin
    result := (currentIndex >= 0) and (currentIndex < arrayLength);
  end;

  procedure ZeroArray (var ints : LongintArray); overload;
  var
  i: Longint;
  begin
    for i := low(ints) to high(ints) do
    begin
        ints[i] := 0;
      end;

  end;
  
  procedure ZeroArray (var singles : SingleArray); overload;
  var
  i : Longint;
  begin
    for i := low(singles) to high(singles) do
    begin
        singles[i] := 0;
      end;

  end;

  procedure ZeroArray (var singles : array of SingleArray); overload;
  var
  i : Longint;
  begin
    for i := low(singles) to high(singles) do
    begin
      ZeroArray(singles[i]);
    end;
  end;

  procedure ZeroArray (var Ints : array of LongintArray); overload;
  var
  i : Longint;
  begin
    for i := low(Ints) to high(Ints) do
    begin
      ZeroArray(Ints[i]);
    end;
  end;
  
  function SingleArrayToRange(singles: SingleArray):String;
  var
  i : Longint;
  begin
    result := '['+FloatToStr(singles[0]);
    for i := low(singles)+1 to high(singles) do
    begin
      result+=','+FloatToStr(Singles[i]);
    end;
    result +=']';
  end; 
  function LongintArrayToRange(ints: LongintArray):String;
  var
    i,temp:longint;
    tempChanged : Boolean;
  begin
    result:= '';
    temp :=0;
    tempChanged := false;
    if not assigned(ints) then exit;
    result := '['+IntToStr(ints[0]);
    for i := low(ints)+1 to high(ints) do
    begin
      //writeln(ints[0], ints[1]);
      if (ints[i]-1) = (ints[i-1]) then
      begin
        if not tempChanged then temp := ints[i]
        else if tempChanged then temp := ints[i];
        tempChanged := true;
      end
      else if tempChanged then
      begin
        result += '-'+IntToStr(temp);
        result += ','+IntToStr(ints[i]);
        tempChanged := false;
        temp := 0;
      end
      else if not tempChanged then result +=','+IntToStr(ints[i]);
    end;
    if tempChanged then
    begin
     result += '-'+IntToStr(temp);
     tempChanged := false;
     temp := 0;
    end;
    
    result += ']';
  end;
  function MyStrToInt(const str: String): Longint; overload;
  begin
    if Length(str) = 0 then result := 0
    else result := StrToInt(Trim(str));
  end;

  function MyStrToInt(const str: String; allowEmpty: Boolean) : Longint; overload;
  begin
    if allowEmpty and (Length(str) = 0) then
    begin
      result := -1;
    end
    else if not TryStrToInt(str, result) then
    begin
      result := 0;
      RaiseException('Value is not an integer : ' + str);
    end
    else if result < 0 then
    begin
      result := 0;
      RaiseException('Value is not an integer : ' + str);
    end;
  end;
  
  function MyStrToFloat(const str: String): Extended; overload;
  begin
    if Length(str) = 0 then result := 0
    else result := StrToFloat(Trim(str));
  end;

  function MyStrToFloat(const str: String; allowEmpty: Boolean) : Extended; overload;
  begin
    if allowEmpty and (Length(str) = 0) then
    begin
      result := 0.0;
    end
    else if not TryStrToFloat(str, result) then
    begin
      result := 0;
      RaiseException('Value is not a number : ' + str);
    end;
  end;

  {$ifndef FPC} 
  // Delphi land
  function ExtractDelimited(index: integer;const value: string; delim: TSysCharSet): string;
  var
    strs: TStrings;
  begin
    // Assumes that delim is [','] and uses simple commatext mode - better check
    if delim <> [','] then
      raise Exception.create('Internal SG bug using ExtractDelimited');
    // okay - let a stringlist do the work
    strs := TStringList.Create();
    strs.CommaText := value;
    if (index >= 0) and (index < strs.Count) then
      result := strs.Strings[index - 1]
    else
      result := '';
    // cleanup
    strs.Free();
  end;
  {$else}
  // proper ExtractDelimited provided by StrUtils
  {$endif}
  
  function ExtractAfterFirstDelim(index: integer;const value: string; delim: Char): string;
  var
    i: Integer;
  begin
    result := '';
    for i := index+1 to CountDelimiter(value , delim)+1 do
    begin
      result += ExtractDelimited(i, value, [delim]);
      if i <> CountDelimiter(value , delim)+1 then result += delim;
    end;
  end;

  function ExtractDelimitedWithRanges(index: Longint;const value: String): String;
  var
    i, count, start: Longint;
    inRange: Boolean;
  begin
    //SetLength(result, 0);
    inRange := false;
    result := '';
    count := 1; //1 is the first index... not 0
  
    // Find the start of this delimited range
    for i := 1 to Length(value) do
    begin
      if count = index then break;
    
      if (not inRange) and (value[i] = ',') then
        count += 1
      else if (inRange) and (value[i] = ']') then
        inRange := false
      else if (not inRange) and (value[i] = '[') then
        inRange := true;
    end;
  
    if count <> index then exit;
    inRange := false;
    start := i;
  
    for i := start to Length(value) do
    begin
      if (not inRange) and (value[i] = ',') then
        break
      else if (inRange) and (value[i] = ']') then
        inRange := false
      else if (not inRange) and (value[i] = '[') then
        inRange := true;
    
      result += value[i];
    end;
  end;

  function CountDelimiter(const value: String; delim: Char): Longint;
  var
    i: Integer;
  begin
    result := 0;
    for i := 1 to Length(value) do
    begin
      if value[i] = delim then 
        result := result + 1;
    end;
  end;
  
  function CountDelimiterWithRanges(const value: String; delim: Char): Longint;
  var
    i: Integer;
    inRange: Boolean;
  begin
    inRange := false;
    result := 0;
    for i := 1 to Length(value) do
    begin
      if (not inRange) and (value[i] = delim) then 
        result := result + 1
      else if (value[i] = '[') then
        inRange := true
      else if (value[i] = ']') then
        inRange := false;
    end;
  end;
  
  function ProcessRange(const valueIn: String): LongintArray;
  var
    i, j, count, temp, lowPart, highPart, dashCount: Longint;
    part, value: String;
  
    procedure _AddToResult(val: Longint);
    begin
      SetLength(result, Length(result) + 1);
      result[High(result)] := val;
    end;  
  begin
    value := Trim(valueIn);
    SetLength(result, 0);
  
    if (value[1] <> '[') or (value[Length(value)] <> ']') then
    begin
      // is number?
      if TryStrToInt(value, temp) then
      begin
        SetLength(result, 1);
        result[0] := temp;
      end;
      exit; //not a range?
    end;
  
    value := MidStr(value, 2, Length(value) - 2);
  
    i := 0;
    count := CountDelimiter(value, ',');
  
    while i <= count do
    begin
      part := Trim(ExtractDelimited(i + 1, value, [',']));
    
      if TryStrToInt(part, temp) then
      begin
        //just "a" so...
        _AddToResult(temp);
      end
      else //Should be range
      begin
        dashCount := CountDelimiter(part, '-');
      
        if (dashCount = 1) or ((dashCount = 2) and (part[1] <> '-')) then //a-b or a--b
          lowPart := MyStrToInt(ExtractDelimited(1, part, ['-']))
        else //assume -a...
          lowPart := -MyStrToInt(ExtractDelimited(2, part, ['-']));
      
        if (dashCount = 1) then //a-b
          highPart := MyStrToInt(ExtractDelimited(2, part, ['-']))
        else if (dashCount = 2) and (part[1] = '-') then //-a-b
          highPart := MyStrToInt(ExtractDelimited(3, part, ['-']))
        else if dashCount = 3 then //assume -a--b
          highPart := -MyStrToInt(ExtractDelimited(4, part, ['-'])) //read last string
        else if dashCount = 2 then //assume a--b
          highPart := -MyStrToInt(ExtractDelimited(3, part, ['-']))
        else
        begin
          RaiseException('Error in range.');
          SetLength(result, 0);
          exit;
        end;
      
        for j := 0 to abs(highPart - lowPart) do
        begin
          //lowPart + j * (-1 or +1)
          _AddToResult(lowPart + (j * sign(highPart - lowPart)));
        end;
      end;
    
      i := i + 1;
    end;
  end;
  
  function ProcessFloatRange(const valueIn: String): SingleArray;
  var
    i, count : Longint;
    temp: Extended;
    part, value: String;
  
    procedure _AddToResult(val: Single);
    begin
      SetLength(result, Length(result) + 1);
      result[High(result)] := val;
    end;
  begin
    value := Trim(valueIn);
    SetLength(result, 0);
  
    if (value[1] <> '[') or (value[Length(value)] <> ']') then
      exit; //not a range

    // Remove the [ and ]
    value := MidStr(value, 2, Length(value) - 2);
  
    i := 0;
    count := CountDelimiter(value, ',');
  
    while i <= count do
    begin
      part := Trim(ExtractDelimited(i + 1, value, [',']));
    
      if TryStrToFloat(part, temp) then
      begin
        //just "123.45" so...
        _AddToResult(temp);
      end;
      
      i := i + 1;
    end;
  end;
  
  
  function ExtractFileAndPath(const fullnameIn: String; out path, filename: String; allowNewFile: Boolean): Boolean;
  var
    fullname: String;
  begin
    result    := false;
    fullname  := ExpandFileName(fullnameIn);
    
    if DirectoryExists(fullname) then
    begin
      // Directory only
      path        := IncludeTrailingPathDelimiter(fullname); // ensure it includes a directory delim at end
      filename    := '';
      result      := true;
    end
    else if FileExists(fullname) then
    begin
      // We have a file...
      filename    := ExtractFileName(fullname);
      path        := IncludeTrailingPathDelimiter(ExtractFilePath(fullname));
      result      := true;
    end
    else
    begin
      // Neither an existing file or directory... try new file in directory
      if not allowNewFile then exit; // not allowed so exit
      
      path      := ExtractFilePath(fullname);
      if not DirectoryExists(path) then exit; // directory does not exist
      
      filename  := ExtractFileName(fullname);
      result := true;
    end;
    
    // WriteLn(path, filename);
  end;
  
  procedure MakeFalse(var visited: Array of Boolean);
  var
    i: Longint;
  begin
    for i := 0 to High(visited) do
    begin
      visited[i] := false;
    end;
  end;
  
  procedure ProcessLinesInFile(const filename: String; kind: ResourceKind; proc: LineProcessor; ptr: Pointer);
  var
    path: String;
    input: Text;
    line: LineData;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'ProcessLinesInFile');
    {$ENDIF}
    
    if not assigned(proc) then exit;
    
    path := FilenameToResource(filename, kind);
    line.lineNo := 0;
    
    if not FileExists(path) then begin RaiseWarning('Unable to load bundle.'); exit; end;
    
    Assign(input, path);
    Reset(input);
    
    line.filename := filename;
    
    try
      try
        while not EOF(input) do
        begin
          line.lineNo := line.lineNo + 1;
          
          ReadLn(input, line.data);
          if Length(line.data) = 0 then continue;  //skip empty lines
          line.data := Trim(line.data);
          if MidStr(line.data,1,2) = '//' then continue; //skip lines starting with //
          proc(line, ptr);
        end;
      except on e: Exception do
        RaiseException('Error processing ' + filename + ' on line ' + IntToStr(line.lineNo) + ': ' + e.Message);
      end;
    finally
      Close(input);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'ProcessLinesInFile');
    {$ENDIF}
  end;
    
  function CheckAssigned(const msg : String; ptr : Pointer): Boolean;
  begin
    result := true;
    
    if not Assigned(ptr) then 
    begin 
      RaiseWarning(msg);
      result := false;
      exit; 
    end;
  end;
end.
