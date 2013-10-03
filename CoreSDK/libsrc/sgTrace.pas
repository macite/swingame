//=============================================================================
// sgTrace.pas
//=============================================================================
//
// Support "tracing" and writing of messages to a trace file (Trace.log).
// "Trace" must be defined before these will be included (see sgTrace.inc).
//
// Change History:
//
// Version 3:
// - 2009-11-11: Andrew : Added code to cycle log files
//                      : Added code to make enter and exit verbose
// - 2009-11-06: Andrew : Fixed formatting
// - 2009-09-11: Andrew : Fixed io exceptions
// - 2009-06-23: Clinton: Comment formatting/cleanup
//
// Version 2:
// - 2008-12-17: Andrew : Moved all integers to Longint
//
// Version 1.1.6:
// - 2008-05-09: Andrew : Introduced unit
//=============================================================================

unit sgTrace;

//=============================================================================
interface
//=============================================================================

  {$IFDEF Trace}
  type
      TraceLevel = (tlNone, tlError, tlWarning, tlInfo, tlVerbose);

    procedure Trace(unitname, action, routine, message: String);
    procedure TraceIf(tl: TraceLevel; unitname, action, routine, message: String);
    procedure TraceEnter(unitName, routine: String); overload;
    procedure TraceEnter(unitName, routine, message: String); overload;
    procedure TraceExit(unitName, routine: String); overload;
    procedure TraceExit(unitName, routine, message: String); overload;
  {$ENDIF}

//=============================================================================
implementation
uses 
  Math,
  SysUtils,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  StrUtils, StringHash;
//=============================================================================


  {$IFDEF Trace}
  {$Info SwinGame Tracing Enabled}
  
  // These vars are read in from the config file
  var 
    MAX_LINES: Longint; //default 10000
    MAX_LOGS: Longint;  //default 10
    TRACE_UNITS: TStringHash;
    TRACE_LEVEL: TraceLevel;
  
  var 
    indentLevel: Longint;
    lineCount: Longint;
    traceLog: Longint;
    output: Text;
  
  procedure ConfigureTrace();
  const
    MAX_LINES_DEFAULT = 10000;
    MAX_LOGS_DEFAULT = 10;
  var
    input: Text;
    line: String;
    idx: Integer;
    inUnits: Boolean;
    
    function ReadInteger(start, default: Longint): Longint;
    var
      subStr: String;
    begin
      subStr := ExtractSubStr(line, start, []);
      result := default;
      TryStrToInt(subStr, result);
    end;
    
  begin
    traceLog := 0;
    lineCount := 0;
    indentLevel := 0;
    TRACE_LEVEL := tlNone;
    
    MAX_LINES := MAX_LINES_DEFAULT;
    MAX_LOGS := MAX_LOGS_DEFAULT;
    
    if FileExists('Trace.cfg') then
    begin
      inUnits := false;
      
      try
        Assign(input, 'Trace.cfg');
        Reset(input);
        
        while not EOF(input) do
        begin
          ReadLn(input, line);
          
          if FindPart('#', line) = 1 then continue;
          
          idx := FindPart('maxlines=', line);
          if idx = 1 then
          begin
            MAX_LINES := ReadInteger(10, MAX_LINES_DEFAULT);
            continue;
          end;
          
          idx := FindPart('maxlogs=', line);
          if idx = 1 then
          begin
            MAX_LOGS := ReadInteger(9, MAX_LOGS_DEFAULT);
            continue;
          end;
          
          idx := FindPart('tracelevel=', line);
          if idx = 1 then
          begin
            TRACE_LEVEL := TraceLevel(ReadInteger(12, 0));
            continue;
          end;
          
          if FindPart('units=', line) = 1 then
          begin
            inUnits := True;
            continue;
          end;
          
          if inUnits then
          begin
            TRACE_UNITS.SetValue(line, nil);
          end;
        end;
        
        Close(input);
      except
      end;
    end
    else
    begin
      //Trace SGSDK.dll by default
      TRACE_UNITS.SetValue('SGSDK.dll', nil);
    end;
  end;
  
  procedure AdvanceTrace();
  var
    newTrace: String;
  begin
    if (lineCount > MAX_LINES) and (indentLevel = 0) then
    begin
      traceLog += 1;
      lineCount := 0;
      newTrace := 'Trace ' + IntToStr(traceLog) + '.log';
      WriteLn(output, 'Trace continues in ', newTrace);
      
      if traceLog > MAX_LOGS then
      begin
        //Delete a log file
        try
          DeleteFile('Trace ' + IntToStr(traceLog - MAX_LOGS) + '.log');
        except
          WriteLn('Failed to delete log file ', 'Trace ' + IntToStr(traceLog - MAX_LOGS) + '.log')
        end;
      end;
      
      Close(output);
      try
        {$IFDEF UNIX}
        fpChmod (newTrace, S_IWUSR or S_IRUSR or S_IWGRP or S_IRGRP or S_IWOTH or S_IROTH);
        {$ENDIF}
        Assign(output, newTrace);
        Rewrite(output);
        WriteLn(output, 'Trace continued...');
      except
        WriteLn('ERROR: Unable to write to trace file. Please make Trace.log writable by this program.')
      end;
    end;
  end;

  procedure Trace(unitname, action, routine, message: String);
  begin
    try
      if (Length(unitname) > 0) and not TRACE_UNITS.containsKey(unitname) then exit;
      
      lineCount += 1;
      WriteLn(output, unitname, ': ':(15 - Length(unitname)), action, ': ':(8 - Length(action)), StringOfChar(' ', indentLevel * 2), routine, ': ', message);
      Flush(output);
      AdvanceTrace();
    except
    end;
  end;
  
  procedure TraceIf(tl: TraceLevel; unitname, action, routine, message: String);
  begin
    if TRACE_LEVEL >= tl then
      Trace(unitname, action, routine, message);
  end;
  
  procedure TraceEnter(unitName, routine: String); overload;
  begin
    TraceEnter(unitName, routine, '');
  end;

  procedure TraceEnter(unitName, routine, message: String); overload;
  begin
    Traceif(tlVerbose, unitName, 'Enter', routine, message);
    indentLevel := indentLevel + 1;
  end;

  procedure TraceExit(unitName, routine: String); overload;
  begin
    TraceExit(unitName, routine, '');
  end;
  
  procedure TraceExit(unitName, routine, message: String); overload;
  begin
    indentLevel := indentLevel - 1;
    if indentLevel = 0 then
      TraceIf(tlVerbose, unitName, 'Exit', routine, message + Char(10) + StringOfChar('-', 50))
    else
      TraceIf(tlVerbose, unitName, 'Exit', routine, message);
  end;

  //=============================================================================
  
  initialization
  begin
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
    TRACE_UNITS := TStringHash.Create(False, 32);
    ConfigureTrace();
    
    try
      {$IFDEF UNIX}
      fpChmod ('Trace.log',S_IWUSR or S_IRUSR or S_IWGRP or S_IRGRP or S_IWOTH or S_IROTH);
      {$ENDIF}
      Assign(output, 'Trace.log');
      Rewrite(output);
      Trace('sgTrace', 'INFO', 'initialization', 'Tracing started in SwinGame.');
    except
      WriteLn('ERROR: Unable to write to trace file. Please make Trace.log writable by this program.')
    end;
  end;

//=============================================================================

  finalization
  begin
    try
      FreeAndNil(TRACE_UNITS);
      Close(output);
    except
    end;
  end;


  {$ENDIF}
end.
