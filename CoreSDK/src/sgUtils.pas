//=============================================================================
// sgUtils.pas
//=============================================================================
//
// The Utils unit contains general utility routins that may be useful for games.
//
// Change History:
//
// Version 3:
// - 2010-12-15: Andrew : Created to take general functionality from Core
//=============================================================================



/// SwinGame's Utils contain a number of helper routines that can be useful in
/// general game development.
///
/// @module Utils
/// @static
unit sgUtils;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================

//----------------------------------------------------------------------------
// Library Version
//----------------------------------------------------------------------------

  /// Retrieves a string representing the version of SwinGame that is executing.
  /// This can be used to check that the version supports the features required
  /// for your game.
  ///
  /// @lib
  function SwinGameVersion(): String;
  
  
  
//----------------------------------------------------------------------------
// Exception Notification/Message
//----------------------------------------------------------------------------
  
  /// This function can be used to retrieve a message containing the details of 
  /// the last error that occurred in SwinGame.
  ///
  /// @lib
  function ExceptionMessage(): String;
  
  /// This function tells you if an error occurred with the last operation in
  /// SwinGame.
  ///
  /// @lib
  function ExceptionOccured(): Boolean;
  
  
  
//----------------------------------------------------------------------------
// Random
//----------------------------------------------------------------------------
  
  /// Generates a random number between 0 and 1.
  ///
  /// @lib
  function Rnd() : Single; overload;
  
  /// Generates a random integer up to (but not including) ubound. Effectively,
  /// the ubound value specifies the number of random values to create.
  ///
  /// @lib RndUpto
  function Rnd(ubound: Longint): Longint; overload;
  
  
  
//----------------------------------------------------------------------------
// Delay / Framerate
//----------------------------------------------------------------------------
  
  /// Returns the average framerate for the last 10 frames as an integer.
  ///
  /// @returns     The current average framerate
  ///
  /// @lib
  function GetFramerate(): Longint;
  
  /// Gets the number of milliseconds that have passed. This can be used to
  /// determine timing operations, such as updating the game elements.
  ///
  /// @returns     The number of milliseconds passed
  ///
  /// @lib
  function GetTicks(): Longword;
  
  /// Puts the process to sleep for a specified number of
  /// milliseconds. This can be used to add delays into your
  /// game.
  ///
  /// @param time - The number of milliseconds to sleep
  ///
  /// Side Effects
  /// - Delay before returning
  ///
  /// @lib
  procedure Delay(time: Longword);
  
  /// Returns the calculated framerate averages, highest, and lowest values along with
  /// the suggested rendering color.
  ///
  /// @lib
  /// @sn calculateFramerateAvg:%s high:%s low:%s color:%s
  procedure CalculateFramerate(out average, highest, lowest: String; out textColor: Color);
  
  
  
//=============================================================================
implementation
  uses 
    SysUtils, Math, Classes, //System
    sgSavePNG, sgShared, sgTrace, sgInputBackend, //SwinGame shared library code
    sgResources, sgGeometry, sgImages, sgGraphics, sgDriverTimer, sgInput; //SwinGame
//=============================================================================


  type
    // Details required for the Frames per second calculations.
    FPSData = record
      values: Array [0..59] of Single;
      pos: Longint;
      max, min, avg: Single;
      ready: Boolean;
    end;
  
  var
    _fpsData: FPSData;

  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------

  //Used to initialise the Frame Per Second data structure.
  procedure _InitFPSData();
  var
    i: Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', '_InitFPSData');
    {$ENDIF}

    // clear the array of values
    for i := Low(_fpsData.values) to High(_fpsData.values) do
      _fpsData.values[i] := 0;
    // zero the current insert position, and the loop count
    _fpsData.pos := 0;
    //_fpsData.loops := 0;
    // set the moving range and average to sensitble defaults
    _fpsData.max := 0;
    _fpsData.min := 0;
    _fpsData.avg := 0;
    _fpsData.ready := false;
    {$IFDEF TRACE}
      TraceExit('sgUtils', '_InitFPSData');
    {$ENDIF}
  end;

//----------------------------------------------------------------------------
// Library Version
//----------------------------------------------------------------------------

  function SwinGameVersion(): String;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'SwinGameVersion');
    {$ENDIF}
    result := DLL_VERSION;
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'SwinGameVersion');
    {$ENDIF}
  end;

//----------------------------------------------------------------------------
// Exception Notification/Message
//----------------------------------------------------------------------------

  function ExceptionMessage(): String;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'ExceptionMessage');
    {$ENDIF}
    result := ErrorMessage;
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'ExceptionMessage', result);
    {$ENDIF}
  end;

  function ExceptionOccured(): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'ExceptionOccured');
    {$ENDIF}
    result := HasException;
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'ExceptionOccured : ' + BoolToStr(result, true));
    {$ENDIF}
  end;
  
//----------------------------------------------------------------------------
// Delay / Framerate
//----------------------------------------------------------------------------

  procedure Delay(time: Longword);
  var
    t: Longword;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'Delay');
    {$ENDIF}

    if time > 0 then
    begin
      if time < 50 then TimerDriver.Delay(time)
      else 
      begin
        for t := 1 to time div 50 do 
        begin
          ProcessEvents();
          if WindowCloseRequested() then exit;
          TimerDriver.Delay(50);
        end;
        t := time mod 50;
        if t > 0 then TimerDriver.Delay(t);
      end;
    end;

    {$IFDEF TRACE}
      TraceExit('sgUtils', 'Delay');
    {$ENDIF}
  end;

  function GetTicks(): Longword;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'GetTicks');
    {$ENDIF}
    result := TimerDriver.GetTicks();
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'GetTicks');
    {$ENDIF}
  end;

  function GetFramerate(): Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'GetFramerate');
    {$ENDIF}
    if _fpsData.avg = 0 then
      result := 60
    else
      result := RoundInt(1000 / _fpsData.avg);
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'GetFramerate');
    {$ENDIF}
  end;

  procedure CalculateFramerate(out average, highest, lowest: String; out textColor: Color);
  var
    avg, hi, lo: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'CalculateFramerate');
    {$ENDIF}
    if not _fpsData.ready then
    begin
      textColor := ColorBlue;
      average :='??.?';
      highest :='??.?';
      lowest  :='??.?';
      {$IFDEF TRACE}
        TraceExit('sgUtils', 'CalculateFramerate');
      {$ENDIF}
      exit;
    end;
    
    if _fpsData.avg = 0 then
      avg := 9999
    else
      avg := (1000 / _fpsData.avg);

    lo := (1000 / _fpsData.max);
    hi := (1000 / _fpsData.min);

    Str(avg:4:1, average);
    Str(hi:4:1, highest);
    Str(lo:4:1, lowest);

    if avg < 10 then
      textColor := ColorRed
    else if avg < 50 then
      textColor := ColorYellow
    else
      textColor := ColorGreen;
    
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'CalculateFramerate');
    {$ENDIF}
  end;


  procedure _UpdateFPSDataProc(delta: Longword);
    function RunningAverage(var values: Array of Single; newValue: Longword; var pos: Longint): Single;
    var
      i: Longint;
      sum: Double;
    begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', '_UpdateFPSData');
    {$ENDIF}
      // insert the newValue as the position specified
      values[pos] := newValue;
      
      // calculate the sum for the average
      sum := 0;
      for i := Low(values) to High(values) do
        sum := sum + values[i];
      result := Single(sum / Length(values));
      
      //Inc position index, and wrap-around to start if needed
      pos := pos + 1;
      if pos > High(values) then
      begin
        pos := Low(values);
        if not _fpsData.ready then
        begin
          _fpsData.max := _fpsData.avg;
          _fpsData.min := _fpsData.avg;
          _fpsData.ready := True;
        end;
      end;
    end;
  begin
    _fpsData.avg := RunningAverage(_fpsData.values, delta, _fpsData.pos);
    
    if _fpsData.avg = 0.0 then _fpsData.avg := 0.01;
    
    //Adjust the min/maxes
    if _fpsData.ready then
    begin
      if _fpsData.avg > _fpsData.max then _fpsData.max := _fpsData.avg
      else if _fpsData.avg < _fpsData.min then _fpsData.min := _fpsData.avg;
    end;
    {$IFDEF TRACE}
      TraceExit('sgUtils', '_UpdateFPSData');
    {$ENDIF}
  end;

//----------------------------------------------------------------------------
// Random
//----------------------------------------------------------------------------
  
  function Rnd() : Single; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'Rnd');
    {$ENDIF}
    result := Single(System.Random());
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'Rnd');
    {$ENDIF}
  end;
  
  function Rnd(ubound: Longint): Longint; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'Rnd');
    {$ENDIF}
    result := System.Random(ubound);
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'Rnd');
    {$ENDIF}
  end;
  
  
  
//=============================================================================
  
  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgUtils', 'initialization');
    {$ENDIF}
    
    InitialiseSwinGame();
    _InitFPSData();
    _UpdateFPSData := @_UpdateFPSDataProc;
    
    {$IFDEF TRACE}
      TraceExit('sgUtils', 'initialization');
    {$ENDIF}
  end;

end.




