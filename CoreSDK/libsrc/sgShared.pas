//=============================================================================
// sgShared.pas
//=============================================================================
//
// The shared data and functions that are private to the SwinGame SDK. This
// unit should not be accessed directly by games.
//
// Change History:
//
// Version 3:
// - 2009-12-21: Andrew : Write exceptions to stderr if not raised.
// - 2009-12-18: Andrew : Added screen rect cache
// - 2009-12-10: Andrew : Added iter free - fixed memory leak
// - 2009-11-11: Andrew : Switched to release rather than drain on the autorelease pool
// - 2009-11-06: Andrew : Added resource management type code
// - 2009-10-16: Andrew : Added the free notifier, so it can be called from many places
// - 2009-07-29: Andrew : Added flag for opened audio.
// - 2009-07-27: Andrew : Added code to cycle auto release pool for Objective C
//                      : Fixed possible double release of AutoRelease pool
// - 2009-07-10: Andrew : Added initialisation code
// - 2009-06-23: Clinton: Comment/format cleanup/tweaks
//                      : Slight optimization to NewSDLRect (redundant code)
//                      : Renamed scr to screen
// - 2009-06-05: Andrew : Created to contain all globals that are hidden from
//                        the library
//=============================================================================

unit sgShared;
  
//=============================================================================
interface
  uses 
    stringhash, sgInputBackend, sgTypes;
//=============================================================================
  
  type
    
    RectPtr = ^Rectangle;
    // The resource contain is an object to hold the resource for the 
    // hash table
    TResourceContainer = class(tObject)
    private
      resource_val : Pointer;
    public
      constructor Create(data: Pointer);
      
      property Resource: Pointer read resource_val;
    end;
        
    // The resource contain is an object to hold the resource for the 
    // hash table
    TIntegerContainer = class(tObject)
    private
      val : Longint;
    public
      constructor Create(data: Longint);

      property Value: Longint read val write val;
    end;

    // Used by release all
    ReleaseFunction = procedure(name: String);
    IntProc = procedure(val: Longword);

  // Immediate if
  function iif(pred: Boolean; trueVal, falseVal: Single): Single; overload;
  
  // Calls Release on all of the resources in the tbl hash table.
  procedure ReleaseAll(tbl: TStringHash; releaser: ReleaseFunction);





  // Rounds `x` up... 1.1 -> 2
  function Ceiling(x: Single): Longint;

  // Used by SwinGame units to register event "handlers" what will be called
  // when SDL events have occured. Events such as mouse movement, button
  // clicking and key changes (up/down). See sgInput.
 // procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  
  // All SwinGame initialisation code must call this before performing any processing...
  procedure InitialiseSwinGame();
  
  procedure RaiseException(message: String);
  procedure RaiseWarning(message: String);
  
  {$IFDEF DARWIN}
      {$IFDEF NO_ARC}
          procedure CyclePool();
      {$ENDIF}
      // procedure BringForeground();
  {$ENDIF}
  
  
  function RoundUByte(val: Double): Byte;
  function RoundInt(val: Double): LongInt;
  function RoundUShort(val: Double): Word;
  function RoundShort(val: Double): Smallint;
  function StrToSingle(val: String): Single;
  function StrToUByte(val: String): Byte;
  
  /// Called when ANY resource is freed to inform other languages to remove from
  /// their caches.
  procedure CallFreeNotifier(p: Pointer);
    
  // Global variables that can be shared.
  var
    // The base path to the program's executable.
    applicationPath: String = '';
    
    // This `Bitmap` wraps the an SDL image (and its double-buffered nature)
    // which is used to contain the current "screen" rendered to the window.
    screen: Bitmap = nil;
    
    // Used for on screen tests...
    screenRect: Rectangle;
    
    // The singleton instance manager used to check events and called
    // registered "handlers". See `RegisterEventProcessor`.
    //sdlManager: TSDLManager = nil;

    // The name of the icon file shown.
    // TODO: Does this work? Full path or just resource/filename?
    iconFile: String = '';

    // Contains the last error message that has occured if `HasException` is
    // true. If multiple error messages have occured, only the last is stored.
    // Used only by the generated library code.
    ErrorMessage: String = '';

    // This flag is set to true if an error message has occured. Used only by
    // the generated library code.
    HasException: Boolean = False;
    
    // This flag indicates if the audio has been opened.
    AudioOpen: Boolean = False;
    
    // The function pointer to call into the other language to tell them something was freed
    _FreeNotifier: FreeNotifier = nil;
    
    // Timing details related to calculating FPS
    _lastUpdateTime: Longword = 0;
    
    // The pointer to the screen's surface
    _screen: Pointer = nil;
    
    _UpdateFPSData: IntProc = nil;
    
    UseExceptions: Boolean = True;
  const
    DLL_VERSION = 'TEST BUILD';
    {$ifndef FPC}
    LineEnding = #13#10; // Delphi Windows \r\n pair
    {$endif}


//=============================================================================
implementation
  uses 
    SysUtils, Math, Classes, StrUtils,
    sgTrace, sgGraphics, 
    sgImages, sgDriver,sgDriverImages;
//=============================================================================
  
  var
    is_initialised: Boolean = False;
//----------------------------------------------------------------------------
// Global variables for Mac OS Autorelease Pool
//----------------------------------------------------------------------------
    {$ifdef DARWIN}
      {$IFDEF NO_ARC}
        NSAutoreleasePool: Pointer;
        pool: Pointer = nil;
      {$ENDIF}
    {$endif}
    
//---------------------------------------------------------------------------
// OS X dylib external link
//---------------------------------------------------------------------------
  
  {$ifdef DARWIN}
    {$linklib libobjc.dylib}
    {$IFNDEF IOS}
        procedure NSApplicationLoad(); cdecl; external 'Cocoa'; {$EXTERNALSYM NSApplicationLoad}  
    {$ENDIF}
    
    function objc_getClass(name: PChar): Pointer; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_getClass}
    function sel_registerName(name: PChar): Pointer; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM sel_registerName}
    function class_respondsToSelector(cls, sel: Pointer): Boolean; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM class_respondsToSelector}
    function objc_msgSend(self, cmd: Pointer): Pointer; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_msgSend}
    // function objc_msgSendSender(self, cmd, sender: Pointer): Pointer; cdecl; external 'libobjc.dylib' name 'objc_msgSend'; {$EXTERNALSYM objc_msgSend}
  {$endif}
  
  constructor TResourceContainer.create(data: Pointer);
  begin
    inherited create;
    resource_val := data;
  end;
  
  constructor TIntegerContainer.create(data: Longint);
  begin
    inherited create;
    val := data;
  end;
  
  // {$ifdef DARWIN}
  // procedure BringForeground();
  // var
  //   NSApplication: Pointer;
  //   app, wnd: Pointer;
  // begin
  //   NSApplication := objc_getClass('NSApplication');
  //   app := objc_msgSend(NSApplication, sel_registerName('sharedApplication'));
  //   WriteLn('App: ', HexStr(app));
  //   objc_msgSendSender(app, sel_registerName('hideOtherApplications:'), nil);
  // end;
  // {$endif}

  procedure InitialiseSwinGame();
  begin
    if is_initialised then exit;
    is_initialised := True;
    {$IFDEF TRACE}
      TraceEnter('sgShared', 'InitialiseSwinGame', '');
    {$ENDIF}
    
    Randomize();
    
    {$ifdef DARWIN}
      {$IFDEF Trace}
        TraceIf(tlInfo, 'sgShared', 'INFO', 'InitialiseSwinGame', 'Loading Mac version');
      {$ENDIF}
      
      //FIX: Error with Mac and FPC 2.2.2
      SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
      
      {$IFNDEF IOS}
        {$IFDEF NO_ARC}
          NSAutoreleasePool := objc_getClass('NSAutoreleasePool');
          pool := objc_msgSend(NSAutoreleasePool, sel_registerName('alloc'));
          pool := objc_msgSend(pool, sel_registerName('init'));
        {$ENDIF}
        NSApplicationLoad();
      {$endif}
    {$endif}
    
    {$IFDEF Trace}
      TraceIf(tlInfo, 'sgShared', 'INFO', 'InitialiseSwinGame', 'About to initialise SDL');
    {$ENDIF}
    
    if not Assigned(Driver.Init) then LoadDefaultDriver();
    Driver.Init();
    
    {$IFDEF TRACE}
      TraceExit('sgShared', 'InitialiseSwinGame');
    {$ENDIF}
  end;
  
  {$ifdef DARWIN}
  {$IFDEF NO_ARC}
  // No longer needed with SDL 1.2 static libraries...?
  //
  procedure CyclePool();
  begin
    if class_respondsToSelector(NSAutoreleasePool, sel_registerName('drain')) then
    begin
      //Drain the pool - releases it
      objc_msgSend(pool, sel_registerName('drain'));
      // WriteLn('Drain');
      //Create a new pool
      pool := objc_msgSend(NSAutoreleasePool, sel_registerName('alloc'));
      pool := objc_msgSend(pool, sel_registerName('init'));
    end;
  end;
  {$ENDIF}
  {$endif}



  function Ceiling(x: Single): Longint;
  begin
    result := Round(x);
    if result < x then result := result + 1;
  end;

  

  
  procedure RaiseException(message: String);
  begin
    HasException := True;
    ErrorMessage := message;
    
    {$IFDEF TRACE}
      TraceIf(tlError, '', 'EXCP', '** Exception Raised **', message);
      TraceExit('', 'Ended with exception');
    {$ENDIF}
    
    if UseExceptions then raise Exception.Create(message)
    else 
    begin
      // Wrap in error handler as exception will be raised on 
      // Windows when program is an app and stderr is "full"
      try
        WriteLn(stderr, message);
      except
      end;
    end; 
  end;

  procedure RaiseWarning(message: String);
  begin
    // Watch for exceptions with this on Windows
    try
      WriteLn(stderr, message);
    except
    end;
    
    {$IFDEF Trace}
      TraceIf(tlWarning, '', 'WARN', '** Warning Raised **', message);
    {$ENDIF}
  end;

  //---------------------------------------------------------------------------

  procedure CallFreeNotifier(p: Pointer);
  begin
    if Assigned(_FreeNotifier) then
    begin
      try
        _FreeNotifier(p);
      except
        ErrorMessage := 'Error calling free notifier';
        HasException := True;
      end;
    end;
  end;
  
  //----------------------------------------------------------------------------
  
  procedure ReleaseAll(tbl: TStringHash; releaser: ReleaseFunction);
  var
    iter: TStrHashIterator;
    names: array of String;
    i: Integer;
  begin
    if tbl.count = 0 then exit;
    
    SetLength(names, tbl.count);
    iter := tbl.getIterator();
    i := 0;
    
    while i < Length(names) do
    begin
      names[i] := iter.key;
      i := i + 1;
      iter.next;
    end;

    for i := Low(names) to High(names) do
    begin
      releaser(names[i]);
    end;
    
    iter.Free();
    tbl.deleteAll();
  end;

function iif(pred: Boolean; trueVal, falseVal: Single): Single; overload;
begin
  if pred then result := trueVal
  else result := falseVal;
end;

function RoundUByte(val: Double): Byte;
begin
  result := Byte(Round(val));
end;

function RoundInt(val: Double): LongInt;
begin
  result := LongInt(Round(val));
end;

function RoundUShort(val: Double): Word;
begin
  result := Word(Round(val));
end;

function RoundShort(val: Double): SmallInt;
begin
  result := SmallInt(Round(val));
end;

function StrToSingle(val: String): Single;
begin
  result := Single(StrToFloat(val));
end;

function StrToUByte(val: String): Byte;
begin
  result := Byte(StrToInt(val));
end;

//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
  end;

//=============================================================================

  finalization
  begin
    {$ifdef DARWIN}
        {$IFDEF NO_ARC}
            if not assigned(pool) then
            begin
                pool := objc_msgSend(NSAutoreleasePool, sel_registerName('alloc'));
                pool := objc_msgSend(pool, sel_registerName('init'));
            end;
        {$ENDIF}
    {$endif}
    
    if screen <> nil then
    begin
        ImagesDriver.FreeSurface(screen);
        Dispose(screen);
        screen := nil;
    end;
    
    Driver.Quit();
    
    {$ifdef DARWIN}
        {$IFDEF NO_ARC}
            // last pool will self drain...
            if assigned(pool) then
            begin
                objc_msgSend(pool, sel_registerName('drain'));
            end;
            pool := nil;
            NSAutoreleasePool := nil;
        {$ENDIF}
    {$endif}
  end;

end.