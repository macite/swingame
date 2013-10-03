//=============================================================================
// sgTimers.pas
//=============================================================================
//
// The Timer unit contains the code used to control time based activity for
// games using SwinGame.
//
// Change History:
//
// Version 3:
// - 2009-11-09: Andrew : Started Timers unit
//
//=============================================================================



/// SwinGame Timers can be used to manage time based actions in your game. Each
/// timer keeps track of the number of milliseconds that have ellapsed since
/// the timer was started. They can be paused, stopped, restarted, etc.
///
/// @module Timers
/// @static
unit sgTimers;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  
  
//----------------------------------------------------------------------------
// Timers
//----------------------------------------------------------------------------
  
  /// Create and return a new Timer. The timer will not be started, and will have
  /// an initial 'ticks' of 0.
  ///
  /// @lib
  ///
  /// @class Timer
  /// @constructor
  /// @sn init
  function CreateTimer(): Timer; overload;
  
  /// Create and return a new Timer. The timer will not be started, and will have
  /// an initial 'ticks' of 0.
  ///
  /// @lib CreateTimerNamed
  ///
  /// @class Timer
  /// @constructor
  /// @sn initWithName:%s
  function CreateTimer(name: String): Timer; overload;
  
  /// Free a created timer.
  ///
  /// @lib
  ///
  /// @class Timer
  /// @dispose
  procedure FreeTimer(var toFree: Timer);
  
  /// Get the timer created with the indicated named.
  ///
  /// @lib
  function TimerNamed(name: String): Timer;
  
  /// Release the resources used by the timer with
  /// the indicated name.
  ///
  /// @lib
  procedure ReleaseTimer(name: String);
  
  /// Releases all of the timers that have been loaded.
  ///
  /// @lib
  procedure ReleaseAllTimers();
  
  
  /// Start a timer recording the time that has passed.
  ///
  /// @lib
  /// 
  /// @class Timer
  /// @method Start
  procedure StartTimer(toStart: Timer);
  
  /// Stop the timer. The time is reset to 0 and you must
  /// recall start to begin the timer ticking again.
  /// 
  /// @lib
  /// 
  /// @class Timer
  /// @method Stop
  procedure StopTimer(toStop: Timer);
  
  /// Pause the timer, getting ticks from a paused timer
  /// will continue to return the same time.
  /// 
  /// @lib
  /// 
  /// @class Timer
  /// @method Pause
  procedure PauseTimer(toPause: Timer);
  
  /// Resumes a paused timer.
  ///
  /// @lib
  ///
  /// @class Timer
  /// @method Resume
  procedure ResumeTimer(toUnpause: Timer);
  
  /// Resets the time of a given timer
  ///
  /// @lib
  ///
  /// @class Timer
  /// @method Reset
  procedure ResetTimer(tmr: Timer);
  
  /// Gets the number of ticks (milliseconds) that have passed since the timer
  /// was started/reset. When paused the timer's ticks will not advance until
  /// the timer is once again resumed.
  ///
  /// @lib
  ///
  /// @class Timer
  /// @getter Ticks
  function TimerTicks(toGet: Timer): Longword;
  
  
  
//=============================================================================
implementation
  uses  sgTrace, sgShared, sgDriverTimer,
        SysUtils,
        stringhash;         // libsrc;
//=============================================================================

var
  _Timers: TStringHash;


function CreateTimer(): Timer; overload;
var
  name: String;
  idx: Integer;
begin
  name := 'Timer';
  idx := 0;
  
  while _Timers.containsKey(name) do
  begin
    name := 'Timer_' + IntToStr(idx);
    idx := idx + 1;
  end;
  
  result := CreateTimer(name);
end;

function CreateTimer(name: String): Timer; overload;
var
  obj: tResourceContainer;
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'CreateTimer', name);
  {$ENDIF}
  
  New(result);
  result^.name := name;
  
  with result^ do
  begin
    startTicks := 0;
    pausedTicks := 0;
    paused := false;
    started := false;
  end;
  
  obj := tResourceContainer.Create(result);
  
  if not _Timers.setValue(name, obj) then
  begin
    RaiseException('Error: Failed to assign timer ' + name);
    Dispose(result);
    result := nil;
    exit;
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'CreateTimer');
  {$ENDIF}
end;

procedure ResetTimer(tmr: Timer);
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ResetTimer');
  {$ENDIF}
  if Assigned(tmr) then tmr^.startTicks := TimerDriver.GetTicks();
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ResetTimer');
  {$ENDIF}
end;

procedure ReleaseTimer(name: String);
var
  tmr: Timer;
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ReleaseTimer', 'tmr = ' + name);
  {$ENDIF}
  
  tmr := TimerNamed(name);
  if assigned(tmr) then
  begin
    _Timers.remove(name).Free();
    
    if Assigned(tmr) then
    begin
      Dispose(tmr);
      CallFreeNotifier(tmr);
    end;
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'ReleaseTimer');
  {$ENDIF}
end;

procedure ReleaseAllTimers();
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ReleaseAllTimers', '');
  {$ENDIF}
  
  ReleaseAll(_Timers, @ReleaseTimer);
  
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'ReleaseAllTimers');
  {$ENDIF}
end;



procedure FreeTimer(var toFree: Timer);
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'FreeTimer');
  {$ENDIF}
  
  if Assigned(toFree) then
  begin
    ReleaseTimer(toFree^.name);
  end;
  
  toFree := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'FreeTimer');
  {$ENDIF}
end;

function TimerNamed(name: String): Timer;
var
  tmp : TObject;
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'TimerNamed', name);
  {$ENDIF}
  
  tmp := _Timers.values[name];
  if assigned(tmp) then result := Timer(tResourceContainer(tmp).Resource)
  else result := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'TimerNamed', HexStr(result));
  {$ENDIF}
end;


procedure StartTimer(toStart: Timer);
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'StartTimer');
  {$ENDIF}
  if not Assigned(toStart) then begin RaiseException('No timer supplied'); exit; end;
  
  with toStart^ do
  begin
    started := true;
    paused := false;
    startTicks := TimerDriver.GetTicks();
  end;
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'StartTimer');
  {$ENDIF}
end;

procedure StopTimer(toStop: Timer);
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'StopTimer');
  {$ENDIF}
  if not Assigned(toStop) then begin RaiseException('No timer supplied'); exit; end;
  with toStop^ do
  begin
    started := false;
    paused := false;
  end;
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'StopTimer');
  {$ENDIF}
end;

procedure PauseTimer(toPause: Timer);
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'PauseTimer');
  {$ENDIF}
  if not Assigned(toPause) then begin RaiseException('No timer supplied'); exit; end;
  with toPause^ do
  begin
    if started and (not paused) then
    begin
      paused := true;
      pausedTicks := Longword(TimerDriver.GetTicks() - startTicks);
    end;
  end;
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'PauseTimer');
  {$ENDIF}
end;

procedure ResumeTimer(toUnpause: Timer);
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ResumeTimer');
  {$ENDIF}
  if not Assigned(toUnpause) then begin RaiseException('No timer supplied'); exit; end;
  with toUnpause^ do
  begin
    if paused then
    begin
      paused := false;
      startTicks := Longword(TimerDriver.GetTicks() - pausedTicks);
      pausedTicks := 0;
    end;
  end;
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'ResumeTimer');
  {$ENDIF}
end;

function TimerTicks(toGet: Timer): Longword;
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'TimerTicks');
  {$ENDIF}
  
  if not Assigned(toGet) then begin RaiseException('No timer supplied'); exit; end;
  
  with toGet^ do
  begin
    if started then
    begin
      if paused then result := pausedTicks
      else result := Longword(TimerDriver.GetTicks() - startTicks);
      exit;
    end;
  end;
  result := 0;
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'TimerTicks');
  {$ENDIF}
end;

//=============================================================================

  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgTimers', 'Initialise', '');
    {$ENDIF}
    
    InitialiseSwinGame();
    _Timers := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
      TraceExit('sgTimers', 'Initialise');
    {$ENDIF}
  end;
  
  finalization
  begin
    ReleaseAllTimers();
    FreeAndNil(_Timers);
  end;
  

end.
