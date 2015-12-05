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
  function CreateTimer(const name: String): Timer; overload;
  
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
  function TimerNamed(const name: String): Timer;
  
  /// Release the resources used by the timer with
  /// the indicated name.
  ///
  /// @lib
  procedure ReleaseTimer(const name: String);
  
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
  
  /// Start a timer recording the time that has passed.
  ///
  /// @lib StartTimerNamed
  procedure StartTimer(const name: String);

  /// Stop the timer. The time is reset to 0 and you must
  /// recall start to begin the timer ticking again.
  /// 
  /// @lib
  /// 
  /// @class Timer
  /// @method Stop
  procedure StopTimer(toStop: Timer);

  /// Stop the timer. The time is reset to 0 and you must
  /// recall start to begin the timer ticking again.
  /// 
  /// @lib StopTimerNamed
  procedure StopTimer(const name: String);

  
  /// Pause the timer, getting ticks from a paused timer
  /// will continue to return the same time.
  /// 
  /// @lib
  /// 
  /// @class Timer
  /// @method Pause
  procedure PauseTimer(toPause: Timer);

  /// Pause the timer, getting ticks from a paused timer
  /// will continue to return the same time.
  /// 
  /// @lib PauseTimerNamed
  procedure PauseTimer(const name: String);
  
  /// Resumes a paused timer.
  ///
  /// @lib
  ///
  /// @class Timer
  /// @method Resume
  procedure ResumeTimer(toUnpause: Timer);

  /// Resumes a paused timer.
  ///
  /// @lib ResumeTimerNamed
  procedure ResumeTimer(const name: String);

  
  /// Resets the time of a given timer
  ///
  /// @lib
  ///
  /// @class Timer
  /// @method Reset
  procedure ResetTimer(tmr: Timer);

  /// Resets the time of a given timer
  ///
  /// @lib ResetTimerNamed
  procedure ResetTimer(const name: String);

  
  /// Gets the number of ticks (milliseconds) that have passed since the timer
  /// was started/reset. When paused the timer's ticks will not advance until
  /// the timer is once again resumed.
  ///
  /// @lib
  ///
  /// @class Timer
  /// @getter Ticks
  function TimerTicks(toGet: Timer): Longword;

  /// Gets the number of ticks (milliseconds) that have passed since the timer
  /// was started/reset. When paused the timer's ticks will not advance until
  /// the timer is once again resumed.
  ///
  /// @lib TimerTicksNamed
  function TimerTicks(const name: String): Longword;
  
  
  
//=============================================================================
implementation
  uses  sgTrace, sgShared, sgDriverTimer,
        SysUtils,
        stringhash, sgBackendTypes;         // libsrc;
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

function CreateTimer(const name: String): Timer; overload;
var
  obj: tResourceContainer;
  t: TimerPtr;
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'CreateTimer', name);
  {$ENDIF}
  
  New(t);
  result := t;
  t^.id := TIMER_PTR;
  t^.name := name;
  
  with t^ do
  begin
    startTicks := 0;
    pausedTicks := 0;
    paused := false;
    started := false;
  end;
  
  obj := tResourceContainer.Create(t);
  
  if not _Timers.setValue(name, obj) then
  begin
    RaiseException('Error: Failed to assign timer ' + name);
    Dispose(t);
    result := nil;
    exit;
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'CreateTimer');
  {$ENDIF}
end;

procedure ResetTimer(const name: String);
begin
  ResetTimer(TimerNamed(name));
end;

procedure ResetTimer(tmr: Timer);
var
  tp: TimerPtr;
begin
  tp := ToTimerPtr(tmr);
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ResetTimer');
  {$ENDIF}
  if Assigned(tp) then
  begin
     tp^.startTicks := TimerDriver.GetTicks();
     tp^.pausedTicks := 0;
  end; 
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ResetTimer');
  {$ENDIF}
end;

procedure ReleaseTimer(const name: String);
var
  tmr: Timer;
  tp: TimerPtr;
begin
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ReleaseTimer', 'tmr = ' + name);
  {$ENDIF}
  
  tmr := TimerNamed(name);

  if assigned(tmr) then
  begin
    _Timers.remove(name).Free();

    tp := ToTimerPtr(tmr);
    
    if Assigned(tp) then
    begin
      CallFreeNotifier(tmr);
      Dispose(tp);
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
var
  tp: TimerPtr;
begin
  tp := ToTimerPtr(toFree);

  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'FreeTimer');
  {$ENDIF}
  
  if Assigned(toFree) then
  begin
    ReleaseTimer(tp^.name);
  end;
  
  toFree := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'FreeTimer');
  {$ENDIF}
end;

function TimerNamed(const name: String): Timer;
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


procedure StartTimer(const name: String);
begin
  StartTimer(TimerNamed(name));
end;

procedure StartTimer(toStart: Timer);
var
  tp: TimerPtr;
begin
  tp := ToTimerPtr(toStart);
  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'StartTimer');
  {$ENDIF}
  if not Assigned(tp) then begin RaiseException('No timer supplied'); exit; end;
  
  with tp^ do
  begin
    started := true;
    paused := false;
    startTicks := TimerDriver.GetTicks();
  end;
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'StartTimer');
  {$ENDIF}
end;

procedure StopTimer(const name: String);
begin
  StopTimer(TimerNamed(name));
end;

procedure StopTimer(toStop: Timer);
var
  tp: TimerPtr;
begin
  tp := ToTimerPtr(toStop);

  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'StopTimer');
  {$ENDIF}
  if not Assigned(tp) then begin RaiseException('No timer supplied'); exit; end;
  with tp^ do
  begin
    started := false;
    paused := false;
  end;
  {$IFDEF TRACE}
    TraceExit('sgTimers', 'StopTimer');
  {$ENDIF}
end;

procedure PauseTimer(const name: String);
begin
  PauseTimer(TimerNamed(name));
end;

procedure PauseTimer(toPause: Timer);
var
  tp: TimerPtr;
begin
  tp := ToTimerPtr(toPause);

  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'PauseTimer');
  {$ENDIF}
  if not Assigned(tp) then begin RaiseException('No timer supplied'); exit; end;
  with tp^ do
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

procedure ResumeTimer(const name: String);
begin
  ResumeTimer(TimerNamed(name));
end;

procedure ResumeTimer(toUnpause: Timer);
var
  tp: TimerPtr;
begin
  tp := ToTimerPtr(toUnpause);

  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'ResumeTimer');
  {$ENDIF}
  if not Assigned(tp) then begin RaiseException('No timer supplied'); exit; end;
  with tp^ do
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

function TimerTicks(const name: String): Longword;
begin
  result := TimerTicks(TimerNamed(name));
end;

function TimerTicks(toGet: Timer): Longword;
var
  tp: TimerPtr;
begin
  tp := ToTimerPtr(toGet);

  {$IFDEF TRACE}
    TraceEnter('sgTimers', 'TimerTicks');
  {$ENDIF}
  
  if not Assigned(tp) then begin RaiseException('No timer supplied'); exit; end;
  
  with tp^ do
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
