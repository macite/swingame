//=============================================================================
// sgInput.pas
//=============================================================================
//
// Responsible for input event processing for mouse visibility, movement and
// button clicks (including the scroll wheel as button clicks) and keyboard
// events for text input and key state checking.
//
// Change History:
//
// Version 3.0:
// - 2012-01-12: Aaron : changed input to use InputDrivers.
// - 2010-02-02: Andrew : Added starting text reading within a region
// - 2009-07-24: Andrew : Renamed mouse code
// - 2009-07-10: Andrew : Added call to initialise SwinGame.
//                      : Fixed missing const modifier on struct parameters
// - 2009-06-15: Clinton: renamed+removed Is/Was and placed Key/Mouse first
//                        moved and added meta comments, tweaked formatting.
// - 2009-06-05: Andrew : Using sgShared
//
// Version 2.2.2:
// - 2008-12-17: Andrew : Moved all integers to Longint
// - 2008-12-16: Andrew : Added WasAKeyPressed
//
// Version 1.1.5:
// - 2008-04-18: Andrew : Added EndTextRead
//
// Version 1.1:
// - 2008-02-13: James  : changed MoveMouse so it dosnt generate mouse movement event
// - 2008-01-25: Stephen: Fixed IsMouseShown
// - 2008-01-25: Andrew : Fixed compiler hints
// - 2008-01-22: James  : changed MoveMouse to Point2D
// - 2008-01-17: Aki + Andrew: Refactor
//
// Version 1.0:
// - Various
//=============================================================================



/// Responsible for input event processing for mouse and keyboard events. This
/// includes mouse visibility, mouse movement and button clicks (including the 
/// scroll wheel as button "click" events) and keyboard events for text input 
/// and key state checking.
///
/// @module Input
/// @static
unit sgInput;

//=============================================================================
interface
//=============================================================================

  uses sgTypes;
  
//----------------------------------------------------------------------------
// Window close and Processing events
//----------------------------------------------------------------------------
  
  /// Checks to see if the window has been asked to close. You need to handle
  /// this if you want the game to end when the window is closed. This value
  /// is updated by the `ProcessEvents` routine.
  ///
  /// @returns: True if the window has been requested to close.
  ///
  /// @lib WindowCloseRequested
  function WindowCloseRequested(): Boolean;
  
  /// ProcessEvents allows the SwinGame API to react to user interactions. This
  /// routine checks the current keyboard and mouse states. This routine must
  /// be called frequently within your game loop to enable user interaction.
  ///
  /// Side Effects
  /// - Reads user interaction events
  /// - Updates keys down, text input, etc.
  ///
  /// @lib ProcessEvents
  procedure ProcessEvents();
  
  
  
//----------------------------------------------------------------------------
// Mouse position
//----------------------------------------------------------------------------
  
  /// Returns The current window position of the mouse as a `Vector`
  ///
  /// @lib
  function MousePositionAsVector(): Vector;

  /// Returns the current window position of the mouse as a `Point2D`
  ///
  /// @lib
  function MousePosition(): Point2D;
  
  /// Returns the current x value of the mouse's position.
  ///
  /// @lib
  function MouseX(): Single;
  
  /// Returns the current y value of the mouse's position.
  ///
  /// @lib
  function MouseY(): Single;
  
  /// Returns the amount of accumulated mouse movement, since the last time 
  /// `ProcessEvents` was called, as a `Vector`. 
  /// 
  /// @lib
  function MouseMovement(): Vector;
  
  /// Returns ``true`` if the specified button is currently pressed down.
  /// 
  /// @lib
  function MouseDown(button: MouseButton): Boolean;
  
  /// Returns ``true`` if the specified button is currently up.
  /// 
  /// @lib
  function MouseUp(button: MouseButton): Boolean;
  
  /// Returns true if the specified button was clicked since the last time
  /// `ProcessEvents` was called
  /// 
  /// @lib
  function MouseClicked(button: MouseButton): Boolean;
  
  /// Moves the mouse cursor to the specified screen location.
  /// 
  /// @lib
  /// @sn moveMouseToX:%s y:%s
  procedure MoveMouse(x, y : Byte); overload;
    
  /// Moves the mouse cursor to the specified screen location.
  /// 
  /// @lib MoveMouseToPoint
  procedure MoveMouse(const point: Point2D);overload;
  
  /// Tells the mouse cursor to be visible if it was previously hidden with 
  /// by a `HideMouse` or `SetMouseVisible`(False) call.
  ///
  /// @lib
  procedure ShowMouse(); overload;
    
  /// Used to explicitly set the mouse cursors visible state (if it is showing
  /// in the window or not) based on the show parameter.
  ///
  /// @lib SetMouseVisible
  /// @sn showMouse:%s
  procedure ShowMouse(show : Boolean); overload;
    
  /// Tells the mouse cursor to hide (no longer visible) if it is currently 
  /// showing. Use `ShowMouse` to make the mouse cursor visible again.
  ///
  /// @lib
  procedure HideMouse();
  
  /// Returns ``true`` if the mouse is currently visible, ``false`` if not.
  /// 
  /// @lib
  function MouseShown(): Boolean;
  
  /// Start reading text within an area. Entry is 
  /// completed when the user presses ENTER, and aborted with ESCAPE.
  /// If the user aborts entry the result is an empty string, and TextEntryCancelled 
  /// will return true. Text entry is updated during `ProcessEvents`, and text is drawn 
  /// to the screen as part of the `RefreshScreen` call.
  ///
  /// @lib StartReadingTextWithinArea
  /// @sn startReadingTextColor:%s maxLen:%s font:%s area:%s
  procedure StartReadingText(textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
    

  
  /// The same as `StartReadingText` but with an additional ``text`` parameter
  /// that is displayed as default text to the user.  
  ///
  /// @lib StartReadingTextWithTextInArea
  /// @sn startReadingTextWith:%s color:%s maxLen:%s font:%s area:%s
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
    
  
  /// The same as `StartReadingTextWithText` but with ``text`` and ``bgColor`` parameter
  /// that is displayed as default text to the user.  
  ///
  /// @lib StartReadingTextWithTextAndColorInArea
  /// @sn startReadingTextWith:%s color:%s bgColor:%s maxLen:%s font:%s area:%s  
  procedure StartReadingTextWithText(text: String; textColor, backGroundColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  
  /// Starts the reading of a string of characters from the user. Entry is 
  /// completed when the user presses ENTER, and aborted with ESCAPE.
  /// If the user aborts entry the result is an empty string, and TextEntryCancelled will return true. 
  /// Text entry is updated during `ProcessEvents`, and text is drawn to the screen as part 
  /// of the `RefreshScreen` call.
  ///
  /// @lib
  /// @sn startReadingTextColor:%s maxLen:%s font:%s x:%s y:%s
  procedure StartReadingText(textColor: Color; maxLength: Longint; theFont: Font; x, y: Longint); overload;
  
  /// The same as `StartReadingText` but with an additional ``text`` parameter
  /// that is displayed as default text to the user.  
  ///
  /// @lib StartReadingTextWithText
  /// @sn startReadingTextWith:%s color:%s maxLen:%s font:%s x:%s y:%s
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; x, y: Longint); overload;
  
  /// The same as `StartReadingText` but with an additional ``text`` parameter
  /// that is displayed as default text to the user.  
  ///
  /// @lib StartReadingTextWithTextAtPt
  /// @sn startReadingTextWith:%s color:%s maxLen:%s font:%s at:%s
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; const pt: Point2D); overload;
  
  /// Returns the string that has been read since `StartReadingText` or 
  /// `StartReadingTextWithText` was called.
  ///
  /// @lib
  function EndReadingText(): String;
  
  /// ReadingText indicates if the API is currently reading text from the
  /// user. Calling StartReadingText will set this to true, and it becomes
  /// false when the user presses enter or escape. At this point you can
  /// read the string entered as either ASCII or Unicode.
  ///
  /// @lib
  function ReadingText(): Boolean;
  
  /// Returns true if the text entry started with `StartReadingText` was cancelled.
  ///
  /// @lib
  function TextEntryCancelled(): Boolean;
  
  /// TextReadAsASCII allows you to read the value of the string entered by the
  /// user as ASCII. See TextReasAsUNICODE, StartReadingText and ReadingText
  /// for more details.
  ///
  /// @lib
  function TextReadAsASCII(): String;
  
  /// Returns true when the key requested is being held down. This is updated
  /// as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  /// to specify the key to be checked.
  ///
  /// @lib
  function KeyDown(key: KeyCode): Boolean;
  
  /// Returns true when the key requested is just pressed down. This is updated
  /// as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  /// to specify the key to be checked. this will only occur once for that key that is
  /// pressed and will not return true again until the key is released and presssed down again
  ///
  /// @lib
  function KeyTyped(key: KeyCode): Boolean;
  
  /// Returns true if the specified key was released since the last time 
  /// `ProcessEvents` was called. This occurs only once for the key that is 
  /// released and will not return true again until the key is pressed down and
  /// released again.
  ///
  /// @lib
  function KeyReleased(key: KeyCode): Boolean;
  
  /// Checks to see if any key has been pressed since the last time 
  /// `ProcessEvents` was called.
  ///
  /// @lib
  function AnyKeyPressed(): Boolean;
  
  /// The KeyName function returns a string name for a given `KeyCode`. For 
  /// example, vk_Comma returns the string 'Comma'. This function could be used
  /// to display more meaningful key names for configuring game controls, etc.
  ///
  /// @lib
  function KeyName(key: KeyCode): String;
  
  
  /// Returns false when the key requested is being held down. This is updated
  /// as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  /// to specify the key to be checked.
  ///
  /// @lib
  function KeyUp(key: KeyCode): Boolean;
  
  /// Returns the number of fingers that are currently
  /// on the screen.
  ///
  /// @lib
  function NumberOfFingersOnScreen() : LongInt;
  
  /// Returns an Array of Fingers that are on the screen.
  ///
  /// @lib
  /// @length NumberOfFingersOnScreen
  function FingersOnScreen() : FingerArray;
  
  /// Returns false when the key requested is being held down. This is updated
  /// as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  /// to specify the key to be checked.
  ///
  /// @lib SetAccelerometerThreshold
  procedure AccelerometerThreshold(value : Single); overload;

  /// Returns false when the key requested is being held down. This is updated
  /// as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  /// to specify the key to be checked.
  ///
  /// @lib GetAccelerometerThreshold
  function AccelerometerThreshold() : Single; overload;

  /// Returns a value ranging from 0 to 1 showing delta
  /// in x Axis from level(being flat on the ground).
  ///
  /// @lib
  function DeviceMovedInXAxis() : Single;

  /// Returns a value ranging from 0 to 1 showing delta
  /// in y Axis from level(being flat on the ground).
  ///
  /// @lib
  function DeviceMovedInYAxis() : Single;

  /// Returns a value ranging from 0 to 1 showing delta
  /// in z Axis from level(being flat on the ground).
  ///
  /// @lib
  function DeviceMovedInZAxis() : Single;
  
  /// returns a boolean indicating if the screen was touched.
  /// @lib
  function ScreenTouched() : Boolean;

  /// Shows iOS Keyboard
  /// @lib
  procedure ShowKeyboard();
  /// Hides iOS Keyboard
  /// @lib
  procedure HideKeyboard();
  /// Toggles iOS Keyboard
  /// @lib
  procedure ToggleKeyboard();
  /// returns boolean indicating if iOS keyboard is shown
  /// @lib
  function KeyboardShown():Boolean;

//=============================================================================
implementation
//=============================================================================

  uses SysUtils, Classes, sgPhysics, sgTrace, sgShared, sgText, sgGeometry, sgSharedUtils, sgInputBackend, sgDriverInput, sgDriver{$IFDEF IOS}, sgDriveriOS {$ENDIF};

  var
  // seems to work well with this value
    _AccelerometerThreshold : Single = 0.01;

//----------------------------------------------------------------------------
// Game Loop Essentials
//----------------------------------------------------------------------------

  function WindowCloseRequested(): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgInput', 'WindowCloseRequested');
    {$ENDIF}
      result := HasQuit();
    {$IFDEF TRACE}
      TraceExit('sgInput', 'WindowCloseRequested');
    {$ENDIF}
  end;

  procedure ProcessEvents();
  var
    x, y: Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgInput', 'ProcessEvents');
    {$ENDIF}
    {$ifdef DARWIN}
        {$IFDEF NO_ARC}
            CyclePool();
        {$ENDIF}
    {$endif}
    InputDriver.GetRelativeMouseState(x, y);
    InputBackendProcessEvents();
    {$IFDEF TRACE}
      TraceExit('sgInput', 'ProcessEvents');
    {$ENDIF}
  end;

//============//
//    iOS    //   
//===========//

//Keyboard
procedure ShowKeyboard();
begin
  {$IFDEF IOS}
  iOSDriver.ShowKeyboard();
  {$ENDIF}
end;

procedure HideKeyboard();
begin
  {$IFDEF IOS}
  iOSDriver.ShowKeyboard();
  {$ENDIF}
end;

procedure ToggleKeyboard();
begin
  {$IFDEF IOS}
  iOSDriver.ToggleKeyboard();
  {$ENDIF}
end;

function KeyboardShown():Boolean;
begin
  result := false;
  {$IFDEF IOS}
  result := iOSDriver.IsShownKeyboard();
  {$ENDIF}
end;


//Touch

function FingersOnScreen() : FingerArray;
begin
  result := GetFingers();
end;

function NumberOfFingersOnScreen() : LongInt;
begin
  result := GetNumberOfFingers();
end;

function ScreenTouched() : Boolean;
begin
  result := iDeviceTouched();
end;


//Accelerometer

procedure AccelerometerThreshold(value : Single); overload;
begin
  _AccelerometerThreshold := value;
end;

function AccelerometerThreshold() : Single; overload;
begin
  result := _AccelerometerThreshold;
end;

//THESE ARE SWITCHED because at the time of coding we only allowed landscape mode.
function DeviceMovedInXAxis() : Single;
begin
  result := GetNormalisedDeltaYAxis();
  if result < _AccelerometerThreshold then
  begin
    result := 0;
  end;
end;

function DeviceMovedInYAxis() : Single;
begin
  result := GetNormalisedDeltaXAxis();
  if result < _AccelerometerThreshold then
  begin
    result := 0;
  end;
end;

function DeviceMovedInZAxis() : Single;
begin
  result := GetNormalisedDeltaZAxis();
  if result < _AccelerometerThreshold then
  begin
    result := 0;
  end;
end;



  //---------------------------------------------------------------------------

  function KeyUp(key: KeyCode): Boolean;
  begin
    result := (not KeyDown(key));
  end;
  function KeyReleased(key: KeyCode): Boolean;
  begin
    result := WasKeyReleased(Driver.GetKeyCode(Longint(key)));
  end;

  function KeyDown(key : keyCode): Boolean;
  begin
    result := WasKeyDown(Driver.GetKeyCode(Longint(key)));
  end;
  
  function KeyTyped(key: KeyCode): Boolean;
  begin
    result := WasKeyJustTyped(Driver.GetKeyCode(Longint(key)));
  end;

  function AnyKeyPressed(): Boolean;
  begin
   result := WasAKeyPressed();
  end;

  //---------------------------------------------------------------------------
  
  procedure StartReadingText(textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    if theFont = nil then begin RaiseException('The specified font to start reading text is nil'); exit; end;
    if maxLength <= 0 then begin RaiseException('Minimum length to start reading text is 1'); exit; end;
    if ReadingText() then begin RaiseException('Already reading text, cannot start reading text again.'); exit; end;
    
    InputBackendStartReadingText(textColor, maxLength, theFont, area);
  end;
  
  procedure StartReadingText(textColor, backgroundColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    if theFont = nil then begin RaiseException('The specified font to start reading text is nil'); exit; end;
    if maxLength <= 0 then begin RaiseException('Minimum length to start reading text is 1'); exit; end;
    if ReadingText() then begin RaiseException('Already reading text, cannot start reading text again.'); exit; end;
    
    InputBackendStartReadingText(textColor,backgroundColor, maxLength, theFont, area);
  end;
  
  procedure StartReadingText(textColor: Color; maxLength: Longint; theFont: Font; x, y: Longint); overload;
  begin
    StartReadingText(textColor, maxLength, theFont, RectangleFrom(x, y, TextWidth(theFont, StringOfChar('M', maxLength)), TextHeight(theFont, 'M')));
  end;
  
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    StartReadingText(textColor, maxLength, theFont, area);
    SetText(text);    
  end;
  
  procedure StartReadingTextWithText(text: String; textColor, backGroundColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    StartReadingText(textColor, backGroundColor, maxLength, theFont, area);
    SetText(text);    
  end;
  
  
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; const pt: Point2D); overload;
  begin
    StartReadingTextWithText(text, textColor, maxLength, theFont, RoundInt(pt.x), RoundInt(pt.y));
  end;
  
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; x, y: Longint); overload;
  begin
    StartReadingText(textColor, maxLength, theFont, x, y);
    SetText(text);
  end;
  
  function ReadingText(): Boolean;
  begin
    result := IsReading();
  end;
  
  function TextEntryCancelled(): Boolean;
  begin
    result := TextEntryWasCancelled();
  end;
  
  function EndReadingText(): String;
  begin
    result := InputBackendEndReadingText();
  end;
  
  function TextReadAsASCII(): String;
  begin
    result := EnteredString();
  end;
  
  
  //---------------------------------------------------------------------------
  
  function MousePositionAsVector(): Vector;
  {$IFNDEF IOS}
  var
    x, y: Longint;
  {$ENDIF}
  begin
    {$IFNDEF IOS}
    x := 0; y := 0;
    InputDriver.GetMouseState(x, y);
    result := VectorTo(x, y);
    {$ELSE}
     result := Vector(LastFingerPosition());
    {$ENDIF}
  end;

  procedure ShowMouse(); overload;
  begin
    ShowMouse(true);
  end;
  
  procedure HideMouse();
  begin
    ShowMouse(false);
  end;
  
  procedure ShowMouse(show : Boolean); overload;
  begin
    try
      if show then InputDriver.ShowCursor(1)
      else InputDriver.ShowCursor(0);
    except
      begin RaiseException('Unable to show or hide mouse'); exit; end;
    end;
  end;
  
  procedure MoveMouse(x, y : Byte);overload;
  begin
    InputDriver.WarpMouse(x,y);
    MouseMovement();
  end;
  
  procedure MoveMouse(const point : Point2d);overload;
  begin
    InputDriver.WarpMouse(RoundUShort(point.x), RoundUShort(point.y));
    MouseMovement();
  end;
  
  function MouseShown(): Boolean;
  begin
    result := InputDriver.ShowCursor(-1) = 1;
  end;
  
  function MousePosition(): Point2D;
  {$IFNDEF IOS}
  var
    x, y: Longint;
  {$ENDIF}
  begin
    {$IFNDEF IOS}
    x := 0; y := 0;
    InputDriver.GetMouseState(x, y);
    result := PointAt(x, y);
    {$ELSE}
    result := LastFingerPosition(); 
    {$ENDIF} 
  end;
  
  function MouseX(): Single;
  begin
    result := MousePosition().x;
  end;
  
  function MouseY(): Single;
  begin
    result := MousePosition().y;
  end;
  
  function MouseMovement(): Vector;
  var
    x, y: Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgInput', 'MouseMovement');
    {$ENDIF}
    
    x := 0; 
    y := 0;
    InputDriver.GetRelativeMouseState(x, y);
    result := VectorTo(x, y);

    {$IFDEF TRACE}
      TraceExit('sgInput', 'MouseMovement');
    {$ENDIF}
  end;
  
  function MouseDown(button: MouseButton): Boolean;
  {$IFNDEF IOS}
  var
    x, y: Longint;
  {$ENDIF}
  begin
    {$IFNDEF IOS}
      x := 0; y := 0;
      result := (InputDriver.GetMouseState(x, y) and InputDriver.Button(Longint(button))) > 0;
    {$ELSE}
      result := MouseClicked(button);
    {$ENDIF}
  end;
  
  function MouseUp(button: MouseButton): Boolean;
  begin
    result := not MouseDown(button);
  end;
  
  function MouseClicked(button: MouseButton): Boolean;
  begin
    result := _ButtonsClicked[button];
  end;
  
  

  function KeyName(key: KeyCode): String;
  begin
    case key of
      vk_Unknown    : result := 'Unknown';
      vk_BACKSPACE  : result := 'Backspace';
      vk_TAB        : result := 'Tab';
      vk_CLEAR      : result := 'Clear';
      vk_RETURN     : result := 'Return';
      vk_PAUSE      : result := 'Pause';
      vk_ESCAPE     : result := 'Escape';
      vk_SPACE      : result := 'Space';
      vk_EXCLAIM    : result := 'Exclaim';
      vk_QUOTEDBL   : result := 'Double Quote';
      vk_HASH       : result := 'Hash';
      vk_DOLLAR     : result := 'Dollar';
      vk_AMPERSAND  : result := 'Ampersand';
      vk_QUOTE      : result := 'Quote';
      vk_LEFTPAREN  : result := 'Left Parenthesis';
      vk_RIGHTPAREN : result := 'Right Parenthesis';
      vk_ASTERISK   : result := 'Asterisk';
      vk_PLUS       : result := 'Plus';
      vk_COMMA      : result := 'Comma';
      vk_MINUS      : result := 'Minus';
      vk_PERIOD     : result := 'Period';
      vk_SLASH      : result := 'Slash';
      vk_0: result := '0';
      vk_1: result := '1';
      vk_2: result := '2';
      vk_3: result := '3';
      vk_4: result := '4';
      vk_5: result := '5';
      vk_6: result := '6';
      vk_7: result := '7';
      vk_8: result := '8';
      vk_9: result := '9';
      vk_COLON     : result := 'Colon';
      vk_SEMICOLON : result := 'Semicolon';
      vk_LESS      : result := 'Less';
      vk_EQUALS    : result := 'Equals';
      vk_GREATER   : result := 'Greater';
      vk_QUESTION  : result := 'Question';
      vk_AT        : result := 'At';

      // Skip uppercase letters 

      vk_LEFTBRACKET  : result := 'Left Bracket';
      vk_BACKSLASH    : result := 'Backslash';
      vk_RIGHTBRACKET : result := 'Right Bracket';
      vk_CARET        : result := 'Caret';
      vk_UNDERSCORE   : result := 'Underscore';
      vk_BACKQUOTE    : result := 'Back Quote';
      vk_a: result := 'a';
      vk_b: result := 'b';
      vk_c: result := 'c';
      vk_d: result := 'd';
      vk_e: result := 'e';
      vk_f: result := 'f';
      vk_g: result := 'g';
      vk_h: result := 'h';
      vk_i: result := 'i';
      vk_j: result := 'j';
      vk_k: result := 'k';
      vk_l: result := 'l';
      vk_m: result := 'm';
      vk_n: result := 'n';
      vk_o: result := 'o';
      vk_p: result := 'p';
      vk_q: result := 'q';
      vk_r: result := 'r';
      vk_s: result := 's';
      vk_t: result := 't';
      vk_u: result := 'u';
      vk_v: result := 'v';
      vk_w: result := 'w';
      vk_x: result := 'x';
      vk_y: result := 'y';
      vk_z: result := 'z';
      vk_DELETE: result := 'Delete';
      // End of ASCII mapped keysyms

      // International keyboard syms
      vk_WORLD_0: result := 'World 0';
      vk_WORLD_1: result := 'World 1';
      vk_WORLD_2: result := 'World 2';
      vk_WORLD_3: result := 'World 3';
      vk_WORLD_4: result := 'World 4';
      vk_WORLD_5: result := 'World 5';
      vk_WORLD_6: result := 'World 6';
      vk_WORLD_7: result := 'World 7';
      vk_WORLD_8: result := 'World 8';
      vk_WORLD_9: result := 'World 9';
      vk_WORLD_10: result := 'World 10';
      vk_WORLD_11: result := 'World 11';
      vk_WORLD_12: result := 'World 12';
      vk_WORLD_13: result := 'World 13';
      vk_WORLD_14: result := 'World 14';
      vk_WORLD_15: result := 'World 15';
      vk_WORLD_16: result := 'World 16';
      vk_WORLD_17: result := 'World 17';
      vk_WORLD_18: result := 'World 18';
      vk_WORLD_19: result := 'World 19';
      vk_WORLD_20: result := 'World 20';
      vk_WORLD_21: result := 'World 21';
      vk_WORLD_22: result := 'World 22';
      vk_WORLD_23: result := 'World 23';
      vk_WORLD_24: result := 'World 24';
      vk_WORLD_25: result := 'World 25';
      vk_WORLD_26: result := 'World 26';
      vk_WORLD_27: result := 'World 27';
      vk_WORLD_28: result := 'World 28';
      vk_WORLD_29: result := 'World 29';
      vk_WORLD_30: result := 'World 30';
      vk_WORLD_31: result := 'World 31';
      vk_WORLD_32: result := 'World 32';
      vk_WORLD_33: result := 'World 33';
      vk_WORLD_34: result := 'World 34';
      vk_WORLD_35: result := 'World 35';
      vk_WORLD_36: result := 'World 36';
      vk_WORLD_37: result := 'World 37';
      vk_WORLD_38: result := 'World 38';
      vk_WORLD_39: result := 'World 39';
      vk_WORLD_40: result := 'World 40';
      vk_WORLD_41: result := 'World 41';
      vk_WORLD_42: result := 'World 42';
      vk_WORLD_43: result := 'World 43';
      vk_WORLD_44: result := 'World 44';
      vk_WORLD_45: result := 'World 45';
      vk_WORLD_46: result := 'World 46';
      vk_WORLD_47: result := 'World 47';
      vk_WORLD_48: result := 'World 48';
      vk_WORLD_49: result := 'World 49';
      vk_WORLD_50: result := 'World 50';
      vk_WORLD_51: result := 'World 51';
      vk_WORLD_52: result := 'World 52';
      vk_WORLD_53: result := 'World 53';
      vk_WORLD_54: result := 'World 54';
      vk_WORLD_55: result := 'World 55';
      vk_WORLD_56: result := 'World 56';
      vk_WORLD_57: result := 'World 57';
      vk_WORLD_58: result := 'World 58';
      vk_WORLD_59: result := 'World 59';
      vk_WORLD_60: result := 'World 60';
      vk_WORLD_61: result := 'World 61';
      vk_WORLD_62: result := 'World 62';
      vk_WORLD_63: result := 'World 63';
      vk_WORLD_64: result := 'World 64';
      vk_WORLD_65: result := 'World 65';
      vk_WORLD_66: result := 'World 66';
      vk_WORLD_67: result := 'World 67';
      vk_WORLD_68: result := 'World 68';
      vk_WORLD_69: result := 'World 69';
      vk_WORLD_70: result := 'World 70';
      vk_WORLD_71: result := 'World 71';
      vk_WORLD_72: result := 'World 72';
      vk_WORLD_73: result := 'World 73';
      vk_WORLD_74: result := 'World 74';
      vk_WORLD_75: result := 'World 75';
      vk_WORLD_76: result := 'World 76';
      vk_WORLD_77: result := 'World 77';
      vk_WORLD_78: result := 'World 78';
      vk_WORLD_79: result := 'World 79';
      vk_WORLD_80: result := 'World 80';
      vk_WORLD_81: result := 'World 81';
      vk_WORLD_82: result := 'World 82';
      vk_WORLD_83: result := 'World 83';
      vk_WORLD_84: result := 'World 84';
      vk_WORLD_85: result := 'World 85';
      vk_WORLD_86: result := 'World 86';
      vk_WORLD_87: result := 'World 87';
      vk_WORLD_88: result := 'World 88';
      vk_WORLD_89: result := 'World 89';
      vk_WORLD_90: result := 'World 90';
      vk_WORLD_91: result := 'World 91';
      vk_WORLD_92: result := 'World 92';
      vk_WORLD_93: result := 'World 93';
      vk_WORLD_94: result := 'World 94';
      vk_WORLD_95: result := 'World 95';

      // Numeric keypad
      vk_KP0: result := 'Keypad 0';
      vk_KP1: result := 'Keypad 1';
      vk_KP2: result := 'Keypad 2';
      vk_KP3: result := 'Keypad 3';
      vk_KP4: result := 'Keypad 4';
      vk_KP5: result := 'Keypad 5';
      vk_KP6: result := 'Keypad 6';
      vk_KP7: result := 'Keypad 7';
      vk_KP8: result := 'Keypad 8';
      vk_KP9: result := 'Keypad 9';
      vk_KP_PERIOD   : result := 'Keypad Period';
      vk_KP_DIVIDE   : result := 'Keypad Divide';
      vk_KP_MULTIPLY : result := 'Keypad Multiply';
      vk_KP_MINUS    : result := 'Keypad Minus';
      vk_KP_PLUS     : result := 'Keypad Plus';
      vk_KP_ENTER    : result := 'Keypad Enter';
      vk_KP_EQUALS   : result := 'Keypad Equals';

      // Arrows + Home/End pad
      vk_UP       : result := 'Up';
      vk_DOWN     : result := 'Down';
      vk_RIGHT    : result := 'Right';
      vk_LEFT     : result := 'Left';
      vk_INSERT   : result := 'Insert';
      vk_HOME     : result := 'Home';
      vk_END      : result := 'End';
      vk_PAGEUP   : result := 'Page Up';
      vk_PAGEDOWN : result := 'Page Down';

      // Function keys
      vk_F1: result := 'F1';
      vk_F2: result := 'F2';
      vk_F3: result := 'F3';
      vk_F4: result := 'F4';
      vk_F5: result := 'F5';
      vk_F6: result := 'F6';
      vk_F7: result := 'F7';
      vk_F8: result := 'F8';
      vk_F9: result := 'F9';
      vk_F10: result := 'F10';
      vk_F11: result := 'F11';
      vk_F12: result := 'F12';
      vk_F13: result := 'F13';
      vk_F14: result := 'F14';
      vk_F15: result := 'F15';

      // Key state modifier keys
      vk_NUMLOCK   : result := 'Numlock';
      vk_CAPSLOCK  : result := 'Caps lock';
      vk_SCROLLOCK : result := 'Scroll Lock';
      vk_RSHIFT    : result := 'Right Shift';
      vk_LSHIFT    : result := 'Left Shift';
      vk_RCTRL     : result := 'Right Ctrl';
      vk_LCTRL     : result := 'Left Ctrl';
      vk_RALT      : result := 'Right Alt';
      vk_LALT      : result := 'Left Alt';
      vk_RMETA     : result := 'Right Meta';
      vk_LMETA     : result := 'Left Meta';
      vk_LSUPER    : result := 'Left Super';
      vk_RSUPER    : result := 'Right Super';
      vk_MODE      : result := 'Mode';
      vk_COMPOSE   : result := 'Compose';
      // Miscellaneous function keys
      vk_HELP      : result := 'Help';
      vk_PRINT     : result := 'Print';
      vk_SYSREQ    : result := 'Sys Req';
      vk_BREAK     : result := 'Break';
      vk_MENU      : result := 'Menu';
      vk_POWER     : result := 'Power';
      vk_EURO      : result := 'Euro';
    end;

  end;


//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
    //RegisterEventProcessor(@ProcessMouseEvent, @StartProcessMouseEvents);
  end;

end.
