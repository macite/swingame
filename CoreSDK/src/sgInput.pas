//=============================================================================
// sgInput.pas
//=============================================================================
//
// Responsible for input event processing for mouse visibility, movement and
// button clicks (including the scroll wheel as button clicks) and keyboard
// events for text input and key state checking.
//
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
  
  /// Checks to see if the user has asked for the application to quit. This value
  /// is updated by the `ProcessEvents` routine.
  ///
  /// @returns: True if the application has been requested to close.
  ///
  /// @lib
  function QuitRequested(): Boolean;
  
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
  procedure MoveMouse(x, y : Longint); overload;
    
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
  procedure StartReadingTextWithText(const text: String; textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
    
  
  /// The same as `StartReadingTextWithText` but with ``text`` and ``bgColor`` parameter
  /// that is displayed as default text to the user.  
  ///
  /// @lib StartReadingTextWithTextAndColorInArea
  /// @sn startReadingTextWith:%s color:%s bgColor:%s maxLen:%s font:%s area:%s  
  procedure StartReadingTextWithText(const text: String; textColor, backGroundColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  
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
  procedure StartReadingTextWithText(const text: String; textColor: Color; maxLength: Longint; theFont: Font; x, y: Longint); overload;
  
  /// The same as `StartReadingText` but with an additional ``text`` parameter
  /// that is displayed as default text to the user.  
  ///
  /// @lib StartReadingTextWithTextAtPt
  /// @sn startReadingTextWith:%s color:%s maxLen:%s font:%s at:%s
  procedure StartReadingTextWithText(const text: String; textColor: Color; maxLength: Longint; theFont: Font; const pt: Point2D); overload;
  
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
  /// example, CommaKey returns the string 'Comma'. This function could be used
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
  
  // /// Returns the number of fingers that are currently
  // /// on the screen.
  // ///
  // /// @lib
  // function NumberOfFingersOnScreen() : LongInt;
  
  // /// Returns an Array of Fingers that are on the screen.
  // ///
  // /// @lib
  // /// @length NumberOfFingersOnScreen
  // function FingersOnScreen() : FingerArray;
  
  // /// Returns false when the key requested is being held down. This is updated
  // /// as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  // /// to specify the key to be checked.
  // ///
  // /// @lib SetAccelerometerThreshold
  // procedure AccelerometerThreshold(value : Single); overload;

  // /// Returns false when the key requested is being held down. This is updated
  // /// as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  // /// to specify the key to be checked.
  // ///
  // /// @lib GetAccelerometerThreshold
  // function AccelerometerThreshold() : Single; overload;

  // /// Returns a value ranging from 0 to 1 showing delta
  // /// in x Axis from level(being flat on the ground).
  // ///
  // /// @lib
  // function DeviceMovedInXAxis() : Single;

  // /// Returns a value ranging from 0 to 1 showing delta
  // /// in y Axis from level(being flat on the ground).
  // ///
  // /// @lib
  // function DeviceMovedInYAxis() : Single;

  // /// Returns a value ranging from 0 to 1 showing delta
  // /// in z Axis from level(being flat on the ground).
  // ///
  // /// @lib
  // function DeviceMovedInZAxis() : Single;
  
  // // /// returns a boolean indicating if the screen was touched.
  // // /// @lib
  // // function ScreenTouched() : Boolean;

  // /// Shows iOS Keyboard
  // /// @lib
  // procedure ShowKeyboard();
  // /// Hides iOS Keyboard
  // /// @lib
  // procedure HideKeyboard();
  // /// Toggles iOS Keyboard
  // /// @lib
  // procedure ToggleKeyboard();
  // /// returns boolean indicating if iOS keyboard is shown
  // /// @lib
  // function KeyboardShown():Boolean;

//=============================================================================
implementation
//=============================================================================

  uses SysUtils, Classes, sgPhysics, sgTrace, sgShared, sgText, sgGeometry, sgSharedUtils, sgInputBackend, sgDriverInput, sgDriver{$IFDEF IOS}, sgDriveriOS {$ENDIF};

  // var
  // // seems to work well with this value
  //   _AccelerometerThreshold : Single = 0.01;

//----------------------------------------------------------------------------
// Game Loop Essentials
//----------------------------------------------------------------------------

  function QuitRequested(): Boolean;
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
    sgDriverInput.RelativeMouseState(x, y);
    InputBackendProcessEvents();
    // CheckNetworkActivity();
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

// function FingersOnScreen() : FingerArray;
// begin
//   result := GetFingers();
// end;

// function NumberOfFingersOnScreen() : LongInt;
// begin
//   result := GetNumberOfFingers();
// end;

// function ScreenTouched() : Boolean;
// begin
//   result := iDeviceTouched();
// end;


//Accelerometer

// procedure AccelerometerThreshold(value : Single); overload;
// begin
//   _AccelerometerThreshold := value;
// end;

// function AccelerometerThreshold() : Single; overload;
// begin
//   result := _AccelerometerThreshold;
// end;

// //THESE ARE SWITCHED because at the time of coding we only allowed landscape mode.
// function DeviceMovedInXAxis() : Single;
// begin
//   result := GetNormalisedDeltaYAxis();
//   if result < _AccelerometerThreshold then
//   begin
//     result := 0;
//   end;
// end;

// function DeviceMovedInYAxis() : Single;
// begin
//   result := GetNormalisedDeltaXAxis();
//   if result < _AccelerometerThreshold then
//   begin
//     result := 0;
//   end;
// end;

// function DeviceMovedInZAxis() : Single;
// begin
//   result := GetNormalisedDeltaZAxis();
//   if result < _AccelerometerThreshold then
//   begin
//     result := 0;
//   end;
// end;



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
  
  procedure StartReadingTextWithText(const text: String; textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    StartReadingText(textColor, maxLength, theFont, area);
    SetText(_CurrentWindow, text);    
  end;
  
  procedure StartReadingTextWithText(const text: String; textColor, backGroundColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    StartReadingText(textColor, backGroundColor, maxLength, theFont, area);
    SetText(_CurrentWindow, text);    
  end;
  
  
  procedure StartReadingTextWithText(const text: String; textColor: Color; maxLength: Longint; theFont: Font; const pt: Point2D); overload;
  begin
    StartReadingTextWithText(text, textColor, maxLength, theFont, RoundInt(pt.x), RoundInt(pt.y));
  end;
  
  procedure StartReadingTextWithText(const text: String; textColor: Color; maxLength: Longint; theFont: Font; x, y: Longint); overload;
  begin
    StartReadingText(textColor, maxLength, theFont, x, y);
    SetText(_CurrentWindow, text);
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
    sgDriverInput.MouseState(x, y);
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
      if show then sgDriverInput.ShowCursor(1)
      else sgDriverInput.ShowCursor(0);
    except
      begin RaiseException('Unable to show or hide mouse'); exit; end;
    end;
  end;
  
  procedure MoveMouse(x, y : Longint);overload;
  begin
    sgDriverInput.WarpMouse(x,y);
    MouseMovement();
  end;
  
  procedure MoveMouse(const point : Point2d);overload;
  begin
    sgDriverInput.WarpMouse(Round(point.x), Round(point.y));
    MouseMovement();
  end;
  
  function MouseShown(): Boolean;
  begin
    result := sgDriverInput.ShowCursor(-1) = 1;
  end;
  
  function MousePosition(): Point2D;
  {$IFNDEF IOS}
  var
    x, y: Longint;
  {$ENDIF}
  begin
    {$IFNDEF IOS}
    x := 0; y := 0;
    sgDriverInput.MouseState(x, y);
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
    sgDriverInput.RelativeMouseState(x, y);
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
      result := (sgDriverInput.MouseState(x, y) and Longint(button)) > 0;
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
      UnknownKey    : result := 'Unknown';
      BackspaceKey  : result := 'Backspace';
      TabKey        : result := 'Tab';
      ClearKey      : result := 'Clear';
      ReturnKey     : result := 'Return';
      PauseKey      : result := 'Pause';
      EscapeKey     : result := 'Escape';
      SpaceKey      : result := 'Space';
      ExclaimKey    : result := 'Exclaim';
      DoubleQuoteKey   : result := 'Double Quote';
      HashKey       : result := 'Hash';
      DollarKey     : result := 'Dollar';
      AmpersandKey  : result := 'Ampersand';
      QuoteKey      : result := 'Quote';
      LeftParenKey  : result := 'Left Parenthesis';
      RightParenKey : result := 'Right Parenthesis';
      AsteriskKey   : result := 'Asterisk';
      PlusKey       : result := 'Plus';
      CommaKey      : result := 'Comma';
      MinusKey      : result := 'Minus';
      PeriodKey     : result := 'Period';
      SlashKey      : result := 'Slash';
      Key0: result := '0';
      Key1: result := '1';
      Key2: result := '2';
      Key3: result := '3';
      Key4: result := '4';
      Key5: result := '5';
      Key6: result := '6';
      Key7: result := '7';
      Key8: result := '8';
      Key9: result := '9';
      ColonKey     : result := 'Colon';
      SemicolonKey : result := 'Semicolon';
      LessKey      : result := 'Less';
      EqualsKey    : result := 'Equals';
      GreaterKey   : result := 'Greater';
      QuestionKey  : result := 'Question';
      AtKey        : result := 'At';

      // Skip uppercase letters 

      LeftBracketKey  : result := 'Left Bracket';
      BackslashKey    : result := 'Backslash';
      RightBracketKey : result := 'Right Bracket';
      CaretKey        : result := 'Caret';
      UnderscoreKey   : result := 'Underscore';
      BackquoteKey    : result := 'Back Quote';
      AKey: result := 'a';
      BKey: result := 'b';
      CKey: result := 'c';
      DKey: result := 'd';
      EKey: result := 'e';
      FKey: result := 'f';
      GKey: result := 'g';
      HKey: result := 'h';
      IKey: result := 'i';
      JKey: result := 'j';
      KKey: result := 'k';
      LKey: result := 'l';
      MKey: result := 'm';
      NKey: result := 'n';
      OKey: result := 'o';
      PKey: result := 'p';
      QKey: result := 'q';
      RKey: result := 'r';
      SKey: result := 's';
      TKey: result := 't';
      UKey: result := 'u';
      VKey: result := 'v';
      WKey: result := 'w';
      XKey: result := 'x';
      YKey: result := 'y';
      ZKey: result := 'z';
      DeleteKey: result := 'Delete';
      // End of ASCII mapped keysyms

      // Numeric keypad
      KeyPad0: result := 'Keypad 0';
      KeyPad1: result := 'Keypad 1';
      KeyPad2: result := 'Keypad 2';
      KeyPad3: result := 'Keypad 3';
      KeyPad4: result := 'Keypad 4';
      KeyPad5: result := 'Keypad 5';
      KeyPad6: result := 'Keypad 6';
      KeyPad7: result := 'Keypad 7';
      KeyPad8: result := 'Keypad 8';
      KeyPad9: result := 'Keypad 9';
      KeyPadPeriod   : result := 'Keypad Period';
      KeyPadDivide   : result := 'Keypad Divide';
      KeyPadMultiply : result := 'Keypad Multiply';
      KeyPadMinus    : result := 'Keypad Minus';
      KeyPadPlus     : result := 'Keypad Plus';
      KeyPadEnter    : result := 'Keypad Enter';
      KeyPadEquals   : result := 'Keypad Equals';

      // Arrows + Home/End pad
      UpKey       : result := 'Up';
      DownKey     : result := 'Down';
      RightKey    : result := 'Right';
      LeftKey     : result := 'Left';
      InsertKey   : result := 'Insert';
      HomeKey     : result := 'Home';
      EndKey      : result := 'End';
      PageUpKey   : result := 'Page Up';
      PageDownKey : result := 'Page Down';

      // Function keys
      F1Key: result := 'F1';
      F2Key: result := 'F2';
      F3Key: result := 'F3';
      F4Key: result := 'F4';
      F5Key: result := 'F5';
      F6Key: result := 'F6';
      F7Key: result := 'F7';
      F8Key: result := 'F8';
      F9Key: result := 'F9';
      F10Key: result := 'F10';
      F11Key: result := 'F11';
      F12Key: result := 'F12';
      F13Key: result := 'F13';
      F14Key: result := 'F14';
      F15Key: result := 'F15';

      // Key state modifier keys
      NumLockKey   : result := 'Numlock';
      CapsLockKey  : result := 'Caps lock';
      ScrollLockKey : result := 'Scroll Lock';
      RightShiftKey    : result := 'Right Shift';
      LeftShiftKey    : result := 'Left Shift';
      RightCtrlKey     : result := 'Right Ctrl';
      LeftCtrlKey     : result := 'Left Ctrl';
      RightAltKey      : result := 'Right Alt';
      LeftAltKey      : result := 'Left Alt';
      RightMetaKey     : result := 'Right Meta';
      LeftMetaKey     : result := 'Left Meta';
      LeftSuperKey    : result := 'Left Super';
      RightSuperKey    : result := 'Right Super';
      ModeKey      : result := 'Mode';
      // Miscellaneous function keys
      HelpKey      : result := 'Help';
      SysReqKey    : result := 'Sys Req';
      MenuKey      : result := 'Menu';
      PowerKey     : result := 'Power';

      ShiftKey    : result := 'Shift';
      CtrlKey     : result := 'Ctrl';
      AltKey    : result := 'Alt';
      CommandKey    : result := 'Command';
      SuperKey    : result := 'Super';
      WindowsKey    : result := 'Windows';
      OptionKey     : result := 'Option';
    end;

  end;


//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
    //RegisterEventProcessor(@ProcessMouseEvent, @StartProcessMouseEvents);
  end;

end.
