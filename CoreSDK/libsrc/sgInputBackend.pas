unit sgInputBackend;

interface
  uses sgTypes, sysutils, sgBackendTypes, sgWindowManager;
    procedure DoQuit();       // #
    procedure CheckQuit();
    procedure HandleKeydownEvent(kyCode, kyChar : LongInt);     // #
    procedure AddKeyData(kyCode, kyChar: Longint);
    procedure HandleKeyupEvent(kyCode: LongInt);
    procedure ProcessKeyPress(kyCode, kyChar: Longint);
    procedure ProcessTextEntry(const input: String);
    // text reading/draw collection
    procedure DrawCollectedText(dest: WindowPtr);
    procedure SetText(wind: WindowPtr; const text: String);
    procedure InputBackendStartReadingText(textColor: Color; maxLength: Longint; theFont: Font; area: Rectangle);overload;
    procedure InputBackendStartReadingText(textColor, backgroundColor: Color; maxLength: Longint; theFont: Font; area: Rectangle);overload;
    function  InputBackendEndReadingText(): String;
    // key pressed/typed tests
    function WasKeyReleased(kyCode: Longint): Boolean;
    function WasAKeyPressed(): Boolean;
    // event register/process Quit
    procedure InputBackendProcessEvents();
    procedure ProcessMouseEvent(button: Byte);
    function HasQuit(): Boolean;
    function isReading() : Boolean;
    function EnteredString: String; 
    function TextEntryWasCancelled: Boolean; 
    function WasKeyDown(kyCode : LongInt) : Boolean;
    function WasKeyJustTyped(kyCode : LongInt) : Boolean;
    // procedure HandleTouchEvent(const fingers : FingerArray; down, up: Boolean);
    // procedure HandleAxisMotionEvent(Accelerometer :  AccelerometerMotion);
    // function GetDeltaXAxis():LongInt;
    // function GetDeltaYAxis():LongInt;
    // function GetDeltaZAxis():LongInt;
    // function GetFingers(): FingerArray;
    // function GetNumberOfFingers(): LongInt;
    // function GetNormalisedDeltaXAxis():Single;
    // function GetNormalisedDeltaYAxis():Single;
    // function GetNormalisedDeltaZAxis():Single;
    // function iDeviceTouched():Boolean;
    // function iDeviceMoved():Boolean;
    // function LastFingerPosition():Point2D;
  type
    KeyDownData = record
      downAt:   Longint;
      code:     Longint;
      keyChar:  Longint;
    end;
     
  var
    _quit:                  Boolean;
    _keyPressed:            Boolean;
    _justTouched:           Boolean;
    _justMoved:             Boolean;

    _lastKeyRepeat:         Longint;
    _KeyDown:               Array of KeyDownData;
    _keyJustTyped:          Array of LongInt;
    _KeyReleased:           Array of Longint;
    _ButtonsClicked:        Array [MouseButton] of Boolean;
    _LastFingerPosition:    Point2D;

    //IOS Variables
    // _fingers:            FingerArray;
    // _deltaAccelerometer: AccelerometerMotion;

implementation
  uses sgDrawingOptions, sgDriverInput, sgDriverTimer, sgSharedUtils, sgDriverImages, sgImages, sgText, sgShared, sgGraphics {$IFDEF IOS},sgDriveriOS{$ENDIF}, sgDriverGraphics, sgDriverSDL2Types;
  
  procedure _InitGlobalVars(); 
  begin
    _quit := false;
    _keyPressed := false;
    _lastKeyRepeat := 0;
    _justMoved:= false;
    _justTouched:=false;
    _LastFingerPosition.x :=0;
    _LastFingerPosition.y :=0;
    SetLength(_KeyDown, 0);
    // SetLength(_fingers, 0);
  end;
  
  function EnteredString: String; 
  begin
    if Assigned(_CurrentWindow) then
      result := _CurrentWindow^.tempString
    else
      result := '';
  end;
  
  function TextEntryWasCancelled: Boolean; 
  begin
    if Assigned(_CurrentWindow) then
      result := _CurrentWindow^.textCancelled
    else
      result := false;
  end;
  
  procedure CheckQuit();
  var
    i, count: Integer;
    wnd: WindowPtr;
  begin
    count := WindowCount();

    // Check close for each window
    for i := 0 to count - 1 do
    begin
      wnd := WindowPtr(WindowAtIndex(i));
      if Assigned(wnd) then
      begin
        wnd^.eventData := _sg_functions^.input.get_window_event_data(@wnd^.image.surface);
      end;
    end;

    if( WasKeyDown(SDLK_Q) and (WasKeyDown(SDLK_LGUI) or WasKeyDown(SDLK_RGUI))) 
      or
      (WasKeyDown(SDLK_F4) and ( WasKeyDown(SDLK_RALT) or WasKeyDown(SDLK_LALT)))
      or 
      ( Assigned(_PrimaryWindow) and (_sg_functions^.input.window_close_requested(@_PrimaryWindow^.image.surface) <> 0)) then
    begin
      DoQuit()
    end;
  end;
  
  procedure ResetMouseState();
  var
    b: MouseButton;
  begin
    for b := LeftButton to MouseX2Button do
      _ButtonsClicked[b] := false;
  end;
  
  procedure InputBackendProcessEvents();
  begin
    _justTouched := false;
    _justMoved := false;
    _keyPressed := false;
    SetLength(_KeyReleased, 0);
    SetLength(_KeyJustTyped, 0);
    ResetMouseState();
  
    sgDriverInput.ProcessEvents();

    CheckQuit();
  end;
  
  function isReading() : Boolean;
  begin
    if Assigned(_CurrentWindow) then
      result := _CurrentWindow^.readingString
    else
      result := false;
  end;
  
  procedure ProcessKeyReleased (kyCode :  LongInt);
  begin
    SetLength(_KeyReleased, Length(_KeyReleased) + 1);
    _KeyReleased[High(_KeyReleased)] := kyCode;
  end;
  
  procedure HandleKeyupEvent(kyCode: LongInt);
  var
    i, keyAt: Integer;
  begin
    keyAt := -1;
    for i := 0 to High(_KeyDown) do
    begin
      if _KeyDown[i].code = kyCode then
      begin
        keyAt := i;
        break;
      end;
    end;
    
    if keyAt = -1 then exit;
    for i := keyAt to High(_KeyDown) -1 do
    begin
      _KeyDown[i] := _KeyDown[i + 1];
    end;
    SetLength(_KeyDown, Length(_KeyDown) - 1);
    
    ProcessKeyReleased(kyCode);
    
  end;

  procedure AddKeyData(kyCode, kyChar: Longint);
  var
    i: Integer;
  begin
    // if (kyCode = LongInt(LSHIFTKey)) or (kyCode = LongInt(RSHIFTKey)) then exit;
    
    for i := 0 to High(_KeyDown) do
    begin
      if _KeyDown[i].code = kyCode then exit;
    end;
    
    SetLength(_KeyDown, Length(_KeyDown) + 1);
    
    with _KeyDown[High(_KeyDown)] do
    begin
      downAt  := TimerDriver.GetTicks();
      code    := kyCode;
      keyChar := kyChar;
    end;
  end;
  
  procedure ProcessKeyJustTyped(kyCode : LongInt);
  var
  i : Integer;
  begin
    for i := 0 to High(_KeyDown)do
    begin
      if(_Keydown[i].code = kyCode) then exit;
    end;
    SetLength(_KeyJustTyped, Length(_KeyJustTyped) + 1);
    _KeyJustTyped[High(_KeyJustTyped)] := kyCode;
  end;
  
  procedure HandleKeydownEvent(kyCode, kyChar : LongInt);
  begin
    _keyPressed := true;
  
    ProcessKeyJustTyped(kyCode);
    
    AddKeyData(kyCode, kyChar);
    ProcessKeyPress(kyCode, kyChar);
  end;

  function WindowPtrWithFocus(): WindowPtr;
  begin
    result := ToWindowPtr(WindowWithFocus());
  end;
  
  procedure ProcessKeyPress(kyCode, kyChar: Longint);
  var
    oldStr : String;
    focusWindow: WindowPtr;
  begin
    focusWindow := WindowPtrWithFocus();
    if not Assigned(focusWindow) then exit;

    if focusWindow^.readingString then
    begin
      oldStr := focusWindow^.tempString;
            
      //If the key is not a control character
      if (kyCode = LongInt(BACKSPACEKey)) and (Length(focusWindow^.tempString) > 0) then
      begin
         focusWindow^.tempString := Copy(focusWindow^.tempString, 1, Length(focusWindow^.tempString) - 1);
      end
      else if (kyCode = LongInt(RETURNKey)) or (kyCode = LongInt(KeyPadENTER)) then
      begin
        focusWindow^.readingString := false;
      end
      else if kyCode = LongInt(ESCAPEKey) then
      begin
        focusWindow^.tempString := '';
        focusWindow^.readingString := false;
        focusWindow^.textCancelled := true;
      end;
      
      //If the string was change
      if oldStr <> focusWindow^.tempString then
      begin
        SetText(focusWindow, focusWindow^.tempString);
      end;
    end;
  end;

  procedure ProcessTextEntry(const input: String);
  var
    focusWindow: WindowPtr;
  begin
    focusWindow := WindowPtrWithFocus();
    if not Assigned(focusWindow) then exit;

    if focusWindow^.readingString then
    begin
      if Length(focusWindow^.tempString) + Length(input) < focusWindow^.maxStringLen then
      begin
        focusWindow^.tempString += input;
        SetText(focusWindow, focusWindow^.tempString);
      end
      else if Length(focusWindow^.tempString) < focusWindow^.maxStringLen then
      begin
        focusWindow^.tempString := focusWindow^.tempString + Copy(input, 1, focusWindow^.maxStringLen - Length(focusWindow^.tempString));
        SetText(focusWindow, focusWindow^.tempString);
      end;
    end;
  end;
  
  procedure ProcessMouseEvent(button: Byte);
  begin
    if( WithinRange(length(_ButtonsClicked), button )) then
    begin
      {$IFDEF IOS}
      if (length(_fingers) > 1) and (MouseButton(button) = LeftButton) then
      begin
        _ButtonsClicked[RightButton] := true;
        exit;
      end;
      {$ENDIF}
      _ButtonsClicked[MouseButton(button)] := true;
    end;
  end;
  
  
  procedure FreeOldSurface(wind: WindowPtr);
  begin
    if Assigned(wind) and Assigned(wind^.textBitmap) and (wind^.textBitmap <> wind^.cursorBitmap) then
      FreeBitmap(wind^.textBitmap);
  end;
  
  procedure RenderTextSurface(wind: WindowPtr; const text : String);
  var
    outStr: String;
  begin
    if not Assigned(wind) then exit;

    if Length(text) > 0 then
    begin
      outStr := text + '|';
      FreeOldSurface(wind);
      wind^.textBitmap := DrawTextToBitmap(wind^.font, outStr, wind^.forecolor, wind^.backgroundColor);
    end
    else
      wind^.textBitmap := wind^.cursorBitmap;  
  end;
  
  procedure InputBackendStartReadingText(textColor, backgroundColor: Color; maxLength: Longint; theFont: Font; area: Rectangle);overload;
  var
    newStr: String;
  begin
    if not Assigned(_CurrentWindow) then exit;

    FreeOldSurface(_CurrentWindow);

    _CurrentWindow^.readingString := true;
    _CurrentWindow^.textCancelled := false;
    _CurrentWindow^.tempString := '';
    _CurrentWindow^.maxStringLen := maxLength;
    _CurrentWindow^.foreColor := textColor;
    _CurrentWindow^.backgroundColor := backgroundColor;
    _CurrentWindow^.font := theFont;
    _CurrentWindow^.area := area;

    newStr := '|';

    if Assigned(_CurrentWindow^.cursorBitmap) then FreeBitmap(_CurrentWindow^.cursorBitmap);
    _CurrentWindow^.cursorBitmap := DrawTextToBitmap(_CurrentWindow^.font, newStr, _CurrentWindow^.foreColor, _CurrentWindow^.backgroundColor);
    _CurrentWindow^.textBitmap := _CurrentWindow^.cursorBitmap;
  end;
  
  procedure InputBackendStartReadingText(textColor: Color; maxLength: Longint; theFont: Font; area: Rectangle);overload;
  begin
    InputBackendStartReadingText(textColor,ColorTransparent,maxLength,theFont,area);
  end;

  procedure DrawCollectedText(dest : WindowPtr);
  begin
    if not Assigned(dest) then exit;

    if dest^.readingString then
    begin
      if Assigned(dest^.textBitmap) then
      begin
        sgDriverImages.DrawBitmap(ToBitmapPtr(dest^.textBitmap), dest^.area.x, dest^.area.y, OptionDrawTo(Window(dest)));
      end;
    end;
  end;
  
  procedure SetText(wind: WindowPtr; const text: String);
  begin
    wind^.tempString := text;
    
    //Render a new text surface
    RenderTextSurface(wind, wind^.tempString);
  end;
  
  function InputBackendEndReadingText(): String;
  begin
    result := '';
    if not Assigned(_CurrentWindow) then exit;

    _CurrentWindow^.readingString := false;
    result := _CurrentWindow^.tempString;
  end;
  
  function WasAKeyPressed(): Boolean;
  begin
    result := _keyPressed;
  end;
  
  procedure DoQuit();
  begin
    _quit := true;
  end;
  
  function HasQuit(): Boolean;
  begin
    result := _quit;
  end;
  
  function WasKeyReleased(kyCode: Longint): Boolean;
  var i: Longint;
  begin
    result := false;
  
    for i := 0 to High(_KeyReleased) do
    begin
      if _KeyReleased[i] = kyCode then
      begin
        result := true;
        exit;
      end;
    end;
  end;
  function WasKeyJustTyped(kyCode : LongInt) : Boolean;
  var
  i : Integer;
  begin
    result := false;
    for i := 0 to High(_KeyJustTyped) do
    begin
      if _KeyJustTyped[i] = kyCode then
      begin
        result := true;
        exit;
      end;
    end;
  end;
  
  function WasKeyDown(kyCode : LongInt) : Boolean;
  var
  i : Integer;
  begin
    result := false;
    for i := 0 to High(_KeyDown) do
    begin
      if _KeyDown[i].code = kyCode then
      begin
        result := true;
        exit;
      end;
    end;
  end;

  //iOS Procedures
  
  // function FingerToString(finger: Finger): String;
  // begin
  //   result := 'Finger: ' + IntToStr(finger.id) +
  //             ' x: ' + FloatToStr(finger.position.x) +     
  //             ' y: ' + FloatToStr(finger.position.y) +    
  //             ' dx: ' + FloatToStr(finger.positionDelta.x) + 
  //             ' dy: ' + FloatToStr(finger.positionDelta.y) +
  //             ' lx: ' + FloatToStr(finger.lastPosition.x) +
  //             ' ly: ' + FloatToStr(finger.lastPosition.y) +
  //             ' p: ' + FloatToStr(finger.pressure) + 
  //             // ' lp: ' + FloatToStr(finger.lastPressure) +
  //             ' d: ' + BoolToStr(finger.down);
  // end;
  
  // function FingerInArray(const f: Finger; const fingers: FingerArray): Boolean;
  // var
  //   i: Integer;
  // begin
  //   result := false;

  //   for i := 0 to High(fingers) do
  //   begin
  //     if f.id = fingers[i].id then
  //     begin
  //       result := true;
  //       exit;
  //     end;
  //   end;
  // end;

  // procedure AddToFingers(const finger: Finger; var fingers: FingerArray);
  // begin
  //   SetLength(fingers, Length(fingers) + 1);
  //   fingers[High(fingers)] := finger;
  // end;

  // procedure RemoveFinger(const f: Finger; var fingers: FingerArray);
  // var
  //   i: Integer;
  // begin
  //   for i := 0 to High(fingers) do
  //   begin
  //     if f.id = fingers[i].id then
  //     begin
  //       fingers[i] := fingers[High(fingers)];
  //       SetLength(fingers, Length(fingers) - 1);
  //       exit;
  //     end;
  //   end;
  // end;

  // function UpdateFingerInArray(const f: Finger; var fingers: FingerArray): Boolean;
  // var
  //   i: Integer;
  // begin
  //   for i := 0 to High(fingers) do
  //   begin
  //     if f.id = fingers[i].id then
  //     begin
  //       fingers[i] := f;
  //       result := true;
  //       exit;
  //     end;
  //   end;

  //   result := false;
  // end;

  // procedure HandleTouchEvent(const fingers : FingerArray; down, up: Boolean);
  // var
  //   i: Integer;
  // begin
  //   _justTouched := _justTouched or down;
  //   for i := 0 to High(fingers) do
  //   begin

  //     if fingers[i].down then
  //     begin
  //       if not UpdateFingerInArray(fingers[i], _fingers) then
  //       begin
  //         // WriteLn('Adding');
  //         AddToFingers(fingers[i], _fingers);
  //       end;
  //     end
  //     else // finger is UP
  //     begin
  //       // WriteLn('Removing');
  //       RemoveFinger(fingers[i], _fingers);
  //     end;
  //   end;

  //   // WriteLn('HandleTouchEvent');
  //   // for i := 0 to High(finger) do
  //   //    WriteLn('Touch event', down, ' ', FingerToString(finger[i]));
    
  //   if length(_fingers) > 0 then
  //   begin
  //     _LastFingerPosition.x := _fingers[0].position.x;
  //     _LastFingerPosition.y := _fingers[0].position.y;
  //   end;
  // end;
  
  // function LastFingerPosition():Point2D;
  // begin
  //   result := _LastFingerPosition;
  // end;

  // function GetFingers(): FingerArray;
  // begin
  //   result := _fingers;
  // end;

  // function GetNumberOfFingers(): LongInt;
  // begin
  //   result := Length(_fingers);
  // end;


  // procedure HandleAxisMotionEvent(Accelerometer :  AccelerometerMotion);
  // begin
  //   _justMoved := true;
  //   _deltaAccelerometer := Accelerometer;    
  // end;

  // function GetDeltaXAxis():LongInt;
  // begin
  //   result := 0;
  //   {$IFDEF IOS}
  //     result := _deltaAccelerometer.xAxis;
  //   {$ENDIF}
  // end;

  // function GetDeltaYAxis():LongInt;
  // begin
  //   result := 0;
  //   {$IFDEF IOS}
  //     result := _deltaAccelerometer.yAxis;
  //   {$ENDIF}
  // end;
  
  // function GetDeltaZAxis():LongInt;
  // begin
  //   result := 0;
  //   {$IFDEF IOS}
  //     result := _deltaAccelerometer.zAxis;
  //   {$ENDIF}
  // end;
  
  // function GetNormalisedDeltaXAxis():Single;
  // begin
  //   result := 0;
  //   {$IFDEF IOS}
  //     result := iOSDriver.AxisToG(_deltaAccelerometer.xAxis);
  //   {$ENDIF}
  // end;

  // function GetNormalisedDeltaYAxis():Single;
  // begin
  //   result := 0;
  // {$IFDEF IOS}
  //   result := iOSDriver.AxisToG(_deltaAccelerometer.yAxis);
  // {$ENDIF}
  // end;
  
  // function GetNormalisedDeltaZAxis():Single;
  // begin
  //   result := 0;
  // {$IFDEF IOS}
  //   result := iOSDriver.AxisToG(_deltaAccelerometer.zAxis);
  // {$ENDIF}
  // end;

  // function iDeviceTouched():Boolean;
  // begin
  //   result := _justTouched;
  // end;

  // function iDeviceMoved():Boolean;
  // begin
  //   result := _justMoved;
  // end;

  
initialization
    _InitGlobalVars();
end.