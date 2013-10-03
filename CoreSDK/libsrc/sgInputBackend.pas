unit sgInputBackend;

interface
  uses sgTypes, sysutils;
    procedure DoQuit();
    procedure CheckQuit();
    procedure HandleKeydownEvent(kyCode, kyChar : LongInt);
    procedure AddKeyData(kyCode, kyChar: Longint);
    procedure HandleKeyupEvent(kyCode: LongInt);
    procedure ProcessKeyPress(kyCode, kyChar: Longint);
    procedure CheckKeyRepeat();
    // text reading/draw collection
    procedure DrawCollectedText(dest: Bitmap);
    procedure SetText(text: String);
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
    procedure HandleTouchEvent(const fingers : FingerArray; down, up: Boolean);
    procedure HandleAxisMotionEvent(Accelerometer :  AccelerometerMotion);
    function GetDeltaXAxis():LongInt;
    function GetDeltaYAxis():LongInt;
    function GetDeltaZAxis():LongInt;
    function GetFingers(): FingerArray;
    function GetNumberOfFingers(): LongInt;
    function GetNormalisedDeltaXAxis():Single;
    function GetNormalisedDeltaYAxis():Single;
    function GetNormalisedDeltaZAxis():Single;
    function iDeviceTouched():Boolean;
    function iDeviceMoved():Boolean;
    function LastFingerPosition():Point2D;
  type
    KeyDownData = record
      downAt:   Longint;
      code:     Longint;
      keyChar:  Longint;
    end;
     
  var
    _quit:                  Boolean;
    _keyPressed:            Boolean;
    _textCancelled:         Boolean;
    _justTouched:           Boolean;
    _justMoved:             Boolean;
    _tempString:            String;

    _lastKeyRepeat:         Longint;
    _KeyDown:               Array of KeyDownData;
    _keyJustTyped:          Array of LongInt;
    _KeyReleased:           Array of Longint;
    _maxStringLen:          LongInt;
    _textBitmap:            Bitmap;
    _cursorBitmap:          Bitmap;
    _font:                  Font;
    _foreColor:             Color;
    _backgroundColor:       Color;
    _area:                  Rectangle;
    _readingString:         Boolean;
    _ButtonsClicked:        Array [MouseButton] of Boolean;
    _LastFingerPosition:    Point2D;

    //IOS Variables
    _fingers:            FingerArray;
    _deltaAccelerometer: AccelerometerMotion;

implementation
  uses sgDriverInput, sgDriverTimer, sgSharedUtils, sgDriverImages, sgImages, sgText, sgShared, sgGraphics {$IFDEF IOS},sgDriveriOS{$ENDIF}, sgDriverGraphics;
  
  procedure _InitGlobalVars(); 
  begin
    _quit := false;
    _keyPressed := false;
    _textBitmap := nil;
    _cursorBitmap := nil;
    _readingString := false;
    _textCancelled := false;
    _lastKeyRepeat := 0;
    _justMoved:= false;
    _justTouched:=false;
    _LastFingerPosition.x :=0;
    _LastFingerPosition.y :=0;
    SetLength(_KeyDown, 0);
    SetLength(_fingers, 0);
  end;
  
  function EnteredString: String; 
  begin
    result := _tempString;
  end;
  
  
  
  function TextEntryWasCancelled: Boolean; 
  begin
    result := _textCancelled;
  end;
  
  procedure CheckQuit();
  begin
    if (InputDriver.CheckQuit()) then
      DoQuit()
  end;
  
  procedure CheckKeyRepeat();
  var
    nowTime, timeDown: Longint;
  begin
    if Length(_KeyDown) <> 1 then exit;
    
    nowTime := TimerDriver.GetTicks();
    
    timeDown := nowTime - _KeyDown[0].downAt;
    
    // 300 is the key repeat delay - hard coded for the moment...
    if timeDown > 300 then
    begin
      ProcessKeyPress(_KeyDown[0].code, _KeyDown[0].keyChar);
      
      // 40 is the key repeat delap - hard coded for the moment...
      _KeyDown[0].downAt := _KeyDown[0].downAt + 30;
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
  
    InputDriver.ProcessEvents();

    CheckKeyRepeat();
    CheckQuit();
  end;
  
  function isReading() : Boolean;
  begin
    result := _readingString;
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
    if (kyCode = LongInt(vk_LSHIFT)) or (kyCode = LongInt(vk_RSHIFT)) then exit;
    
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
  
  procedure ProcessKeyPress(kyCode, kyChar: Longint);
  var
    oldStr : String;
    
  begin
    if _readingString then
    begin
      oldStr := _tempString;
            
      //If the key is not a control character
      if (kyCode = LongInt(vk_BACKSPACE)) and (Length(_tempString) > 0) then
      begin
         _tempString := Copy(_tempString, 1, Length(_tempString) - 1);
      end
      else if (kyCode = LongInt(vk_RETURN)) or (kyCode = LongInt(vk_KP_ENTER)) then
      begin
        _readingString := false;
      end
      else if kyCode = LongInt(vk_ESCAPE) then
      begin
        _tempString := '';
        _readingString := false;
        _textCancelled := true;
      end
      else if Length(_tempString) < _maxStringLen then
      begin
        case kyChar of
          //Skip non printable characters
          0..31: ;
          127..High(Byte): ;
          else //Append the character
            _tempString := _tempString + Char(kyChar);
        end;
      end;
      
      //If the string was change
      if oldStr <> _tempString then
      begin
        SetText(_tempString);
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
  
  
  procedure FreeOldSurface();
  begin
    if imagesDriver.SurfaceExists(_textBitmap) and (not imagesDriver.SameBitmap(_textBitmap, _cursorBitmap)) then
      FreeBitmap(_textBitmap);
  end;
  
  procedure RenderTextSurface(text : String);
  var
    outStr: String;
  begin
    if Length(text) > 0 then
    begin
      outStr := text + '|';
      FreeOldSurface();
      _textBitmap := DrawTextTo(_font, outStr, _forecolor, _backgroundColor);
    end
    else
      _textBitmap := _cursorBitmap;  
  end;
  
  procedure InputBackendStartReadingText(textColor, backgroundColor: Color; maxLength: Longint; theFont: Font; area: Rectangle);overload;
  var
    newStr: String;
  begin
    FreeOldSurface();

    _readingString := true;
    _textCancelled := false;
    _tempString := '';
    _maxStringLen := maxLength;
    _foreColor := textColor;
    _backgroundColor := backgroundColor;
    _font := theFont;
    _area := area;

    newStr := '|';

    if imagesDriver.SurfaceExists(_cursorBitmap) then FreeBitmap(_cursorBitmap);
    _cursorBitmap := DrawTextTo(_font, newStr, _foreColor,_backgroundColor);
    _textBitmap := _cursorBitmap;
  end;
  
  procedure InputBackendStartReadingText(textColor: Color; maxLength: Longint; theFont: Font; area: Rectangle);overload;
  begin
    InputBackendStartReadingText(textColor,ColorTransparent,maxLength,theFont,area);
  end;

  procedure DrawCollectedText(dest : Bitmap);
  var
    srect, drect: Rectangle;
    textWidth: Longint;
  begin
    if (not imagesDriver.SurfaceExists(dest)) then exit;

    if _readingString then
      if (imagesDriver.SurfaceExists(_textBitmap)) then
        begin
          textWidth := _textBitmap^.width;

          if textWidth > _area.width then
            srect.x := SmallInt(textWidth - _area.width)
          else
            srect.x := 0;
          srect.y := 0;

          srect.width := _area.width;
          srect.height := _area.height;

          drect := _area;
          imagesDriver.BlitSurface(_textBitmap,  dest, @srect, @drect);
        end;
  end;
  


  
  
  procedure SetText(text: String);
  begin
    _tempString := text;
    
    //Render a new text surface
    RenderTextSurface(_tempString);
  end;
  
  function InputBackendEndReadingText(): String;
  begin
    _readingString := false;
    result := _tempString;
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
  
  function FingerToString(finger: Finger): String;
  begin
    result := 'Finger: ' + IntToStr(finger.id) +
              ' x: ' + FloatToStr(finger.position.x) +     
              ' y: ' + FloatToStr(finger.position.y) +    
              ' dx: ' + FloatToStr(finger.positionDelta.x) + 
              ' dy: ' + FloatToStr(finger.positionDelta.y) +
              ' lx: ' + FloatToStr(finger.lastPosition.x) +
              ' ly: ' + FloatToStr(finger.lastPosition.y) +
              ' p: ' + FloatToStr(finger.pressure) + 
              // ' lp: ' + FloatToStr(finger.lastPressure) +
              ' d: ' + BoolToStr(finger.down);
  end;
  
  function FingerInArray(const f: Finger; const fingers: FingerArray): Boolean;
  var
    i: Integer;
  begin
    result := false;

    for i := 0 to High(fingers) do
    begin
      if f.id = fingers[i].id then
      begin
        result := true;
        exit;
      end;
    end;
  end;

  procedure AddToFingers(const finger: Finger; var fingers: FingerArray);
  begin
    SetLength(fingers, Length(fingers) + 1);
    fingers[High(fingers)] := finger;
  end;

  procedure RemoveFinger(const f: Finger; var fingers: FingerArray);
  var
    i: Integer;
  begin
    for i := 0 to High(fingers) do
    begin
      if f.id = fingers[i].id then
      begin
        fingers[i] := fingers[High(fingers)];
        SetLength(fingers, Length(fingers) - 1);
        exit;
      end;
    end;
  end;

  function UpdateFingerInArray(const f: Finger; var fingers: FingerArray): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to High(fingers) do
    begin
      if f.id = fingers[i].id then
      begin
        fingers[i] := f;
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  procedure HandleTouchEvent(const fingers : FingerArray; down, up: Boolean);
  var
    i: Integer;
  begin
    _justTouched := _justTouched or down;
    for i := 0 to High(fingers) do
    begin

      if fingers[i].down then
      begin
        if not UpdateFingerInArray(fingers[i], _fingers) then
        begin
          // WriteLn('Adding');
          AddToFingers(fingers[i], _fingers);
        end;
      end
      else // finger is UP
      begin
        // WriteLn('Removing');
        RemoveFinger(fingers[i], _fingers);
      end;
    end;

    // WriteLn('HandleTouchEvent');
    // for i := 0 to High(finger) do
    //    WriteLn('Touch event', down, ' ', FingerToString(finger[i]));
    
    if length(_fingers) > 0 then
    begin
      _LastFingerPosition.x := _fingers[0].position.x;
      _LastFingerPosition.y := _fingers[0].position.y;
    end;
  end;
  
  function LastFingerPosition():Point2D;
  begin
    result := _LastFingerPosition;
  end;

  function GetFingers(): FingerArray;
  begin
    result := _fingers;
  end;

  function GetNumberOfFingers(): LongInt;
  begin
    result := Length(_fingers);
  end;


  procedure HandleAxisMotionEvent(Accelerometer :  AccelerometerMotion);
  begin
    _justMoved := true;
    _deltaAccelerometer := Accelerometer;    
  end;

  function GetDeltaXAxis():LongInt;
  begin
    result := 0;
    {$IFDEF IOS}
      result := _deltaAccelerometer.xAxis;
    {$ENDIF}
  end;

  function GetDeltaYAxis():LongInt;
  begin
    result := 0;
    {$IFDEF IOS}
      result := _deltaAccelerometer.yAxis;
    {$ENDIF}
  end;
  
  function GetDeltaZAxis():LongInt;
  begin
    result := 0;
    {$IFDEF IOS}
      result := _deltaAccelerometer.zAxis;
    {$ENDIF}
  end;
  
  function GetNormalisedDeltaXAxis():Single;
  begin
    result := 0;
    {$IFDEF IOS}
      result := iOSDriver.AxisToG(_deltaAccelerometer.xAxis);
    {$ENDIF}
  end;

  function GetNormalisedDeltaYAxis():Single;
  begin
    result := 0;
  {$IFDEF IOS}
    result := iOSDriver.AxisToG(_deltaAccelerometer.yAxis);
  {$ENDIF}
  end;
  
  function GetNormalisedDeltaZAxis():Single;
  begin
    result := 0;
  {$IFDEF IOS}
    result := iOSDriver.AxisToG(_deltaAccelerometer.zAxis);
  {$ENDIF}
  end;

  function iDeviceTouched():Boolean;
  begin
    result := _justTouched;
  end;

  function iDeviceMoved():Boolean;
  begin
    result := _justMoved;
  end;

  
initialization
    _InitGlobalVars();
end.