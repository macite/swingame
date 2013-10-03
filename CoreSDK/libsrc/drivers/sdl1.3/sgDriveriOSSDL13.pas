unit sgDriveriOSSDL13;
//=============================================================================
// sgDriveriOSSDL.pas
//=============================================================================
//
// The iOS driver is responsible for controling iOS functions 
// between SDL 1.3 and SwinGame.
// Notes:

//=============================================================================

interface 
    uses SDL2, sgTypes, sgShared, sgGraphics, sgDriverGraphicsOpenGL;
    procedure LoadSDL13iOSDriver();
    
    //iphone max g force.
    const MAX_G  = 32767;   
    
implementation
    uses sgDriveriOS;

  {$IFDEF IOS}
  var
    accelerometer :PSDL_Joystick;
  {$ENDIF}
  
    procedure InitProcedure();
    begin
        {$IFDEF IOS}
            if (SDL_InitSubSystem(SDL_INIT_JOYSTICK) = -1) then 
            begin
                RaiseWarning('Error Initialising Joystick. '+ SDL_GetError());  exit;
            end;

            if (SDL_NumJoySticks() > 0) then 
            begin
                    // assume you are using iOS and as of 2012 it only has 1.
                    accelerometer := SDL_JoystickOpen(0);
                    if (accelerometer = nil) then 
                    begin
                        RaiseWarning('Could not open accelerometer'+ SDL_GetError()); exit;
                    end;
            end

            else
                RaiseWarning('No accelerometer detected');  
        {$ENDIF}
        exit;   
    end;

    function HandlAxisMotionEventProcedure(): AccelerometerMotion; 
    var
        accel : AccelerometerMotion;
    begin
        accel.xAxis := 0;
        accel.yAxis := 0;
        accel.zAxis := 0;
        {$IFDEF IOS}
            if (accelerometer = nil) then exit;
            accel.xAxis := SDL_JoystickGetAxis(accelerometer,0);
            accel.yAxis := SDL_JoystickGetAxis(accelerometer,1);
            accel.zAxis := SDL_JoystickGetAxis(accelerometer,2);
        {$ENDIF}
        result := accel;
    end;

    function AxisToGProcedure(value : LongInt) : Single; 
    begin
        result := value / MAX_G;
    end;

    function SDLFingerToFinger(sdlFinger : PSDL_Finger; touchState : PSDL_TouchFingerEvent ): Finger;
    begin
        result.id   := touchState^.fingerId;
        result.down := false;

        if not assigned(sdlFinger) then exit;
        
        result.id               := sdlFinger^.id;
        result.position.x       := touchState^.x * ScreenWidth();
        result.position.y       := touchState^.y * ScreenHeight();
        result.positionDelta.x  := touchState^.dx * ScreenWidth();
        result.positionDelta.y  := touchState^.dy * ScreenHeight();
        result.lastPosition.x   := result.position.x - result.positionDelta.x;
        result.lastPosition.y   := result.position.y - result.positionDelta.y;
        result.pressure         := touchState^.pressure;
        // result.lastPressure     := sdlFinger^.last_pressure;
        result.down             := touchState^.type_ <> Uint32(SDL_FINGERUP);
    end;

    function ProcessTouchEventProcedure(touch : Pointer): FingerArray; 
    var
        sdlTouch : PSDL_TouchFingerEvent;
        numberOfFingers, count : LongInt;
        fptr: PSDL_Finger;
    begin
        SetLength(result, 0);
        sdlTouch := PSDL_TouchFingerEvent(touch);
        if (touch = nil) then exit;

        // if sdlTouch^.type_ <> Uint32(SDL_FINGERMOTION) then
        //     WriteLn('Touch: ', sdlTouch^.type_, ' ', Uint32(SDL_FINGERUP), ' ', sdlTouch^.fingerId);

        numberOfFingers := SDL_GetNumTouchFingers(sdlTouch^.touchID);

        SetLength(result, 1);
        result[0].id    := sdlTouch^.fingerId;
        result[0].down  := false;

        for count := 0 to numberOfFingers - 1 do
        begin
            fptr := SDL_GetTouchFinger(sdlTouch^.touchID, count);
            if Assigned(fptr) and (fptr^.id = sdlTouch^.fingerId) then
                result[0] := SDLFingerToFinger(fptr, sdlTouch);
        end;
    end;

    procedure ShowKeyboardProcedure();
    begin
        {$IFDEF IOS}
        SDL_StartTextInput();
        //SDL_iPhoneKeyboardShow(POpenGLWindow(_screen)^.window);
        {$ENDIF}
    end;

    procedure HideKeyboardProcedure();
    begin
        {$IFDEF IOS}
        SDL_StopTextInput();
        //SDL_iPhoneKeyboardHide(POpenGLWindow(_screen)^.window)
        {$ENDIF}
    end;
    
    procedure ToggleKeyboardProcedure();
    begin
        {$IFDEF IOS}
        if SDL_IsTextInputActive() then
            HideKeyboardProcedure()
        else
            ShowKeyboardProcedure();
        // SDL_iPhoneKeyboardToggle(POpenGLWindow(_screen)^.window)
        {$ENDIF}
    end;
    
    function IsShownKeyboardProcedure(): Boolean;
    begin
        result := false;
        {$IFDEF IOS}
            result := SDL_IsTextInputActive();
        {$ENDIF}
    end;
    
    procedure LoadSDL13iOSDriver();
    begin
            iOSDriver.ShowKeyboard                      := @ShowKeyboardProcedure;
            iOSDriver.HideKeyboard                      := @HideKeyboardProcedure;
            iOSDriver.ToggleKeyboard                    := @ToggleKeyboardProcedure;
            iOSDriver.IsShownKeyboard                   := @IsShownKeyboardProcedure;
            iOSDriver.Init                                      := @InitProcedure;
            iOSDriver.ProcessAxisMotionEvent    := @HandlAxisMotionEventProcedure;
            iOSDriver.ProcessTouchEvent             := @ProcessTouchEventProcedure;
            iOSDriver.AxisToG                                   := @AxisToGProcedure;
    end;
end.