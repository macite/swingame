unit sgDriveriOS;
//=============================================================================
// sgiOSDriver.pas
//=============================================================================
//
//
//=============================================================================

interface
	uses sgTypes {$IFDEF SWINGAME_SDL13},sgDriveriOSSDL13{$ENDIF};
	
	type
		ShowKeyboardProcedure						= procedure();
		HideKeyboardProcedure						= procedure();
		ToggleKeyboardProcedure					= procedure();
		IsShownKeyboardProcedure				= function():Boolean;
		InitProcedure										= procedure();
		ProcessAxisMotionEventProcedure	= function() : AccelerometerMotion;
		AxisToGProcedure								= function( value : LongInt ) : Single;
		ProcessTouchEventProcedure			= function (touch: Pointer) : FingerArray;

	iOSDriverRecord = record
		
		ShowKeyboard						: ShowKeyboardProcedure;
		HideKeyboard						: HideKeyboardProcedure;
		ToggleKeyboard					: ToggleKeyboardProcedure;
		IsShownKeyboard					: IsShownKeyboardProcedure;
		Init										: InitProcedure;
		ProcessAxisMotionEvent	: ProcessAxisMotionEventProcedure;
		AxisToG									: AxisToGProcedure;
		ProcessTouchEvent				: ProcessTouchEventProcedure;
	end;



	var
		iOSDriver : iOSDriverRecord;

implementation
	procedure LoadDefaultiOSDriver();
	begin
	  {$IFDEF SWINGAME_SDL13}
		  LoadSDL13iOSDriver();
		{$ELSE}
		  //LoadSDLiOSDriver();
		{$ENDIF}
	end;


	procedure DefaultShowKeyboardProcedure();
	begin
		LoadDefaultiOSDriver();
		exit;
		iOSDriver.ShowKeyboard();
	end;

	procedure DefaultHideKeyboardProcedure();
	begin
		LoadDefaultiOSDriver();
		exit;
		iOSDriver.HideKeyboard();
	end;

	procedure DefaultToggleKeyboardProcedure();
	begin

		LoadDefaultiOSDriver();
		exit;
		iOSDriver.ToggleKeyboard();
	end;

	function DefaultIsShownKeyboardProcedure() : Boolean;
	begin
		LoadDefaultiOSDriver();
		exit;
		result := iOSDriver.IsShownKeyboard();
	end;

	function DefaultProcessAxisMotionEventProcedure() : AccelerometerMotion;
	begin
		LoadDefaultiOSDriver();
		result := iOSDriver.ProcessAxisMotionEvent();
	end;

	function DefaultProcessTouchEventProcedure(touch : Pointer): FingerArray;
	begin
		LoadDefaultiOSDriver();
		result := iOSDriver.ProcessTouchEvent(touch);
	end;

	function DefaultAxisToGProcedure(value : LongInt): Single;
	begin
		LoadDefaultiOSDriver();
		result :=  iOSDriver.AxisToG(value);
	end;

	procedure DefaultInitProcedure();
	begin
    LoadDefaultiOSDriver();
    {$IFDEF IOS}
    iOSDriver.Init();
    {$ENDIF}
	end;



	initialization
	begin
		iOSDriver.ShowKeyboard						:= @DefaultShowKeyboardProcedure;
		iOSDriver.HideKeyboard						:= @DefaultHideKeyboardProcedure;
		iOSDriver.ToggleKeyboard					:= @DefaultToggleKeyboardProcedure;
		iOSDriver.IsShownKeyboard					:= @DefaultIsShownKeyboardProcedure;
		iOSDriver.Init										:= @DefaultInitProcedure;
		iOSDriver.ProcessAxisMotionEvent	:= @DefaultProcessAxisMotionEventProcedure;
		iOSDriver.AxisToG									:= @DefaultAxisToGProcedure;
		iOSDriver.ProcessTouchEvent				:= @DefaultProcessTouchEventProcedure;
   	iOSDriver.Init();
	end;
end.

