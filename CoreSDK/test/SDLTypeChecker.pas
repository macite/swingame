program SDLTypeChecker;
uses SDL, SysUtils;



   procedure outputEnum_SDL_bool();
   var
     _SDL_bool : SDL_bool;
   begin 
     WriteLn('Base Enum: SDL_bool Size: ', sizeof(SDL_bool));
     for _SDL_bool := Low(SDL_bool) to High(SDL_bool) do
     begin
        try
            WriteLn(_SDL_bool, ' Value: ', LongInt(_SDL_bool));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_DUMMY_ENUM();
   var
     _SDL_DUMMY_ENUM : SDL_DUMMY_ENUM;
   begin 
     WriteLn('Base Enum: SDL_DUMMY_ENUM Size: ', sizeof(SDL_DUMMY_ENUM));
     for _SDL_DUMMY_ENUM := Low(SDL_DUMMY_ENUM) to High(SDL_DUMMY_ENUM) do
     begin
        try
            WriteLn(_SDL_DUMMY_ENUM, ' Value: ', LongInt(_SDL_DUMMY_ENUM));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_assert_state();
   var
     _SDL_assert_state : SDL_assert_state;
   begin 
     WriteLn('Base Enum: SDL_assert_state Size: ', sizeof(SDL_assert_state));
     for _SDL_assert_state := Low(SDL_assert_state) to High(SDL_assert_state) do
     begin
        try
            WriteLn(_SDL_assert_state, ' Value: ', LongInt(_SDL_assert_state));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_errorcode();
   var
     _SDL_errorcode : SDL_errorcode;
   begin 
     WriteLn('Base Enum: SDL_errorcode Size: ', sizeof(SDL_errorcode));
     for _SDL_errorcode := Low(SDL_errorcode) to High(SDL_errorcode) do
     begin
        try
            WriteLn(_SDL_errorcode, ' Value: ', LongInt(_SDL_errorcode));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_ThreadPriority();
   var
     _SDL_ThreadPriority : SDL_ThreadPriority;
   begin 
     WriteLn('Base Enum: SDL_ThreadPriority Size: ', sizeof(SDL_ThreadPriority));
     for _SDL_ThreadPriority := Low(SDL_ThreadPriority) to High(SDL_ThreadPriority) do
     begin
        try
            WriteLn(_SDL_ThreadPriority, ' Value: ', LongInt(_SDL_ThreadPriority));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_AudioStatus();
   var
     _SDL_AudioStatus : SDL_AudioStatus;
   begin 
     WriteLn('Base Enum: SDL_AudioStatus Size: ', sizeof(SDL_AudioStatus));
     for _SDL_AudioStatus := Low(SDL_AudioStatus) to High(SDL_AudioStatus) do
     begin
        try
            WriteLn(_SDL_AudioStatus, ' Value: ', LongInt(_SDL_AudioStatus));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_BlendMode();
   var
     _SDL_BlendMode : SDL_BlendMode;
   begin 
     WriteLn('Base Enum: SDL_BlendMode Size: ', sizeof(SDL_BlendMode));
     for _SDL_BlendMode := Low(SDL_BlendMode) to High(SDL_BlendMode) do
     begin
        try
            WriteLn(_SDL_BlendMode, ' Value: ', LongInt(_SDL_BlendMode));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_WindowFlags();
   var
     _SDL_WindowFlags : SDL_WindowFlags;
   begin 
     WriteLn('Base Enum: SDL_WindowFlags Size: ', sizeof(SDL_WindowFlags));
     for _SDL_WindowFlags := Low(SDL_WindowFlags) to High(SDL_WindowFlags) do
     begin
        try
            WriteLn(_SDL_WindowFlags, ' Value: ', LongInt(_SDL_WindowFlags));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_WindowEventID();
   var
     _SDL_WindowEventID : SDL_WindowEventID;
   begin 
     WriteLn('Base Enum: SDL_WindowEventID Size: ', sizeof(SDL_WindowEventID));
     for _SDL_WindowEventID := Low(SDL_WindowEventID) to High(SDL_WindowEventID) do
     begin
        try
            WriteLn(_SDL_WindowEventID, ' Value: ', LongInt(_SDL_WindowEventID));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_GLattr();
   var
     _SDL_GLattr : SDL_GLattr;
   begin 
     WriteLn('Base Enum: SDL_GLattr Size: ', sizeof(SDL_GLattr));
     for _SDL_GLattr := Low(SDL_GLattr) to High(SDL_GLattr) do
     begin
        try
            WriteLn(_SDL_GLattr, ' Value: ', LongInt(_SDL_GLattr));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_Scancode();
   var
     _SDL_Scancode : SDL_Scancode;
   begin 
     WriteLn('Base Enum: SDL_Scancode Size: ', sizeof(SDL_Scancode));
     for _SDL_Scancode := Low(SDL_Scancode) to High(SDL_Scancode) do
     begin
        try
            WriteLn(_SDL_Scancode, ' Value: ', LongInt(_SDL_Scancode));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_Keymod();
   var
     _SDL_Keymod : SDL_Keymod;
   begin 
     WriteLn('Base Enum: SDL_Keymod Size: ', sizeof(SDL_Keymod));
     for _SDL_Keymod := Low(SDL_Keymod) to High(SDL_Keymod) do
     begin
        try
            WriteLn(_SDL_Keymod, ' Value: ', LongInt(_SDL_Keymod));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_EventType();
   var
     _SDL_EventType : SDL_EventType;
   begin 
     WriteLn('Base Enum: SDL_EventType Size: ', sizeof(SDL_EventType));
     for _SDL_EventType := Low(SDL_EventType) to High(SDL_EventType) do
     begin
        try
            WriteLn(_SDL_EventType, ' Value: ', LongInt(_SDL_EventType));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_eventaction();
   var
     _SDL_eventaction : SDL_eventaction;
   begin 
     WriteLn('Base Enum: SDL_eventaction Size: ', sizeof(SDL_eventaction));
     for _SDL_eventaction := Low(SDL_eventaction) to High(SDL_eventaction) do
     begin
        try
            WriteLn(_SDL_eventaction, ' Value: ', LongInt(_SDL_eventaction));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_HintPriority();
   var
     _SDL_HintPriority : SDL_HintPriority;
   begin 
     WriteLn('Base Enum: SDL_HintPriority Size: ', sizeof(SDL_HintPriority));
     for _SDL_HintPriority := Low(SDL_HintPriority) to High(SDL_HintPriority) do
     begin
        try
            WriteLn(_SDL_HintPriority, ' Value: ', LongInt(_SDL_HintPriority));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_LogPriority();
   var
     _SDL_LogPriority : SDL_LogPriority;
   begin 
     WriteLn('Base Enum: SDL_LogPriority Size: ', sizeof(SDL_LogPriority));
     for _SDL_LogPriority := Low(SDL_LogPriority) to High(SDL_LogPriority) do
     begin
        try
            WriteLn(_SDL_LogPriority, ' Value: ', LongInt(_SDL_LogPriority));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_PowerState();
   var
     _SDL_PowerState : SDL_PowerState;
   begin 
     WriteLn('Base Enum: SDL_PowerState Size: ', sizeof(SDL_PowerState));
     for _SDL_PowerState := Low(SDL_PowerState) to High(SDL_PowerState) do
     begin
        try
            WriteLn(_SDL_PowerState, ' Value: ', LongInt(_SDL_PowerState));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_RendererFlags();
   var
     _SDL_RendererFlags : SDL_RendererFlags;
   begin 
     WriteLn('Base Enum: SDL_RendererFlags Size: ', sizeof(SDL_RendererFlags));
     for _SDL_RendererFlags := Low(SDL_RendererFlags) to High(SDL_RendererFlags) do
     begin
        try
            WriteLn(_SDL_RendererFlags, ' Value: ', LongInt(_SDL_RendererFlags));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_TextureAccess();
   var
     _SDL_TextureAccess : SDL_TextureAccess;
   begin 
     WriteLn('Base Enum: SDL_TextureAccess Size: ', sizeof(SDL_TextureAccess));
     for _SDL_TextureAccess := Low(SDL_TextureAccess) to High(SDL_TextureAccess) do
     begin
        try
            WriteLn(_SDL_TextureAccess, ' Value: ', LongInt(_SDL_TextureAccess));
        except
        end;
     end;
   end;
    

   procedure outputEnum_SDL_TextureModulate();
   var
     _SDL_TextureModulate : SDL_TextureModulate;
   begin 
     WriteLn('Base Enum: SDL_TextureModulate Size: ', sizeof(SDL_TextureModulate));
     for _SDL_TextureModulate := Low(SDL_TextureModulate) to High(SDL_TextureModulate) do
     begin
        try
            WriteLn(_SDL_TextureModulate, ' Value: ', LongInt(_SDL_TextureModulate));
        except
        end;
     end;
   end;
    
begin
     outputEnum_SDL_bool();
     outputEnum_SDL_DUMMY_ENUM();
     outputEnum_SDL_assert_state();
     outputEnum_SDL_errorcode();
     outputEnum_SDL_ThreadPriority();
     outputEnum_SDL_AudioStatus();
     outputEnum_SDL_BlendMode();
     outputEnum_SDL_WindowFlags();
     outputEnum_SDL_WindowEventID();
     outputEnum_SDL_GLattr();
     outputEnum_SDL_Scancode();
     outputEnum_SDL_Keymod();
     outputEnum_SDL_EventType();
     outputEnum_SDL_eventaction();
     outputEnum_SDL_HintPriority();
     outputEnum_SDL_LogPriority();
     outputEnum_SDL_PowerState();
     outputEnum_SDL_RendererFlags();
     outputEnum_SDL_TextureAccess();
     outputEnum_SDL_TextureModulate();
end.