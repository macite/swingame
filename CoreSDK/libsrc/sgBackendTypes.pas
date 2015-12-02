//=============================================================================
// sgBackendTypes.pas
//=============================================================================
//
// This provides the type details for records pointed to by abstract types
// from sgTypes.
//

unit sgBackendTypes;

interface
uses sgTypes;

  //
  // The kinds of ponters we manage
  //
  const
    AUDIO_PTR = 'AUDO';
    MUSIC_PTR = 'MUSI';
    ANIMATION_PTR = 'ANIM';
    ANIMATION_SCRIPT_PTR = 'ASCR';
    NONE_PTR = 'NONE'; // done after clear


  type
    PointerIdentifier = array [0..3] of Char;

    //
    // SoundEffect -> SoundEffectData
    //
    SoundEffectData = packed record
      id: PointerIdentifier;
      effect: Pointer;
      filename, name: String;
    end;

    SoundEffectPtr = ^SoundEffectData;
    MusicPtr = ^SoundEffectData;

    AnimationFrame = ^AnimationFrameData;
    AnimationFrameData = packed record
      index:      Longint;          // The index of the frame in the animation template
      cellIndex:  Longint;          // Which cell of the current bitmap is drawn
      sound:      SoundEffect;      // Which sound should be played on entry
      duration:   Single;           // How long should this animation frame play for
      movement:   Vector;           // Movement data associated with the frame
      next:       AnimationFrame;   // What is the next frame in this animation
    end;
    
    AnimationScriptPtr = ^AnimationScriptData;

    // Details of how an animation plays out -- a sequence of frames with and animation ids (link to starting frames)
    AnimationScriptData = packed record
      id:       PointerIdentifier;
      name:     String;                       // The name of the animation template so it can be retrieved from resources
      filename: String;                       // The filename from which this template was loaded
      
      animationIds: NamedIndexCollection;     // The names and ids of the animations. This links to animations.
      animations:   LongintArray;             // The starting index of the animations in this template.
      frames:       Array of AnimationFrame;  // The frames of the animations within this template.
      
      animObjs:     Array of Pointer;         // The animations created from this script
      nextAnimIdx:  LongInt;                  // The index of the last animObjs
    end;

    AnimationPtr = ^AnimationData;

    // An individual data for an animation -- what state it is at
    AnimationData = packed record
      id:             PointerIdentifier;
      firstFrame:     AnimationFrame;       // Where did it start?
      currentFrame:   AnimationFrame;       // Where is the animation up to
      lastFrame:      AnimationFrame;       // The last frame used, so last image can be drawn
      frameTime:      Single;               // How long have we spent in this frame?
      enteredFrame:   Boolean;              // Did we just enter this frame? (can be used for sound playing)
      script:         AnimationScriptPtr;   // Which script was it created from?
      animationName:  String;               // The name of the animation - when it was started
    end;


  function ToSoundEffectPtr(s: SoundEffect): SoundEffectPtr;
  function ToMusicPtr(m: Music): MusicPtr;
  function ToAnimationScriptPtr(a: AnimationScript): AnimationScriptPtr;
  function ToAnimationPtr(a: Animation): AnimationPtr;

implementation
uses sgShared;

  function ToSoundEffectPtr(s: SoundEffect): SoundEffectPtr;
  begin
    result := SoundEffectPtr(s);
    if Assigned(result) and (result^.id <> AUDIO_PTR) then
    begin
      RaiseWarning('Attempted to access a SoundEffect that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToMusicPtr(m: Music): MusicPtr;
  begin
    result := MusicPtr(m);
    if Assigned(result) and (result^.id <> MUSIC_PTR) then
    begin
      RaiseWarning('Attempted to access a Music that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToAnimationScriptPtr(a: AnimationScript): AnimationScriptPtr;
  begin
    result := AnimationScriptPtr(a);
    if Assigned(result) and (result^.id <> ANIMATION_SCRIPT_PTR) then
    begin
      RaiseWarning('Attempted to access an Animation Script that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToAnimationPtr(a: Animation): AnimationPtr;
  begin
    result := AnimationPtr(a);
    if Assigned(result) and (result^.id <> ANIMATION_PTR) then
    begin
      RaiseWarning('Attempted to access an Animation that appears to be an invalid pointer');
      result := nil;
    end;
  end;

end.
