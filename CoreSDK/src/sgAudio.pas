//=============================================================================
// sgAudio.pas
//=============================================================================
//
// The Audio unit is responsible for managing SDL audio for music and sound
// effects. This includes initialisation, loading, freeing, playing, and
// checking if music or sound is playing.
//
// Change History:
//
// Version 3:
// - 2011-05-12: Josh   : Added PauseMusic and Resume Music
// - 2010-12-08: Andrew : Changed to RaiseWarning, and checked audio open
//                      : Added interacting with sounds and music by name
// - 2010-01-28: David  : Changed LoadSoundEffectNamed to use an already
//                                              loaded sound if found
// - 2009-11-10: Andrew : Added sn and csn tags to code
// - 2009-11-06: Andrew : Returned loading code
//                      : Added extra comments and tracing
// - 2009-07-29: Andrew : Open Audio now indicates if audio has been opened.
// - 2009-07-14: Andrew : Removed loading and freeing code.
// - 2009-06-16: Clinton: Commenting/format tweaks
// - 2009-06-04: Andrew : Finished processing comments.
//                        Added fading capabilities.
//                        Fixed comments in implementation.
// Version 2.1:
// - 2009-05-19: Andrew:  Added PlaySoundEffect with volume
//                        Added meta comments
// Version 2.0:
// - 2008-12-17: Andrew: Moved all integers to Longint
// - 2008-12-16: Andrew: Added volume controls
//
// Version 1.1:
// - 2008-03-09: Andrew: Added extra exception handling
// - 2008-01-17: Aki + Andrew: Refactor
//
// Version 1.0:
// - Various
//=============================================================================



/// SwinGame's Audio is responsible for loading and playing music and sound
/// effects. The main functionality exists in `LoadMusic`, `PlayMusic`,
/// `LoadSoundEffect`, and `PlaySoundEffect`. Associated with these are the
/// `Music` and `SoundEffect` types.
///
/// @module Audio
/// @static
///
/// @doc_types SoundEffect, Music
unit sgAudio;

//=============================================================================
interface
    uses sgTypes;
//=============================================================================

//----------------------------------------------------------------------------
// Opening and Closing Audio support
//----------------------------------------------------------------------------
    
    /// `TryOpenAudio` attempts to open the audio device for SwinGame to use.
    /// If this fails `TryOpenAudio` returns false to indicate that the audio
    /// device has not opened correctly and audio cannot be played.
    ///
    /// @lib
    function TryOpenAudio(): Boolean;
    
    /// `OpenAudio` is used to initialise the SwinGame audio code. This should be
    /// called at the start of your programs code, and is usually coded into the
    /// starting project templates. After initialising the audio code you can
    /// load and play `Music` using `LoadMusic` and `PlayMusic`, load and play
    /// `SoundEffect`s using `LoadSoundEffect` and `PlaySoundEffect`. At the end
    /// of the program you need to call `CloseAudio` to ensure that the audio
    /// code is correctly terminated.
    ///
    /// @lib
    procedure OpenAudio();
    
    /// `AudioReady` indicates if SwinGame's audio has been opened. Sound effects
    /// and Music can only be played with the audio is "ready".
    ///
    /// @lib
    function AudioReady(): Boolean;
    
    /// `CloseAudio` is used to clean up the resources used by SwinGame audio. If
    /// `OpenAudio` is called, this must be called to return the resources used
    /// before the program terminates.
    ///
    /// @lib
    procedure CloseAudio();
    
    
    
//----------------------------------------------------------------------------
// Loading & Releasing Sound Effects
//----------------------------------------------------------------------------
    
    /// Loads the `SoundEffect` from the supplied filename. The sound will be loaded
    /// from the Resources/sounds folder unless a full path to the file is passed
    /// in. If you are passing in the full path and you want to ensure that your game
    /// is able to work across multiple platforms correctly then use
    /// `PathToResource` to get the full path to the files in the projects
    /// resources folder.
    ///
    /// LoadSoundEffect can load wav and ogg audio files.
    ///
    /// `FreeSoundEffect` should be called to free the resources used by the 
    /// `SoundEffect` data once the resource is no longer needed.
    ///
    /// @param filename the filename of the sound effect file to load. 
    ///
    /// @lib
    ///
    /// @class SoundEffect
    /// @constructor
    /// @csn initFromFile:%s
    function LoadSoundEffect(filename: String): SoundEffect;
    
    /// Loads and returns a sound effect. The supplied ``filename`` is used to
    /// locate the sound effect to load. The supplied ``name`` indicates the 
    /// name to use to refer to this SoundEffect. The `SoundEffect` can then be
    /// retrieved by passing this ``name`` to the `SoundEffectNamed` function. 
    ///
    /// @lib
    /// @sn loadSoundEffectNamed:%s fromFile:%s
    ///
    /// @class SoundEffect
    /// @constructor
    /// @csn initWithName:%s fromFile:%s
    function LoadSoundEffectNamed(name, filename: String): SoundEffect;
    
    /// Determines if SwinGame has a sound effect loaded for the supplied name.
    /// This checks against all sounds loaded, those loaded without a name
    /// are assigned the filename as a default
    ///
    /// @lib
    function HasSoundEffect(name: String): Boolean;
    
    /// Returns the `SoundEffect` that has been loaded with the specified name,
    /// see `LoadSoundEffectNamed`.
    ///
    /// @lib
    function SoundEffectNamed(name: String): SoundEffect;
    
    /// Releases the SwinGame resources associated with the sound effect of the
    /// specified ``name``.
    ///
    /// @lib
    procedure ReleaseSoundEffect(name: String);
    
    /// Releases all of the sound effects that have been loaded.
    ///
    /// @lib
    procedure ReleaseAllSoundEffects();
    
    /// Frees the resources used by a `SoundEffect` resource. All loaded
    /// `SoundEffect`s should be freed once they are no longer needed.
    ///
    /// @lib
    ///
    /// @class SoundEffect
    /// @dispose
    procedure FreeSoundEffect(var effect: SoundEffect);
    
    
    
//----------------------------------------------------------------------------
// Loading & Releasing Music
//----------------------------------------------------------------------------
    
    /// Loads and returns a music value. The supplied ``filename`` is used to
    /// locate the music file to load. The supplied ``name`` indicates the 
    /// name to use to refer to this Music value. The `Music` can then be
    /// retrieved by passing this ``name`` to the `MusicNamed` function. 
    ///
    /// @lib
    /// @sn loadMusicNamed:%s fromFile:%s
    ///
    /// @class Music
    /// @constructor
    /// @csn initWithName:%s fromFile:%s
    function LoadMusicNamed(name, filename: String): Music;
    
    /// Determines if SwinGame has a music value loaded for the supplied name.
    /// This checks against all music values loaded using `LoadMusicNamed`.
    ///
    /// @lib
    function HasMusic(name: String): Boolean;
    
    /// Returns the `Music` that has been loaded with the specified name.
    /// This works with music data loaded using `LoadMusicNamed`.
    ///
    /// @lib
    function MusicNamed(name: String): Music;
    
    /// Releases the music that have been loaded with the supplied name.
    ///
    /// @lib
    procedure ReleaseMusic(name: String);
    
    /// Releases all of the music data that have been loaded.
    ///
    /// @lib
    procedure ReleaseAllMusic();
    
    /// Loads the `Music` from the supplied filename. The music will be loaded
    /// from the Resources/sounds folder unless a full path to the file is passed
    /// in. If you are passing in the full path and you want toensure that your game
    /// is able to work across multiple platforms correctly ensure that you use
    /// `PathToResource` to get the full path to the files in the projects 
    /// resources folder.
    ///
    /// LoadMusic can load mp3, wav and ogg audio files.
    ///
    /// `FreeMusic` should be called to free the resources used by the 
    /// `Music` data once the resource is no longer needed.
    ///
    /// @param filename the filename to the music file to load.
    ///
    /// @lib
    ///
    /// @class Music
    /// @constructor
    /// @csn initFromFile:%s
    function LoadMusic(filename: String): Music;
    
    /// Frees the resources used by a `Music` resource. All loaded
    /// `Music` should be freed once it is no longer needed. 
    ///
    /// @lib
    ///
    /// @class Music
    /// @dispose
    procedure FreeMusic(var mus: Music);
    
    
    
//----------------------------------------------------------------------------
// Playing Sound Effects
//----------------------------------------------------------------------------
    
    /// There are several versions of PlaySoundEffect that can be used to control
    /// the way the sound effect plays, allowing you to control its volume and 
    /// the number of times the code loops. In all cases the started sound effect
    /// is mixed with the currently playing sound effects and music.
    ///
    /// With this version of PlaySoundEffect, the started sound effect will be 
    /// played at full volume.
    ///
    /// @param effect The effect indicates which sound effect to start playing. This
    ///               effect is played once at its full volume.
    ///
    /// @lib PlaySoundEffectWithLoopAndVolume(effect,1,1.0)
    /// @uname PlaySoundEffect
    ///
    /// @class SoundEffect
    /// @method Play
    ///
    /// @doc_idx 1
    procedure PlaySoundEffect(effect: SoundEffect); overload;
        
    /// This version of PlaySoundEffect allows you to indicate the number of times
    /// the sound effect is repeated. Setting the loops parameter to -1 will cause
    /// the sound effect to be looped infinitely, setting it to a value larger than
    /// 0 plays the sound effect the number of times indicated, calling with a 
    /// value of 0 means the sound effect is not played.
    ///
    /// @param effect The effect indicates which sound effect to start playing. This
    ///               effect is played once at its full volume.
    ///
    /// @param loops Controls the number of times the sound effect is played. -1
    ///              means the sound effect is repeated infinitely.
    ///
    /// @lib PlaySoundEffectWithLoopAndVolume(effect, loops, 1.0)
    /// @uname PlaySoundEffectWithLoop
    /// @sn playSoundEffect:%s looped:%s
    ///
    /// @class SoundEffect
    /// @overload Play PlayWithLoops
    /// @csn playLooped:%s
    ///
    /// @doc_details
    procedure PlaySoundEffect(effect: SoundEffect; loops: Longint); overload;
                
    /// This version of PlaySoundEffect allows you to control the volume of the 
    /// sounds playback. The vol parameter will take a value between 0 and 1 
    /// indicating the percentage of full volume to play at.
    /// For example, 0.1 plays the sound effect at 10% of its original volume.
    ///
    /// @param effect The effect indicates which sound effect to start playing. 
    /// @param vol Indicates the percentage of the original volume to play the 
    ///            `SoundEffect` at. This must be between 0 and 1.
    ///
    /// @lib PlaySoundEffectWithLoopAndVolume(effect, 1, vol)
    /// @uname PlaySoundEffectWithVolume
    /// @sn playSoundEffect:%s atVolume:%s
    /// @version 2.1
    ///
    /// @class SoundEffect
    /// @overload Play PlayWithVolume
    /// @csn playVolume:%s
    ///
    /// @doc_details
    procedure PlaySoundEffect(effect: SoundEffect; vol: Single); overload;
    
    /// This version of PlaySoundEffect allows you to control both the number
    /// of times the `SoundEffect` is repeated, and its playback volume.
    ///
    /// @param effect The effect indicates which sound effect to start playing. 
    /// @param loops Controls the number of times the sound effect is played.
    /// @param vol Indicates the percentage of the original volume to play the 
    ///                      `SoundEffect` at. This must be between 0 and 1.
    ///
    /// @lib PlaySoundEffectWithLoopAndVolume
    /// @sn playSoundEffect:%s looped:%s atVolume:%s
    /// @version 2.0
    ///
    /// @class SoundEffect
    /// @overload Play PlayWithLoopsAndVolume
    /// @csn playLooped:%s vol:%s
    ///
    /// @doc_details
    procedure PlaySoundEffect(effect: SoundEffect; loops: Longint; vol: Single); overload;
    
    /// This version of PlaySoundEffect allows you to control both the number
    /// of times the `SoundEffect` is repeated, and its playback volume.
    ///
    /// @param name The name of the sound effect to start playing. 
    /// @param loops Controls the number of times the sound effect is played.
    /// @param vol Indicates the percentage of the original volume to play the 
    ///                      `SoundEffect` at. This must be between 0 and 1.
    ///
    /// @lib PlaySoundEffectNamedWithLoopAndVolume
    /// @sn playSoundEffectNamed:%s looped:%s atVolume:%s
    ///
    /// @doc_details
    procedure PlaySoundEffect(name: String; loops: Longint; vol: Single); overload;
    
    /// This version of PlaySoundEffect allows you to indicate the number of times
    /// the sound effect is repeated. Setting the loops parameter to -1 will cause
    /// the sound effect to be looped infinitely, setting it to a value larger than
    /// 0 plays the sound effect the number of times indicated, calling with a 
    /// value of 0 means the sound effect is not played.
    ///
    /// @param name The name of the sound effect to start playing.
    ///
    /// @param loops Controls the number of times the sound effect is played. -1
    ///                          means the sound effect is repeated infinitely.
    ///
    /// @lib PlaySoundEffectNamedWithLoopAndVolume(name, loops, 1.0)
    /// @uname PlaySoundEffectNamedWithLoop
    /// @sn playSoundEffectNamed:%s looped:%s
    ///
    /// @doc_details
    procedure PlaySoundEffect(name: String; loops: Longint); overload;
    
    /// There are several versions of PlaySoundEffect that can be used to control
    /// the way the sound effect plays, allowing you to control its volume and 
    /// the number of times the code loops. In all cases the started sound effect
    /// is mixed with the currently playing sound effects and music.
    ///
    /// With this version of PlaySoundEffect, the started sound effect will be 
    /// played at full volume.
    ///
    /// @param name The name of the sound effect to play
    ///
    /// @lib PlaySoundEffectNamedWithLoopAndVolume(name,1,1.0)
    /// @uname PlaySoundEffectNamed
    procedure PlaySoundEffect(name: String); overload;
    
    /// This version of PlaySoundEffect allows you to control the volume of the 
    /// sounds playback. The vol parameter will take a value between 0 and 1 
    /// indicating the percentage of full volume to play at.
    /// For example, 0.1 plays the sound effect at 10% of its original volume.
    ///
    /// @param name The name of the sound effect to play.
    /// @param vol Indicates the percentage of the original volume to play the 
    ///                      `SoundEffect` at. This must be between 0 and 1.
    ///
    /// @lib PlaySoundEffectNamedWithLoopAndVolume(name, 1, vol)
    /// @uname PlaySoundEffectNamedWithVolume
    /// @sn playSoundEffectNamed:%s atVolume:%s
    ///
    /// @doc_details
    procedure PlaySoundEffect(name: String; vol: Single); overload;
    
    
    
//----------------------------------------------------------------------------
// Playing Music
//----------------------------------------------------------------------------
    
    /// PlayMusic starts playing a `Music` resource. SwinGame only allows one 
    /// music resource to be played at a time. Starting to play a new music 
    /// resource will stop the currently playing music track. You can also stop
    /// the music by calling `StopMusic`.
    ///
    /// By default SwinGame starts playing music at its full volume. This can be 
    /// controlled by calling `SetMusicVolume`. The current volume can be checked 
    /// with `MusicVolume`.
    ///
    /// To test if a `Music` resource is currently playing you can use the 
    /// `MusicPlaying` function.
    ///
    /// This version of PlayMusic can be used to play background music that is 
    /// looped infinitely. The currently playing music is stopped and the new 
    /// music resource will start playing, and will repeat until `StopMusic` is 
    /// called, or another resource is played. 
    ///
    /// @param mus The `Music` resource to play.
    ///
    /// @lib PlayMusicWithLoops(mus, -1)
    /// @uname PlayMusic
    ///
    /// @class Music
    /// @method Play
    procedure PlayMusic(mus: Music); overload;
    
    /// This version of PlayMusic allows you to control the number of times the 
    /// `Music` resource is repeated. It starts playing the supplied `Music` 
    /// resource, repeating it the numder of times specified in the loops 
    /// parameter. Setting loops to -1 repeats the music infinitely, other values
    /// larger than 0 indicate the number of times that the music should be 
    /// played.
    ///
    /// @param mus The `Music` resource to be played.
    /// @param loops The number of times that the music should be played, -1 for 
    ///              repeat infinitely
    ///
    /// @lib PlayMusicWithLoops
    /// @sn playMusic:%s looped:%s
    ///
    /// @class Music
    /// @overload Play PlayWithLoops
    procedure PlayMusic(mus: Music; loops: Longint); overload;
    
    /// Fades the music in over a number of milliseconds, and then continues to
    /// play the music repeatedly until the program ends or the music is stopped. 
    /// The music fades from 0 volume up to the currently set music volume.
    /// 
    /// @param mus The `Music` resource to be played.
    /// @param ms The number of milliseconds over which to fade the music in to 
    //            the current music volume.
    ///
    /// @lib FadeMusicIn
    /// @sn playMusic:%s fadeIn:%s
    ///
    /// @class Music
    /// @method FadeIn
    /// @csn playFadeIn:%s
    procedure FadeMusicIn(mus: Music; ms: Longint); overload;

    /// This version of FadeMusicIn fades the music in then plays the 'Music' 
    /// for a given number of loops.Setting loops to -1 repeats the music 
    /// infinitely, other values larger than 0 indicate the number of times that
    /// the music should be played.
    ///
    /// @param mus The `Music` resource to be played.
    /// @param loops The number of times that the music should be played, -1 for 
    ///              repeat infinitely
    /// @param ms The number of milliseconds over which to fade the music in to 
    ///           the current music volume.
    ///
    /// @lib FadeMusicInWithLoops
    /// @sn playMusic:%s looped:%s fadeIn:%s
    ///
    /// @class Music
    /// @overload FadeIn FadeInWithLoops
    /// @csn playLooped:%s fadeIn:%s
    procedure FadeMusicIn(mus: Music; loops, ms: Longint); overload;
    
    /// This version of PlayMusic allows you to control the number of times the 
    /// `Music` resource is repeated. It starts playing the supplied `Music` 
    /// resource, repeating it the numder of times specified in the loops 
    /// parameter. Setting loops to -1 repeats the music infinitely, other values
    /// larger than 0 indicate the number of times that the music should be 
    /// played.
    ///
    /// @param name The name of the `Music` resource to be played.
    /// @param loops The number of times that the music should be played, -1 for 
    ///              repeat infinitely
    ///
    /// @lib PlayMusicNamedWithLoops
    /// @sn playMusicNamed:%s looped:%s
    procedure PlayMusic(name: String; loops: Longint); overload;
    
    /// PlayMusic starts playing a `Music` resource. SwinGame only allows one 
    /// music resource to be played at a time. Starting to play a new music 
    /// resource will stop the currently playing music track. You can also stop
    /// the music by calling `StopMusic`.
    ///
    /// By default SwinGame starts playing music at its full volume. This can be 
    /// controlled by calling `SetMusicVolume`. The current volume can be checked 
    /// with `MusicVolume`.
    ///
    /// To test if a `Music` resource is currently playing you can use the 
    /// `MusicPlaying` function.
    ///
    /// This version of PlayMusic can be used to play background music that is 
    /// looped infinitely. The currently playing music is stopped and the new 
    /// music resource will start playing, and will repeat until `StopMusic` is 
    /// called, or another resource is played. 
    ///
    /// @param name The name of the `Music` resource to play.
    ///
    /// @lib PlayMusicNamedWithLoops(name, -1)
    /// @uname PlayMusicNamed
    procedure PlayMusic(name: String); overload;
    
    /// This version of FadeMusicIn fades the music in then plays the 'Music' 
    /// for a given number of loops.Setting loops to -1 repeats the music 
    /// infinitely, other values larger than 0 indicate the number of times that
    /// the music should be played.
    ///
    /// @param name The name of the `Music` resource to be played.
    /// @param loops The number of times that the music should be played, -1 for 
    ///              repeat infinitely
    /// @param ms The number of milliseconds over which to fade the music in to 
    ///           the current music volume.
    ///
    /// @lib FadeMusicNamedInWithLoops
    /// @sn playMusicNamed:%s looped:%s fadeIn:%s
    procedure FadeMusicIn(name: String; loops, ms: Longint); overload;
    
    /// Fades the music in over a number of milliseconds, and then continues to
    /// play the music repeatedly until the program ends or the music is stopped. 
    /// The music fades from 0 volume up to the currently set music volume.
    /// 
    /// @param name The name of the `Music` resource to be played.
    /// @param ms The number of milliseconds over which to fade the music in to 
    //            the current music volume.
    ///
    /// @lib FadeMusicNamedIn
    /// @sn playMusicNamed:%s fadeIn:%s
    procedure FadeMusicIn(name: String; ms: Longint); overload;
    
    /// Pauses the currently playing music. See `ResumeMusic`.
    ///
    /// @lib
    procedure PauseMusic();
    
    /// Resume currently paused music. See `PauseMusic`.
    ///
    /// @lib
    procedure ResumeMusic();
    
    
//----------------------------------------------------------------------------
// Query music
//----------------------------------------------------------------------------
    
    /// This procedure allows you to set the volume of the currently playing 
    /// music. The vol parameter indicates the percentage of the original volume,
    /// for example, 0.1 sets the playback volume to 10% of its full volume.
    ///
    /// @param value Indicates the percentage of the original volume to play the 
    ///              `Music` at. This must be between 0 and 1, e.g. 0.1 is 10%.
    ///
    /// @lib SetMusicVolume
    ///
    /// @class Music
    /// @static
    /// @setter Volume
    procedure SetMusicVolume(value: Single);
    
    /// This function returns the current volume of the music. This will be a 
    /// value between 0 and 1, with 1 indicating 100% of the `Music` resources
    /// volume.
    ///
    /// @returns The volume of the currently playing music.
    ///
    /// @lib MusicVolume
    ///
    /// @class Music
    /// @static
    /// @getter Volume
    function MusicVolume(): Single;
    
    /// This function indicates if music is currently playing. As only one music 
    /// resource can be playing at a time this does not need to be told which
    /// music resource to check for.
    ///
    /// @returns true if the music is playing
    ///
    /// @lib MusicPlaying
    ///
    /// @class Music
    /// @static
    /// @method IsPlaying
    function MusicPlaying(): Boolean;
    
    /// Returns the name that SwinGame uses to refer to this music data. This
    /// name can be used to fetch and release this music resource.
    ///
    /// @lib
    ///
    /// @class Music
    /// @getter Name
    function MusicName(mus: Music): String;
    
    /// Returns the filename that SwinGame uses to load to this music data.
    ///
    /// @lib
    ///
    /// @class Music
    /// @getter Filename
    function MusicFilename(mus: Music): String;
    
    
    
//----------------------------------------------------------------------------
// Query Sound Effects
//----------------------------------------------------------------------------
    
    /// This function can be used to check if a sound effect is currently 
    /// playing. 
    ///
    /// @param effect The sound effect to check.
    /// @returns true if the effect `SoundEffect` is playing.
    ///
    /// @lib SoundEffectPlaying
    ///
    /// @class SoundEffect
    /// @method IsPlaying
    function SoundEffectPlaying(effect: SoundEffect): Boolean; overload;
    
    /// This function can be used to check if a sound effect is currently 
    /// playing. 
    ///
    /// @param name The name of the sound effect to check.
    /// @returns true if the effect `SoundEffect` is playing.
    ///
    /// @lib SoundEffectNamedPlaying
    function SoundEffectPlaying(name: String): Boolean; overload;
    
    /// Returns the name that SwinGame uses to refer to this sound effect. This
    /// name can be used to fetch and release this sound effect resource.
    ///
    /// @lib
    ///
    /// @class SoundEffect
    /// @getter Name
    function SoundEffectName(effect: SoundEffect): String;
    
    /// Returns the filename that SwinGame used to load to this sound effect.
    ///
    /// @lib
    ///
    /// @class SoundEffect
    /// @getter Filename
    function SoundEffectFilename(effect: SoundEffect): String;
    
    
    
//----------------------------------------------------------------------------
// Stop Music & Sound Effects
//----------------------------------------------------------------------------
    
    /// Stops all occurances of the effect `SoundEffect` that is currently playing.
    ///
    /// @param effect The sound to stop.
    ///
    /// @lib StopSoundEffect
    ///
    /// @class SoundEffect
    /// @method Stop
    procedure StopSoundEffect(effect: SoundEffect);
    
    /// Stops all occurances of the named `SoundEffect` that are currently playing.
    ///
    /// @param name The name of the sound effect to stop.
    ///
    /// @lib StopSoundEffectNamed
    procedure StopSoundEffect(name: String);
    
    /// Stops playing the current music resource.
    ///
    /// @lib StopMusic
    ///
    /// @class Music
    /// @static
    /// @method Stop
    procedure StopMusic();
    
    /// Fades the currently playing music out over a number of milli seconds.
    ///
    /// @param ms The number of milliseconds over which to fade the music to 0 volume.
    ///
    /// @lib FadeMusicOut
    ///
    /// @class Music
    /// @static
    /// @method FadeOut
    procedure FadeMusicOut(ms: Longint);
    
    
    
//=============================================================================
implementation
    uses
        SysUtils, Classes, 
        stringhash,                 // libsrc
        sgShared, sgResources, sgTrace, sgDriverAudio;
//=============================================================================

    var
        _SoundEffects: TStringHash;
        _Music: TStringHash;
    
    function TryOpenAudio(): Boolean;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'TryOpenAudio', '');
        {$ENDIF}
        
        sgShared.AudioOpen := AudioDriver.OpenAudio();
        result := sgShared.AudioOpen;

        {$IFDEF TRACE}
            TraceExit('sgAudio', 'TryOpenAudio', BoolToStr(result, true));
        {$ENDIF}
    end;
    
    procedure OpenAudio();
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'OpenAudio', '');
        {$ENDIF}
        
        if not TryOpenAudio() then RaiseException('Error opening audio device: ' + string(AudioDriver.GetError()));
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'OpenAudio');
        {$ENDIF}
    end;
    
    function AudioReady(): Boolean;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'AudioReady', '');
        {$ENDIF}
        
        result := sgShared.AudioOpen;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'AudioReady', BoolToStr(result, true));
        {$ENDIF}
    end;
    
    procedure CloseAudio();
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'CloseAudio', '');
        {$ENDIF}
        
        AudioOpen := False;
        AudioDriver.CloseAudio();
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'CloseAudio');
        {$ENDIF}
    end;
    
    procedure SetMusicVolume(value: Single);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'SetMusicVolume', FloatToStr(value));
        {$ENDIF}
        
        if (value < 0) then value := 0
        else if value > 1 then value := 1;

        AudioDriver.SetMusicVolume(value);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'SetMusicVolume');
        {$ENDIF}
    end;
    
    function MusicVolume(): Single;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'MusicVolume', '');
        {$ENDIF}
        
     result := AudioDriver.GetMusicVolume();
     
     {$IFDEF TRACE}
         TraceExit('sgAudio', 'MusicVolume', FloatToStr(result));
     {$ENDIF}
    end;
    
    //----------------------------------------------------------------------------
    
    // Private:
    // Called by LoadSoundEffectNamed
    function DoLoadSoundEffect(filename, name: String): SoundEffect;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'DoLoadSoundEffect', name + ' = ' + filename);
        {$ENDIF}
        
        if not FileExists(filename) then
        begin
            filename := PathToResource(filename, SoundResource);
            if not FileExists(filename) then
            begin
                RaiseWarning('Unable to locate ' + name + ' sound effect file at ' + filename);
                result := nil;
                exit;
            end;
        end;
		
		result := AudioDriver.LoadSoundEffect(filename, name);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'DoLoadSoundEffect', HexStr(result));
        {$ENDIF}
    end;
    
    function LoadSoundEffect(filename: String): SoundEffect;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'LoadSoundEffect', filename);
        {$ENDIF}
        
        result := LoadSoundEffectNamed(filename, filename);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'LoadSoundEffect');
        {$ENDIF}
    end;
    
    function LoadSoundEffectNamed(name, filename: String): SoundEffect;
    var
        obj: tResourceContainer;
        snd: SoundEffect;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'LoadSoundEffectNamed', name + ' = ' + filename);
        {$ENDIF}
        
        if not AudioReady() then
        begin
            {$IFDEF TRACE}
                TraceExit('sgAudio', 'LoadSoundEffectNamed', 'Audio Closed');
            {$ENDIF}
            result := nil;
            exit;
        end;
        
        if _SoundEffects.containsKey(name) then
        begin
            result := SoundEffectNamed(name);
            exit;
        end;
        
        snd := DoLoadSoundEffect(filename, name);
        if not assigned(snd) then
        begin
            result := nil;
            exit;
        end;
        
        obj := tResourceContainer.Create(snd);
        
        if not _SoundEffects.setValue(name, obj) then
        begin
            RaiseWarning('** Leaking: Caused by Sound Effect resource loading twice, ' + name);
            result := nil;
            exit;
        end;
        result := snd;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'LoadSoundEffectNamed');
        {$ENDIF}
    end;
    
    // private:
    // Called to actually free the resource
    procedure DoFreeSoundEffect(var effect: SoundEffect);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'DoFreeSoundEffect', 'effect = ' + HexStr(effect));
        {$ENDIF}
        
        if assigned(effect) then
        begin
            CallFreeNotifier(effect);
            AudioDriver.FreeSoundEffect(effect);
            Dispose(effect);
        end;
        effect := nil;
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'DoFreeSoundEffect');
        {$ENDIF}
    end;
    
    procedure FreeSoundEffect(var effect: SoundEffect);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'FreeSoundEffect', 'effect = ' + HexStr(effect));
        {$ENDIF}
        
        if(assigned(effect)) then
        begin
            ReleaseSoundEffect(effect^.name);
        end;
        effect := nil;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'FreeSoundEffect');
        {$ENDIF}
    end;
    
    procedure ReleaseSoundEffect(name: String);
    var
        snd: SoundEffect;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'ReleaseSoundEffect', 'effect = ' + name);
        {$ENDIF}
        
        snd := SoundEffectNamed(name);
        if (assigned(snd)) then
        begin
            _SoundEffects.remove(name).Free();
            DoFreeSoundEffect(snd);
        end;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'ReleaseSoundEffect');
        {$ENDIF}
    end;
    
    procedure ReleaseAllSoundEffects();
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'ReleaseAllSoundEffects', '');
        {$ENDIF}
        
        ReleaseAll(_SoundEffects, @ReleaseSoundEffect);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'ReleaseAllSoundEffects');
        {$ENDIF}
    end;
    
    //----------------------------------------------------------------------------
    
    function DoLoadMusic(filename, name: String): Music;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'DoLoadMusic', name + ' = ' + filename);
        {$ENDIF}
        
        if not FileExists(filename) then
        begin
            filename := PathToResource(filename, SoundResource);
            if not FileExists(filename) then
            begin
                RaiseWarning('Unable to locate ' + name + ' music file at ' + filename);
                result := nil;
                exit;
            end;
        end;
		
		result := AudioDriver.LoadMusic(filename, name);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'DoLoadMusic');
        {$ENDIF}
    end;
    
    function LoadMusic(filename: String): Music;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'LoadMusic', filename);
        {$ENDIF}
        
        result := LoadMusicNamed(filename, filename);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'LoadMusic');
        {$ENDIF}
    end;
    
    function LoadMusicNamed(name, filename: String): Music;
    var
        obj: tResourceContainer;
        mus: Music;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'LoadMusicNamed', name + ' = ' + filename);
        {$ENDIF}
        
        if not AudioReady() then
        begin
            {$IFDEF TRACE}
                TraceExit('sgAudio', 'LoadMusicNamed', 'Audio Closed');
            {$ENDIF}
            result := nil;
            exit;
        end;
        
        
        if _Music.containsKey(name) then
        begin
            RaiseWarning('Error loaded Music resource twice, ' + name);
            result := nil;
            exit;
        end;
        
        mus := DoLoadMusic(filename, name);
        if not assigned(mus) then
        begin
            result := nil;
            exit;
        end;
        
        obj := tResourceContainer.Create(mus);
        
        if not _Music.setValue(name, obj) then
        begin
            RaiseWarning('** Leaking due to loading Music resource twice, ' + name);
            exit;
        end;
        
        result := mus;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'LoadMusicNamed');
        {$ENDIF}
    end;
    
    procedure DoFreeMusic(var mus: Music);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'DoFreeMusic', 'mus = ' + HexStr(mus));
        {$ENDIF}
        
        if assigned(mus) then
        begin
            CallFreeNotifier(mus);
            AudioDriver.FreeMusic(mus);
        end;
        mus := nil;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'DoFreeMusic');
        {$ENDIF}        
    end;
    
    procedure FreeMusic(var mus: Music);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'FreeMusic', 'mus = ' + HexStr(mus));
        {$ENDIF}
        
        if assigned(mus) then ReleaseMusic(mus^.name);
        mus := nil;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'FreeMusic');
        {$ENDIF}
    end;
    
    procedure ReleaseMusic(name: String);
    var
        mus: Music;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'ReleaseMusic', name);
        {$ENDIF}
        
        mus := MusicNamed(name);
        if (assigned(mus)) then
        begin
            _Music.remove(name).Free();
            DoFreeMusic(mus);
        end;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'ReleaseMusic');
        {$ENDIF}
    end;
    
    procedure ReleaseAllMusic();
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'ReleaseAllMusic', '');
        {$ENDIF}
        
        ReleaseAll(_Music, @ReleaseMusic);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'ReleaseAllMusic');
        {$ENDIF}
    end;
    
    
    
    //----------------------------------------------------------------------------
    
    procedure PlaySoundEffect(name: String; loops: Longint; vol: Single); overload;
    begin
        PlaySoundEffect(SoundEffectNamed(name), loops, vol);
    end;
    
    procedure PlaySoundEffect(effect: SoundEffect; loops: Longint; vol: Single); overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'PlaySoundEffect', HexStr(effect) + ' loops = ' + IntToStr(loops) + ' vol = ' + FloatToStr(vol));
        {$ENDIF}
        
        if not AudioReady() then
        begin
            {$IFDEF TRACE}
                TraceExit('sgAudio', 'PlaySoundEffect', 'Audio Closed');
            {$ENDIF}
            exit;
        end;
        
        if not Assigned(effect) then
        begin
            {$IFDEF TRACE}
                TraceExit('sgAudio', 'PlaySoundEffect', 'No sound effect supplied.');
            {$ENDIF}
            RaiseWarning('No sound effect supplied to PlaySoundEffect');
            exit;
        end;
        
        //dont play if loops = 0
        if loops = 0 then exit;
        
        //correct volume to be between 0 and 1
        if (vol < 0) then vol := 0
        else if vol > 1 then vol := 1;
        
        //alter repeats for multiple loops
        if loops >= 1 then loops := loops- 1;
        
        //play the effect, seaching for a channel
        if (not AudioDriver.PlaySoundEffect(effect, loops, vol)) then RaiseWarning('Error playing sound effect' + AudioDriver.GetError());

        {$IFDEF TRACE}
            TraceExit('sgAudio', 'PlaySoundEffect');
        {$ENDIF}        
    end;
    
    procedure PlaySoundEffect(name: String; loops: Longint); overload;
    begin
        PlaySoundEffect(SoundEffectNamed(name), loops);
    end;
    
    procedure PlaySoundEffect(effect: SoundEffect; loops: Longint); overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'PlaySoundEffect', HexStr(effect) + ' loops = ' + IntToStr(loops));
        {$ENDIF}
        
        PlaySoundEffect(effect, loops, 1.0);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'PlaySoundEffect');
        {$ENDIF}
    end;
    
    procedure PlaySoundEffect(name: String); overload;
    begin
        PlaySoundEffect(SoundEffectNamed(name));
    end;
    
    procedure PlaySoundEffect(effect: SoundEffect); overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'PlaySoundEffect', HexStr(effect));
        {$ENDIF}
        
        PlaySoundEffect(effect, 1, 1.0);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'PlaySoundEffect');
        {$ENDIF}        
    end;
    
    procedure PlaySoundEffect(name: String; vol: Single); overload;
    begin
        PlaySoundEffect(SoundEffectNamed(name), vol);
    end;
    
    procedure PlaySoundEffect(effect: SoundEffect; vol: Single); overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'PlaySoundEffect', HexStr(effect) + ' vol = ' + FloatToStr(vol));
        {$ENDIF}
        
     PlaySoundEffect(effect, 1, vol);
     
     {$IFDEF TRACE}
         TraceExit('sgAudio', 'PlaySoundEffect');
     {$ENDIF}
    end;
    
    
    //----------------------------------------------------------------------------
    
    procedure PlayMusic(name: String; loops: Longint); overload;
    begin
        PlayMusic(MusicNamed(name), loops);
    end;
    
    procedure PlayMusic(mus: Music; loops: Longint); overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'PlayMusic', HexStr(mus) + ' loops = ' + IntToStr(loops));
        {$ENDIF}
        
        if not AudioReady() then
        begin
            {$IFDEF TRACE}
                TraceExit('sgAudio', 'PlayMusic', 'Audio Closed');
            {$ENDIF}
            exit;
        end;
        
        if not Assigned(mus) then 
		begin 
			RaiseWarning('Music not supplied to PlayMusic'); 
			exit; 
		end;
        AudioDriver.PlayMusic(mus, loops);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'PlayMusic');
        {$ENDIF}        
    end;
    
    procedure PlayMusic(name: String); overload;
    begin
        PlayMusic(MusicNamed(name));
    end;
    
    procedure PlayMusic(mus: Music); overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'PlayMusic', HexStr(mus));
        {$ENDIF}
        
        PlayMusic(mus, -1);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'PlayMusic');
        {$ENDIF}        
    end;
    
    procedure FadeMusicIn(name: String; loops, ms: Longint); overload;
    begin
        FadeMusicIn(MusicNamed(name), loops, ms);
    end;
    
    procedure FadeMusicIn(mus: Music; loops, ms: Longint); overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'FadeMusicIn', HexStr(mus) + ' loops = ' + IntToStr(loops) + ' ms = ' + IntToStr(ms));
        {$ENDIF}
        
        if not AudioReady() then
        begin
            {$IFDEF TRACE}
                TraceExit('sgAudio', 'FadeMusicIn', 'Audio Closed');
            {$ENDIF}
            exit;
        end;
        
        if not Assigned(mus) then 
		begin 
			RaiseWarning('Music not supplied to FadeMusicIn'); 
			exit; 
		end;
        AudioDriver.FadeMusicIn(mus, loops, ms);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'FadeMusicIn');
        {$ENDIF}
    end;
    
    procedure FadeMusicIn(name: String; ms: Longint); overload;
    begin
        FadeMusicIn(MusicNamed(name), ms);
    end;
    
    procedure FadeMusicIn(mus: Music; ms: Longint); overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'FadeMusicIn', HexStr(mus) + ' ms = ' + IntToStr(ms));
        {$ENDIF}
        
        FadeMusicIn(mus, -1, ms);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'FadeMusicIn');
        {$ENDIF}
    end;
    
    procedure FadeMusicOut(ms: Longint);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'FadeMusicOut', IntToStr(ms));
        {$ENDIF}
        
        if not AudioReady() then
        begin
            {$IFDEF TRACE}
                TraceExit('sgAudio', 'FadeMusicOut', 'Audio Closed');
            {$ENDIF}
            exit;
        end;
        
        AudioDriver.FadeMusicOut(ms);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'FadeMusicOut');
        {$ENDIF}
    end;
    
    function SoundEffectPlaying(name: String): Boolean; overload;
    begin
        result := SoundEffectPlaying(SoundEffectNamed(name));
    end;
    
    function SoundEffectPlaying(effect: SoundEffect): Boolean; overload;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'SoundEffectPlaying', HexStr(effect));
        {$ENDIF}
        
        result := false;
        
        if AudioReady() then
        begin
            result := AudioDriver.SoundEffectPlaying(effect);
        end;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'SoundEffectPlaying', BoolToStr(result, true));
        {$ENDIF}
    end;
    
    function MusicPlaying(): Boolean;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'MusicPlaying', '');
        {$ENDIF}
        
        if AudioReady() then
            result := AudioDriver.MusicPlaying()
        else
            result := false;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'MusicPlaying', BoolToStr(result, true));
        {$ENDIF}
    end;
    
    procedure PauseMusic();
    begin
        AudioDriver.PauseMusic();
    end;
    
    procedure ResumeMusic();
    begin
        AudioDriver.ResumeMusic();
    end;
    
    
    //----------------------------------------------------------------------------
    
    function HasSoundEffect(name: String): Boolean;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'HasSoundEffect', name);
        {$ENDIF}
        
        result := _SoundEffects.containsKey(name);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'HasSoundEffect', BoolToStr(result, true));
        {$ENDIF}
    end;

    function SoundEffectNamed(name: String): SoundEffect;
    var
        tmp : TObject;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'SoundEffectNamed', name);
        {$ENDIF}
        
        tmp := _SoundEffects.values[name];
        if assigned(tmp) then result := SoundEffect(tResourceContainer(tmp).Resource)
        else result := nil;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'SoundEffectNamed', HexStr(result));
        {$ENDIF}
    end;
    
    function SoundEffectName(effect: SoundEffect): String;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'SoundEffectName', HexStr(effect));
        {$ENDIF}
        
        if assigned(effect) then result := effect^.name
        else result := '';
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'SoundEffectName', result);
        {$ENDIF}
    end;
    
    function SoundEffectFilename(effect: SoundEffect): String;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'SoundEffectFilename', HexStr(effect));
        {$ENDIF}
        
        if assigned(effect) then result := effect^.filename
        else result := '';
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'SoundEffectFilename', result);
        {$ENDIF}
    end;
    
    
    //----------------------------------------------------------------------------
    
    function HasMusic(name: String): Boolean;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'HasMusic', name);
        {$ENDIF}
        
        result := _Music.containsKey(name);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'HasMusic', BoolToStr(result, true));
        {$ENDIF}
    end;
    
    function MusicNamed(name: String): Music;
    var
        tmp : TObject;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'MusicNamed', name);
        {$ENDIF}
        
        tmp := _Music.values[name];
        if assigned(tmp) then result := Music(tResourceContainer(tmp).Resource)
        else result := nil;
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'MusicNamed', HexStr(result));
        {$ENDIF}
    end;
    
    function MusicName(mus: Music): String;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'MusicName', HexStr(mus));
        {$ENDIF}
        
        if assigned(mus) then result := mus^.name
        else result := '';
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'MusicName', result);
        {$ENDIF}
    end;
    
    function MusicFilename(mus: Music): String;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'MusicFilename', HexStr(mus));
        {$ENDIF}
        
        if assigned(mus) then result := mus^.filename
        else result := '';
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'MusicFilename', result);
        {$ENDIF}
    end;
    
    //----------------------------------------------------------------------------
    
    procedure StopMusic();
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'StopMusic', '');
        {$ENDIF}
        
        if AudioReady() then AudioDriver.StopMusic();
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'StopMusic');
        {$ENDIF}
    end;
    
    procedure StopSoundEffect(name: String);
    begin
        StopSoundEffect(SoundEffectNamed(name));
    end;
    
    procedure StopSoundEffect(effect: SoundEffect);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'StopSoundEffect', HexStr(effect));
        {$ENDIF}
        
        if not AudioReady() then exit;
		AudioDriver.StopSoundEffect(effect);
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'StopSoundEffect');
        {$ENDIF}
    end;

//=============================================================================

    initialization
    begin
        {$IFDEF TRACE}
            TraceEnter('sgAudio', 'Initialise', '');
        {$ENDIF}
        
        InitialiseSwinGame();
        _SoundEffects := TStringHash.Create(False, 1024);
        _Music := TStringHash.Create(False, 1024);
        
        {$IFDEF TRACE}
            TraceExit('sgAudio', 'Initialise');
        {$ENDIF}
    end;
    
    finalization
    begin
        ReleaseAllMusic();
        ReleaseAllSoundEffects();
        FreeAndNil(_SoundEffects);
        FreeAndNil(_Music);
    end;
end.
