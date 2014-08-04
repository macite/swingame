// SwinGame.pas was generated on 2014-08-04 11:37:35.751193
// 
// This is a wrapper unit that exposes all of the SwinGame API in a single
// location. To create a SwinGame project all you should need to use is
// SwinGame and sgTypes.

unit SwinGame;

interface
uses sgTypes, sgAnimations, sgAudio, sgCamera, sgCharacters, sgGeometry, sgGraphics, sgImages, sgInput, sgNetworking, sgPhysics, sgResources, sgSprites, sgText, sgTimers, sgUtils, sgUserInterface, sgArduino;

  type LongintArray = sgTypes.LongintArray;

  type StringArray = sgTypes.StringArray;

  type SingleArray = sgTypes.SingleArray;

  type NamedIndexCollection = sgTypes.NamedIndexCollection;

  type Point2D = sgTypes.Point2D;

  type Vector = sgTypes.Vector;

  type Point2DArray = sgTypes.Point2DArray;

  type Rectangle = sgTypes.Rectangle;

  type Finger = sgTypes.Finger;

  type FingerArray = sgTypes.FingerArray;

  type Resolution = sgTypes.Resolution;

  type ResolutionArray = sgTypes.ResolutionArray;

  type Circle = sgTypes.Circle;

  type AccelerometerMotion = sgTypes.AccelerometerMotion;

  type LineSegment = sgTypes.LineSegment;

  type Triangle = sgTypes.Triangle;

  type LinesArray = sgTypes.LinesArray;

  type TriangleArray = sgTypes.TriangleArray;

  type SoundEffectData = sgTypes.SoundEffectData;

  type SoundEffect = sgTypes.SoundEffect;

  type MusicData = sgTypes.MusicData;

  type Music = sgTypes.Music;

  type Matrix2D = sgTypes.Matrix2D;

  type Color = sgTypes.Color;

  type AnimationFrame = sgTypes.AnimationFrame;

  type AnimationFrameData = sgTypes.AnimationFrameData;

  type AnimationScriptData = sgTypes.AnimationScriptData;

  type AnimationScript = sgTypes.AnimationScript;

  type AnimationData = sgTypes.AnimationData;

  type Animation = sgTypes.Animation;

  type BitmapData = sgTypes.BitmapData;

  type Bitmap = sgTypes.Bitmap;

  type DrawingOptions = sgTypes.DrawingOptions;

  type BitmapArray = sgTypes.BitmapArray;

  type BitmapCell = sgTypes.BitmapCell;

  type CollisionSide = sgTypes.CollisionSide;

  type ResourceKind = sgTypes.ResourceKind;

  type CollisionTestKind = sgTypes.CollisionTestKind;

  type SpriteEventKind = sgTypes.SpriteEventKind;

  type Sprite = sgTypes.Sprite;

  type SpriteEventHandler = sgTypes.SpriteEventHandler;

  type SpriteFunction = sgTypes.SpriteFunction;

  type SpriteSingleFunction = sgTypes.SpriteSingleFunction;

  type SpriteEventHandlerArray = sgTypes.SpriteEventHandlerArray;

  type SpriteData = sgTypes.SpriteData;

  type TimerData = sgTypes.TimerData;

  type Timer = sgTypes.Timer;

  type FontData = sgTypes.FontData;

  type Font = sgTypes.Font;

  type FontStyle = sgTypes.FontStyle;

  type FontAlignment = sgTypes.FontAlignment;

  type MouseButton = sgTypes.MouseButton;

  type KeyCode = sgTypes.KeyCode;

  type FreeNotifier = sgTypes.FreeNotifier;

  type DirectionAngles = sgTypes.DirectionAngles;

  type DirStateData = sgTypes.DirStateData;

  type CharacterData = sgTypes.CharacterData;

  type Character = sgTypes.Character;

  type GUIElementKind = sgTypes.GUIElementKind;

  type EventKind = sgTypes.EventKind;

  type GUIList = sgTypes.GUIList;

  type GUIListItem = sgTypes.GUIListItem;

  type GUIListData = sgTypes.GUIListData;

  type GUILabelData = sgTypes.GUILabelData;

  type GUILabel = sgTypes.GUILabel;

  type GUICheckboxData = sgTypes.GUICheckboxData;

  type GUICheckbox = sgTypes.GUICheckbox;

  type FileDialogSelectType = sgTypes.FileDialogSelectType;

  type Panel = sgTypes.Panel;

  type Region = sgTypes.Region;

  type GUITextbox = sgTypes.GUITextbox;

  type GUIEventCallback = sgTypes.GUIEventCallback;

  type RegionData = sgTypes.RegionData;

  type GUIRadioGroupData = sgTypes.GUIRadioGroupData;

  type GUIRadioGroup = sgTypes.GUIRadioGroup;

  type GUITextboxData = sgTypes.GUITextboxData;

  type PanelData = sgTypes.PanelData;

  type MessagePtr = sgTypes.MessagePtr;

  type MessageLink = sgTypes.MessageLink;

  type ConnectionData = sgTypes.ConnectionData;

  type Connection = sgTypes.Connection;

  type ArduinoData = sgTypes.ArduinoData;

  type ArduinoDevice = sgTypes.ArduinoDevice;

  // Returns the current cell (the part of the image or sprite) of this animation.
  // This can be used to animate an image or sprite.
  function AnimationCurrentCell(anim: Animation): Longint; overload;

  // Returns the vector assigned to the current frame in the animation.
  function AnimationCurrentVector(anim: Animation): Vector; overload;

  // Indicates if an animation has ended. Animations with loops will never end.
  function AnimationEnded(anim: Animation): Boolean; overload;

  // Returns true if the animation entered a new frame on its last update.
  // This can be used to trigger actions on frames within an animation.
  function AnimationEnteredFrame(anim: Animation): Boolean; overload;

  // Returns the amount of time spent in the current frame. When this exceeds
  // the frames duration the animation moves to the next frame.
  function AnimationFrameTime(anim: Animation): Single; overload;

  // The index of the animation within the animation template that has the supplied name.
  function AnimationIndex(temp: AnimationScript; name: String): Longint; overload;

  // The name of the animation currently being played.
  function AnimationName(temp: Animation): String; overload;

  // The name of the animation within the animation template at the specified index.
  function AnimationName(temp: AnimationScript; idx: Longint): String; overload;

  // Returns the name of the Animation Script.
  function AnimationScriptName(script: AnimationScript): String; overload;

  // Returns the `AnimationScript` that has been loaded with the specified ``name``,
  // see `LoadAnimationScriptNamed`.
  function AnimationScriptNamed(name: String): AnimationScript; overload;

  // Assign a new starting animation to the passed in animation from the `AnimationScript`.
  // This may play a sound if the first frame of the animation is linked to a sound effect.
  procedure AssignAnimation(anim: Animation; name: String; script: AnimationScript); overload;

  // Assign a new starting animation to the passed in animation from the `AnimationScript`.
  // This may play a sound if the first frame of the animation is linked to a sound effect.
  procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript); overload;

  // Assign a new starting animation to the passed in animation from the `AnimationScript`.
  // This may play a sound if the first frame of the animation is linked to a sound effect, and withSound is true.
  procedure AssignAnimation(anim: Animation; name: String; script: AnimationScript; withSound: Boolean); overload;

  // Assign a new starting animation to the passed in animation from the `AnimationScript`.
  // This may play a sound if the first frame of the animation is linked to a sound effect, and 
  // ``withSound`` is ``true``.
  procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript; withSound: Boolean); overload;

  // Creates an animation from an `AnimationScript`. This may play a sound effect
  // if the animation is set to play a sound effect on its first frame.
  function CreateAnimation(identifier: Longint; script: AnimationScript): Animation; overload;

  // Creates an animation from a `AnimationScript`. This may play a sound effect
  // if the animation is set to play a sound effect on its first frame.
  function CreateAnimation(identifier: String; script: AnimationScript): Animation; overload;

  // Creates an animation from an `AnimationScript`. If ``withSound`` is ``true``, this may
  // play a sound effect if the animation is set to play a sound effect on its first frame.
  function CreateAnimation(identifier: Longint; script: AnimationScript; withSound: Boolean): Animation; overload;

  // Creates an animation from a `AnimationScript`. If ``withSound`` is ``true``, this may
  // play a sound effect if the animation is set to play a sound effect on its first frame.
  function CreateAnimation(identifier: String; script: AnimationScript; withSound: Boolean): Animation; overload;

  // Uses the animation information to draw a bitmap at the specified
  // point.
  procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;

  // Uses the `Animation` information to draw a `Bitmap` at the specified
  // ``x``,``y`` location.
  procedure DrawAnimation(ani: Animation; bmp: Bitmap; x: Longint; y: Longint); overload;

  // Uses the animation information to draw a bitmap at the specified
  // point on a destination bitmap.
  procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; const pt: Point2D); overload;

  // Uses the animation information to draw a bitmap at the specified
  // x,y location on a destination bitmap.
  procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; x: Longint; y: Longint); overload;

  // Uses the animation information to draw a bitmap to the screen at the specified
  // point.
  procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;

  // Uses the animation information to draw a bitmap to the screen at the specified
  // x,y location.
  procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; x: Longint; y: Longint); overload;

  // Disposes of the resources used in the animation.
  procedure FreeAnimation(var ani: Animation); overload;

  // Frees loaded animation frames data. Use this when you will no longer be 
  // using the animation for any purpose, including within sprites.
  procedure FreeAnimationScript(var scriptToFree: AnimationScript); overload;

  // Determines if SwinGame has animation frames loaded for the supplied ``name``.
  // This checks against all loaded animation frames, those loaded without a name
  // are assigned the filename as a default.
  function HasAnimationScript(name: String): Boolean; overload;

  // Load animation details from a animation frames file.
  function LoadAnimationScript(filename: String): AnimationScript; overload;

  // Loads and returns a `AnimationScript`. The supplied ``filename`` is used to
  // locate the `AnimationScript` to load. The supplied ``name`` indicates the 
  // name to use to refer to this in SwinGame. The `AnimationScript` can then be
  // retrieved by passing this ``name`` to the `AnimationScriptNamed` function.
  function LoadAnimationScriptNamed(name: String; filename: String): AnimationScript; overload;

  // Releases all of the animation templates that have been loaded.
  procedure ReleaseAllAnimationScripts(); overload;

  // Releases the SwinGame resources associated with the animation template of the
  // specified ``name``.
  procedure ReleaseAnimationScript(name: String); overload;

  // Restarts the animation. This may play a sound effect if the first frame
  // triggers a sound.
  procedure RestartAnimation(anim: Animation); overload;

  // Restarts the animation. This may play a sound effect if the first frame
  // triggers a sound and withSound is true.
  procedure RestartAnimation(anim: Animation; withSound: Boolean); overload;

  // Updates the animation, updating the time spent and possibly moving to a new
  // frame in the animation. This may play a sound effect if the new frame
  // triggers a sound.
  procedure UpdateAnimation(anim: Animation); overload;

  // Updates the animation a certain percentage and possibly moving to a new
  // frame in the animation. This may play a sound effect if the new frame
  // triggers a sound.
  procedure UpdateAnimation(anim: Animation; pct: Single); overload;

  // Updates the animation a certain percentage and possibly moving to a new
  // frame in the animation. This may play a sound effect if the new frame
  // triggers a sound and withSound is true.
  procedure UpdateAnimation(anim: Animation; pct: Single; withSound: Boolean); overload;

  // `AudioReady` indicates if SwinGame's audio has been opened. Sound effects
  // and Music can only be played with the audio is "ready".
  function AudioReady(): Boolean; overload;

  // `CloseAudio` is used to clean up the resources used by SwinGame audio. If
  // `OpenAudio` is called, this must be called to return the resources used
  // before the program terminates.
  procedure CloseAudio(); overload;

  // Fades the music in over a number of milliseconds, and then continues to
  // play the music repeatedly until the program ends or the music is stopped. 
  // The music fades from 0 volume up to the currently set music volume.
  procedure FadeMusicIn(mus: Music; ms: Longint); overload;

  // Fades the music in over a number of milliseconds, and then continues to
  // play the music repeatedly until the program ends or the music is stopped. 
  // The music fades from 0 volume up to the currently set music volume.
  procedure FadeMusicIn(name: String; ms: Longint); overload;

  // This version of FadeMusicIn fades the music in then plays the 'Music' 
  // for a given number of loops.Setting loops to -1 repeats the music 
  // infinitely, other values larger than 0 indicate the number of times that
  // the music should be played.
  procedure FadeMusicIn(name: String; loops: Longint; ms: Longint); overload;

  // This version of FadeMusicIn fades the music in then plays the 'Music' 
  // for a given number of loops.Setting loops to -1 repeats the music 
  // infinitely, other values larger than 0 indicate the number of times that
  // the music should be played.
  procedure FadeMusicIn(mus: Music; loops: Longint; ms: Longint); overload;

  // Fades the currently playing music out over a number of milli seconds.
  procedure FadeMusicOut(ms: Longint); overload;

  // Frees the resources used by a `Music` resource. All loaded
  // `Music` should be freed once it is no longer needed.
  procedure FreeMusic(var mus: Music); overload;

  // Frees the resources used by a `SoundEffect` resource. All loaded
  // `SoundEffect`s should be freed once they are no longer needed.
  procedure FreeSoundEffect(var effect: SoundEffect); overload;

  // Determines if SwinGame has a music value loaded for the supplied name.
  // This checks against all music values loaded using `LoadMusicNamed`.
  function HasMusic(name: String): Boolean; overload;

  // Determines if SwinGame has a sound effect loaded for the supplied name.
  // This checks against all sounds loaded, those loaded without a name
  // are assigned the filename as a default
  function HasSoundEffect(name: String): Boolean; overload;

  // Loads the `Music` from the supplied filename. The music will be loaded
  // from the Resources/sounds folder unless a full path to the file is passed
  // in. If you are passing in the full path and you want toensure that your game
  // is able to work across multiple platforms correctly ensure that you use
  // `PathToResource` to get the full path to the files in the projects 
  // resources folder.
  //
  // LoadMusic can load mp3, wav and ogg audio files.
  //
  // `FreeMusic` should be called to free the resources used by the 
  // `Music` data once the resource is no longer needed.
  function LoadMusic(filename: String): Music; overload;

  // Loads and returns a music value. The supplied ``filename`` is used to
  // locate the music file to load. The supplied ``name`` indicates the 
  // name to use to refer to this Music value. The `Music` can then be
  // retrieved by passing this ``name`` to the `MusicNamed` function.
  function LoadMusicNamed(name: String; filename: String): Music; overload;

  // Loads the `SoundEffect` from the supplied filename. The sound will be loaded
  // from the Resources/sounds folder unless a full path to the file is passed
  // in. If you are passing in the full path and you want to ensure that your game
  // is able to work across multiple platforms correctly then use
  // `PathToResource` to get the full path to the files in the projects
  // resources folder.
  //
  // LoadSoundEffect can load wav and ogg audio files.
  //
  // `FreeSoundEffect` should be called to free the resources used by the 
  // `SoundEffect` data once the resource is no longer needed.
  function LoadSoundEffect(filename: String): SoundEffect; overload;

  // Loads and returns a sound effect. The supplied ``filename`` is used to
  // locate the sound effect to load. The supplied ``name`` indicates the 
  // name to use to refer to this SoundEffect. The `SoundEffect` can then be
  // retrieved by passing this ``name`` to the `SoundEffectNamed` function.
  function LoadSoundEffectNamed(name: String; filename: String): SoundEffect; overload;

  // Returns the filename that SwinGame uses to load to this music data.
  function MusicFilename(mus: Music): String; overload;

  // Returns the name that SwinGame uses to refer to this music data. This
  // name can be used to fetch and release this music resource.
  function MusicName(mus: Music): String; overload;

  // Returns the `Music` that has been loaded with the specified name.
  // This works with music data loaded using `LoadMusicNamed`.
  function MusicNamed(name: String): Music; overload;

  // This function indicates if music is currently playing. As only one music 
  // resource can be playing at a time this does not need to be told which
  // music resource to check for.
  function MusicPlaying(): Boolean; overload;

  // This function returns the current volume of the music. This will be a 
  // value between 0 and 1, with 1 indicating 100% of the `Music` resources
  // volume.
  function MusicVolume(): Single; overload;

  // `OpenAudio` is used to initialise the SwinGame audio code. This should be
  // called at the start of your programs code, and is usually coded into the
  // starting project templates. After initialising the audio code you can
  // load and play `Music` using `LoadMusic` and `PlayMusic`, load and play
  // `SoundEffect`s using `LoadSoundEffect` and `PlaySoundEffect`. At the end
  // of the program you need to call `CloseAudio` to ensure that the audio
  // code is correctly terminated.
  procedure OpenAudio(); overload;

  // Pauses the currently playing music. See `ResumeMusic`.
  procedure PauseMusic(); overload;

  // PlayMusic starts playing a `Music` resource. SwinGame only allows one 
  // music resource to be played at a time. Starting to play a new music 
  // resource will stop the currently playing music track. You can also stop
  // the music by calling `StopMusic`.
  //
  // By default SwinGame starts playing music at its full volume. This can be 
  // controlled by calling `SetMusicVolume`. The current volume can be checked 
  // with `MusicVolume`.
  //
  // To test if a `Music` resource is currently playing you can use the 
  // `MusicPlaying` function.
  //
  // This version of PlayMusic can be used to play background music that is 
  // looped infinitely. The currently playing music is stopped and the new 
  // music resource will start playing, and will repeat until `StopMusic` is 
  // called, or another resource is played.
  procedure PlayMusic(mus: Music); overload;

  // PlayMusic starts playing a `Music` resource. SwinGame only allows one 
  // music resource to be played at a time. Starting to play a new music 
  // resource will stop the currently playing music track. You can also stop
  // the music by calling `StopMusic`.
  //
  // By default SwinGame starts playing music at its full volume. This can be 
  // controlled by calling `SetMusicVolume`. The current volume can be checked 
  // with `MusicVolume`.
  //
  // To test if a `Music` resource is currently playing you can use the 
  // `MusicPlaying` function.
  //
  // This version of PlayMusic can be used to play background music that is 
  // looped infinitely. The currently playing music is stopped and the new 
  // music resource will start playing, and will repeat until `StopMusic` is 
  // called, or another resource is played.
  procedure PlayMusic(name: String); overload;

  // This version of PlayMusic allows you to control the number of times the 
  // `Music` resource is repeated. It starts playing the supplied `Music` 
  // resource, repeating it the numder of times specified in the loops 
  // parameter. Setting loops to -1 repeats the music infinitely, other values
  // larger than 0 indicate the number of times that the music should be 
  // played.
  procedure PlayMusic(mus: Music; loops: Longint); overload;

  // This version of PlayMusic allows you to control the number of times the 
  // `Music` resource is repeated. It starts playing the supplied `Music` 
  // resource, repeating it the numder of times specified in the loops 
  // parameter. Setting loops to -1 repeats the music infinitely, other values
  // larger than 0 indicate the number of times that the music should be 
  // played.
  procedure PlayMusic(name: String; loops: Longint); overload;

  // There are several versions of PlaySoundEffect that can be used to control
  // the way the sound effect plays, allowing you to control its volume and 
  // the number of times the code loops. In all cases the started sound effect
  // is mixed with the currently playing sound effects and music.
  //
  // With this version of PlaySoundEffect, the started sound effect will be 
  // played at full volume.
  procedure PlaySoundEffect(effect: SoundEffect); overload;

  // There are several versions of PlaySoundEffect that can be used to control
  // the way the sound effect plays, allowing you to control its volume and 
  // the number of times the code loops. In all cases the started sound effect
  // is mixed with the currently playing sound effects and music.
  //
  // With this version of PlaySoundEffect, the started sound effect will be 
  // played at full volume.
  procedure PlaySoundEffect(name: String); overload;

  // This version of PlaySoundEffect allows you to control the volume of the 
  // sounds playback. The vol parameter will take a value between 0 and 1 
  // indicating the percentage of full volume to play at.
  // For example, 0.1 plays the sound effect at 10% of its original volume.
  procedure PlaySoundEffect(name: String; vol: Single); overload;

  // This version of PlaySoundEffect allows you to control the volume of the 
  // sounds playback. The vol parameter will take a value between 0 and 1 
  // indicating the percentage of full volume to play at.
  // For example, 0.1 plays the sound effect at 10% of its original volume.
  procedure PlaySoundEffect(effect: SoundEffect; vol: Single); overload;

  // This version of PlaySoundEffect allows you to indicate the number of times
  // the sound effect is repeated. Setting the loops parameter to -1 will cause
  // the sound effect to be looped infinitely, setting it to a value larger than
  // 0 plays the sound effect the number of times indicated, calling with a 
  // value of 0 means the sound effect is not played.
  procedure PlaySoundEffect(name: String; loops: Longint); overload;

  // This version of PlaySoundEffect allows you to indicate the number of times
  // the sound effect is repeated. Setting the loops parameter to -1 will cause
  // the sound effect to be looped infinitely, setting it to a value larger than
  // 0 plays the sound effect the number of times indicated, calling with a 
  // value of 0 means the sound effect is not played.
  procedure PlaySoundEffect(effect: SoundEffect; loops: Longint); overload;

  // This version of PlaySoundEffect allows you to control both the number
  // of times the `SoundEffect` is repeated, and its playback volume.
  procedure PlaySoundEffect(effect: SoundEffect; loops: Longint; vol: Single); overload;

  // This version of PlaySoundEffect allows you to control both the number
  // of times the `SoundEffect` is repeated, and its playback volume.
  procedure PlaySoundEffect(name: String; loops: Longint; vol: Single); overload;

  // Releases all of the music data that have been loaded.
  procedure ReleaseAllMusic(); overload;

  // Releases all of the sound effects that have been loaded.
  procedure ReleaseAllSoundEffects(); overload;

  // Releases the music that have been loaded with the supplied name.
  procedure ReleaseMusic(name: String); overload;

  // Releases the SwinGame resources associated with the sound effect of the
  // specified ``name``.
  procedure ReleaseSoundEffect(name: String); overload;

  // Resume currently paused music. See `PauseMusic`.
  procedure ResumeMusic(); overload;

  // This procedure allows you to set the volume of the currently playing 
  // music. The vol parameter indicates the percentage of the original volume,
  // for example, 0.1 sets the playback volume to 10% of its full volume.
  procedure SetMusicVolume(value: Single); overload;

  // Returns the filename that SwinGame used to load to this sound effect.
  function SoundEffectFilename(effect: SoundEffect): String; overload;

  // Returns the name that SwinGame uses to refer to this sound effect. This
  // name can be used to fetch and release this sound effect resource.
  function SoundEffectName(effect: SoundEffect): String; overload;

  // Returns the `SoundEffect` that has been loaded with the specified name,
  // see `LoadSoundEffectNamed`.
  function SoundEffectNamed(name: String): SoundEffect; overload;

  // This function can be used to check if a sound effect is currently 
  // playing.
  function SoundEffectPlaying(effect: SoundEffect): Boolean; overload;

  // This function can be used to check if a sound effect is currently 
  // playing.
  function SoundEffectPlaying(name: String): Boolean; overload;

  // Stops playing the current music resource.
  procedure StopMusic(); overload;

  // Stops all occurances of the effect `SoundEffect` that is currently playing.
  procedure StopSoundEffect(effect: SoundEffect); overload;

  // Stops all occurances of the named `SoundEffect` that are currently playing.
  procedure StopSoundEffect(name: String); overload;

  // `TryOpenAudio` attempts to open the audio device for SwinGame to use.
  // If this fails `TryOpenAudio` returns false to indicate that the audio
  // device has not opened correctly and audio cannot be played.
  function TryOpenAudio(): Boolean; overload;

  // Returns the current camera position in world coordinates. This is the top
  // left hand corner of the screen.
  function CameraPos(): Point2D; overload;

  // Returns the rectangle that encompases the area of the game world
  // that is currently on the screen.
  function CameraScreenRect(): Rectangle; overload;

  // Returns the x location of the camera in game coordinates. This represents
  // the left most x value shown on the screen, with the right of the screen
  // being at `CameraX` + `ScreenWidth`.
  function CameraX(): Single; overload;

  // Returns the y location of the camera in game coordinates. This represents
  // the stop most y value shown on the screen, with bottom of screen being
  // at `CameraY` + `ScreenHeight`.
  function CameraY(): Single; overload;

  // Set the camera view to be centered over the specific Character. The offset
  // vector allows you to move the sprite from the direct center of the screen.
  procedure CenterCameraOn(c: Character; const offset: Vector); overload;

  // Set the camera view to be centered over the specific sprite. The offset
  // vector allows you to move the sprite from the direct center of the screen.
  procedure CenterCameraOn(s: Sprite; const offset: Vector); overload;

  // Set the camera view to be centered over the specified sprite, with an 
  // offset from the center of the sprite if needed. The sprites size (width
  // and height) are taken into account. Use x and y offset of 0.0 if you want 
  // the camera to be exaclty over the center of the sprite.
  procedure CenterCameraOn(s: Sprite; offsetX: Longint; offsetY: Longint); overload;

  // Move the camera (offset its world x and y values) using the specified 
  // vector. For example, if you move the camera by the same speed vector of 
  // a sprite the camera will "track" (be locked on to) the sprite as it moves.
  procedure MoveCameraBy(const offset: Vector); overload;

  // Move the camera (offset its world x and y values) using the specified 
  // dx (change in x) and dy (change in x) values.
  procedure MoveCameraBy(dx: Single; dy: Single); overload;

  // Move the camera view to a world location specified as a Point2D.
  // This will be the new top left corner of the screen.
  procedure MoveCameraTo(const pt: Point2D); overload;

  // Move the camera view to a world location specified by the x and y values.
  // This will be the new top left corner of the screen.
  procedure MoveCameraTo(x: Single; y: Single); overload;

  // Tests if the point pt is on the screen.
  function PointOnScreen(const pt: Point2D): Boolean; overload;

  // Tests if the rectangle rect is on the screen.
  function RectOnScreen(const rect: Rectangle): Boolean; overload;

  // Change the position of the camera to a specified world coordinate. This
  // will then be the new top left most position of the screen within the world.
  procedure SetCameraPos(const pt: Point2D); overload;

  // Change the X position of the camera to a specified world coordinate. This
  // will then be the new left most position of the screen within the world.
  procedure SetCameraX(x: Single); overload;

  // Change the Y position of the camera to a specified world coordinate. This
  // will then be the new top most position of the screen within the world.
  procedure SetCameraY(y: Single); overload;

  // Translate a Point2D from world coordinates to screen coordinates.
  function ToScreen(const worldPoint: Point2D): Point2D; overload;

  // Translate the points in a rectangle to screen coordinates. This can 
  // be used to indicate the screen area used by a rectangle in game
  // coordinates.
  function ToScreen(const rect: Rectangle): Rectangle; overload;

  // Translate a world x value to the current screen x value which is based on
  // the camera position.
  function ToScreenX(worldX: Single): Longint; overload;

  // Translate a world y value to the current screen y value set by the camera.
  function ToScreenY(worldY: Single): Longint; overload;

  // Translate a Point2D from screen coordinates to world coordinates.
  function ToWorld(const screenPoint: Point2D): Point2D; overload;

  // Translate a screen x value (based on the camera) to a world x value
  function ToWorldX(screenX: Longint): Single; overload;

  // Translate a screen y value (based on the camera) to a world y value
  function ToWorldY(screenY: Longint): Single; overload;

  // Returns the DirectionAngles data at the selected index. The min and max
  // of this record can be accessed by .min and .max
  function CharacterAngleAt(c: Character; index: Longint): DirectionAngles; overload;

  // Returns the maximum angle in the DirectionAngles record at the index
  // specified
  function CharacterAngleMaxAt(c: Character; index: Longint): Longint; overload;

  // Returns the minimum angle in the DirectionAngles record at the index
  // specified
  function CharacterAngleMinAt(c: Character; index: Longint): Longint; overload;

  // Returns the count of the Angles of the character
  function CharacterAnglesLength(c: Character): Longint; overload;

  // Returns the Character's name
  function CharacterCharacterName(c: Character): String; overload;

  // Returns the index of the current direction of the character
  function CharacterCurrentDirection(c: Character): Longint; overload;

  // Returns the index of the current state of the character
  function CharacterCurrentState(c: Character): Longint; overload;

  // Returns the count of the amount of directions that the character has
  function CharacterDirectionCount(c: Character): Longint; overload;

  // Returns all of the possible directions of the character
  function CharacterDirections(c: Character): StringArray; overload;

  // Returns the name of the file that was used to load the character's
  // details.
  function CharacterFilename(c: Character): String; overload;

  // Returns the name of the character. This name can be used to
  // retrieve this character using the `CharacterNamed` function.
  function CharacterName(c: Character): String; overload;

  // Returns the `Character` with the given name. You can specify
  // the name to use in the resource bundle, or by calling the
  // `LoadCharacterNamed` function.
  function CharacterNamed(name: String): Character; overload;

  // Sets the current direction of the character
  procedure CharacterSetCurrentDirection(c: Character; direction: Longint); overload;

  // Sets the current state of the character
  procedure CharacterSetCurrentState(c: Character; state: Longint); overload;

  // Sets the Character's name
  procedure CharacterSetName(c: Character; name: String); overload;

  // Sets the the name of the Type of Character(eg boss, grunt etc)
  procedure CharacterSetType(c: Character; name: String); overload;

  // Set the value of the character.
  procedure CharacterSetValue(c: Character; idx: Longint; val: Single); overload;

  // Set the value of the character.
  procedure CharacterSetValue(c: Character; name: String; val: Single); overload;

  // Returns whether or not the layer at the selected index is drawn
  function CharacterShownLayersAt(c: Character; index: Longint): Boolean; overload;

  // Returns the character's sprite
  function CharacterSprite(c: Character): Sprite; overload;

  // Returns all of the possible states of the character
  function CharacterStateCount(c: Character): Longint; overload;

  // Returns all of the possible directions of the character
  function CharacterStates(c: Character): StringArray; overload;

  // Returns the string value of the character's type
  function CharacterType(c: Character): String; overload;

  // Returns the character's value at the index specified
  function CharacterValueAt(c: Character; index: Longint): Single; overload;

  // Returns the count of character values
  function CharacterValueCount(c: Character): Longint; overload;

  // Returns the names of all of the values of the character
  function CharacterValueNames(c: Character): StringArray; overload;

  // Draw Character without a stationary state with default facing down when not moving
  procedure DrawCharacter(c: Character); overload;

  // Draws the character's sprite with no additional functionality
  procedure DrawCharacterSprite(c: Character); overload;

  // Draw Character that changes state when it's velocity is 0 to be the stationary
  // state which is specified.
  procedure DrawCharacterWithStationary(c: Character; stationaryState: Longint; state: Longint); overload;

  // Free the resources associated with a Character. Please note
  // that this also frees the `Sprite` that exists within the
  // Character.
  procedure FreeCharacter(var c: Character); overload;

  // Returns ``true`` if SwinGame has loaded a character with the
  // indicated name.
  function HasCharacter(name: String): Boolean; overload;

  // Loads the character from a text file
  function LoadCharacter(filename: String): Character; overload;

  // Loads the character from a text file, and assigns the character
  // the indicated name. This name can then be used to refer to this
  // character in the `CharacterNamed` function.
  function LoadCharacterNamed(name: String; filename: String): Character; overload;

  // Release all of the characters currently loaded into SwinGame.
  procedure ReleaseAllCharacters(); overload;

  // Free the resources associated with a Character with the
  // given name.
  procedure ReleaseCharacter(name: String); overload;

  // Sets the active layers from the shown layers cache, using the current
  // states and directions for the indexes of the array
  procedure SetActiveLayer(c: Character); overload;

  // Toggles whether or not the layer at the specified index is drawn or not
  procedure ToggleLayerVisibility(c: Character; index: Longint); overload;

  // Update the animation of the character depending on it's direction. Returns true
  // if the direction was changed and false if it was no changed
  function UpdateDirectionAnimation(c: Character): Boolean; overload;

  // Update the animation of the character depending on it's direction, including updating
  //When the character's state goes stationary
  function UpdateDirectionAnimationWithStationary(c: Character; state: Longint; newState: Longint): Boolean; overload;

  // Adds the two parameter vectors (``v1`` and ``v2``) together and returns 
  // the result as a new `Vector`.
  function AddVectors(const v1: Vector; const v2: Vector): Vector; overload;

  // Apply the passed in Matrix2D to all of the points in the 
  // Point2DArray.
  procedure ApplyMatrix(const m: Matrix2D; var pts: Point2DArray); overload;

  // Use a matrix to transform all of the points in a triangle.
  procedure ApplyMatrix(const m: Matrix2D; var tri: Triangle); overload;

  // Calculates the angle from one vector to another.
  function CalculateAngle(const v1: Vector; const v2: Vector): Single; overload;

  // Calculates the angle between two sprites.
  function CalculateAngle(s1: Sprite; s2: Sprite): Single; overload;

  // Calculates the angle from x1,y1 to x2,y2.
  function CalculateAngle(x1: Single; y1: Single; x2: Single; y2: Single): Single; overload;

  // Calculates the angle between two points.
  function CalculateAngleBetween(const pt1: Point2D; const pt2: Point2D): Single; overload;

  // Return the center point of a circle.
  function CenterPoint(const c: Circle): Point2D; overload;

  // Creates a circle at the given x,y location with the indicated radius.
  function CircleAt(x: Single; y: Single; radius: Longint): Circle; overload;

  // Creates a Circle at the point pt with the given radius.
  function CircleAt(const pt: Point2D; radius: Longint): Circle; overload;

  // Returns the radius of the passed in circle.
  function CircleRadius(const c: Circle): Longint; overload;

  // Returns true if the circle is completely within the rectangle.
  function CircleWithinRect(const c: Circle; const rect: Rectangle): Boolean; overload;

  // Returns the X value of the center point of a circle.
  function CircleX(const c: Circle): Single; overload;

  // Returns the Y value of the center point of a circle.
  function CircleY(const c: Circle): Single; overload;

  // Returns the point that lies on the circle's radius that is closest to the fromPt.
  function ClosestPointOnCircle(const fromPt: Point2D; const c: Circle): Point2D; overload;

  // Returns the closest point on the line from the x,y point.
  function ClosestPointOnLine(x: Single; y: Single; const line: LineSegment): Point2D; overload;

  // Returns the point on the line that is closest to the indicated point.
  function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;

  // Returns the point on the line that is closest to the circle.
  function ClosestPointOnLineFromCircle(const c: Circle; const line: LineSegment): Point2D; overload;

  // Returns the point on the lines that is closest in the indicated array from the circle.
  function ClosestPointOnLinesFromCircle(const c: Circle; const lines: LinesArray): Point2D; overload;

  // Returns the point on the rectangle that is closest to the circle.
  function ClosestPointOnRectFromCircle(const c: Circle; const rect: Rectangle): Point2D; overload;

  // Returns the cosine of the passed in angle (in degrees).
  function Cosine(angle: Single): Single; overload;

  // Creates a circle at the given x,y location with the indicated radius.
  function CreateCircle(x: Single; y: Single; radius: Longint): Circle; overload;

  // Creates a Circle at the point pt with the given radius.
  function CreateCircle(const pt: Point2D; radius: Longint): Circle; overload;

  // Returns a line segment from x1,y1 to x2,y2.
  function CreateLine(x1: Single; y1: Single; x2: Single; y2: Single): LineSegment; overload;

  // Returns a line from pt1 to pt2.
  function CreateLine(const pt1: Point2D; const pt2: Point2D): LineSegment; overload;

  // Returns a new `Vector` created from the start and end points of a 
  // `LineSegment`. Useful for calculating angle vectors or extracting a 
  // normal vector (see `LineNormal`) for the line.
  function CreateLineAsVector(const line: LineSegment): Vector; overload;

  // Returns a line from the origin to the end of the mv vector.
  function CreateLineFromVector(const mv: Vector): LineSegment; overload;

  // Returns a line from a starting point to the point at the end of the
  // mv vector.
  function CreateLineFromVector(const pt: Point2D; const mv: Vector): LineSegment; overload;

  // Returns a line from the x,y starting point to the point at the end of the
  // mv vector.
  function CreateLineFromVector(x: Single; y: Single; const mv: Vector): LineSegment; overload;

  // Returns a rectangle from a given x,y location with a given width
  // and height.
  function CreateRectangle(x: Single; y: Single; w: Longint; h: Longint): Rectangle; overload;

  // Returns a rectangle that encloses the lines in the lines array.
  function CreateRectangle(const lines: LinesArray): Rectangle; overload;

  // Returns a rectangle that encloses th epoints in a triangle.
  function CreateRectangle(const tri: Triangle): Rectangle; overload;

  // Returns a rectangle that encloses the two points on the line segment.
  function CreateRectangle(const line: LineSegment): Rectangle; overload;

  // Returns a rectangle that encloses a circle.
  function CreateRectangle(const c: Circle): Rectangle; overload;

  // Returns a rectangle with pt1 and pt2 defining the two distant edge points.
  function CreateRectangle(const pt1: Point2D; const pt2: Point2D): Rectangle; overload;

  // Returns a rectangle at a given point with a specified width and height.
  function CreateRectangle(const pt: Point2D; width: Longint; height: Longint): Rectangle; overload;

  // Returns a triangle from the points passed in.
  function CreateTriangle(ax: Single; ay: Single; bx: Single; by: Single; cx: Single; cy: Single): Triangle; overload;

  // Returns a triangle made up of the three points passed in.
  function CreateTriangle(const a: Point2D; const b: Point2D; const c: Point2D): Triangle; overload;

  // Returns a new `Vector` created using the angle and magnitude (length). 
  // The angle and magnitude are scalar values and the angle is in degrees.
  function CreateVectorFromAngle(angle: Single; magnitude: Single): Vector; overload;

  // Returns a vector from a point to the specified rectangle.
  function CreateVectorFromPointToRect(const pt: Point2D; const rect: Rectangle): Vector; overload;

  // Returns a vector from the specified point to the specified rectangle.
  function CreateVectorFromPointToRect(x: Single; y: Single; const rect: Rectangle): Vector; overload;

  // Returns a vector from the specified point to the specified rectangle.
  function CreateVectorFromPointToRect(x: Single; y: Single; rectX: Single; rectY: Single; rectWidth: Longint; rectHeight: Longint): Vector; overload;

  // Returns a `Vector` created from the difference from the ``p1`` to 
  // the second ``p2`` points (`Point2D`).
  function CreateVectorFromPoints(const p1: Point2D; const p2: Point2D): Vector; overload;

  // Returns a new `Vector` using the x and y value of a Point2D parameter.
  function CreateVectorToPoint(const p1: Point2D): Vector; overload;

  // Returns the point at the opposite side of a circle from a given point ``pt``.
  function DistantPointOnCircle(const pt: Point2D; const c: Circle): Point2D; overload;

  // Finds the opposite side of a circle from a given point ``pt`` when travelling along the
  // vector ``heading``. Returns False if the ray projected from point ``pt`` misses the circle.
  function DistantPointOnCircleHeading(const pt: Point2D; const c: Circle; const heading: Vector; out oppositePt: Point2D): Boolean; overload;

  // Calculates the dot product (scalar product) between the two vector
  // parameters  rovided (``v1`` and ``v2``). It returns the result as a
  // scalar value.
  //
  // If the result is 0.0 it means that the vectors are orthogonal (at right
  // angles to each other). If ``v1`` and ``v2`` are unit vectors (length of
  // 1.0) and the dot product is 1.0, it means that ``v1`` and ``v2`` vectors
  // are parallel.
  function DotProduct(const v1: Vector; const v2: Vector): Single; overload;

  // Ensures that the passed in rectangle has a positive width and height.
  procedure FixRectangle(var rect: Rectangle); overload;

  // Ensures that the passed in rectangle has a positive width and height.
  procedure FixRectangle(var x: Single; var y: Single; var width: Longint; var height: Longint); overload;

  // Returns the identity matrix. When a Matrix2D or Vector is multiplied by
  // the identity matrix the result is the original matrix or vector.
  function IdentityMatrix(): Matrix2D; overload;

  // Returns a rectangle that is inset from rect the amount specified.
  function InsetRectangle(const rect: Rectangle; insetAmount: Longint): Rectangle; overload;

  // Returns the intersection of two rectangles.
  function Intersection(const rect1: Rectangle; const rect2: Rectangle): Rectangle; overload;

  // Returns a new Vector that is an inverted version of the parameter
  // vector (v). In other words, the -/+ sign of the x and y values are changed.
  function InvertVector(const v: Vector): Vector; overload;

  // Returns a new `Vector` that is a based on the parameter ``v`` however
  // its magnitude (length) will be limited (truncated) if it exceeds the
  // specified limit value.
  function LimitVector(const v: Vector; limit: Single): Vector; overload;

  // Returns a new `Vector` created from the start and end points of a 
  // `LineSegment`. Useful for calculating angle vectors or extracting a 
  // normal vector (see `LineNormal`) for the line.
  function LineAsVector(const line: LineSegment): Vector; overload;

  // Which of the lines from the array of line segments did the circle collide with given the
  // indicated velocity.
  function LineCircleHit(const c: Circle; const velocity: Vector; const lines: LinesArray; out found: LineSegment): Boolean; overload;

  // Returns a line segment from x1,y1 to x2,y2.
  function LineFrom(x1: Single; y1: Single; x2: Single; y2: Single): LineSegment; overload;

  // Returns a line from pt1 to pt2.
  function LineFrom(const pt1: Point2D; const pt2: Point2D): LineSegment; overload;

  // Returns a line from the origin to the end of the mv vector.
  function LineFromVector(const mv: Vector): LineSegment; overload;

  // Returns a line from a starting point to the point at the end of the
  // mv vector.
  function LineFromVector(const pt: Point2D; const mv: Vector): LineSegment; overload;

  // Returns a line from the x,y starting point to the point at the end of the
  // mv vector.
  function LineFromVector(x: Single; y: Single; const mv: Vector): LineSegment; overload;

  // Returns the intersection point of two lines.
  function LineIntersectionPoint(const line1: LineSegment; const line2: LineSegment; out pt: Point2D): Boolean; overload;

  // Returns true if the line segment intersects the circle.
  function LineIntersectsCircle(const l: LineSegment; const c: Circle): Boolean; overload;

  // Returns true if the line intersect any of the lines in the array.
  function LineIntersectsLines(const line: LineSegment; const lines: LinesArray): Boolean; overload;

  // Returns true if the line intersects the rectangle.
  function LineIntersectsRect(const line: LineSegment; const rect: Rectangle): Boolean; overload;

  // Returns the squared line magnitude.
  function LineMagnitudeSq(const line: LineSegment): Single; overload;

  // Returns the squared magnitude of the line from the points given.
  function LineMagnitudeSq(x1: Single; y1: Single; x2: Single; y2: Single): Single; overload;

  // Returns the mid point of the line segment.
  function LineMidPoint(const line: LineSegment): Point2D; overload;

  // Returns a unit vector (length is 1.0) that is "normal" (prependicular) to
  // the ``line`` parameter. A normal vector is useful for calculating the
  // result of a collision such as sprites bouncing off walls (lines).
  function LineNormal(const line: LineSegment): Vector; overload;

  // Returns true if the two line segments intersect.
  function LineSegmentsIntersect(const line1: LineSegment; const line2: LineSegment): Boolean; overload;

  // Get a text description of the line segment.
  function LineToString(const ln: LineSegment): String; overload;

  // Returns an array containing the four lines of the rectangle.
  function LinesFrom(const rect: Rectangle): LinesArray; overload;

  // Returns an array containing the three lines from the triangle.
  function LinesFrom(const tri: Triangle): LinesArray; overload;

  // Returns true if any of the lines in the array intersect with the Rectangle `r`.
  function LinesRectIntersect(const lines: LinesArray; const r: Rectangle): Boolean; overload;

  // Multiplies the `Vector` parameter ``v`` with the `Matrix2D` ``m`` and 
  // returns the result as a `Vector`. Use this to transform the vector with 
  // the matrix (to apply scaling, rotation or translation effects).
  function MatrixMultiply(const m: Matrix2D; const v: Vector): Vector; overload;

  // Multiplies the two `Matrix2D` parameters, ``m1`` by ``m2``, and returns
  // the result as a new `Matrix2D`. Use this to combine the effects to two 
  // matrix transformations.
  function MatrixMultiply(const m1: Matrix2D; const m2: Matrix2D): Matrix2D; overload;

  // This function returns a string representation of a Matrix.
  function MatrixToString(const m: Matrix2D): String; overload;

  // Returns the sum of pt1 and pt2
  function PointAdd(const pt1: Point2D; const pt2: Point2D): Point2D; overload;

  // Create a Point2D that points at the X,Y location passed in.
  function PointAt(x: Single; y: Single): Point2D; overload;

  // Create a Point2D that points at the point from the startPoint at the end of the offset vector.
  function PointAt(const startPoint: Point2D; const offset: Vector): Point2D; overload;

  // Returns True if the point ``pt`` is in the circle.
  function PointInCircle(const pt: Point2D; const c: Circle): Boolean; overload;

  // Returns True if the point ``ptX``, ``ptY`` is in the circle.
  function PointInCircle(ptX: Single; ptY: Single; cX: Single; cY: Single; radius: Single): Boolean; overload;

  // Returns True if point ``pt`` is in the Rectangle ``rect``.
  function PointInRect(const pt: Point2D; const rect: Rectangle): Boolean; overload;

  // Returns true if the x,y point is within the rectangle.
  function PointInRect(x: Single; y: Single; const rect: Rectangle): Boolean; overload;

  // Returns true if the point is within the rectangle.
  function PointInRect(const pt: Point2D; x: Single; y: Single; w: Single; h: Single): Boolean; overload;

  // Returns true if the point (ptX, ptY) is within the rectangle.
  function PointInRect(ptX: Single; ptY: Single; x: Single; y: Single; w: Single; h: Single): Boolean; overload;

  // Returns true if the point ``pt`` is in the Triangle ``tri``.
  function PointInTriangle(const pt: Point2D; const tri: Triangle): Boolean; overload;

  // Returns the distance from the x,y point to the line segment.
  function PointLineDistance(x: Single; y: Single; const line: LineSegment): Single; overload;

  // Returns distance from the line, or if the intersecting point on the line nearest
  //    the point tested is outside the endpoints of the line, the distance to the
  //    nearest endpoint.
  //
  //  Returns -1 on zero-valued denominator conditions to return an illegal distance. (
  //    modification of Brandon Crosby's VBA code)
  function PointLineDistance(const pt: Point2D; const line: LineSegment): Single; overload;

  // Returns True if point ``pt`` is on the line segment ``line``.
  function PointOnLine(const pt: Point2D; const line: LineSegment): Boolean; overload;

  // Returns True of `pt1` is at the same point as `pt2`.
  function PointOnPoint(const pt1: Point2D; const pt2: Point2D): Boolean; overload;

  // Returns the distance from point to point.
  function PointPointDistance(const pt1: Point2D; const pt2: Point2D): Single; overload;

  // Get a text description of the point2D.
  function PointToString(const pt: Point2D): String; overload;

  // Returns the four points from the corners of a rectangle.
  function PointsFrom(const rect: Rectangle): Point2DArray; overload;

  // Returns the two points from the ends of a line segment.
  function PointsFrom(const line: LineSegment): Point2DArray; overload;

  // Create a Point2D that points at the X,Y location passed in.
  function RandomScreenPoint(): Point2D; overload;

  // Returns the distance from the ray origin to the edge of the circle where the ray heads in the
  // direction indicated in the ray_heading parameter. This returns -1 where the ray does not hit
  // the circle.
  function RayCircleIntersectDistance(const ray_origin: Point2D; const ray_heading: Vector; const c: Circle): Single; overload;

  // Returns the intersection point of a ray with a line, returning true if the ray intesects with the line.
  function RayIntersectionPoint(const fromPt: Point2D; const heading: Vector; const line: LineSegment; out pt: Point2D): Boolean; overload;

  // Returns the rectangle details after it moved the amount specified within
  // the vector.
  function RectangleAfterMove(const rect: Rectangle; const mv: Vector): Rectangle; overload;

  // Returns the bottom (y) value of a rectangle.
  function RectangleBottom(const rect: Rectangle): Single; overload;

  // Returns the bottom left corner of the rectangle.
  function RectangleBottomLeft(const rect: Rectangle): Point2D; overload;

  // Returns the bottom right corner of the rectangle.
  function RectangleBottomRight(const rect: Rectangle): Point2D; overload;

  // Returns the center point of the rectangle.
  function RectangleCenter(const rect: Rectangle): Point2D; overload;

  // Returns the center of the bottom line of the rectangle.
  function RectangleCenterBottom(const rect: Rectangle): Point2D; overload;

  // Returns the center of the left line of the rectangle.
  function RectangleCenterLeft(const rect: Rectangle): Point2D; overload;

  // Returns the center of the right line of the rectangle.
  function RectangleCenterRight(const rect: Rectangle): Point2D; overload;

  // Returns the center of the top line of the rectangle.
  function RectangleCenterTop(const rect: Rectangle): Point2D; overload;

  // Returns a rectangle from a given x,y location with a given width
  // and height.
  function RectangleFrom(x: Single; y: Single; w: Longint; h: Longint): Rectangle; overload;

  // Returns a rectangle that encloses th epoints in a triangle.
  function RectangleFrom(const tri: Triangle): Rectangle; overload;

  // Returns a rectangle that encloses the lines in the lines array.
  function RectangleFrom(const lines: LinesArray): Rectangle; overload;

  // Returns a rectangle that encloses the two points on the line segment.
  function RectangleFrom(const line: LineSegment): Rectangle; overload;

  // Returns a rectangle that encloses a circle.
  function RectangleFrom(const c: Circle): Rectangle; overload;

  // Returns a rectangle with pt1 and pt2 defining the two distant edge points.
  function RectangleFrom(const pt1: Point2D; const pt2: Point2D): Rectangle; overload;

  // Returns a rectangle at a given point with a specified width and height.
  function RectangleFrom(const pt: Point2D; width: Longint; height: Longint): Rectangle; overload;

  // Returns the left (x) value of a rectangle.
  function RectangleLeft(const rect: Rectangle): Single; overload;

  // Returns a rectangle that is offset by the vector.
  function RectangleOffset(const rect: Rectangle; const vec: Vector): Rectangle; overload;

  // Returns the right (x) value of a rectangle.
  function RectangleRight(const rect: Rectangle): Single; overload;

  // Get a text description of the rectangle.
  function RectangleToString(const rect: Rectangle): String; overload;

  // Returns the top (y) value of a rectangle.
  function RectangleTop(const rect: Rectangle): Single; overload;

  // Returns the top left corner of the rectangle.
  function RectangleTopLeft(const rect: Rectangle): Point2D; overload;

  // Returns the top right corner of the rectangle.
  function RectangleTopRight(const rect: Rectangle): Point2D; overload;

  // Returns true if the two rectangles intersect.
  function RectanglesIntersect(const rect1: Rectangle; const rect2: Rectangle): Boolean; overload;

  // Returns a rotation matrix that rotates 2d points by the angle.
  function RotationMatrix(deg: Single): Matrix2D; overload;

  // Returns a matrix that can be used to scale 2d points (both x and y).
  function ScaleMatrix(scale: Single): Matrix2D; overload;

  // Create a scale matrix that scales x and y to
  // different degrees.
  function ScaleMatrix(const scale: Point2D): Matrix2D; overload;

  // Create a matrix that can scale, rotate then translate geometry points.
  function ScaleRotateTranslateMatrix(const scale: Point2D; deg: Single; const translate: Point2D): Matrix2D; overload;

  // Returns the sine of the passed in angle (in degrees).
  function Sine(angle: Single): Single; overload;

  // Subtracts the second vector parameter (``v2``) from the first vector
  // (``v1``) and returns the result as new `Vector`.
  function SubtractVectors(const v1: Vector; const v2: Vector): Vector; overload;

  // Returns the tangent of the passed in angle (in degrees).
  function Tangent(angle: Single): Single; overload;

  // Returns the two tangent points on the circle given the indicated vector.
  function TangentPoints(const fromPt: Point2D; const c: Circle; out p1: Point2D; out p2: Point2D): Boolean; overload;

  // Returns a matrix that can be used to translate 2d points. Moving them
  // by dx and dy.
  function TranslationMatrix(dx: Single; dy: Single): Matrix2D; overload;

  // Returns a translation matric used to translate 2d points by the
  // distance in the Point2D.
  function TranslationMatrix(const pt: Point2D): Matrix2D; overload;

  // Returns the barycenter point of the triangle.
  function TriangleBarycenter(const tri: Triangle): Point2D; overload;

  // Returns a triangle from the points passed in.
  function TriangleFrom(ax: Single; ay: Single; bx: Single; by: Single; cx: Single; cy: Single): Triangle; overload;

  // Returns a triangle made up of the three points passed in.
  function TriangleFrom(const a: Point2D; const b: Point2D; const c: Point2D): Triangle; overload;

  // Returns true if the triangle intersects with the rectangle.
  function TriangleRectangleIntersect(const tri: Triangle; const rect: Rectangle): Boolean; overload;

  // Get a text description of the triangle.
  function TriangleToString(const tri: Triangle): String; overload;

  // Returns true if any of the triangles intersects with the rectangle.
  function TrianglesRectangleIntersect(const tri: TriangleArray; const rect: Rectangle): Boolean; overload;

  // Returns the unit vector of the parameter vector (v). The unit vector has a
  // magnitude of 1, resulting in a vector that indicates the direction of
  // the original vector.
  function UnitVector(const v: Vector): Vector; overload;

  // Calculates the angle of a vector.
  function VectorAngle(const v: Vector): Single; overload;

  // Returns a new `Vector` created using the angle and magnitude (length). 
  // The angle and magnitude are scalar values and the angle is in degrees.
  function VectorFromAngle(angle: Single; magnitude: Single): Vector; overload;

  // Returns a vector from a point to the specified rectangle.
  function VectorFromPointToRect(const pt: Point2D; const rect: Rectangle): Vector; overload;

  // Returns a vector from the specified point to the specified rectangle.
  function VectorFromPointToRect(x: Single; y: Single; const rect: Rectangle): Vector; overload;

  // Returns a vector from the specified point to the specified rectangle.
  function VectorFromPointToRect(x: Single; y: Single; rectX: Single; rectY: Single; rectWidth: Longint; rectHeight: Longint): Vector; overload;

  // Returns a `Vector` created from the difference from the ``p1`` to 
  // the second ``p2`` points (`Point2D`).
  function VectorFromPoints(const p1: Point2D; const p2: Point2D): Vector; overload;

  // Returns true if the vector ends within the rectangle when started at the origin.
  function VectorInRect(const v: Vector; const rect: Rectangle): Boolean; overload;

  // Return true if the vector (used as a point) is within the rectangle
  function VectorInRect(const v: Vector; x: Single; y: Single; w: Single; h: Single): Boolean; overload;

  // Test to see if the ``x`` and ``y`` components of the provided vector
  // parameter ``v`` are zero.
  function VectorIsZero(const v: Vector): Boolean; overload;

  // Returns the magnitude (or "length") of the parameter vector (v) as a 
  // scalar value.
  function VectorMagnitude(const v: Vector): Single; overload;

  // Returns the squared magnitude (or "length") of the parameter vector (v) as a 
  // scalar value.
  function VectorMagnitudeSq(const v: Vector): Single; overload;

  // Multiplies each component (``x`` and ``y`` values) of the ``v1`` vector
  // by the ``s`` scalar value and returns the result as a new `Vector`.
  function VectorMultiply(const v: Vector; s: Single): Vector; overload;

  // Returns a new `Vector` that is perpendicular ("normal") to the parameter
  // vector ``v`` provided. The concept of a "normal" vector is usually
  // extracted from (or associated with) a line. See `LineNormal`.
  function VectorNormal(const v: Vector): Vector; overload;

  // Returns a vector out of a circle for a given circle.
  function VectorOutOfCircleFromCircle(const src: Circle; const bounds: Circle; const velocity: Vector): Vector; overload;

  // Returns the vector out of a circle from a given point.
  function VectorOutOfCircleFromPoint(const pt: Point2D; const c: Circle; const velocity: Vector): Vector; overload;

  // Returns a vector that can be used to move a circle out of a rectangle.
  function VectorOutOfRectFromCircle(const c: Circle; const rect: Rectangle; const velocity: Vector): Vector; overload;

  // Determines the vector needed to move from point ``pt`` out of rectangle ``rect`` given the velocity specified
  function VectorOutOfRectFromPoint(const pt: Point2D; const rect: Rectangle; const velocity: Vector): Vector; overload;

  // Returns the vector needed to move rectangle ``src`` out of rectangle``bounds`` given the velocity specified.
  function VectorOutOfRectFromRect(const src: Rectangle; const bounds: Rectangle; const velocity: Vector): Vector; overload;

  // Returns a vector that can be used to move a circle over the lines in the array.
  function VectorOverLinesFromCircle(const c: Circle; const lines: LinesArray; const velocity: Vector; out maxIdx: Longint): Vector; overload;

  // Returns a vector that can be used to move a group of lines back over other lines. This is
  // used internally to determine vectors that can be used to move a rectangle back out of another rectangle
  // and similar operations.
  function VectorOverLinesFromLines(const srcLines: LinesArray; const boundLines: LinesArray; const velocity: Vector; out maxIdx: Longint): Vector; overload;

  // Returns a new `Vector` using the ``x`` and ``y`` values provided.
  function VectorTo(x: Single; y: Single): Vector; overload;

  // Creates a new `Vector` with the ``x`` and ``y`` values provided, and will 
  // invert the ``y`` value if the ``invertY`` parameter is True. The inversion 
  // of the ``y`` value provides a convienient option for handling screen 
  // related vectors.
  function VectorTo(x: Single; y: Single; invertY: Boolean): Vector; overload;

  // Returns a new `Vector` using the x and y value of a Point2D parameter.
  function VectorToPoint(const p1: Point2D): Vector; overload;

  // Determines if two vectors are equal.
  function VectorsEqual(const v1: Vector; const v2: Vector): Boolean; overload;

  // Determines if two vectors are not equal.
  function VectorsNotEqual(const v1: Vector; const v2: Vector): Boolean; overload;

  // Returns the two widest points on the circle that lie along the indicated vector.
  procedure WidestPoints(const c: Circle; const along: Vector; out pt1: Point2D; out pt2: Point2D); overload;

  // Returns a list of the available resolutions.
  function AvailableResolutions(): ResolutionArray; overload;

  // Get the blue value of ``color``.
  function BlueOf(c: Color): Byte; overload;

  // Get the brightness of the ``color``.
  function BrightnessOf(c: Color): Single; overload;

  // Changes the size of the screen.
  procedure ChangeScreenSize(width: Longint; height: Longint); overload;

  // Clear the screen black.
  procedure ClearScreen(); overload;

  // Clear the screen to a specified color.
  procedure ClearScreen(toColor: Color); overload;

  // Gets a color given its RGBA components.
  procedure ColorComponents(c: Color; out r: Byte; out g: Byte; out b: Byte; out a: Byte); overload;

  // Maps a color from a given bitmap. This is used when determining color
  // keys for transparent images.
  function ColorFrom(bmp: Bitmap; apiColor: Color): Color; overload;

  // returns color to string.
  function ColorToString(c: Color): String; overload;

  // Returns the rectangle of the currentl clip of bitmap
  function CurrentClip(): Rectangle; overload;

  // Returns the rectangle of the currentl clip of bitmap
  function CurrentClip(bmp: Bitmap): Rectangle; overload;

  // Draw a circle in the game.
  procedure DrawCircle(clr: Color; x: Single; y: Single; radius: Single); overload;

  // Draw a circle in the game.
  procedure DrawCircle(clr: Color; const c: Circle); overload;

  // Draw a circle onto a destination bitmap.
  procedure DrawCircle(clr: Color; const c: Circle; const opts: DrawingOptions); overload;

  // Draw a circle onto a destination bitmap.
  procedure DrawCircle(clr: Color; x: Single; y: Single; radius: Single; const opts: DrawingOptions); overload;

  // Draw a ellipse in the game.
  procedure DrawEllipse(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single); overload;

  // Draw a ellipse in the game.
  procedure DrawEllipse(clr: Color; const rec: Rectangle); overload;

  // Draw a ellipse onto a destination bitmap.
  procedure DrawEllipse(clr: Color; const rec: Rectangle; const opts: DrawingOptions); overload;

  // Draw a ellipse onto a destination bitmap.
  procedure DrawEllipse(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single; const opts: DrawingOptions); overload;

  // Draw a line in the game.
  procedure DrawLine(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single); overload;

  // Draw a line in the game.
  procedure DrawLine(clr: Color; const l: LineSegment); overload;

  // Draw a line onto a destination bitmap.
  procedure DrawLine(clr: Color; const l: LineSegment; const opts: DrawingOptions); overload;

  // Draw a line onto a destination bitmap.
  procedure DrawLine(clr: Color; xPosStart: Single; yPosStart: Single; xPosEnd: Single; yPosEnd: Single; const opts: DrawingOptions); overload;

  // Draw a pixel in the game.
  procedure DrawPixel(clr: Color; const position: Point2D); overload;

  // Draw a pixel in the game.
  procedure DrawPixel(clr: Color; x: Single; y: Single); overload;

  // Draw a pixel onto a destination.
  procedure DrawPixel(dest: Bitmap; clr: Color; const position: Point2D); overload;

  // Draw a pixel onto a destination.
  procedure DrawPixel(dest: Bitmap; clr: Color; x: Longint; y: Longint); overload;

  // Draw a pixel on the screen.
  procedure DrawPixelOnScreen(clr: Color; const position: Point2D); overload;

  // Draw a pixel on the screen.
  procedure DrawPixelOnScreen(clr: Color; x: Longint; y: Longint); overload;

  // Draw a rectangle in the game.
  procedure DrawRectangle(clr: Color; x: Single; y: Single; width: Single; height: Single); overload;

  // Draw a rectangle in the game.
  procedure DrawRectangle(clr: Color; const rect: Rectangle); overload;

  // Draw a rectangle onto a destination bitmap.
  procedure DrawRectangle(clr: Color; const rect: Rectangle; const opts: DrawingOptions); overload;

  // Draw a rectangle onto a destination bitmap.
  procedure DrawRectangle(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single; const opts: DrawingOptions); overload;

  // Draw a triangle in the game.
  procedure DrawTriangle(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single); overload;

  // Draw a triangle in the game.
  procedure DrawTriangle(clr: Color; const tri: Triangle); overload;

  // Draw a triangle onto a destination bitmap.
  procedure DrawTriangle(clr: Color; const tri: Triangle; const opts: DrawingOptions); overload;

  // Draw a triangle onto a destination bitmap.
  procedure DrawTriangle(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; const opts: DrawingOptions); overload;

  // Fill a circle in the game.
  procedure FillCircle(clr: Color; x: Single; y: Single; radius: Single); overload;

  // Fill a circle in the game.
  procedure FillCircle(clr: Color; const c: Circle); overload;

  // Fill a circle onto a destination bitmap.
  procedure FillCircle(clr: Color; const c: Circle; const opts: DrawingOptions); overload;

  // Fill a circle onto a destination bitmap.
  procedure FillCircle(clr: Color; x: Single; y: Single; radius: Single; const opts: DrawingOptions); overload;

  // Fill a ellipse in the game.
  procedure FillEllipse(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single); overload;

  // Fill a ellipse in the game.
  procedure FillEllipse(clr: Color; const rec: Rectangle); overload;

  // Fill a ellipse onto a destination bitmap.
  procedure FillEllipse(clr: Color; const rec: Rectangle; const opts: DrawingOptions); overload;

  // Fill a ellipse onto a destination bitmap.
  procedure FillEllipse(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single; const opts: DrawingOptions); overload;

  // Fill a rectangle in the game.
  procedure FillRectangle(clr: Color; x: Single; y: Single; width: Single; height: Single); overload;

  // Fill a rectangle in the game.
  procedure FillRectangle(clr: Color; const rect: Rectangle); overload;

  // Fill a rectangle onto a destination bitmap.
  procedure FillRectangle(clr: Color; const rect: Rectangle; const opts: DrawingOptions); overload;

  // Fill a rectangle onto a destination bitmap.
  procedure FillRectangle(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single; const opts: DrawingOptions); overload;

  // Fill a triangle in the game.
  procedure FillTriangle(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single); overload;

  // Fill a triangle in the game.
  procedure FillTriangle(clr: Color; const tri: Triangle); overload;

  // Fill a triangle onto a destination bitmap.
  procedure FillTriangle(clr: Color; const tri: Triangle; const opts: DrawingOptions); overload;

  // Fill a triangle onto a destination bitmap.
  procedure FillTriangle(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; const opts: DrawingOptions); overload;

  // Returns the color of the pixel at the x,y location on
  // the supplied bitmap.
  function GetPixel(bmp: Bitmap; x: Longint; y: Longint): Color; overload;

  // Returns the color of the pixel at the given x,y location.
  function GetPixelFromScreen(x: Longint; y: Longint): Color; overload;

  // Get the green value of ``color``.
  function GreenOf(c: Color): Byte; overload;

  // Returs a color from the HSB input.
  function HSBColor(hue: Single; saturation: Single; brightness: Single): Color; overload;

  // Gets the hue ``h``, saturation ``s``, and brightness ``b`` values from
  // the color.
  procedure HSBValuesOf(c: Color; out h: Single; out s: Single; out b: Single); overload;

  // Get the hue of the ``color``.
  function HueOf(c: Color): Single; overload;

  // Returns the number of resolutions in the list of available resolutions.
  function NumberOfResolutions(): Longint; overload;

  // Opens the graphical window as an 800 x 600 window. See OpenGramhicsWinddow
  // for more options.
  procedure OpenGraphicsWindow(caption: String); overload;

  // Opens the graphical window so that it can be drawn onto. You can set the
  // icon for this window using `SetIcon`. The window itself is only drawn when
  // you call `RefreshScreen`. All windows are opened at 32 bits per pixel. You
  // can toggle fullscreen using `ToggleFullScreen`. The window is closed when
  // the application terminates.
  procedure OpenGraphicsWindow(caption: String; width: Longint; height: Longint); overload;

  // Pop the clip rectangle of the screen.
  procedure PopClip(); overload;

  // Pop the clipping rectangle of a bitmap.
  procedure PopClip(bmp: Bitmap); overload;

  // Push a clip rectangle to the screen. This can be undone using PopClip.
  procedure PushClip(const r: Rectangle); overload;

  // Add the clipping rectangle of a bitmap and uses the intersect between the new rectangle and previous clip.
  procedure PushClip(bmp: Bitmap; const r: Rectangle); overload;

  // Push a clip rectangle to the screen. This can be undone using PopClip.
  procedure PushClip(x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Sets the color of the pixel to the specified value.
  procedure PutPixel(bmp: Bitmap; value: Color; x: Longint; y: Longint); overload;

  // Gets a color given its RGBA components.
  function RGBAColor(red: Byte; green: Byte; blue: Byte; alpha: Byte): Color; overload;

  // Returns a color from a floating point RBGA value set.
  function RGBAFloatColor(r: Single; g: Single; b: Single; a: Single): Color; overload;

  // Gets a color given its RGB components.
  function RGBColor(red: Byte; green: Byte; blue: Byte): Color; overload;

  // Returns a color from a floating point RBG value set.
  function RGBFloatColor(r: Single; g: Single; b: Single): Color; overload;

  // Creates and returns a random color where R, G, B and A are all randomised.
  function RandomColor(): Color; overload;

  // Creates and returns a random color where R, G, and B are all randomised, and A is set
  // to the passed in value.
  function RandomRGBColor(alpha: Byte): Color; overload;

  // Get the red value of ``color``.
  function RedOf(c: Color): Byte; overload;

  // Draws the current drawing to the screen. This must be called to display
  // anything to the screen. This will draw all drawing operations, as well
  // as the text being entered by the user.
  //
  // Side Effects:
  // - The current drawing is shown on the screen.
  procedure RefreshScreen(); overload;

  // Refresh with a target FPS. This will delay a period of time that will 
  // approximately meet the targetted frames per second.
  procedure RefreshScreen(TargetFPS: Longword); overload;

  // Reset the clipping rectangle of the screen.
  procedure ResetClip(); overload;

  // Reset the clipping rectangle on a bitmap.
  procedure ResetClip(bmp: Bitmap); overload;

  // Get the saturation of the ``color``.
  function SaturationOf(c: Color): Single; overload;

  // Returns the height of the screen currently displayed.
  function ScreenHeight(): Longint; overload;

  // Returns the width of the screen currently displayed.
  function ScreenWidth(): Longint; overload;

  // Set the clip rectangle of the screen.
  procedure SetClip(const r: Rectangle); overload;

  // Set the clip rectangle of the bitmap.
  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;

  // Set the clip rectangle of the screen.
  procedure SetClip(x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Set the clip rectangle of the bitmap.
  procedure SetClip(bmp: Bitmap; x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Sets the icon for the window. This must be called before openning the
  // graphics window. The icon is loaded as a bitmap, though this can be from
  // any kind of bitmap file.
  procedure SetIcon(filename: String); overload;

  // Shows the SwinGame intro splash screen.
  // It would be great if you could include this at the start of
  // your game to help us promote the SwinGame API.
  procedure ShowSwinGameSplashScreen(); overload;

  // Saves the current screen a bitmap file. The file will be saved into the
  // current directory.
  procedure TakeScreenshot(basename: String); overload;

  // Switches the application to full screen or back from full screen to
  // windowed.
  //
  // Side Effects:
  // - The window switched between fullscreen and windowed
  procedure ToggleFullScreen(); overload;

  // Toggle the Window border mode. This enables you to toggle from a bordered
  // window to a borderless window.
  procedure ToggleWindowBorder(); overload;

  // Get the transpareny value of ``color``.
  function TransparencyOf(c: Color): Byte; overload;

  // Creates a circle from within a cell in a bitmap, uses the larger of the width and
  // height.
  function BitmapCellCircle(bmp: Bitmap; const pt: Point2D): Circle; overload;

  // Creates a circle from within a cell in a bitmap, uses the larger of the width and
  // height.
  function BitmapCellCircle(bmp: Bitmap; x: Longint; y: Longint): Circle; overload;

  // Returns the number of columns of cells in the specified bitmap.
  function BitmapCellColumns(bmp: Bitmap): Longint; overload;

  // Returns the number of cells in the specified bitmap.
  function BitmapCellCount(bmp: Bitmap): Longint; overload;

  // Returns the height of a cell within the bitmap.
  function BitmapCellHeight(bmp: Bitmap): Longint; overload;

  // Returns a bitmap cell for the cell of the indicated bitmap.
  function BitmapCellOf(bmp: Bitmap; cell: Longint): BitmapCell; overload;

  // Returns a bounding rectangle for a cell of the bitmap at the origin.
  function BitmapCellRectangle(bmp: Bitmap): Rectangle; overload;

  // Returns a rectangle for a cell of the bitmap at the indicated point.
  function BitmapCellRectangle(const pt: Point2D; bmp: Bitmap): Rectangle; overload;

  // Returns a rectangle for a cell of the bitmap at the indicated point.
  function BitmapCellRectangle(x: Single; y: Single; bmp: Bitmap): Rectangle; overload;

  // Returns the number of rows of cells in the specified bitmap.
  function BitmapCellRows(bmp: Bitmap): Longint; overload;

  // Returns the width of a cell within the bitmap.
  function BitmapCellWidth(bmp: Bitmap): Longint; overload;

  // Creates a circle from within a bitmap, uses the larger of the width and
  // height.
  function BitmapCircle(bmp: Bitmap; const pt: Point2D): Circle; overload;

  // Creates a circle from within a bitmap, uses the larger of the width and
  // height.
  function BitmapCircle(bmp: Bitmap; x: Longint; y: Longint): Circle; overload;

  // Returns the Filename of the bitmap
  function BitmapFilename(bmp: Bitmap): String; overload;

  // Returns the height of the bitmap cell.
  function BitmapHeight(const bmp: BitmapCell): Longint; overload;

  // Returns the height of the entire bitmap.
  function BitmapHeight(bmp: Bitmap): Longint; overload;

  // Returns the name of the bitmap
  function BitmapName(bmp: Bitmap): String; overload;

  // Returns the `Bitmap` that has been loaded with the specified name,
  // see `LoadBitmapNamed`.
  function BitmapNamed(name: String): Bitmap; overload;

  // Returns a bounding rectangle for the bitmap, at the origin.
  function BitmapRectangle(bmp: Bitmap): Rectangle; overload;

  // Returns a bounding rectangle for the bitmap.
  function BitmapRectangle(x: Single; y: Single; bmp: Bitmap): Rectangle; overload;

  // Returns a rectangle for the location of the indicated cell within the
  // bitmap.
  function BitmapRectangleOfCell(src: Bitmap; cell: Longint): Rectangle; overload;

  // This is used to define the number of cells in a bitmap, and 
  // their width and height. The cells are
  // traversed in rows so that the format would be [0 - 1 - 2] 
  // [3 - 4 - 5] etc. The count can be used to restrict which of the 
  // parts of the bitmap actually contain cells that can be drawn.
  procedure BitmapSetCellDetails(bmp: Bitmap; width: Longint; height: Longint; columns: Longint; rows: Longint; count: Longint); overload;

  // Returns the width of the entire bitmap.
  function BitmapWidth(bmp: Bitmap): Longint; overload;

  // Returns the width of the bitmap cell.
  function BitmapWidth(const bmp: BitmapCell): Longint; overload;

  // Are the two bitmaps of a similar format that they could be used in
  // place of each other. This returns true if they have the same cell
  // details (count, width, and height).
  function BitmapsInterchangable(bmp1: Bitmap; bmp2: Bitmap): Boolean; overload;

  // Clears the drawing on the Bitmap to black.
  procedure ClearSurface(dest: Bitmap); overload;

  // Clear the drawing on the Bitmap to the passed in color.
  procedure ClearSurface(dest: Bitmap; toColor: Color); overload;

  // Creates a new bitmap by combining together the bitmaps from an array. 
  // The bitmaps will be arranged in the number of columns specified, the 
  // number of rows will be determined by the number of columns specified.
  // This can be used to create a bitmap that can be used by a Sprite for
  // animation.
  function CombineIntoGrid(const bitmaps: BitmapArray; cols: Longint): Bitmap; overload;

  // Creates a bitmap in memory that is the specified width and height (in pixels).
  // The new bitmap is initially transparent and can be used as the target 
  // for various drawing operations. Once you have drawn the desired image onto
  // the bitmap you can call OptimiseBitmap to optimise the surface.
  function CreateBitmap(width: Longint; height: Longint): Bitmap; overload;

  // Creates a bitmap in memory that is the specified width and height (in pixels).
  // The new bitmap is initially transparent and can be used as the target 
  // for various drawing operations. Once you have drawn the desired image onto
  // the bitmap you can call OptimiseBitmap to optimise the surface.
  function CreateBitmap(name: String; width: Longint; height: Longint): Bitmap; overload;

  // Draw the passed in bitmap onto the game.
  procedure DrawBitmap(src: Bitmap; x: Single; y: Single); overload;

  // Draw the named bitmap onto the game.
  procedure DrawBitmap(name: String; x: Single; y: Single); overload;

  // Draw the passed in bitmap onto the game.
  procedure DrawBitmap(name: String; const position: Point2D); overload;

  // Draw the passed in bitmap onto the game.
  procedure DrawBitmap(src: Bitmap; const position: Point2D); overload;

  // Draws the source bitmap onto the destination
  procedure DrawBitmap(dest: Bitmap; src: Bitmap; const position: Point2D); overload;

  // Draw the bitmap using the passed in options
  procedure DrawBitmap(name: String; x: Longint; y: Longint; const opts: DrawingOptions); overload;

  // Draws the source bitmap onto the destination.
  procedure DrawBitmap(dest: Bitmap; src: Bitmap; x: Longint; y: Longint); overload;

  // Draw the bitmap using the passed in options
  procedure DrawBitmap(src: Bitmap; x: Longint; y: Longint; const opts: DrawingOptions); overload;

  // Draw the cell of the passed in bitmap onto the game.
  procedure DrawBitmapCell(const src: BitmapCell; const position: Point2D); overload;

  // Draw the cell of the passed in bitmap onto the game.
  procedure DrawBitmapCell(const src: BitmapCell; x: Single; y: Single); overload;

  // Draws the cell of the source bitmap onto the destination
  procedure DrawBitmapCell(dest: Bitmap; const src: BitmapCell; const position: Point2D); overload;

  // Draws the cell of the source bitmap onto the destination.
  procedure DrawBitmapCell(dest: Bitmap; const src: BitmapCell; x: Longint; y: Longint); overload;

  // Draw the cell of the bitmap onto the screen.
  procedure DrawBitmapCellOnScreen(const src: BitmapCell; const position: Point2D); overload;

  // Draw the cell of the bitmap onto the screen.
  procedure DrawBitmapCellOnScreen(const src: BitmapCell; x: Longint; y: Longint); overload;

  // Draw the bitmap onto the screen.
  procedure DrawBitmapOnScreen(src: Bitmap; const position: Point2D); overload;

  // Draw the bitmap onto the screen.
  procedure DrawBitmapOnScreen(src: Bitmap; x: Longint; y: Longint); overload;

  // Draw part of a bitmap onto the game.
  procedure DrawBitmapPart(src: Bitmap; const source: Rectangle; const position: Point2D); overload;

  // Draw part of a bitmap onto the game
  procedure DrawBitmapPart(src: Bitmap; const source: Rectangle; x: Single; y: Single); overload;

  // Draw part of the source bitmap onto the destination
  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; const position: Point2D); overload;

  // Draw part of the source bitmap onto the destination.
  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; x: Longint; y: Longint); overload;

  // Draw part of a bitmap onto the game
  procedure DrawBitmapPart(src: Bitmap; srcX: Longint; srcY: Longint; srcW: Longint; srcH: Longint; x: Single; y: Single); overload;

  // Draw part of the source onto the desitination.
  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; srcX: Longint; srcY: Longint; srcW: Longint; srcH: Longint; x: Longint; y: Longint); overload;

  // Draw part of the bitmap on the screen.
  procedure DrawBitmapPartOnScreen(src: Bitmap; const source: Rectangle; const position: Point2D); overload;

  // Draw part of the bitmap on the screen.
  procedure DrawBitmapPartOnScreen(src: Bitmap; const source: Rectangle; x: Longint; y: Longint); overload;

  // Draw part of the bitmap on the screen.
  procedure DrawBitmapPartOnScreen(src: Bitmap; srcX: Longint; srcY: Longint; srcW: Longint; srcH: Longint; x: Longint; y: Longint); overload;

  // Draw a cell from a bitmap onto the game.
  procedure DrawCell(src: Bitmap; cell: Longint; const position: Point2D); overload;

  // Draw a cell from a bitmap onto the destination.
  procedure DrawCell(dest: Bitmap; src: Bitmap; cell: Longint; const position: Point2D); overload;

  // Draw a cell from a bitmap onto the game.
  procedure DrawCell(src: Bitmap; cell: Longint; x: Single; y: Single); overload;

  // Draw a cell from a bitmap onto the destination.
  procedure DrawCell(dest: Bitmap; src: Bitmap; cell: Longint; x: Single; y: Single); overload;

  // Draw a cell from a bitmap onto the game.
  procedure DrawCellOnScreen(src: Bitmap; cell: Longint; const position: Point2D); overload;

  // Draw a cell from a bitmap onto the screen.
  procedure DrawCellOnScreen(src: Bitmap; cell: Longint; x: Single; y: Single); overload;

  // Frees a loaded bitmap. Use this when you will no longer be drawing the
  // bitmap (including within Sprites), and when the program exits.
  procedure FreeBitmap(var bitmapToFree: Bitmap); overload;

  // Determines if SwinGame has a bitmap loaded for the supplied name.
  // This checks against all bitmaps loaded, those loaded without a name
  // are assigned the filename as a default.
  function HasBitmap(name: String): Boolean; overload;

  // Loads a bitmap from file into a Bitmap variable. This can then be drawn to
  // the screen. Bitmaps can be of bmp, jpeg, gif, png, etc. Images may also
  // contain alpha values, which will be drawn correctly by the API. All
  // bitmaps must be freed using the FreeBitmap once you are finished with
  // them.
  function LoadBitmap(filename: String): Bitmap; overload;

  // Loads a bitmap from file using where the specified transparent color
  // is used as a color key for the transparent color.
  function LoadBitmap(filename: String; transparent: Boolean; transparentColor: Color): Bitmap; overload;

  // Loads and returns a bitmap. The supplied ``filename`` is used to
  // locate the Bitmap to load. The supplied ``name`` indicates the 
  // name to use to refer to this Bitmap in SwinGame. The `Bitmap` can then be
  // retrieved by passing this ``name`` to the `BitmapNamed` function.
  function LoadBitmapNamed(name: String; filename: String): Bitmap; overload;

  // Loads a bitmap with a transparent color key. The transparent color is then
  // setup as the color key to ensure the image is drawn correctly. Alpha
  // values of Images loaded in this way will be ignored. All bitmaps must be
  // freed using the `FreeBitmap` once you are finished with them.
  function LoadTransparentBitmap(filename: String; transparentColor: Color): Bitmap; overload;

  // Loads and returns a bitmap with a given color code use for transparency.
  // The supplied ``filename`` is used to locate the Bitmap to load. The supplied
  // ``name`` indicates thename to use to refer to this Bitmap in SwinGame. The 
  // `Bitmap` can then be retrieved by passing this ``name`` to the `BitmapNamed` function.
  function LoadTransparentBitmapNamed(name: String; filename: String; transparentColor: Color): Bitmap; overload;

  // Removes any surface level transparency from the supplied bitmap.
  procedure MakeOpaque(bmp: Bitmap); overload;

  // Turns on the surface level transparency for the supplied bitmap, the
  // transparency value is then set to 0 (fully transparent).
  procedure MakeTransparent(bmp: Bitmap); overload;

  // Created bitmaps can be optimised for faster drawing to the screen. This
  // optimisation should be called only once after all drawing to the bitmap
  // is complete. Optimisation should not be used if the bitmap is to be
  // drawn onto in the future. All loaded bitmaps are optimised during loading.
  procedure OptimiseBitmap(surface: Bitmap); overload;

  // Checks if a pixel is drawn at the specified x,y location.
  function PixelDrawnAtPoint(bmp: Bitmap; x: Longint; y: Longint): Boolean; overload;

  // Releases all of the bitmaps that have been loaded.
  procedure ReleaseAllBitmaps(); overload;

  // Releases the SwinGame resources associated with the bitmap of the
  // specified ``name``.
  procedure ReleaseBitmap(name: String); overload;

  // Rotate and Scale the passed in bitmap.
  function RotateScaleBitmap(src: Bitmap; degRot: Single; scale: Single): Bitmap; overload;

  // Returns true of the two bitmap cells refer to the same cell
  // in the one bitmap.
  function SameBitmapCell(const bmp1: BitmapCell; const bmp2: BitmapCell): Boolean; overload;

  // Save Bitmap to specific directory.
  procedure SaveBitmap(src: Bitmap; filepath: String); overload;

  // Saves the bitmap to a png file at the specified location.
  procedure SaveToPNG(bmp: Bitmap; filename: String); overload;

  // Turns on the surface level transparency for the supplied bitmap, 
  // and set the transparency value to the percentage supplied in pct.
  procedure SetOpacity(bmp: Bitmap; pct: Single); overload;

  // Setting the color passed in to be transparent on the bitmap. This edits the
  // passed in bitmap, altering the color to transparent.
  procedure SetTransparentColor(src: Bitmap; clr: Color); overload;

  // Setup the passed in bitmap for pixel level collisions.
  procedure SetupBitmapForCollisions(src: Bitmap); overload;

  // Returns false when the key requested is being held down. This is updated
  // as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  // to specify the key to be checked.
  function AccelerometerThreshold(): Single; overload;

  // Returns false when the key requested is being held down. This is updated
  // as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  // to specify the key to be checked.
  procedure AccelerometerThreshold(value: Single); overload;

  // Checks to see if any key has been pressed since the last time 
  // `ProcessEvents` was called.
  function AnyKeyPressed(): Boolean; overload;

  // Returns a value ranging from 0 to 1 showing delta
  // in x Axis from level(being flat on the ground).
  function DeviceMovedInXAxis(): Single; overload;

  // Returns a value ranging from 0 to 1 showing delta
  // in y Axis from level(being flat on the ground).
  function DeviceMovedInYAxis(): Single; overload;

  // Returns a value ranging from 0 to 1 showing delta
  // in z Axis from level(being flat on the ground).
  function DeviceMovedInZAxis(): Single; overload;

  // Returns the string that has been read since `StartReadingText` or 
  // `StartReadingTextWithText` was called.
  function EndReadingText(): String; overload;

  // Returns an Array of Fingers that are on the screen.
  function FingersOnScreen(): FingerArray; overload;

  // Hides iOS Keyboard
  procedure HideKeyboard(); overload;

  // Tells the mouse cursor to hide (no longer visible) if it is currently 
  // showing. Use `ShowMouse` to make the mouse cursor visible again.
  procedure HideMouse(); overload;

  // Returns true when the key requested is being held down. This is updated
  // as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  // to specify the key to be checked.
  function KeyDown(key: KeyCode): Boolean; overload;

  // The KeyName function returns a string name for a given `KeyCode`. For 
  // example, vk_Comma returns the string 'Comma'. This function could be used
  // to display more meaningful key names for configuring game controls, etc.
  function KeyName(key: KeyCode): String; overload;

  // Returns true if the specified key was released since the last time 
  // `ProcessEvents` was called. This occurs only once for the key that is 
  // released and will not return true again until the key is pressed down and
  // released again.
  function KeyReleased(key: KeyCode): Boolean; overload;

  // Returns true when the key requested is just pressed down. This is updated
  // as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  // to specify the key to be checked. this will only occur once for that key that is
  // pressed and will not return true again until the key is released and presssed down again
  function KeyTyped(key: KeyCode): Boolean; overload;

  // Returns false when the key requested is being held down. This is updated
  // as part of the `ProcessEvents` call. Use the key codes from `KeyCode`
  // to specify the key to be checked.
  function KeyUp(key: KeyCode): Boolean; overload;

  // returns boolean indicating if iOS keyboard is shown
  function KeyboardShown(): Boolean; overload;

  // Returns true if the specified button was clicked since the last time
  // `ProcessEvents` was called
  function MouseClicked(button: MouseButton): Boolean; overload;

  // Returns ``true`` if the specified button is currently pressed down.
  function MouseDown(button: MouseButton): Boolean; overload;

  // Returns the amount of accumulated mouse movement, since the last time 
  // `ProcessEvents` was called, as a `Vector`.
  function MouseMovement(): Vector; overload;

  // Returns the current window position of the mouse as a `Point2D`
  function MousePosition(): Point2D; overload;

  // Returns The current window position of the mouse as a `Vector`
  function MousePositionAsVector(): Vector; overload;

  // Returns ``true`` if the mouse is currently visible, ``false`` if not.
  function MouseShown(): Boolean; overload;

  // Returns ``true`` if the specified button is currently up.
  function MouseUp(button: MouseButton): Boolean; overload;

  // Returns the current x value of the mouse's position.
  function MouseX(): Single; overload;

  // Returns the current y value of the mouse's position.
  function MouseY(): Single; overload;

  // Moves the mouse cursor to the specified screen location.
  procedure MoveMouse(const point: Point2D); overload;

  // Moves the mouse cursor to the specified screen location.
  procedure MoveMouse(x: Longint; y: Longint); overload;

  // Returns the number of fingers that are currently
  // on the screen.
  function NumberOfFingersOnScreen(): Longint; overload;

  // ProcessEvents allows the SwinGame API to react to user interactions. This
  // routine checks the current keyboard and mouse states. This routine must
  // be called frequently within your game loop to enable user interaction.
  //
  // Side Effects
  // - Reads user interaction events
  // - Updates keys down, text input, etc.
  procedure ProcessEvents(); overload;

  // ReadingText indicates if the API is currently reading text from the
  // user. Calling StartReadingText will set this to true, and it becomes
  // false when the user presses enter or escape. At this point you can
  // read the string entered as either ASCII or Unicode.
  function ReadingText(): Boolean; overload;

  // returns a boolean indicating if the screen was touched.
  function ScreenTouched(): Boolean; overload;

  // Shows iOS Keyboard
  procedure ShowKeyboard(); overload;

  // Tells the mouse cursor to be visible if it was previously hidden with 
  // by a `HideMouse` or `SetMouseVisible`(False) call.
  procedure ShowMouse(); overload;

  // Used to explicitly set the mouse cursors visible state (if it is showing
  // in the window or not) based on the show parameter.
  procedure ShowMouse(show: Boolean); overload;

  // Start reading text within an area. Entry is 
  // completed when the user presses ENTER, and aborted with ESCAPE.
  // If the user aborts entry the result is an empty string, and TextEntryCancelled 
  // will return true. Text entry is updated during `ProcessEvents`, and text is drawn 
  // to the screen as part of the `RefreshScreen` call.
  procedure StartReadingText(textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;

  // Starts the reading of a string of characters from the user. Entry is 
  // completed when the user presses ENTER, and aborted with ESCAPE.
  // If the user aborts entry the result is an empty string, and TextEntryCancelled will return true. 
  // Text entry is updated during `ProcessEvents`, and text is drawn to the screen as part 
  // of the `RefreshScreen` call.
  procedure StartReadingText(textColor: Color; maxLength: Longint; theFont: Font; x: Longint; y: Longint); overload;

  // The same as `StartReadingText` but with an additional ``text`` parameter
  // that is displayed as default text to the user.
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; const pt: Point2D); overload;

  // The same as `StartReadingText` but with an additional ``text`` parameter
  // that is displayed as default text to the user.
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;

  // The same as `StartReadingTextWithText` but with ``text`` and ``bgColor`` parameter
  // that is displayed as default text to the user.
  procedure StartReadingTextWithText(text: String; textColor: Color; backGroundColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;

  // The same as `StartReadingText` but with an additional ``text`` parameter
  // that is displayed as default text to the user.
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; x: Longint; y: Longint); overload;

  // Returns true if the text entry started with `StartReadingText` was cancelled.
  function TextEntryCancelled(): Boolean; overload;

  // TextReadAsASCII allows you to read the value of the string entered by the
  // user as ASCII. See TextReasAsUNICODE, StartReadingText and ReadingText
  // for more details.
  function TextReadAsASCII(): String; overload;

  // Toggles iOS Keyboard
  procedure ToggleKeyboard(); overload;

  // Checks to see if the window has been asked to close. You need to handle
  // this if you want the game to end when the window is closed. This value
  // is updated by the `ProcessEvents` routine.
  function WindowCloseRequested(): Boolean; overload;

  // Accepts an incomming connection from another client.
  // Returns the amount of new connections that have been
  // accepted.
  function AcceptTCPConnection(): Longint; overload;

  // Broadcasts a message through all open connections.
  procedure BroadcastTCPMessage(aMsg: String); overload;

  // Sends a UDP packet to All connections with the message.
  procedure BroadcastUDPMessage(aMsg: String); overload;

  // Clears the Message Queue
  procedure ClearMessageQueue(aConnection: Connection); overload;

  // Closes All TCP Receiver Sockets
  procedure CloseAllConnections(); overload;

  // Closes all sockets that have been created.
  procedure CloseAllSockets(); overload;

  // Closes All TCP Host Sockets
  procedure CloseAllTCPHostSockets(); overload;

  // Closes All UDP Listener Sockets
  procedure CloseAllUDPSockets(); overload;

  // Closes the specified Socket, removed it from the Socket Array, and removes
  // the identifier from the NamedIndexCollection.
  // Refers to TCP Receiver Sockets
  function CloseConnection(var aConnection: Connection): Boolean; overload;

  // Closes the specified Socket, removed it from the Socket Array, and removes
  // the identifier from the NamedIndexCollection.
  // Refers to TCP Host Sockets
  function CloseTCPHostSocket(aPort: Longint): Boolean; overload;

  // Closes the specified Socket, removed it from the Socket Array, and removes
  // the identifier from the NamedIndexCollection.
  // Refers to UDP Listener Sockets
  function CloseUDPSocket(aPort: Longint): Boolean; overload;

  // Returns the count of Active Connections
  function ConnectionCount(): Longint; overload;

  // Gets the Decimal IP of the destination for the connection
  function ConnectionIP(aConnection: Connection): Longword; overload;

  // Gets the Port of the destination for the connectiom
  function ConnectionPort(aConnection: Connection): Longint; overload;

  // Returns the size of the New Connection List
  function ConnectionQueueSize(): Longint; overload;

  // Opens a connection to a peer using the IP and port
  // Creates a Socket for the purpose of two way messages. 
  // Returns a new connection if successful or nil if failed.
  function CreateTCPConnection(aIP: String; aPort: Longint): Connection; overload;

  // Creates a socket that listens for connections based
  // on the port given. Returns true if success or false
  // if the binding failed. Uses TCP.
  function CreateTCPHost(aPort: Longint): Boolean; overload;

  // Creates the connection and sets the ip and port values. Creates a
  // socket if there is no socket attached to the specified port. this
  // socket can be used to send and receive messages. Returns the connection
  // if this has been successful, or will return nil on failure.
  function CreateUDPConnection(aDestIP: String; aDestPort: Longint; aInPort: Longint): Connection; overload;

  // Creates a socket that listens for connections based
  // on the port given. Returns the index of the Socket in the
  // socket array.
  function CreateUDPHost(aPort: Longint): Longint; overload;

  // Converts an Integer to a Hex value and returns it as a string.
  function DecToHex(aDec: Longword): String; overload;

  // Queues a message to the end of the Message Queue
  procedure EnqueueMessage(aMsg: String; aConnection: Connection); overload;

  // Adds a connection to the list of new connections. This is called by the 
  // Accept connection in TCP and Receive message in UDP (if the message has
  // been sent by a new connection). This is used in conjunction with Fetch
  // connection, that will pop the new connection out of the list.
  procedure EnqueueNewConnection(aConnection: Connection); overload;

  // Removes the top connection from the New connection queue and
  // returns it.
  function FetchConnection(): Connection; overload;

  // An internal function used to close the specified Socket. 
  // Call ``CloseConnection`` instead.
  procedure FreeConnection(var aConnection: Connection); overload;

  // Converts a Hex String to an IPV4 Address (0.0.0.0)
  function HexStrToIPv4(aHex: String): String; overload;

  // Converts a Hex String to a Decimal Value as a String.
  function HexToDecString(aHex: String): String; overload;

  // Converts an IP to a decimal value
  function IPv4ToDec(aIP: String): Longword; overload;

  // Gets the Size of the Message Queue
  function MessageCount(aConnection: Connection): Longint; overload;

  // Returns the caller's IP.
  function MyIP(): String; overload;

  // Dequeues the Last (Newest) Message
  function ReadLastMessage(aConnection: Connection): String; overload;

  // Dequeues the Top (Oldest) Message
  function ReadMessage(aConnection: Connection): String; overload;

  // Releases All resources used by the Networking code.
  procedure ReleaseAllConnections(); overload;

  // Retrieves the connection at the specified index
  function RetreiveConnection(aConnectionAt: Longint): Connection; overload;

  // Sends the message to the specified client, attached to the socket
  // Retuns the connection if the message fails to
  // send so that it may be closed. Returns nil if the message has been sent
  // successfully.
  function SendTCPMessage(aMsg: String; aConnection: Connection): Connection; overload;

  // Sends a UDP packet to the port and ip specified in the connection
  // with the message.
  function SendUDPMessage(aMsg: String; aConnection: Connection): Boolean; overload;

  // Checks if a message has been received. If a message has been received,
  // It will automatically add it to the message queue, with the message,
  // source's IP and the port it received the message on. Returns true if
  // a new message has been received.
  function TCPMessageReceived(): Boolean; overload;

  // Checks all UDP listening sockets to see if a packet has been received.
  // If a packet has been received, it will Enqueue the message into the message
  // queue. This will set the message, sender's address and sender's port. it
  // will return true if a message has been received or false if there has been
  // no message.
  function UDPMessageReceived(): Boolean; overload;

  // Returns True if two bitmaps have collided using per pixel testing if required. 
  // The ``pt1`` and ``pt2`` (`Point2D`) parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``).
  function BitmapCollision(bmp1: Bitmap; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D): Boolean; overload;

  // Returns True if two bitmaps have collided using per pixel testing if required.
  // The ``x`` and ``y`` parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``).
  function BitmapCollision(bmp1: Bitmap; x1: Longint; y1: Longint; bmp2: Bitmap; x2: Longint; y2: Longint): Boolean; overload;

  // Returns True if the specified parts (``part1`` and ``part2`` rectangles) of the two 
  // bitmaps (``bmp1`` and ``bmpt2``) have collided, using pixel level collision if required. 
  // The ``pt1`` and ``pt2`` (`Point2D`) parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``).
  function BitmapCollision(bmp1: Bitmap; const pt1: Point2D; const part1: Rectangle; bmp2: Bitmap; const pt2: Point2D; const part2: Rectangle): Boolean; overload;

  // Returns True if a point (``pt``) is located within the ``part`` (rectangle) of the bitmap
  // ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions. For bounding box collisions
  // use the rectangle collision functions.
  // The ``x`` and ``y`` values specify the world location of the bitmap.
  // The point ``pt`` needs to be provided in world coordinates.
  function BitmapPartPointCollision(bmp: Bitmap; x: Longint; y: Longint; const part: Rectangle; const pt: Point2D): Boolean; overload;

  // Returns True if a point (``ptX``,``ptY``) is located within the ``part`` (rectangle) of the bitmap
  // ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions. For bounding box collisions
  // use the rectangle collision functions.
  // The ``x`` and ``y`` values specify the world location of the bitmap.
  // The ``ptX`` and ``ptY`` needs to be provided in world coordinates.
  function BitmapPartPointCollision(bmp: Bitmap; x: Longint; y: Longint; const part: Rectangle; ptX: Single; ptY: Single): Boolean; overload;

  // Returns True if a point (``pt``) is located within the bitmap
  // ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions.
  // The ``x`` and ``y`` values specify the world location of the bitmap.
  // The point ``pt`` needs to be provided in world coordinates.
  function BitmapPointCollision(bmp: Bitmap; x: Longint; y: Longint; const pt: Point2D): Boolean; overload;

  // Returns True if a point (``ptX``,``ptY``) is located within the bitmap
  // ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions.
  // The ``x`` and ``y`` values specify the world location of the bitmap.
  // The ``ptX`` and ``ptY`` needs to be provided in world coordinates.
  function BitmapPointCollision(bmp: Bitmap; x: Longint; y: Longint; ptX: Single; ptY: Single): Boolean; overload;

  // Returns True if the indicated part of the bitmap has collided with the specified
  // rectangle.
  function BitmapRectCollision(bmp: Bitmap; const pt: Point2D; const part: Rectangle; const rect: Rectangle): Boolean; overload;

  // Returns True if the bitmap ``bmp`` has collided with the rectangle
  // specified using pixel level testing if required.
  // The ``x`` and ``y`` values specify the world location of the bitmap.
  // The rectangle ``rect`` needs to be provided in world coordinates.
  function BitmapRectCollision(bmp: Bitmap; x: Longint; y: Longint; const rect: Rectangle): Boolean; overload;

  // Returns True if the indicated part of the bitmap has collided with the specified
  // rectangle.
  function BitmapRectCollision(bmp: Bitmap; x: Longint; y: Longint; const part: Rectangle; const rect: Rectangle): Boolean; overload;

  // Returns True if the bitmap ``bmp`` has collided with the rectangle
  // specified using pixel level testing if required.
  // The ``x`` and ``y`` values specify the world location of the bitmap.
  // The rectangles world position (``rectX`` and ``rectY``) and size
  // (``rectWidth`` and ``rectHeight``) need to be provided.
  function BitmapRectCollision(bmp: Bitmap; x: Longint; y: Longint; rectX: Longint; rectY: Longint; rectWidth: Longint; rectHeight: Longint): Boolean; overload;

  // Returns true if the cell in the specified bitmap has collided with a bitmap.
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D): Boolean; overload;

  // Returns true if the cell in the specified bitmap has collided with a part of a bitmap.
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D; const part: Rectangle): Boolean; overload;

  // Returns true if the cell in the specified bitmap has collided with a bitmap.
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; x1: Longint; y1: Longint; bmp2: Bitmap; x2: Longint; y2: Longint): Boolean; overload;

  // Returns true if the cell in the specified bitmap has collided with a part of a bitmap.
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; x1: Longint; y1: Longint; bmp2: Bitmap; x2: Longint; y2: Longint; const part: Rectangle): Boolean; overload;

  // Returns true if the cells within the two bitmaps have collided at the given points.
  function CellCollision(bmp1: Bitmap; cell1: Longint; const pt1: Point2D; bmp2: Bitmap; cell2: Longint; const pt2: Point2D): Boolean; overload;

  // Returns true if the cells within the two bitmaps have collided at their specified x,y locations.
  function CellCollision(bmp1: Bitmap; cell1: Longint; x1: Longint; y1: Longint; bmp2: Bitmap; cell2: Longint; x2: Longint; y2: Longint): Boolean; overload;

  // Returns true if the cell of the bitmap has collided with a given rectangle.
  function CellRectCollision(bmp: Bitmap; cell: Longint; const pt: Point2D; const rect: Rectangle): Boolean; overload;

  // Returns true if the cell of the bitmap has collided with a given rectangle.
  function CellRectCollision(bmp: Bitmap; cell: Longint; x: Longint; y: Longint; const rect: Rectangle): Boolean; overload;

  // Returns True if the circles have collided.
  function CircleCircleCollision(const c1: Circle; const c2: Circle): Boolean; overload;

  // Returns True if the `Sprite` ``s``, represented by a bounding circle, has 
  // collided with a ``line``. The diameter for the bounding circle is 
  // based on the sprites width or height value -- whatever is largest.
  function CircleLineCollision(s: Sprite; const line: LineSegment): Boolean; overload;

  // Returns True if the circle has collided with any of the lines from the ``rect`` rectangle.
  function CircleLinesCollision(const c: Circle; const lines: LinesArray): Boolean; overload;

  // Returns True if the Circle collised with rectangle ``rect``.
  function CircleRectCollision(const c: Circle; const rect: Rectangle): Boolean; overload;

  // Returns True if the Circle has collided with the Triangle ``tri``.
  function CircleTriangleCollision(const c: Circle; const tri: Triangle): Boolean; overload;

  // Perform a physical collidion with a sprite circle bouncing off a
  // stationary circle.
  procedure CollideCircleCircle(s: Sprite; const c: Circle); overload;

  // Perform a physical collision with a circle bouncing off a line.
  procedure CollideCircleLine(s: Sprite; const line: LineSegment); overload;

  // Perform a physical collision with a sprite as a circle bouncing off
  // the closest line in the array of lines.
  procedure CollideCircleLines(s: Sprite; const lines: LinesArray); overload;

  // Perform a physical collision with a sprite as a circle bouncing off
  // a stationary rectangle.
  procedure CollideCircleRectangle(s: Sprite; const rect: Rectangle); overload;

  // Perform a physical collision between two circular sprites.
  procedure CollideCircles(s1: Sprite; s2: Sprite); overload;

  // Returns True if the bounding rectangle of the `Sprite` ``s`` has collided 
  // with the ``line`` specified.
  function RectLineCollision(s: Sprite; const line: LineSegment): Boolean; overload;

  // Returns True if the rectangle ``rect`` provided has collided with the
  // ``line``.
  function RectLineCollision(const rect: Rectangle; const line: LineSegment): Boolean; overload;

  // Returns the side of that needs to be checked for collisions given the
  // movement velocity.
  function SideForCollisionTest(const velocity: Vector): CollisionSide; overload;

  // Determines if the `Sprite` ``s`` has collided with the bitmap ``bmp`` using
  // pixel level testing if required.
  // The ``pt`` (`Point2D`) value specifies the world location of the bitmap.
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D): Boolean; overload;

  // Determines if the `Sprite` ``s`` has collided with the bitmap ``bmp`` using
  // pixel level testing if required.
  // The ``x`` and ``y`` values specify the world location of the bitmap.
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; x: Single; y: Single): Boolean; overload;

  // Returns ``true`` if the specifed sprites (``s1`` and ``s2``) have
  // collided. Will use simple bounding box tests first, and low-level pixel
  // tests if needed.
  function SpriteCollision(s1: Sprite; s2: Sprite): Boolean; overload;

  // Returns true if the sprite has collided with a rectangle.
  function SpriteRectCollision(s: Sprite; const r: Rectangle): Boolean; overload;

  // Determined if a sprite has collided with a given rectangle. The rectangles
  // coordinates are expressed in "world" coordinates.
  function SpriteRectCollision(s: Sprite; x: Single; y: Single; width: Longint; height: Longint): Boolean; overload;

  // Returns true if the triangle and the line have collided.
  function TriangleLineCollision(const tri: Triangle; const ln: LineSegment): Boolean; overload;

  // Returns the application path set within SwinGame. This is the path
  // used to determine the location of the game's resources.
  function AppPath(): String; overload;

  // Returns the path to the file with the passed in name for a given resource
  // kind. This checks if the path exists, throwing an exception if the file
  // does not exist in the expected locations.
  function FilenameToResource(name: String; kind: ResourceKind): String; overload;

  // Returns ``true`` if the resource bundle is loaded.
  function HasResourceBundle(name: String): Boolean; overload;

  // Load a resource bundle showing load progress.
  procedure LoadResourceBundle(name: String); overload;

  // Load a resource bundle showing load progress.
  procedure LoadResourceBundle(name: String; showProgress: Boolean); overload;

  // Load a resource bundle mapping it to a given name, showing progress.
  procedure LoadResourceBundleNamed(name: String; filename: String; showProgress: Boolean); overload;

  // Returns the path to the filename within the game's resources folder.
  function PathToResource(filename: String): String; overload;

  // Returns the path to the filename for a given file resource.
  function PathToResource(filename: String; kind: ResourceKind): String; overload;

  // Returns the path to the filename that exists within the game's resources folder
  // in the indicated sub directory. For example, to get the "level1.txt" file from
  // the Resources/levels folder you call this passing in "level1.txt" as the filename
  // and "levels" as the subdir.
  function PathToResource(filename: String; subdir: String): String; overload;

  // Returns the path to a resource given its filename, kind, and any subPaths. For example: to load
  // the image ``bullet01.png`` from the ``bullets`` subdirectory you pass in ``bullet01.png`` as the filename,
  // ``ImageResource`` as the kind, and ``bullets`` as the subPaths. This will then return the full path
  // to the resource according to the platform in question. 
  //
  // For example: ``.../Resources/images/bullets/bullet01.png``
  function PathToResource(filename: String; kind: ResourceKind; const subPaths: StringArray): String; overload;

  // Returns the path to the filename that exists within the game's resources folder
  // in the indicated sub directory of the directory for the given resource kind .
  // For example, to get the "background.png" file from "level1" folder in the images folder
  // (i.e. Resources/images/level1/background.png) you call this passing in ``background.png`` as the filename
  // ``ImageResource`` as the kind and ``level1`` as the subdir.
  function PathToResource(filename: String; kind: ResourceKind; subdir: String): String; overload;

  // Returns the path to a resource based on a base path and a the resource kind.
  function PathToResourceWithBase(path: String; filename: String): String; overload;

  // Returns the path to a resource based on a base path and a the resource kind.
  function PathToResourceWithBase(path: String; filename: String; kind: ResourceKind): String; overload;

  // Using this procedure you can register a callback that is executed
  // each time a resource is freed. This is called by different versions of
  // SwinGame to keep track of resources and should not be called by user code.
  procedure RegisterFreeNotifier(fn: FreeNotifier); overload;

  // Release all of the resources loaded by SwinGame.
  procedure ReleaseAllResources(); overload;

  // Release the resource bundle with the given name.
  procedure ReleaseResourceBundle(name: String); overload;

  // Sets the path to the executable. This path is used for locating game
  // resources.
  procedure SetAppPath(path: String); overload;

  // Sets the path to the executable. This path is used for locating game
  // resources.
  procedure SetAppPath(path: String; withExe: Boolean); overload;

  // Call the supplied function for all sprites.
  procedure CallForAllSprites(fn: SpriteFunction); overload;

  // Register a procedure to be called when an events occur on any sprite.
  procedure CallOnSpriteEvent(handler: SpriteEventHandler); overload;

  // Returns the center point of the passed in Sprite. This is based on the Sprite's 
  // Position, Width and Height.
  function CenterPoint(s: Sprite): Point2D; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, the layer have name 'layer1'.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(layer: Bitmap): Sprite; overload;

  // Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, the layer names 'layer1', 'layer2',... .
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(const layers: BitmapArray): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, and the specified layer have name.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(layer: Bitmap; layerName: String): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, the layer have name 'layer1', and position the sprite at the
  // given location.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(layer: Bitmap; pt: Point2D): Sprite; overload;

  // Creates a sprite. The bitmapName is used to indicate the bitmap the sprite will use, and the 
  // animationName is used to indicate which AnimationScript to use.
  function CreateSprite(bitmapName: String; animationName: String): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, the layer have name 'layer1'.
  function CreateSprite(name: String; layer: Bitmap): Sprite; overload;

  // Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, and the specified layer names.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, the specified animation template, the layer have name 'layer1'.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(layer: Bitmap; ani: AnimationScript): Sprite; overload;

  // Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, the layer names 'layer1', 'layer2',... .
  function CreateSprite(name: String; const layers: BitmapArray): Sprite; overload;

  // Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, the specified animation template, the layer names 'layer1', 'layer2',... .
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(const layers: BitmapArray; ani: AnimationScript): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, the specified animation template, the layer have name 'layer1'.
  function CreateSprite(name: String; layer: Bitmap; ani: AnimationScript): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, the specified animation template, the layer have name 'layer1', at the given
  // location.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(layer: Bitmap; ani: AnimationScript; pt: Point2D): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, and the specified layer have name.
  function CreateSprite(name: String; layer: Bitmap; layerName: String): Sprite; overload;

  // Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, the layer names 'layer1', 'layer2',... .
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray; ani: AnimationScript): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, the specified animation template, and layer name.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(layer: Bitmap; layerName: String; ani: AnimationScript): Sprite; overload;

  // Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, and the specified layer names.
  function CreateSprite(name: String; const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;

  // Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, the specified animation template, the layer names 'layer1', 'layer2',... .
  function CreateSprite(name: String; const layers: BitmapArray; ani: AnimationScript): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, the layer have name 'layer1', and position the sprite at the
  // given x,y location.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(layer: Bitmap; x: Single; y: Single): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, the specified animation template, and layer name.
  function CreateSprite(name: String; layer: Bitmap; layerName: String; ani: AnimationScript): Sprite; overload;

  // Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, the specified animation template, the layer have name 'layer1', at the given
  // x, y location.
  //
  // This version of the constructor will assign a default name to the sprite for resource management purposes.
  function CreateSprite(layer: Bitmap; ani: AnimationScript; x: Single; y: Single): Sprite; overload;

  // Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  // sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  // pixel level collisions, no animation, the layer names 'layer1', 'layer2',... .
  function CreateSprite(name: String; const layers: BitmapArray; const layerNames: StringArray; ani: AnimationScript): Sprite; overload;

  // Create a new SpritePack with a given name. This pack can then be 
  // selected and used to control which sprites are drawn/updated in
  // the calls to DrawAllSprites and UpdateAllSprites.
  procedure CreateSpritePack(name: String); overload;

  // Returns the name of the currently selected SpritePack.
  function CurrentSpritePack(): String; overload;

  // Draws all of the sprites in the current Sprite pack. Packs can be
  // switched to select between different sets of sprites.
  procedure DrawAllSprites(); overload;

  // Draws the sprite at its location in the world. This is effected by the
  // position of the camera and the sprites current location.
  //
  // This is the standard routine for drawing sprites to the screen and should be
  // used in most cases.
  procedure DrawSprite(s: Sprite); overload;

  // Draws the sprite at its position in the game offset by a given amount. Only
  // use this method when you want to draw the sprite displaced from its location
  // in your game. Otherwise you should change the sprite's location and then
  // use the standard ''DrawSprite'' routine.
  procedure DrawSprite(s: Sprite; const position: Point2D); overload;

  // Draws the sprite at its position in the game offset by a given amount. Only
  // use this method when you want to draw the sprite displaced from its location
  // in your game. Otherwise you should change the sprite's location and then
  // use the standard ''DrawSprite'' routine.
  procedure DrawSprite(s: Sprite; xOffset: Longint; yOffset: Longint); overload;

  // Free the resources associated with a sprite.
  procedure FreeSprite(var s: Sprite); overload;

  // Determines if SwinGame has a sprite for the supplied name.
  // This checks against all sprites, those loaded without a name
  // are assigned a default.
  function HasSprite(name: String): Boolean; overload;

  // Indicates if a given SpritePack has already been created.
  function HasSpritePack(name: String): Boolean; overload;

  // Moves the sprite as indicated by its velocity. You can call this directly ot 
  // alternatively, this action is performed when the sprite is updated using
  // the ''UpdateSprite'' routine.
  procedure MoveSprite(s: Sprite); overload;

  // Moves the sprite a given distance based on the value passed in rather than
  // based on the sprite's velocity. Typically this method is used to apply
  // other movement actions to the sprite and the velocity of the sprite is
  // used the intended movement of the sprite.
  procedure MoveSprite(s: Sprite; const distance: Vector); overload;

  // Moves the sprite as indicated by a percentage of its velocity. You can call 
  // this directly ot alternatively, this action is performed when the sprite is
  // updated using the ''UpdateSprite'' routines that require a percentage.
  procedure MoveSprite(s: Sprite; pct: Single); overload;

  // Moves the sprite a percentage of a given distance based on the value 
  // passed in rather than based on the sprite's velocity. Typically this 
  // method is used to apply other movement actions to the sprite and the
  // velocity of the sprite is used the intended movement of the sprite.
  procedure MoveSprite(s: Sprite; const distance: Vector; pct: Single); overload;

  // This method moves a sprite to a given position in the game.
  procedure MoveSpriteTo(s: Sprite; x: Longint; y: Longint); overload;

  // Releases all of the sprites that have been loaded.
  procedure ReleaseAllSprites(); overload;

  // Releases the SwinGame resources associated with the sprite of the
  // specified ``name``.
  procedure ReleaseSprite(name: String); overload;

  // Selects the named SpritePack (if it has been created). The
  // selected SpritePack determines which sprites are drawn and updated
  // with the DrawAllSprites and UpdateAllSprites code.
  procedure SelectSpritePack(name: String); overload;

  // Adds a new layer to the sprite.
  function SpriteAddLayer(s: Sprite; newLayer: Bitmap; layerName: String): Longint; overload;

  // Alters the current velocity of the Sprite, adding the passed in vector to the current velocity.
  //
  // When the Sprite is updated (see ``UpdateSprite``)
  // this vector is used to move the Sprite.
  procedure SpriteAddToVelocity(s: Sprite; const value: Vector); overload;

  // Adds a new kind of value to the Sprite
  procedure SpriteAddValue(s: Sprite; name: String); overload;

  // Adds a new kind of value to the Sprite, setting the initial value
  // to the value passed in.
  procedure SpriteAddValue(s: Sprite; name: String; initVal: Single); overload;

  // Indicates if the sprites animation has ended.
  function SpriteAnimationHasEnded(s: Sprite): Boolean; overload;

  // Returns the name of the Sprite's current animation.
  function SpriteAnimationName(s: Sprite): String; overload;

  // Sends the layer specified forward in the visible layer order.
  procedure SpriteBringLayerForward(s: Sprite; visibleLayer: Longint); overload;

  // Sends the layer specified to the front in the visible layer order.
  procedure SpriteBringLayerToFront(s: Sprite; visibleLayer: Longint); overload;

  // Register a procedure to call when events occur on the sprite.
  procedure SpriteCallOnEvent(s: Sprite; handler: SpriteEventHandler); overload;

  // Gets a circle in the bounds of the base layer of the indicated sprite.
  function SpriteCircle(s: Sprite): Circle; overload;

  // Returns the bitmap used by the Sprite to determine if it has collided with
  // other objects in the game.
  function SpriteCollisionBitmap(s: Sprite): Bitmap; overload;

  // Gets a circle in the bounds of the indicated sprite's collision rectangle.
  function SpriteCollisionCircle(s: Sprite): Circle; overload;

  // Returns the kind of collision used with this Sprite. This is used when
  // determining if the Sprite has collided with other objects in the game.
  function SpriteCollisionKind(s: Sprite): CollisionTestKind; overload;

  // Returns the collision rectangle for the specified sprite.
  function SpriteCollisionRectangle(s: Sprite): Rectangle; overload;

  // Returns the current animation cell for an Animated Sprite. The cell is
  // updated when the sprite's animation data is updated.
  function SpriteCurrentCell(s: Sprite): Longint; overload;

  // Returns a rectangle of the current cell within the Sprite's image. This is used
  // to determine what part of the bitmap should be used when the Sprite is drawn.
  function SpriteCurrentCellRectangle(s: Sprite): Rectangle; overload;

  // Returns the X value of the Sprite's velocity.
  function SpriteDX(s: Sprite): Single; overload;

  // Returns the Y value of the Sprite's velocity.
  function SpriteDY(s: Sprite): Single; overload;

  // Returns the direction the Sprite is heading in degrees.
  function SpriteHeading(s: Sprite): Single; overload;

  // The current Height of the sprite (aligned to the Y axis).
  function SpriteHeight(s: Sprite): Longint; overload;

  // Hide the specified layer of the sprite.
  procedure SpriteHideLayer(s: Sprite; name: String); overload;

  // Hide the specified layer of the sprite.
  procedure SpriteHideLayer(s: Sprite; id: Longint); overload;

  // Returns the bitmap of the indicated layer of the sprite.
  function SpriteLayer(s: Sprite; idx: Longint): Bitmap; overload;

  // Returns the bitmap of the indicated layer of the sprite.
  function SpriteLayer(s: Sprite; name: String): Bitmap; overload;

  // Gets a circle in the bounds of the indicated layer.
  function SpriteLayerCircle(s: Sprite; idx: Longint): Circle; overload;

  // Gets a circle in the bounds of the indicated layer.
  function SpriteLayerCircle(s: Sprite; name: String): Circle; overload;

  // Returns the number of layers within the Sprite.
  function SpriteLayerCount(s: Sprite): Longint; overload;

  // The height of a given layer of the Sprite (aligned to the Y axis).
  function SpriteLayerHeight(s: Sprite; name: String): Longint; overload;

  // The height of a given layer of the Sprite (aligned to the Y axis).
  function SpriteLayerHeight(s: Sprite; idx: Longint): Longint; overload;

  // Returns the index of the specified layer.
  function SpriteLayerIndex(s: Sprite; name: String): Longint; overload;

  // Returns the name of the specified layer.
  function SpriteLayerName(s: Sprite; idx: Longint): String; overload;

  // Gets the offset of the specified layer.
  function SpriteLayerOffset(s: Sprite; name: String): Point2D; overload;

  // Gets the offset of the specified layer.
  function SpriteLayerOffset(s: Sprite; idx: Longint): Point2D; overload;

  // Returns the offets of the layers in the Sprite.
  function SpriteLayerOffsets(s: Sprite): Point2DArray; overload;

  // Gets a rectangle that surrounds the indicated layer.
  function SpriteLayerRectangle(s: Sprite; name: String): Rectangle; overload;

  // Gets a rectangle that surrounds the indicated layer.
  function SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle; overload;

  // The width of a given layer of the Sprite (aligned to the X axis).
  function SpriteLayerWidth(s: Sprite; name: String): Longint; overload;

  // The width of a given layer of the Sprite (aligned to the X axis).
  function SpriteLayerWidth(s: Sprite; idx: Longint): Longint; overload;

  // Returns the bitmaps of the layers in the Sprite.
  function SpriteLayers(s: Sprite): BitmapArray; overload;

  // This indicates the mass of the Sprite for any of the collide methods from
  // Physics. The mass of two colliding sprites will determine the relative
  // velocitys after the collision.
  function SpriteMass(s: Sprite): Single; overload;

  // This procedure starts the sprite moving to the indicated
  // destination point, over a specified number of seconds. When the 
  // sprite arrives it will raise the SpriteArrived event.
  procedure SpriteMoveTo(s: Sprite; const pt: Point2D; takingSeconds: Longint); overload;

  // Returns the name of the sprite. This name is used for resource management
  // and can be used to interact with the sprite in various routines.
  function SpriteName(sprt: Sprite): String; overload;

  // Returns the `Sprite` with the specified name,
  // see `CreateBasicSprite`.
  function SpriteNamed(name: String): Sprite; overload;

  // Returns True if the sprite is entirely off the screen.
  function SpriteOffscreen(s: Sprite): Boolean; overload;

  // Returns True if a pixel of the `Sprite` ``s`` is at the screen location
  // specified (``pt``), which is converted to a world location.
  function SpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;

  // Returns True if a pixel of the `Sprite` ``s`` is at the screen location
  // specified (``x`` and ``y``) which is converted to a world location.
  function SpriteOnScreenAt(s: Sprite; x: Longint; y: Longint): Boolean; overload;

  // Returns the Sprite's position.
  function SpritePosition(s: Sprite): Point2D; overload;

  // Restart the sprite's current animation, this will play a sound if the
  // first cell of the animation is associated with a sound effect.
  procedure SpriteReplayAnimation(s: Sprite); overload;

  // Restart the sprite's current animation, this will play a sound if
  // withSound is true and the first cell of the animation is associated with a sound effect.
  procedure SpriteReplayAnimation(s: Sprite; withSound: Boolean); overload;

  // This indicates the angle of rotation of the Sprite. This will rotate any 
  // images of the sprite before drawing, which can be very slow. Avoid using
  // this method with bitmap based Sprites where possible.
  function SpriteRotation(s: Sprite): Single; overload;

  // This indicates the scale of the Sprite. This will scale any 
  // images of the sprite before drawing, which can be very slow. Avoid using
  // this method with bitmap based Sprites where possible.
  function SpriteScale(s: Sprite): Single; overload;

  // Returns the rectangle representing the location of the Sprite on the
  // screen.
  function SpriteScreenRectangle(s: Sprite): Rectangle; overload;

  // Sends the layer specified backward in the visible layer order.
  procedure SpriteSendLayerBackward(s: Sprite; visibleLayer: Longint); overload;

  // Sends the layer specified to the back in the visible layer order.
  procedure SpriteSendLayerToBack(s: Sprite; visibleLayer: Longint); overload;

  // Sets the bitmap used by the Sprite to determine if it has collided with
  // other objects in the game. By default the CollisionBitmap is set to the
  // bitmap from the Sprite's first layer.
  procedure SpriteSetCollisionBitmap(s: Sprite; bmp: Bitmap); overload;

  // Sets the kind of collision used with this Sprite. This is used when
  // determining if the Sprite has collided with other objects in the game.
  procedure SpriteSetCollisionKind(s: Sprite; value: CollisionTestKind); overload;

  // Sets the X value of the Sprite's velocity.
  procedure SpriteSetDX(s: Sprite; value: Single); overload;

  // Sets the Y value of the Sprite's velocity.
  procedure SpriteSetDY(s: Sprite; value: Single); overload;

  // Alters the direction the Sprite is heading without changing the speed.
  procedure SpriteSetHeading(s: Sprite; value: Single); overload;

  // Sets the offset of the specified layer.
  procedure SpriteSetLayerOffset(s: Sprite; name: String; const value: Point2D); overload;

  // Sets the offset of the specified layer.
  procedure SpriteSetLayerOffset(s: Sprite; idx: Longint; const value: Point2D); overload;

  // Sets the layer offsets for the sprite.
  procedure SpriteSetLayerOffsets(s: Sprite; const values: Point2DArray); overload;

  // Allows you to change the mass of a Sprite.
  procedure SpriteSetMass(s: Sprite; value: Single); overload;

  // Sets the Sprite's position.
  procedure SpriteSetPosition(s: Sprite; const value: Point2D); overload;

  // Allows you to change the rotation of a Sprite.
  procedure SpriteSetRotation(s: Sprite; value: Single); overload;

  // Allows you to change the scale of a Sprite.
  procedure SpriteSetScale(s: Sprite; value: Single); overload;

  // Alters the speed of the Sprite without effecting the direction.
  procedure SpriteSetSpeed(s: Sprite; value: Single); overload;

  // Assigns a value to the Sprite.
  procedure SpriteSetValue(s: Sprite; name: String; val: Single); overload;

  // Assigns a value to the Sprite.
  procedure SpriteSetValue(s: Sprite; idx: Longint; val: Single); overload;

  // Sets the current velocity of the Sprite. When the Sprite is updated (see ``UpdateSprite``)
  // this vector is used to move the Sprite.
  procedure SpriteSetVelocity(s: Sprite; const value: Vector); overload;

  // Sets the X position of the Sprite.
  procedure SpriteSetX(s: Sprite; value: Single); overload;

  // Sets the Y position of the Sprite.
  procedure SpriteSetY(s: Sprite; value: Single); overload;

  // Show the specified layer of the sprite.
  function SpriteShowLayer(s: Sprite; id: Longint): Longint; overload;

  // Show the specified layer of the sprite.
  function SpriteShowLayer(s: Sprite; name: String): Longint; overload;

  // Returns the current speed (distance travelled per update) of the Sprite.
  function SpriteSpeed(s: Sprite): Single; overload;

  // Start playing an animation from the sprite's animation template.
  // This will play a sound effect if the first cell of the animation
  // has a sound.
  procedure SpriteStartAnimation(s: Sprite; idx: Longint); overload;

  // Start playing an animation from the sprite's animation template.
  // This will play a sound effect if the first cell of the animation
  // has a sound.
  procedure SpriteStartAnimation(s: Sprite; named: String); overload;

  // Start playing an animation from the sprite's animation template.
  // The withSound parameter determines whether to play a sound effect 
  // if the first cell of the animation has a sound.
  procedure SpriteStartAnimation(s: Sprite; named: String; withSound: Boolean); overload;

  // Start playing an animation from the sprite's animation template.
  // The withSound parameter determines whether to play a sound effect 
  // if the first cell of the animation has a sound.
  procedure SpriteStartAnimation(s: Sprite; idx: Longint; withSound: Boolean); overload;

  // Removes an event handler from the sprite, stopping events from this 
  // Sprite calling the indicated method.
  procedure SpriteStopCallingOnEvent(s: Sprite; handler: SpriteEventHandler); overload;

  // Toggle the visibility of the specified layer of the sprite.
  procedure SpriteToggleLayerVisible(s: Sprite; id: Longint); overload;

  // Toggle the visibility of the specified layer of the sprite.
  procedure SpriteToggleLayerVisible(s: Sprite; name: String); overload;

  // Returns the sprite's value at the index specified
  function SpriteValue(s: Sprite; index: Longint): Single; overload;

  // Returns the indicated value of the sprite
  function SpriteValue(s: Sprite; name: String): Single; overload;

  // Returns the count of sprite's values.
  function SpriteValueCount(s: Sprite): Longint; overload;

  // Returns the names of all of the values of the sprite
  function SpriteValueNames(s: Sprite): StringArray; overload;

  // Returns the current velocity of the Sprite. When the Sprite is updated (see ``UpdateSprite``)
  // this vector is used to move the Sprite.
  function SpriteVelocity(s: Sprite): Vector; overload;

  // Returns the index (z-order) of the sprite's layer.
  function SpriteVisibleIndexOfLayer(s: Sprite; id: Longint): Longint; overload;

  // Returns the index (z-order) of the sprite's layer.
  function SpriteVisibleIndexOfLayer(s: Sprite; name: String): Longint; overload;

  // Returns the index of the n'th (idx parameter) visible layer.
  function SpriteVisibleLayer(s: Sprite; idx: Longint): Longint; overload;

  // Returns the number of layers that are currently visible for the sprite.
  function SpriteVisibleLayerCount(s: Sprite): Longint; overload;

  // Returns the ids of the layers that are currently visible. In order back to front.
  function SpriteVisibleLayerIds(s: Sprite): LongintArray; overload;

  // The current Width of the sprite (aligned to the X axis).
  function SpriteWidth(s: Sprite): Longint; overload;

  // Returns the X position of the Sprite.
  function SpriteX(s: Sprite): Single; overload;

  // Returns the Y position of the Sprite.
  function SpriteY(s: Sprite): Single; overload;

  // Removes an global event handler, stopping events calling the indicated procedure.
  procedure StopCallingOnSpriteEvent(handler: SpriteEventHandler); overload;

  // Update all of the sprites in the current Sprite pack.
  procedure UpdateAllSprites(); overload;

  // Update all of the sprites in the current Sprite pack, passing in a
  // percentage value to indicate the percentage to update.
  procedure UpdateAllSprites(pct: Single); overload;

  // Update the position and animation details of the Sprite.
  // This will play a sound effect if the new cell of the animation
  // has a sound.
  procedure UpdateSprite(s: Sprite); overload;

  // Update the position and animation details of the Sprite.
  // This will play a sound effect if the new cell of the animation
  // has a sound and withSound is true.
  procedure UpdateSprite(s: Sprite; withSound: Boolean); overload;

  // Update the position and animation details of the Sprite by a 
  // given percentage of a single unit of movement/animation.
  // This will play a sound effect if the new cell of the animation
  // has a sound.
  procedure UpdateSprite(s: Sprite; pct: Single); overload;

  // Update the position and animation details of the Sprite by a 
  // given percentage of a single unit of movement/animation.
  // This will play a sound effect if the new cell of the animation
  // has a sound and withSound is true.
  procedure UpdateSprite(s: Sprite; pct: Single; withSound: Boolean); overload;

  // Updates the animation details of the sprite. 
  // This will play a sound effect if the new cell of the animation
  // has a sound.
  procedure UpdateSpriteAnimation(s: Sprite); overload;

  // Update the animation details of the Sprite.
  // This will play a sound effect if the new cell of the animation
  // has a sound and withSound is true.
  procedure UpdateSpriteAnimation(s: Sprite; withSound: Boolean); overload;

  // Update the animation details of the Sprite by a 
  // given percentage of a single unit of movement/animation.
  // This will play a sound effect if the new cell of the animation
  // has a sound.
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single); overload;

  // Update the position and animation details of the Sprite by a 
  // given percentage of a single unit of movement/animation.
  // This will play a sound effect if the new cell of the animation
  // has a sound and withSound is true.
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single; withSound: Boolean); overload;

  // Returns a `Vector` that is the difference in location from the center of
  // the sprite ``s`` to the point ``pt``.
  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector; overload;

  // Returns a `Vector` that is the difference in the position of two sprites
  // (``s1`` and ``s2``).
  function VectorFromTo(s1: Sprite; s2: Sprite): Vector; overload;

  // Draws the framerate to the screen using a simple font built into SwinGame.
  procedure DrawFramerate(x: Longint; y: Longint); overload;

  // Draws the framerate to the screen using the supplied font.
  procedure DrawFramerate(x: Longint; y: Longint; font: Font); overload;

  // Draws the framerate to the screen using the supplied font.
  procedure DrawFramerate(x: Longint; y: Longint; name: String); overload;

  // Draws the framerate to the screen using the supplied font.
  procedure DrawFramerate(x: Longint; y: Longint; name: String; size: Longint); overload;

  // Draws text using a simple bitmap font that is built into SwinGame.
  procedure DrawText(theText: String; textColor: Color; const pt: Point2D); overload;

  // Draws the text at the specified point using the color and font indicated.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;

  // Draws text using a simple bitmap font that is built into SwinGame.
  procedure DrawText(theText: String; textColor: Color; x: Single; y: Single); overload;

  // Draws the text at the specified point using the color and font indicated.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(theText: String; textColor: Color; name: String; const pt: Point2D); overload;

  // Draws the text onto the bitmap at the specified point using the color and font indicated.
  // Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  // good idea if the text does not change frequently.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; const pt: Point2D); overload;

  // Draws the text at the specified point using the color and font indicated.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(theText: String; textColor: Color; theFont: Font; x: Single; y: Single); overload;

  // Draws the text at the specified point using the color and font indicated.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(theText: String; textColor: Color; name: String; x: Single; y: Single); overload;

  // Draws text using a simple bitmap font that is built into SwinGame.
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; x: Single; y: Single); overload;

  // Draws the text onto the bitmap at the specified point using the color and font indicated.
  // Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  // good idea if the text does not change frequently.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;

  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;

  // Draws the text onto the bitmap at the specified point using the color and font indicated.
  // Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  // good idea if the text does not change frequently.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;

  // Draws theText at the specified point using the color and font indicated.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; x: Single; y: Single); overload;

  // Draws the text onto the bitmap at the specified x,y location using the color and font indicated.
  // Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  // good idea if the text does not change frequently.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; theFont: Font; x: Longint; y: Longint); overload;

  // Draws the text onto the bitmap at the specified x,y location using the color and font indicated.
  // Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  // good idea if the text does not change frequently.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; x: Longint; y: Longint); overload;

  // Draws the text onto the bitmap at the specified x,y location using the color and font indicated.
  // Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  // good idea if the text does not change frequently.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; size: Longint; x: Longint; y: Longint); overload;

  // Draws the text at the specified x,y location using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; x: Single; y: Single; w: Longint; h: Longint); overload;

  // Draws the text onto the bitmap at the specified x,y location using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Draws the text onto the bitmap at the specified x,y location using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Draws theText onto the bitmap at the specified x,y location using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Draws the text in the specified rectangle using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws the text in the specified rectangle using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws the text onto the bitmap in the rectangle using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws the text onto the bitmap in the rectangle using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws theText in the specified rectangle using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws the text onto the bitmap in the rectangle using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws the text at the specified x,y location using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; x: Single; y: Single; w: Longint; h: Longint); overload;

  // Draws the text at the specified x,y location using the fore and back colors, and the font indicated.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawText` procedure.
  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; x: Single; y: Single; w: Longint; h: Longint); overload;

  // Draws the text onto the screen at the specified x,y location using the fore and back colors, and the font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Draws the text onto the screen in the specified rectangle using the fore and back colors, and the font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws the text onto the screen in the specified rectangle using the fore and back colors, and the font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws the text onto the screen in the specified rectangle using the fore and back colors, and the font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;

  // Draws the text onto the screen at the specified x,y location using the fore and back colors, and the font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Draws the text onto the screen at the specified x,y location using the fore and back colors, and the font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version should be used to draw text that contains multiple lines, to draw a single line of text
  // use the `DrawTextOnScreen` procedure.
  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;

  // Draws text using a simple bitmap font that is built into SwinGame.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  procedure DrawTextOnScreen(theText: String; textColor: Color; x: Single; y: Single); overload;

  // Draws theText onto the screen at the specified point using the color and font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLinesOnScreen` procedure.
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; const pt: Point2D); overload;

  // Draws theText onto the screen at the specified point using the color and font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLinesOnScreen` procedure.
  procedure DrawTextOnScreen(theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;

  // Draws the text onto the screen at the specified x,y location using the color and font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLinesOnScreen` procedure.
  procedure DrawTextOnScreen(theText: String; textColor: Color; theFont: Font; x: Longint; y: Longint); overload;

  // Draws the text onto the screen at the specified x,y location using the color and font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLinesOnScreen` procedure.
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; x: Longint; y: Longint); overload;

  // Draws theText onto the screen at the specified point using the color and font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLinesOnScreen` procedure.
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;

  // Draws the text onto the screen at the specified x,y location using the color and font indicated.
  // As the text is draw directly onto the screen the camera location does not effect its position.
  // This is useful for drawing text on a HUD or similar actions.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLinesOnScreen` procedure.
  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; size: Longint; x: Longint; y: Longint); overload;

  // Draws the text onto the bitmap at the specified point using the color and font indicated.
  // Drawing text is a slow operation, and drawing it to a bitmap, then drawing the bitmap to screen is a
  // good idea if the text does not change frequently.
  //
  // This version only draws a single line of text, to draw text that contains line breaks use the
  // `DrawTextLines` procedure.
  function DrawTextTo(font: Font; str: String; clrFg: Color; backgroundColor: Color): Bitmap; overload;

  // Returns the style settings for the font.
  function FontFontStyle(font: Font): FontStyle; overload;

  // Determines the name that will be used for a font loaded with
  // the indicated fontName and size.
  function FontNameFor(fontName: String; size: Longint): String; overload;

  // Returns the `Font` that has been loaded with the specified name,
  // see `LoadFontNamed`.
  function FontNamed(name: String): Font; overload;

  // Returns the `Font` that has been loaded with the specified name,
  // and font size using `LoadFont`.
  function FontNamed(name: String; size: Longint): Font; overload;

  // Alters the style of the font. This is time consuming, so load
  // fonts multiple times and set the style for each if needed.
  procedure FontSetStyle(font: Font; value: FontStyle); overload;

  // Frees the resources used by the loaded Font.
  procedure FreeFont(var fontToFree: Font); overload;

  // Determines if SwinGame has a font loaded for the supplied name.
  // This checks against all fonts loaded, those loaded without a name
  // are assigned the filename as a default.
  function HasFont(name: String): Boolean; overload;

  // Loads a font from file with the specified side. Fonts must be freed using
  // the FreeFont routine once finished with. Once the font is loaded you
  // can set its style using SetFontStyle. Fonts are then used to draw and
  // measure text in your programs.
  function LoadFont(fontName: String; size: Longint): Font; overload;

  // Loads and returns a font that can be used to draw text. The supplied
  // ``filename`` is used to locate the font to load. The supplied ``name`` indicates the 
  // name to use to refer to this Font in SwinGame. The `Font` can then be
  // retrieved by passing this ``name`` to the `FontNamed` function.
  function LoadFontNamed(name: String; filename: String; size: Longint): Font; overload;

  // Releases all of the fonts that have been loaded.
  procedure ReleaseAllFonts(); overload;

  // Releases the SwinGame resources associated with the font of the
  // specified ``name``.
  procedure ReleaseFont(name: String); overload;

  // Returns the font alignment for the passed in character (l = left. r = right, c = center).
  function TextAlignmentFrom(str: String): FontAlignment; overload;

  // Returns the height (in pixels) of the passed in text and the font it will be drawn with.
  function TextHeight(theFont: Font; theText: String): Longint; overload;

  // Returns the width (in pixels) of the passed in text and the font it will be drawn with.
  function TextWidth(theFont: Font; theText: String): Longint; overload;

  // Create and return a new Timer. The timer will not be started, and will have
  // an initial 'ticks' of 0.
  function CreateTimer(): Timer; overload;

  // Create and return a new Timer. The timer will not be started, and will have
  // an initial 'ticks' of 0.
  function CreateTimer(name: String): Timer; overload;

  // Free a created timer.
  procedure FreeTimer(var toFree: Timer); overload;

  // Pause the timer, getting ticks from a paused timer
  // will continue to return the same time.
  procedure PauseTimer(toPause: Timer); overload;

  // Pause the timer, getting ticks from a paused timer
  // will continue to return the same time.
  procedure PauseTimer(name: String); overload;

  // Releases all of the timers that have been loaded.
  procedure ReleaseAllTimers(); overload;

  // Release the resources used by the timer with
  // the indicated name.
  procedure ReleaseTimer(name: String); overload;

  // Resets the time of a given timer
  procedure ResetTimer(name: String); overload;

  // Resets the time of a given timer
  procedure ResetTimer(tmr: Timer); overload;

  // Resumes a paused timer.
  procedure ResumeTimer(name: String); overload;

  // Resumes a paused timer.
  procedure ResumeTimer(toUnpause: Timer); overload;

  // Start a timer recording the time that has passed.
  procedure StartTimer(name: String); overload;

  // Start a timer recording the time that has passed.
  procedure StartTimer(toStart: Timer); overload;

  // Stop the timer. The time is reset to 0 and you must
  // recall start to begin the timer ticking again.
  procedure StopTimer(toStop: Timer); overload;

  // Stop the timer. The time is reset to 0 and you must
  // recall start to begin the timer ticking again.
  procedure StopTimer(name: String); overload;

  // Get the timer created with the indicated named.
  function TimerNamed(name: String): Timer; overload;

  // Gets the number of ticks (milliseconds) that have passed since the timer
  // was started/reset. When paused the timer's ticks will not advance until
  // the timer is once again resumed.
  function TimerTicks(name: String): Longword; overload;

  // Gets the number of ticks (milliseconds) that have passed since the timer
  // was started/reset. When paused the timer's ticks will not advance until
  // the timer is once again resumed.
  function TimerTicks(toGet: Timer): Longword; overload;

  // Returns the calculated framerate averages, highest, and lowest values along with
  // the suggested rendering color.
  procedure CalculateFramerate(out average: String; out highest: String; out lowest: String; out textColor: Color); overload;

  // Puts the process to sleep for a specified number of
  // milliseconds. This can be used to add delays into your
  // game.
  procedure Delay(time: Longword); overload;

  // This function can be used to retrieve a message containing the details of 
  // the last error that occurred in SwinGame.
  function ExceptionMessage(): String; overload;

  // This function tells you if an error occurred with the last operation in
  // SwinGame.
  function ExceptionOccured(): Boolean; overload;

  // Returns the average framerate for the last 10 frames as an integer.
  function GetFramerate(): Longint; overload;

  // Gets the number of milliseconds that have passed. This can be used to
  // determine timing operations, such as updating the game elements.
  function GetTicks(): Longword; overload;

  // Generates a random number between 0 and 1.
  function Rnd(): Single; overload;

  // Generates a random integer up to (but not including) ubound. Effectively,
  // the ubound value specifies the number of random values to create.
  function Rnd(ubound: Longint): Longint; overload;

  // Retrieves a string representing the version of SwinGame that is executing.
  // This can be used to check that the version supports the features required
  // for your game.
  function SwinGameVersion(): String; overload;

  // Activate the passed in panel. If shown, the panel will be clickable. This is the default state of a panel.
  procedure ActivatePanel(p: Panel); overload;

  // Takes a radiogroup and returns the active button
  function ActiveRadioButton(grp: GUIRadioGroup): Region; overload;

  // Takes an ID and returns the active button
  function ActiveRadioButton(id: String): Region; overload;

  // Takes a panel and an ID and returns the active button
  function ActiveRadioButton(pnl: Panel; id: String): Region; overload;

  // Takes a radiogroup and returns the active button's index.
  function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): Longint; overload;

  // Takes a radiogroup and returns the active button's index.
  function ActiveRadioButtonIndex(id: String): Longint; overload;

  // Takes a radiogroup and returns the active button's index.
  function ActiveRadioButtonIndex(pnl: Panel; id: String): Longint; overload;

  // Returns the parent panel of the active textbox
  function ActiveTextBoxParent(): Panel; overload;

  // Returns the index of the active textbox's region.
  function ActiveTextIndex(): Longint; overload;

  // Returns true when the region has been clicked.
  function ButtonClicked(name: String): Boolean; overload;

  // Returns true when the region has been clicked.
  function ButtonClicked(r: Region): Boolean; overload;

  // Takes a region and returns the checkbox of that region
  function CheckboxFromRegion(r: Region): GUICheckbox; overload;

  // Sets the checkbox state to val.
  procedure CheckboxSetState(chk: GUICheckbox; val: Boolean); overload;

  // Sets the checkbox state to val given the ID.
  procedure CheckboxSetState(id: String; val: Boolean); overload;

  // Sets the checkbox state to val.
  procedure CheckboxSetState(r: Region; val: Boolean); overload;

  // Sets the checkbox state to val.
  procedure CheckboxSetState(pnl: Panel; id: String; val: Boolean); overload;

  // Returns checkbox state of the checkbox with ID from string
  function CheckboxState(r: Region): Boolean; overload;

  // Returns checkbox state of the checkbox with ID from string
  function CheckboxState(s: String): Boolean; overload;

  // Returns checkbox state of the given checkbox
  function CheckboxState(chk: GUICheckbox): Boolean; overload;

  // Returns checkbox state of the checkbox with ID in a given Panel
  function CheckboxState(p: Panel; s: String): Boolean; overload;

  // Deactivate the panel. The panel will become unclickable, it will remain visible if it was already.
  procedure DeactivatePanel(p: Panel); overload;

  // Deactivates the active textbox
  procedure DeactivateTextBox(); overload;

  // Gets if the dialog has been cancelled
  function DialogCancelled(): Boolean; overload;

  // Gets if the dialog has been Completed
  function DialogComplete(): Boolean; overload;

  // Gets the path of the dialog
  function DialogPath(): String; overload;

  // Sets the path of the dialog
  procedure DialogSetPath(fullname: String); overload;

  // Sets the GUI whether or not to use Vector Drawing
  procedure DrawGUIAsVectors(b: Boolean); overload;

  // Draw the currently visible panels (For use in the main loop)
  procedure DrawInterface(); overload;

  // Finishes reading text and stores in the active textbox
  procedure FinishReadingText(); overload;

  // Disposes of the panel by panel
  procedure FreePanel(var pnl: Panel); overload;

  // Returns true if any of the panels in the user interface have been clicked.
  function GUIClicked(): Boolean; overload;

  // Sets the active textbox from region
  procedure GUISetActiveTextbox(r: Region); overload;

  // Sets the active textbox to the one with the
  // indicated name.
  procedure GUISetActiveTextbox(name: String); overload;

  // Sets the active textbox
  procedure GUISetActiveTextbox(t: GUITextbox); overload;

  // Sets the Background color of the GUI
  procedure GUISetBackgroundColor(c: Color); overload;

  // Sets the inactive ForeGround color of the GUI
  procedure GUISetBackgroundColorInactive(c: Color); overload;

  // Sets the ForeGround color of the GUI
  procedure GUISetForegroundColor(c: Color); overload;

  // Sets the inactive ForeGround color of the GUI
  procedure GUISetForegroundColorInactive(c: Color); overload;

  // Returns the textbox in which text was changed/added into most recently.
  function GUITextBoxOfTextEntered(): GUITextbox; overload;

  // Checks if TextEntry finished, returns true/false
  function GUITextEntryComplete(): Boolean; overload;

  // Returns if panel is in Index Collection
  function HasPanel(name: String): Boolean; overload;

  // Hide the panel, stop drawing it. Panels which are not being draw can not be interacted with by the user.
  procedure HidePanel(name: String); overload;

  // Hide the panel, stop drawing it. Panels which are not being draw can not be interacted with by the user.
  procedure HidePanel(p: Panel); overload;

  // Returns the index of the region of the textbox in which text was changed/added into most recently.
  function IndexOfLastUpdatedTextBox(): Longint; overload;

  // Returns if anything is currently being dragged
  function IsDragging(): Boolean; overload;

  // Returns if panel is currently being dragged
  function IsDragging(pnl: Panel): Boolean; overload;

  // Returns FontAlignment from label given region
  function LabelAlignment(r: Region): FontAlignment; overload;

  // Returns FontAlignment from label given region
  function LabelAlignment(lbl: GUILabel): FontAlignment; overload;

  // Get Font From Label
  function LabelFont(r: Region): Font; overload;

  // Get Font From Label
  function LabelFont(l: GUILabel): Font; overload;

  // returns a GUILabel From given region
  function LabelFromRegion(r: Region): GUILabel; overload;

  // Sets FontAlignment for label given region
  procedure LabelSetAlignment(r: Region; align: FontAlignment); overload;

  // Sets FontAlignment for label given region
  procedure LabelSetAlignment(tb: GUILabel; align: FontAlignment); overload;

  // Set Font For Label
  procedure LabelSetFont(l: GUILabel; s: String); overload;

  // Set text for Label
  procedure LabelSetText(lb: GUILabel; newString: String); overload;

  // Set text for Label
  procedure LabelSetText(r: Region; newString: String); overload;

  // Set text for Label
  procedure LabelSetText(id: String; newString: String); overload;

  // Set text for Label
  procedure LabelSetText(pnl: Panel; id: String; newString: String); overload;

  // Get text From Label
  function LabelText(r: Region): String; overload;

  // Get text From Label
  function LabelText(lb: GUILabel): String; overload;

  // Get text From Label
  function LabelText(id: String): String; overload;

  // Get text From Label
  function LabelText(pnl: Panel; id: String): String; overload;

  // Returns active item's index from the list
  function ListActiveItemIndex(lst: GUIList): Longint; overload;

  // Returns active item's index from the list
  function ListActiveItemIndex(id: String): Longint; overload;

  // Returns active item's index from the list of the region
  function ListActiveItemIndex(r: Region): Longint; overload;

  // Returns active item's index from the list
  function ListActiveItemIndex(pnl: Panel; id: String): Longint; overload;

  // Returns the text of the active item in the list of the region
  function ListActiveItemText(list: GUIList): String; overload;

  // Returns the text of the active item in the list of the region
  function ListActiveItemText(r: Region): String; overload;

  // Returns the active item text of the List in with ID
  function ListActiveItemText(ID: String): String; overload;

  // Returns the active item text of the List in panel, pnl- with ID, ID
  function ListActiveItemText(pnl: Panel; ID: String): String; overload;

  // Adds an item to the list by bitmap
  procedure ListAddItem(lst: GUIList; img: Bitmap); overload;

  // Adds an item to the list by bitmap
  procedure ListAddItem(id: String; img: Bitmap); overload;

  // Adds an item to the list where the items shows a cell of a
  // bitmap.
  procedure ListAddItem(lst: GUIList; const img: BitmapCell); overload;

  // Adds an item to the list by text
  procedure ListAddItem(lst: GUIList; text: String); overload;

  // Adds an item to the list by text
  procedure ListAddItem(id: String; text: String); overload;

  // Adds an item to the list by bitmap
  procedure ListAddItem(r: Region; img: Bitmap); overload;

  // Adds an item to the list where the items shows a cell of a
  // bitmap.
  procedure ListAddItem(r: Region; const img: BitmapCell); overload;

  // Adds an item to the list where the items shows a cell of a
  // bitmap.
  procedure ListAddItem(id: String; const img: BitmapCell); overload;

  // Adds an item to the list by text
  procedure ListAddItem(r: Region; text: String); overload;

  // Adds an item to the list by bitmap
  procedure ListAddItem(pnl: Panel; id: String; img: Bitmap); overload;

  // Adds an item to the list where the items shows a cell of a
  // bitmap and some text.
  procedure ListAddItem(r: Region; const img: BitmapCell; text: String); overload;

  // Adds an item to the list by text and Bitmap
  procedure ListAddItem(lst: GUIList; img: Bitmap; text: String); overload;

  // Adds an item to the list where the items shows a cell of a
  // bitmap.
  procedure ListAddItem(pnl: Panel; id: String; const img: BitmapCell); overload;

  // Adds an item to the list by text and Bitmap
  procedure ListAddItem(id: String; img: Bitmap; text: String); overload;

  // Adds an item to the list
  procedure ListAddItem(r: Region; img: Bitmap; text: String); overload;

  // Adds an item to the list where the items shows a cell of a
  // bitmap and some text.
  procedure ListAddItem(lst: GUIList; const img: BitmapCell; text: String); overload;

  // Adds an item to the list by text
  procedure ListAddItem(pnl: Panel; id: String; text: String); overload;

  // Adds an item to the list where the items shows a cell of a
  // bitmap and some text.
  procedure ListAddItem(id: String; const img: BitmapCell; text: String); overload;

  // Adds an item to the list where the items shows a cell of a
  // bitmap and some text.
  procedure ListAddItem(pnl: Panel; id: String; const img: BitmapCell; text: String); overload;

  // Adds an item to the list by text and Bitmap
  procedure ListAddItem(pnl: Panel; id: String; img: Bitmap; text: String); overload;

  // Returns the index of the item with the bitmap, img
  function ListBitmapIndex(lst: GUIList; img: Bitmap): Longint; overload;

  // Returns the index of the item with the bitmap and cell.
  function ListBitmapIndex(lst: GUIList; const img: BitmapCell): Longint; overload;

  // Removes all items from the list.
  procedure ListClearItems(id: String); overload;

  // Removes all items from the list.
  procedure ListClearItems(lst: GUIList); overload;

  // Removes all items from the list of the region
  procedure ListClearItems(r: Region); overload;

  // Removes all items from the list.
  procedure ListClearItems(pnl: Panel; id: String); overload;

  // Returns the font of the list
  function ListFont(lst: GUIList): Font; overload;

  // Returns the font of the list of the region
  function ListFont(r: Region): Font; overload;

  // Returns the font alignment of a list
  function ListFontAlignment(lst: GUIList): FontAlignment; overload;

  // Returns the font alignment of a list from region
  function ListFontAlignment(r: Region): FontAlignment; overload;

  // Returns Returns the list of the region r
  function ListFromRegion(r: Region): GUIList; overload;

  // Returns the number of items in the list
  function ListItemCount(lst: GUIList): Longint; overload;

  // Returns the number of items in the list of the region
  function ListItemCount(r: Region): Longint; overload;

  // Returns the number of items in the list
  function ListItemCount(id: String): Longint; overload;

  // Returns the number of items in the list
  function ListItemCount(pnl: Panel; id: String): Longint; overload;

  // Returns the text of the item at index idx
  function ListItemText(id: String; idx: Longint): String; overload;

  // Returns the text of the item at index idx
  function ListItemText(lst: GUIList; idx: Longint): String; overload;

  // Returns the text of the item at index idx from the List of the Region
  function ListItemText(r: Region; idx: Longint): String; overload;

  // Returns the text of the item at index idx
  function ListItemText(pnl: Panel; id: String; idx: Longint): String; overload;

  // Returns the largest index that startingAt should be set to.
  function ListLargestStartIndex(lst: GUIList): Longint; overload;

  // Removes the active item from a list
  procedure ListRemoveActiveItem(r: Region); overload;

  // Removes the active item from a list
  procedure ListRemoveActiveItem(id: String); overload;

  // Removes the active item from a list
  procedure ListRemoveActiveItem(pnl: Panel; id: String); overload;

  // Removes item at index idx from the list
  procedure ListRemoveItem(id: String; idx: Longint); overload;

  // Removes item at index idx from the list
  procedure ListRemoveItem(lst: GUIList; idx: Longint); overload;

  // Removes item at index idx from the list
  procedure ListRemoveItem(pnl: Panel; id: String; idx: Longint); overload;

  // Returns the largest index that startingAt should be set to.
  function ListScrollIncrement(lst: GUIList): Longint; overload;

  // Set the active item in the list to the item at index idx
  procedure ListSetActiveItemIndex(id: String; idx: Longint); overload;

  // Set the active item in the list to the item at index idx
  procedure ListSetActiveItemIndex(lst: GUIList; idx: Longint); overload;

  // Set the active item in the list to the item at index idx
  procedure ListSetActiveItemIndex(pnl: Panel; id: String; idx: Longint); overload;

  // Sets the font of the list to font f
  procedure ListSetFont(lst: GUIList; f: Font); overload;

  // Returns the font alignment of a list from region
  procedure ListSetFontAlignment(r: Region; align: FontAlignment); overload;

  // Returns the font alignment of a list
  procedure ListSetFontAlignment(lst: GUIList; align: FontAlignment); overload;

  // Sets the starting point for the list from region
  procedure ListSetStartAt(r: Region; idx: Longint); overload;

  // Sets the starting point for the list
  procedure ListSetStartAt(lst: GUIList; idx: Longint); overload;

  // Returns the starting point for the list
  function ListStartAt(lst: GUIList): Longint; overload;

  // Returns the starting point for the list from region
  function ListStartAt(r: Region): Longint; overload;

  // returns the id of a value in the list.
  function ListTextIndex(lst: GUIList; value: String): Longint; overload;

  // Loads panel from panel directory with filename
  function LoadPanel(filename: String): Panel; overload;

  // maps panel to name in Hash Table.
  function LoadPanelNamed(name: String; filename: String): Panel; overload;

  // Move panel along vector
  procedure MovePanel(p: Panel; const mvmt: Vector); overload;

  // Creates a new panel
  function NewPanel(pnlName: String): Panel; overload;

  // Returns whether panel is active
  function PanelActive(pnl: Panel): Boolean; overload;

  // Returns the panel at the point passed in. Returns nil if there is no panel.
  function PanelAtPoint(const pt: Point2D): Panel; overload;

  // Returns the last panel clicked.
  function PanelClicked(): Panel; overload;

  // Returns true when the panel was clicked.
  function PanelClicked(pnl: Panel): Boolean; overload;

  // Returns whether or not the passed panel is currently draggable
  function PanelDraggable(p: Panel): Boolean; overload;

  // Returns panel filename
  function PanelFilename(pnl: Panel): String; overload;

  // Returns panel h value
  function PanelHeight(p: Panel): Longint; overload;

  // Returns height of the panel
  function PanelHeight(name: String): Longint; overload;

  // Returns the name of the panel
  function PanelName(pnl: Panel): String; overload;

  // Returns panel with the name name
  function PanelNamed(name: String): Panel; overload;

  // Sets panel's draggability to the passed Boolean
  procedure PanelSetDraggable(p: Panel; b: Boolean); overload;

  // Returns true if panel is currently visible.
  function PanelVisible(p: Panel): Boolean; overload;

  // Returns the panel's width
  function PanelWidth(name: String): Longint; overload;

  // Returns the panel's width
  function PanelWidth(p: Panel): Longint; overload;

  // Returns panel x value
  function PanelX(p: Panel): Single; overload;

  // Returns panel y value
  function PanelY(p: Panel): Single; overload;

  // Returns true if point is in any region within the panel
  function PointInRegion(const pt: Point2D; p: Panel): Boolean; overload;

  // Returns true if point is in a region with the indicate `kind` of the panel `p`.
  function PointInRegion(const pt: Point2D; p: Panel; kind: GUIElementKind): Boolean; overload;

  // Takes an ID and returns the RadioGroup.
  function RadioGroupFromId(id: String): GUIRadioGroup; overload;

  // Takes panel and ID and returns the RadioGroup.
  function RadioGroupFromId(pnl: Panel; id: String): GUIRadioGroup; overload;

  // Takes region and returns the RadioGroup.
  function RadioGroupFromRegion(r: Region): GUIRadioGroup; overload;

  // Returns true when the region is active.
  function RegionActive(forRegion: Region): Boolean; overload;

  // Returns the region from the panel at the point
  function RegionAtPoint(p: Panel; const pt: Point2D): Region; overload;

  // Returns the last region clicked on by user.
  function RegionClicked(): Region; overload;

  // Returns the ID of the last region clicked on by user.
  function RegionClickedID(): String; overload;

  // Returns the Region height value
  function RegionHeight(r: Region): Longint; overload;

  // Returns the ID of the last region clicked on by user.
  function RegionID(r: Region): String; overload;

  // Returns the region of the textbox in which text was changed/added into most recently.
  function RegionOfLastUpdatedTextBox(): Region; overload;

  // Returns the Region with the ID passed from the panel passed
  function RegionPanel(r: Region): Panel; overload;

  // Returns the Region Wdith value
  function RegionWidth(r: Region): Longint; overload;

  // Returns the Region with the ID passed
  function RegionWithID(ID: String): Region; overload;

  // Returns the Region with the ID passed from the panel passed
  function RegionWithID(pnl: Panel; ID: String): Region; overload;

  // Returns the Region X value
  function RegionX(r: Region): Single; overload;

  // Returns the Region Y value
  function RegionY(r: Region): Single; overload;

  // Registers the callback with the panel, when an event related to this
  // region occurs the procedure registered will be called.
  procedure RegisterEventCallback(r: Region; callback: GUIEventCallback); overload;

  // Disposes of all panels
  procedure ReleaseAllPanels(); overload;

  // Disposes of the panel by name, removing it from the index collection, setting its dragging to nil, and hiding it first to avoid crashes.
  procedure ReleasePanel(name: String); overload;

  // Takes a region and an ID and selects the button
  procedure SelectRadioButton(r: Region); overload;

  // Takes an ID and returns the active button
  procedure SelectRadioButton(id: String); overload;

  // Takes a RadioGroup and Region and selects the button
  procedure SelectRadioButton(rGroup: GUIRadioGroup; r: Region); overload;

  // Takes a panel and an ID and selects the button
  procedure SelectRadioButton(pnl: Panel; id: String); overload;

  // Takes a RadioGroup and index and selects the button
  procedure SelectRadioButton(rGroup: GUIRadioGroup; idx: Longint); overload;

  // Sets the region active to Boolean
  procedure SetRegionActive(forRegion: Region; b: Boolean); overload;

  // Displays an OpenDialog
  procedure ShowOpenDialog(); overload;

  // Displays an OpenDialog file/folder/both filter
  procedure ShowOpenDialog(select: FileDialogSelectType); overload;

  // Display the panel on screen at panel's co-ordinates.
  procedure ShowPanel(p: Panel); overload;

  // Display the panel on screen at panel's co-ordinates.
  procedure ShowPanel(name: String); overload;

  // shows dialog panel
  procedure ShowPanelDialog(p: Panel); overload;

  // Displays a SaveDialog
  procedure ShowSaveDialog(); overload;

  // Displays a SaveDialog with file/folder/both filter
  procedure ShowSaveDialog(select: FileDialogSelectType); overload;

  // returns font of textbox
  function TextBoxFont(tb: GUITextbox): Font; overload;

  // returns font of region's textbox (If it has one)
  function TextBoxFont(r: Region): Font; overload;

  // The the TextBox from an ID
  function TextBoxFromID(id: String): GUITextbox; overload;

  // Sets the textbox text from region
  function TextBoxFromRegion(r: Region): GUITextbox; overload;

  // Gets the textbox text
  function TextBoxText(tb: GUITextbox): String; overload;

  // Gets the textbox text from region
  function TextBoxText(id: String): String; overload;

  // Gets the textbox text from region
  function TextBoxText(r: Region): String; overload;

  // Gets the textbox text from region
  function TextBoxText(pnl: Panel; id: String): String; overload;

  // Returns the alignment of the text in textbox passed in as region
  function TextboxAlignment(r: Region): FontAlignment; overload;

  // Gets the textbox alignmnet of the textbox
  function TextboxAlignment(tb: GUITextbox): FontAlignment; overload;

  // Set the textbox alignment of the textbox passed in
  procedure TextboxSetAlignment(r: Region; align: FontAlignment); overload;

  // Set the textbox alignment of the textbox
  procedure TextboxSetAlignment(tb: GUITextbox; align: FontAlignment); overload;

  // Sets the textbox font
  procedure TextboxSetFont(Tb: GUITextbox; f: Font); overload;

  // Sets the textbox text from Id
  procedure TextboxSetText(id: String; i: Longint); overload;

  // Sets the textbox text from region
  procedure TextboxSetText(id: String; single: Single); overload;

  // Sets the textbox text from region
  procedure TextboxSetText(r: Region; s: String); overload;

  // Sets the textbox text from region
  procedure TextboxSetText(tb: GUITextbox; s: String); overload;

  // Sets the textbox text from Id
  procedure TextboxSetText(id: String; s: String); overload;

  // Sets the textbox text from region
  procedure TextboxSetText(r: Region; i: Longint); overload;

  // Sets the textbox text from region
  procedure TextboxSetText(tb: GUITextbox; i: Longint); overload;

  // Sets the textbox text from Textbox
  procedure TextboxSetText(tb: GUITextbox; single: Single); overload;

  // Sets the textbox text from region
  procedure TextboxSetText(r: Region; single: Single); overload;

  // Sets the textbox text from Panel and Id
  procedure TextboxSetText(pnl: Panel; id: String; s: String); overload;

  // Sets the textbox text from panel and Id
  procedure TextboxSetText(pnl: Panel; id: String; i: Longint); overload;

  // Sets the textbox text from Panel and Id
  procedure TextboxSetText(pnl: Panel; id: String; single: Single); overload;

  // Activates the panel if deactivated, and deactivates if activated.
  procedure ToggleActivatePanel(p: Panel); overload;

  // Toggles the state of a checkbox (ticked/unticked)
  procedure ToggleCheckboxState(id: String); overload;

  // Toggles the state of a checkbox (ticked/unticked)
  procedure ToggleCheckboxState(c: GUICheckbox); overload;

  // Toggles the state of a checkbox (ticked/unticked)
  procedure ToggleCheckboxState(pnl: Panel; id: String); overload;

  // Toggles the region active state
  procedure ToggleRegionActive(forRegion: Region); overload;

  // Toggles whether the panel is being shown or not.
  procedure ToggleShowPanel(p: Panel); overload;

  // UpdateInterface main loop, checks the draggable, checks the region clicked, updates the interface
  procedure UpdateInterface(); overload;

  // Returns the ArduinoDevice with the indicated name.
  function ArduinoDeviceNamed(name: String): ArduinoDevice; overload;

  // Returns true if there is data waiting to be read from the device.
  function ArduinoHasData(dev: ArduinoDevice): Boolean; overload;

  // Read a Byte from the ArduinoDevice. Has a short
  // timeout and returns 0 if no byte is read within the given time.
  function ArduinoReadByte(dev: ArduinoDevice): Byte; overload;

  // Reads a byte from the ArduinoDevice, with the given timeout in milliseconds.
  // Returns 0 if no byte is read within the given time.
  function ArduinoReadByte(dev: ArduinoDevice; timeout: Longint): Byte; overload;

  // Reads a line of text from the ArduinoDevice. This
  // returns an empty string if nothing is read within a
  // few milliseconds.
  function ArduinoReadLine(dev: ArduinoDevice): String; overload;

  // Reads a line of text from the ArduinoDevice, within a 
  // specified amount of time.
  function ArduinoReadLine(dev: ArduinoDevice; timeout: Longint): String; overload;

  // Send a byte value to the arduino device.
  procedure ArduinoSendByte(dev: ArduinoDevice; value: Byte); overload;

  // Send a string value to the arduino device.
  procedure ArduinoSendString(dev: ArduinoDevice; value: String); overload;

  // Send a string value to the arduino device, along with a newline
  // so the arduino can identify the end of the sent data.
  procedure ArduinoSendStringLine(dev: ArduinoDevice; value: String); overload;

  // Creates an Arduino device at the specified port, with
  // the indicated baud. The name of the device matches its port.
  function CreateArduinoDevice(port: String; baud: Longint): ArduinoDevice; overload;

  // Creates an Arduino device with the given name, 
  // at the specified port, with the indicated baud.
  function CreateArduinoDevice(name: String; port: String; baud: Longint): ArduinoDevice; overload;

  // Close the connection to the Arduino Device and dispose
  // of the resources associated with the Device.
  procedure FreeArduinoDevice(var dev: ArduinoDevice); overload;

  // Does an ArduinoDevice exist with the indicated name?
  function HasArduinoDevice(name: String): Boolean; overload;

  // Release all of the ArduinoDevices
  procedure ReleaseAllArduinoDevices(); overload;

  // Release the ArduinoDevice with the indicated name.
  procedure ReleaseArduinoDevice(name: String); overload;

Procedure LoadDefaultColors();

var
      ColorWhite :        Color = $FFFFFFFF;
      ColorGreen :        Color = $FF00FF00;
      ColorBlue :         Color = $FF0000FF;
      ColorBlack :        Color = $FF000000;
      ColorRed :          Color = $FFFF0000;
      ColorYellow :       Color = $FFFFFF00;
      ColorPink :         Color = $FFFF1493;
      ColorTurquoise :    Color = $FF00CED1;
      ColorGrey :         Color = $FF808080;
      ColorMagenta :      Color = $FF00FFFF;
      ColorTransparent :  Color = $00000000;
      ColorLightGrey :    Color = $FFC8C8C8;

implementation

  procedure LoadDefaultColors();
  begin
      ColorWhite :=         sgGraphics.ColorWhite;
      ColorGreen :=         sgGraphics.ColorGreen;
      ColorBlue :=          sgGraphics.ColorBlue ;
      ColorBlack :=         sgGraphics.ColorBlack;
      ColorRed :=           sgGraphics.ColorRed  ;
      ColorYellow :=        sgGraphics.ColorYellow;
      ColorPink :=          sgGraphics.ColorPink;
      ColorTurquoise :=     sgGraphics.ColorTurquoise;
      ColorGrey :=          sgGraphics.ColorGrey;
      ColorMagenta :=       sgGraphics.ColorMagenta;
      ColorTransparent :=   sgGraphics.ColorTransparent;
      ColorLightGrey :=     sgGraphics.ColorLightGrey;
  end;
    
  function AnimationCurrentCell(anim: Animation): Longint; overload;
  begin
    result := sgAnimations.AnimationCurrentCell(anim);
  end;

  function AnimationCurrentVector(anim: Animation): Vector; overload;
  begin
    result := sgAnimations.AnimationCurrentVector(anim);
  end;

  function AnimationEnded(anim: Animation): Boolean; overload;
  begin
    result := sgAnimations.AnimationEnded(anim);
  end;

  function AnimationEnteredFrame(anim: Animation): Boolean; overload;
  begin
    result := sgAnimations.AnimationEnteredFrame(anim);
  end;

  function AnimationFrameTime(anim: Animation): Single; overload;
  begin
    result := sgAnimations.AnimationFrameTime(anim);
  end;

  function AnimationIndex(temp: AnimationScript; name: String): Longint; overload;
  begin
    result := sgAnimations.AnimationIndex(temp,name);
  end;

  function AnimationName(temp: Animation): String; overload;
  begin
    result := sgAnimations.AnimationName(temp);
  end;

  function AnimationName(temp: AnimationScript; idx: Longint): String; overload;
  begin
    result := sgAnimations.AnimationName(temp,idx);
  end;

  function AnimationScriptName(script: AnimationScript): String; overload;
  begin
    result := sgAnimations.AnimationScriptName(script);
  end;

  function AnimationScriptNamed(name: String): AnimationScript; overload;
  begin
    result := sgAnimations.AnimationScriptNamed(name);
  end;

  procedure AssignAnimation(anim: Animation; name: String; script: AnimationScript); overload;
  begin
    sgAnimations.AssignAnimation(anim,name,script);
  end;

  procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript); overload;
  begin
    sgAnimations.AssignAnimation(anim,idx,script);
  end;

  procedure AssignAnimation(anim: Animation; name: String; script: AnimationScript; withSound: Boolean); overload;
  begin
    sgAnimations.AssignAnimation(anim,name,script,withSound);
  end;

  procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript; withSound: Boolean); overload;
  begin
    sgAnimations.AssignAnimation(anim,idx,script,withSound);
  end;

  function CreateAnimation(identifier: Longint; script: AnimationScript): Animation; overload;
  begin
    result := sgAnimations.CreateAnimation(identifier,script);
  end;

  function CreateAnimation(identifier: String; script: AnimationScript): Animation; overload;
  begin
    result := sgAnimations.CreateAnimation(identifier,script);
  end;

  function CreateAnimation(identifier: Longint; script: AnimationScript; withSound: Boolean): Animation; overload;
  begin
    result := sgAnimations.CreateAnimation(identifier,script,withSound);
  end;

  function CreateAnimation(identifier: String; script: AnimationScript; withSound: Boolean): Animation; overload;
  begin
    result := sgAnimations.CreateAnimation(identifier,script,withSound);
  end;

  procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
  begin
    sgAnimations.DrawAnimation(ani,bmp,pt);
  end;

  procedure DrawAnimation(ani: Animation; bmp: Bitmap; x: Longint; y: Longint); overload;
  begin
    sgAnimations.DrawAnimation(ani,bmp,x,y);
  end;

  procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
  begin
    sgAnimations.DrawAnimation(dest,ani,bmp,pt);
  end;

  procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; x: Longint; y: Longint); overload;
  begin
    sgAnimations.DrawAnimation(dest,ani,bmp,x,y);
  end;

  procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
  begin
    sgAnimations.DrawAnimationOnScreen(ani,bmp,pt);
  end;

  procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; x: Longint; y: Longint); overload;
  begin
    sgAnimations.DrawAnimationOnScreen(ani,bmp,x,y);
  end;

  procedure FreeAnimation(var ani: Animation); overload;
  begin
    sgAnimations.FreeAnimation(ani);
  end;

  procedure FreeAnimationScript(var scriptToFree: AnimationScript); overload;
  begin
    sgAnimations.FreeAnimationScript(scriptToFree);
  end;

  function HasAnimationScript(name: String): Boolean; overload;
  begin
    result := sgAnimations.HasAnimationScript(name);
  end;

  function LoadAnimationScript(filename: String): AnimationScript; overload;
  begin
    result := sgAnimations.LoadAnimationScript(filename);
  end;

  function LoadAnimationScriptNamed(name: String; filename: String): AnimationScript; overload;
  begin
    result := sgAnimations.LoadAnimationScriptNamed(name,filename);
  end;

  procedure ReleaseAllAnimationScripts(); overload;
  begin
    sgAnimations.ReleaseAllAnimationScripts();
  end;

  procedure ReleaseAnimationScript(name: String); overload;
  begin
    sgAnimations.ReleaseAnimationScript(name);
  end;

  procedure RestartAnimation(anim: Animation); overload;
  begin
    sgAnimations.RestartAnimation(anim);
  end;

  procedure RestartAnimation(anim: Animation; withSound: Boolean); overload;
  begin
    sgAnimations.RestartAnimation(anim,withSound);
  end;

  procedure UpdateAnimation(anim: Animation); overload;
  begin
    sgAnimations.UpdateAnimation(anim);
  end;

  procedure UpdateAnimation(anim: Animation; pct: Single); overload;
  begin
    sgAnimations.UpdateAnimation(anim,pct);
  end;

  procedure UpdateAnimation(anim: Animation; pct: Single; withSound: Boolean); overload;
  begin
    sgAnimations.UpdateAnimation(anim,pct,withSound);
  end;

  function AudioReady(): Boolean; overload;
  begin
    result := sgAudio.AudioReady();
  end;

  procedure CloseAudio(); overload;
  begin
    sgAudio.CloseAudio();
  end;

  procedure FadeMusicIn(mus: Music; ms: Longint); overload;
  begin
    sgAudio.FadeMusicIn(mus,ms);
  end;

  procedure FadeMusicIn(name: String; ms: Longint); overload;
  begin
    sgAudio.FadeMusicIn(name,ms);
  end;

  procedure FadeMusicIn(name: String; loops: Longint; ms: Longint); overload;
  begin
    sgAudio.FadeMusicIn(name,loops,ms);
  end;

  procedure FadeMusicIn(mus: Music; loops: Longint; ms: Longint); overload;
  begin
    sgAudio.FadeMusicIn(mus,loops,ms);
  end;

  procedure FadeMusicOut(ms: Longint); overload;
  begin
    sgAudio.FadeMusicOut(ms);
  end;

  procedure FreeMusic(var mus: Music); overload;
  begin
    sgAudio.FreeMusic(mus);
  end;

  procedure FreeSoundEffect(var effect: SoundEffect); overload;
  begin
    sgAudio.FreeSoundEffect(effect);
  end;

  function HasMusic(name: String): Boolean; overload;
  begin
    result := sgAudio.HasMusic(name);
  end;

  function HasSoundEffect(name: String): Boolean; overload;
  begin
    result := sgAudio.HasSoundEffect(name);
  end;

  function LoadMusic(filename: String): Music; overload;
  begin
    result := sgAudio.LoadMusic(filename);
  end;

  function LoadMusicNamed(name: String; filename: String): Music; overload;
  begin
    result := sgAudio.LoadMusicNamed(name,filename);
  end;

  function LoadSoundEffect(filename: String): SoundEffect; overload;
  begin
    result := sgAudio.LoadSoundEffect(filename);
  end;

  function LoadSoundEffectNamed(name: String; filename: String): SoundEffect; overload;
  begin
    result := sgAudio.LoadSoundEffectNamed(name,filename);
  end;

  function MusicFilename(mus: Music): String; overload;
  begin
    result := sgAudio.MusicFilename(mus);
  end;

  function MusicName(mus: Music): String; overload;
  begin
    result := sgAudio.MusicName(mus);
  end;

  function MusicNamed(name: String): Music; overload;
  begin
    result := sgAudio.MusicNamed(name);
  end;

  function MusicPlaying(): Boolean; overload;
  begin
    result := sgAudio.MusicPlaying();
  end;

  function MusicVolume(): Single; overload;
  begin
    result := sgAudio.MusicVolume();
  end;

  procedure OpenAudio(); overload;
  begin
    sgAudio.OpenAudio();
  end;

  procedure PauseMusic(); overload;
  begin
    sgAudio.PauseMusic();
  end;

  procedure PlayMusic(mus: Music); overload;
  begin
    sgAudio.PlayMusic(mus);
  end;

  procedure PlayMusic(name: String); overload;
  begin
    sgAudio.PlayMusic(name);
  end;

  procedure PlayMusic(mus: Music; loops: Longint); overload;
  begin
    sgAudio.PlayMusic(mus,loops);
  end;

  procedure PlayMusic(name: String; loops: Longint); overload;
  begin
    sgAudio.PlayMusic(name,loops);
  end;

  procedure PlaySoundEffect(effect: SoundEffect); overload;
  begin
    sgAudio.PlaySoundEffect(effect);
  end;

  procedure PlaySoundEffect(name: String); overload;
  begin
    sgAudio.PlaySoundEffect(name);
  end;

  procedure PlaySoundEffect(name: String; vol: Single); overload;
  begin
    sgAudio.PlaySoundEffect(name,vol);
  end;

  procedure PlaySoundEffect(effect: SoundEffect; vol: Single); overload;
  begin
    sgAudio.PlaySoundEffect(effect,vol);
  end;

  procedure PlaySoundEffect(name: String; loops: Longint); overload;
  begin
    sgAudio.PlaySoundEffect(name,loops);
  end;

  procedure PlaySoundEffect(effect: SoundEffect; loops: Longint); overload;
  begin
    sgAudio.PlaySoundEffect(effect,loops);
  end;

  procedure PlaySoundEffect(effect: SoundEffect; loops: Longint; vol: Single); overload;
  begin
    sgAudio.PlaySoundEffect(effect,loops,vol);
  end;

  procedure PlaySoundEffect(name: String; loops: Longint; vol: Single); overload;
  begin
    sgAudio.PlaySoundEffect(name,loops,vol);
  end;

  procedure ReleaseAllMusic(); overload;
  begin
    sgAudio.ReleaseAllMusic();
  end;

  procedure ReleaseAllSoundEffects(); overload;
  begin
    sgAudio.ReleaseAllSoundEffects();
  end;

  procedure ReleaseMusic(name: String); overload;
  begin
    sgAudio.ReleaseMusic(name);
  end;

  procedure ReleaseSoundEffect(name: String); overload;
  begin
    sgAudio.ReleaseSoundEffect(name);
  end;

  procedure ResumeMusic(); overload;
  begin
    sgAudio.ResumeMusic();
  end;

  procedure SetMusicVolume(value: Single); overload;
  begin
    sgAudio.SetMusicVolume(value);
  end;

  function SoundEffectFilename(effect: SoundEffect): String; overload;
  begin
    result := sgAudio.SoundEffectFilename(effect);
  end;

  function SoundEffectName(effect: SoundEffect): String; overload;
  begin
    result := sgAudio.SoundEffectName(effect);
  end;

  function SoundEffectNamed(name: String): SoundEffect; overload;
  begin
    result := sgAudio.SoundEffectNamed(name);
  end;

  function SoundEffectPlaying(effect: SoundEffect): Boolean; overload;
  begin
    result := sgAudio.SoundEffectPlaying(effect);
  end;

  function SoundEffectPlaying(name: String): Boolean; overload;
  begin
    result := sgAudio.SoundEffectPlaying(name);
  end;

  procedure StopMusic(); overload;
  begin
    sgAudio.StopMusic();
  end;

  procedure StopSoundEffect(effect: SoundEffect); overload;
  begin
    sgAudio.StopSoundEffect(effect);
  end;

  procedure StopSoundEffect(name: String); overload;
  begin
    sgAudio.StopSoundEffect(name);
  end;

  function TryOpenAudio(): Boolean; overload;
  begin
    result := sgAudio.TryOpenAudio();
  end;

  function CameraPos(): Point2D; overload;
  begin
    result := sgCamera.CameraPos();
  end;

  function CameraScreenRect(): Rectangle; overload;
  begin
    result := sgCamera.CameraScreenRect();
  end;

  function CameraX(): Single; overload;
  begin
    result := sgCamera.CameraX();
  end;

  function CameraY(): Single; overload;
  begin
    result := sgCamera.CameraY();
  end;

  procedure CenterCameraOn(c: Character; const offset: Vector); overload;
  begin
    sgCamera.CenterCameraOn(c,offset);
  end;

  procedure CenterCameraOn(s: Sprite; const offset: Vector); overload;
  begin
    sgCamera.CenterCameraOn(s,offset);
  end;

  procedure CenterCameraOn(s: Sprite; offsetX: Longint; offsetY: Longint); overload;
  begin
    sgCamera.CenterCameraOn(s,offsetX,offsetY);
  end;

  procedure MoveCameraBy(const offset: Vector); overload;
  begin
    sgCamera.MoveCameraBy(offset);
  end;

  procedure MoveCameraBy(dx: Single; dy: Single); overload;
  begin
    sgCamera.MoveCameraBy(dx,dy);
  end;

  procedure MoveCameraTo(const pt: Point2D); overload;
  begin
    sgCamera.MoveCameraTo(pt);
  end;

  procedure MoveCameraTo(x: Single; y: Single); overload;
  begin
    sgCamera.MoveCameraTo(x,y);
  end;

  function PointOnScreen(const pt: Point2D): Boolean; overload;
  begin
    result := sgCamera.PointOnScreen(pt);
  end;

  function RectOnScreen(const rect: Rectangle): Boolean; overload;
  begin
    result := sgCamera.RectOnScreen(rect);
  end;

  procedure SetCameraPos(const pt: Point2D); overload;
  begin
    sgCamera.SetCameraPos(pt);
  end;

  procedure SetCameraX(x: Single); overload;
  begin
    sgCamera.SetCameraX(x);
  end;

  procedure SetCameraY(y: Single); overload;
  begin
    sgCamera.SetCameraY(y);
  end;

  function ToScreen(const worldPoint: Point2D): Point2D; overload;
  begin
    result := sgCamera.ToScreen(worldPoint);
  end;

  function ToScreen(const rect: Rectangle): Rectangle; overload;
  begin
    result := sgCamera.ToScreen(rect);
  end;

  function ToScreenX(worldX: Single): Longint; overload;
  begin
    result := sgCamera.ToScreenX(worldX);
  end;

  function ToScreenY(worldY: Single): Longint; overload;
  begin
    result := sgCamera.ToScreenY(worldY);
  end;

  function ToWorld(const screenPoint: Point2D): Point2D; overload;
  begin
    result := sgCamera.ToWorld(screenPoint);
  end;

  function ToWorldX(screenX: Longint): Single; overload;
  begin
    result := sgCamera.ToWorldX(screenX);
  end;

  function ToWorldY(screenY: Longint): Single; overload;
  begin
    result := sgCamera.ToWorldY(screenY);
  end;

  function CharacterAngleAt(c: Character; index: Longint): DirectionAngles; overload;
  begin
    result := sgCharacters.CharacterAngleAt(c,index);
  end;

  function CharacterAngleMaxAt(c: Character; index: Longint): Longint; overload;
  begin
    result := sgCharacters.CharacterAngleMaxAt(c,index);
  end;

  function CharacterAngleMinAt(c: Character; index: Longint): Longint; overload;
  begin
    result := sgCharacters.CharacterAngleMinAt(c,index);
  end;

  function CharacterAnglesLength(c: Character): Longint; overload;
  begin
    result := sgCharacters.CharacterAnglesLength(c);
  end;

  function CharacterCharacterName(c: Character): String; overload;
  begin
    result := sgCharacters.CharacterCharacterName(c);
  end;

  function CharacterCurrentDirection(c: Character): Longint; overload;
  begin
    result := sgCharacters.CharacterCurrentDirection(c);
  end;

  function CharacterCurrentState(c: Character): Longint; overload;
  begin
    result := sgCharacters.CharacterCurrentState(c);
  end;

  function CharacterDirectionCount(c: Character): Longint; overload;
  begin
    result := sgCharacters.CharacterDirectionCount(c);
  end;

  function CharacterDirections(c: Character): StringArray; overload;
  begin
    result := sgCharacters.CharacterDirections(c);
  end;

  function CharacterFilename(c: Character): String; overload;
  begin
    result := sgCharacters.CharacterFilename(c);
  end;

  function CharacterName(c: Character): String; overload;
  begin
    result := sgCharacters.CharacterName(c);
  end;

  function CharacterNamed(name: String): Character; overload;
  begin
    result := sgCharacters.CharacterNamed(name);
  end;

  procedure CharacterSetCurrentDirection(c: Character; direction: Longint); overload;
  begin
    sgCharacters.CharacterSetCurrentDirection(c,direction);
  end;

  procedure CharacterSetCurrentState(c: Character; state: Longint); overload;
  begin
    sgCharacters.CharacterSetCurrentState(c,state);
  end;

  procedure CharacterSetName(c: Character; name: String); overload;
  begin
    sgCharacters.CharacterSetName(c,name);
  end;

  procedure CharacterSetType(c: Character; name: String); overload;
  begin
    sgCharacters.CharacterSetType(c,name);
  end;

  procedure CharacterSetValue(c: Character; idx: Longint; val: Single); overload;
  begin
    sgCharacters.CharacterSetValue(c,idx,val);
  end;

  procedure CharacterSetValue(c: Character; name: String; val: Single); overload;
  begin
    sgCharacters.CharacterSetValue(c,name,val);
  end;

  function CharacterShownLayersAt(c: Character; index: Longint): Boolean; overload;
  begin
    result := sgCharacters.CharacterShownLayersAt(c,index);
  end;

  function CharacterSprite(c: Character): Sprite; overload;
  begin
    result := sgCharacters.CharacterSprite(c);
  end;

  function CharacterStateCount(c: Character): Longint; overload;
  begin
    result := sgCharacters.CharacterStateCount(c);
  end;

  function CharacterStates(c: Character): StringArray; overload;
  begin
    result := sgCharacters.CharacterStates(c);
  end;

  function CharacterType(c: Character): String; overload;
  begin
    result := sgCharacters.CharacterType(c);
  end;

  function CharacterValueAt(c: Character; index: Longint): Single; overload;
  begin
    result := sgCharacters.CharacterValueAt(c,index);
  end;

  function CharacterValueCount(c: Character): Longint; overload;
  begin
    result := sgCharacters.CharacterValueCount(c);
  end;

  function CharacterValueNames(c: Character): StringArray; overload;
  begin
    result := sgCharacters.CharacterValueNames(c);
  end;

  procedure DrawCharacter(c: Character); overload;
  begin
    sgCharacters.DrawCharacter(c);
  end;

  procedure DrawCharacterSprite(c: Character); overload;
  begin
    sgCharacters.DrawCharacterSprite(c);
  end;

  procedure DrawCharacterWithStationary(c: Character; stationaryState: Longint; state: Longint); overload;
  begin
    sgCharacters.DrawCharacterWithStationary(c,stationaryState,state);
  end;

  procedure FreeCharacter(var c: Character); overload;
  begin
    sgCharacters.FreeCharacter(c);
  end;

  function HasCharacter(name: String): Boolean; overload;
  begin
    result := sgCharacters.HasCharacter(name);
  end;

  function LoadCharacter(filename: String): Character; overload;
  begin
    result := sgCharacters.LoadCharacter(filename);
  end;

  function LoadCharacterNamed(name: String; filename: String): Character; overload;
  begin
    result := sgCharacters.LoadCharacterNamed(name,filename);
  end;

  procedure ReleaseAllCharacters(); overload;
  begin
    sgCharacters.ReleaseAllCharacters();
  end;

  procedure ReleaseCharacter(name: String); overload;
  begin
    sgCharacters.ReleaseCharacter(name);
  end;

  procedure SetActiveLayer(c: Character); overload;
  begin
    sgCharacters.SetActiveLayer(c);
  end;

  procedure ToggleLayerVisibility(c: Character; index: Longint); overload;
  begin
    sgCharacters.ToggleLayerVisibility(c,index);
  end;

  function UpdateDirectionAnimation(c: Character): Boolean; overload;
  begin
    result := sgCharacters.UpdateDirectionAnimation(c);
  end;

  function UpdateDirectionAnimationWithStationary(c: Character; state: Longint; newState: Longint): Boolean; overload;
  begin
    result := sgCharacters.UpdateDirectionAnimationWithStationary(c,state,newState);
  end;

  function AddVectors(const v1: Vector; const v2: Vector): Vector; overload;
  begin
    result := sgGeometry.AddVectors(v1,v2);
  end;

  procedure ApplyMatrix(const m: Matrix2D; var pts: Point2DArray); overload;
  begin
    sgGeometry.ApplyMatrix(m,pts);
  end;

  procedure ApplyMatrix(const m: Matrix2D; var tri: Triangle); overload;
  begin
    sgGeometry.ApplyMatrix(m,tri);
  end;

  function CalculateAngle(const v1: Vector; const v2: Vector): Single; overload;
  begin
    result := sgGeometry.CalculateAngle(v1,v2);
  end;

  function CalculateAngle(s1: Sprite; s2: Sprite): Single; overload;
  begin
    result := sgGeometry.CalculateAngle(s1,s2);
  end;

  function CalculateAngle(x1: Single; y1: Single; x2: Single; y2: Single): Single; overload;
  begin
    result := sgGeometry.CalculateAngle(x1,y1,x2,y2);
  end;

  function CalculateAngleBetween(const pt1: Point2D; const pt2: Point2D): Single; overload;
  begin
    result := sgGeometry.CalculateAngleBetween(pt1,pt2);
  end;

  function CenterPoint(const c: Circle): Point2D; overload;
  begin
    result := sgGeometry.CenterPoint(c);
  end;

  function CircleAt(x: Single; y: Single; radius: Longint): Circle; overload;
  begin
    result := sgGeometry.CircleAt(x,y,radius);
  end;

  function CircleAt(const pt: Point2D; radius: Longint): Circle; overload;
  begin
    result := sgGeometry.CircleAt(pt,radius);
  end;

  function CircleRadius(const c: Circle): Longint; overload;
  begin
    result := sgGeometry.CircleRadius(c);
  end;

  function CircleWithinRect(const c: Circle; const rect: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.CircleWithinRect(c,rect);
  end;

  function CircleX(const c: Circle): Single; overload;
  begin
    result := sgGeometry.CircleX(c);
  end;

  function CircleY(const c: Circle): Single; overload;
  begin
    result := sgGeometry.CircleY(c);
  end;

  function ClosestPointOnCircle(const fromPt: Point2D; const c: Circle): Point2D; overload;
  begin
    result := sgGeometry.ClosestPointOnCircle(fromPt,c);
  end;

  function ClosestPointOnLine(x: Single; y: Single; const line: LineSegment): Point2D; overload;
  begin
    result := sgGeometry.ClosestPointOnLine(x,y,line);
  end;

  function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;
  begin
    result := sgGeometry.ClosestPointOnLine(fromPt,line);
  end;

  function ClosestPointOnLineFromCircle(const c: Circle; const line: LineSegment): Point2D; overload;
  begin
    result := sgGeometry.ClosestPointOnLineFromCircle(c,line);
  end;

  function ClosestPointOnLinesFromCircle(const c: Circle; const lines: LinesArray): Point2D; overload;
  begin
    result := sgGeometry.ClosestPointOnLinesFromCircle(c,lines);
  end;

  function ClosestPointOnRectFromCircle(const c: Circle; const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.ClosestPointOnRectFromCircle(c,rect);
  end;

  function Cosine(angle: Single): Single; overload;
  begin
    result := sgGeometry.Cosine(angle);
  end;

  function CreateCircle(x: Single; y: Single; radius: Longint): Circle; overload;
  begin
    result := sgGeometry.CreateCircle(x,y,radius);
  end;

  function CreateCircle(const pt: Point2D; radius: Longint): Circle; overload;
  begin
    result := sgGeometry.CreateCircle(pt,radius);
  end;

  function CreateLine(x1: Single; y1: Single; x2: Single; y2: Single): LineSegment; overload;
  begin
    result := sgGeometry.CreateLine(x1,y1,x2,y2);
  end;

  function CreateLine(const pt1: Point2D; const pt2: Point2D): LineSegment; overload;
  begin
    result := sgGeometry.CreateLine(pt1,pt2);
  end;

  function CreateLineAsVector(const line: LineSegment): Vector; overload;
  begin
    result := sgGeometry.CreateLineAsVector(line);
  end;

  function CreateLineFromVector(const mv: Vector): LineSegment; overload;
  begin
    result := sgGeometry.CreateLineFromVector(mv);
  end;

  function CreateLineFromVector(const pt: Point2D; const mv: Vector): LineSegment; overload;
  begin
    result := sgGeometry.CreateLineFromVector(pt,mv);
  end;

  function CreateLineFromVector(x: Single; y: Single; const mv: Vector): LineSegment; overload;
  begin
    result := sgGeometry.CreateLineFromVector(x,y,mv);
  end;

  function CreateRectangle(x: Single; y: Single; w: Longint; h: Longint): Rectangle; overload;
  begin
    result := sgGeometry.CreateRectangle(x,y,w,h);
  end;

  function CreateRectangle(const lines: LinesArray): Rectangle; overload;
  begin
    result := sgGeometry.CreateRectangle(lines);
  end;

  function CreateRectangle(const tri: Triangle): Rectangle; overload;
  begin
    result := sgGeometry.CreateRectangle(tri);
  end;

  function CreateRectangle(const line: LineSegment): Rectangle; overload;
  begin
    result := sgGeometry.CreateRectangle(line);
  end;

  function CreateRectangle(const c: Circle): Rectangle; overload;
  begin
    result := sgGeometry.CreateRectangle(c);
  end;

  function CreateRectangle(const pt1: Point2D; const pt2: Point2D): Rectangle; overload;
  begin
    result := sgGeometry.CreateRectangle(pt1,pt2);
  end;

  function CreateRectangle(const pt: Point2D; width: Longint; height: Longint): Rectangle; overload;
  begin
    result := sgGeometry.CreateRectangle(pt,width,height);
  end;

  function CreateTriangle(ax: Single; ay: Single; bx: Single; by: Single; cx: Single; cy: Single): Triangle; overload;
  begin
    result := sgGeometry.CreateTriangle(ax,ay,bx,by,cx,cy);
  end;

  function CreateTriangle(const a: Point2D; const b: Point2D; const c: Point2D): Triangle; overload;
  begin
    result := sgGeometry.CreateTriangle(a,b,c);
  end;

  function CreateVectorFromAngle(angle: Single; magnitude: Single): Vector; overload;
  begin
    result := sgGeometry.CreateVectorFromAngle(angle,magnitude);
  end;

  function CreateVectorFromPointToRect(const pt: Point2D; const rect: Rectangle): Vector; overload;
  begin
    result := sgGeometry.CreateVectorFromPointToRect(pt,rect);
  end;

  function CreateVectorFromPointToRect(x: Single; y: Single; const rect: Rectangle): Vector; overload;
  begin
    result := sgGeometry.CreateVectorFromPointToRect(x,y,rect);
  end;

  function CreateVectorFromPointToRect(x: Single; y: Single; rectX: Single; rectY: Single; rectWidth: Longint; rectHeight: Longint): Vector; overload;
  begin
    result := sgGeometry.CreateVectorFromPointToRect(x,y,rectX,rectY,rectWidth,rectHeight);
  end;

  function CreateVectorFromPoints(const p1: Point2D; const p2: Point2D): Vector; overload;
  begin
    result := sgGeometry.CreateVectorFromPoints(p1,p2);
  end;

  function CreateVectorToPoint(const p1: Point2D): Vector; overload;
  begin
    result := sgGeometry.CreateVectorToPoint(p1);
  end;

  function DistantPointOnCircle(const pt: Point2D; const c: Circle): Point2D; overload;
  begin
    result := sgGeometry.DistantPointOnCircle(pt,c);
  end;

  function DistantPointOnCircleHeading(const pt: Point2D; const c: Circle; const heading: Vector; out oppositePt: Point2D): Boolean; overload;
  begin
    result := sgGeometry.DistantPointOnCircleHeading(pt,c,heading,oppositePt);
  end;

  function DotProduct(const v1: Vector; const v2: Vector): Single; overload;
  begin
    result := sgGeometry.DotProduct(v1,v2);
  end;

  procedure FixRectangle(var rect: Rectangle); overload;
  begin
    sgGeometry.FixRectangle(rect);
  end;

  procedure FixRectangle(var x: Single; var y: Single; var width: Longint; var height: Longint); overload;
  begin
    sgGeometry.FixRectangle(x,y,width,height);
  end;

  function IdentityMatrix(): Matrix2D; overload;
  begin
    result := sgGeometry.IdentityMatrix();
  end;

  function InsetRectangle(const rect: Rectangle; insetAmount: Longint): Rectangle; overload;
  begin
    result := sgGeometry.InsetRectangle(rect,insetAmount);
  end;

  function Intersection(const rect1: Rectangle; const rect2: Rectangle): Rectangle; overload;
  begin
    result := sgGeometry.Intersection(rect1,rect2);
  end;

  function InvertVector(const v: Vector): Vector; overload;
  begin
    result := sgGeometry.InvertVector(v);
  end;

  function LimitVector(const v: Vector; limit: Single): Vector; overload;
  begin
    result := sgGeometry.LimitVector(v,limit);
  end;

  function LineAsVector(const line: LineSegment): Vector; overload;
  begin
    result := sgGeometry.LineAsVector(line);
  end;

  function LineCircleHit(const c: Circle; const velocity: Vector; const lines: LinesArray; out found: LineSegment): Boolean; overload;
  begin
    result := sgGeometry.LineCircleHit(c,velocity,lines,found);
  end;

  function LineFrom(x1: Single; y1: Single; x2: Single; y2: Single): LineSegment; overload;
  begin
    result := sgGeometry.LineFrom(x1,y1,x2,y2);
  end;

  function LineFrom(const pt1: Point2D; const pt2: Point2D): LineSegment; overload;
  begin
    result := sgGeometry.LineFrom(pt1,pt2);
  end;

  function LineFromVector(const mv: Vector): LineSegment; overload;
  begin
    result := sgGeometry.LineFromVector(mv);
  end;

  function LineFromVector(const pt: Point2D; const mv: Vector): LineSegment; overload;
  begin
    result := sgGeometry.LineFromVector(pt,mv);
  end;

  function LineFromVector(x: Single; y: Single; const mv: Vector): LineSegment; overload;
  begin
    result := sgGeometry.LineFromVector(x,y,mv);
  end;

  function LineIntersectionPoint(const line1: LineSegment; const line2: LineSegment; out pt: Point2D): Boolean; overload;
  begin
    result := sgGeometry.LineIntersectionPoint(line1,line2,pt);
  end;

  function LineIntersectsCircle(const l: LineSegment; const c: Circle): Boolean; overload;
  begin
    result := sgGeometry.LineIntersectsCircle(l,c);
  end;

  function LineIntersectsLines(const line: LineSegment; const lines: LinesArray): Boolean; overload;
  begin
    result := sgGeometry.LineIntersectsLines(line,lines);
  end;

  function LineIntersectsRect(const line: LineSegment; const rect: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.LineIntersectsRect(line,rect);
  end;

  function LineMagnitudeSq(const line: LineSegment): Single; overload;
  begin
    result := sgGeometry.LineMagnitudeSq(line);
  end;

  function LineMagnitudeSq(x1: Single; y1: Single; x2: Single; y2: Single): Single; overload;
  begin
    result := sgGeometry.LineMagnitudeSq(x1,y1,x2,y2);
  end;

  function LineMidPoint(const line: LineSegment): Point2D; overload;
  begin
    result := sgGeometry.LineMidPoint(line);
  end;

  function LineNormal(const line: LineSegment): Vector; overload;
  begin
    result := sgGeometry.LineNormal(line);
  end;

  function LineSegmentsIntersect(const line1: LineSegment; const line2: LineSegment): Boolean; overload;
  begin
    result := sgGeometry.LineSegmentsIntersect(line1,line2);
  end;

  function LineToString(const ln: LineSegment): String; overload;
  begin
    result := sgGeometry.LineToString(ln);
  end;

  function LinesFrom(const rect: Rectangle): LinesArray; overload;
  begin
    result := sgGeometry.LinesFrom(rect);
  end;

  function LinesFrom(const tri: Triangle): LinesArray; overload;
  begin
    result := sgGeometry.LinesFrom(tri);
  end;

  function LinesRectIntersect(const lines: LinesArray; const r: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.LinesRectIntersect(lines,r);
  end;

  function MatrixMultiply(const m: Matrix2D; const v: Vector): Vector; overload;
  begin
    result := sgGeometry.MatrixMultiply(m,v);
  end;

  function MatrixMultiply(const m1: Matrix2D; const m2: Matrix2D): Matrix2D; overload;
  begin
    result := sgGeometry.MatrixMultiply(m1,m2);
  end;

  function MatrixToString(const m: Matrix2D): String; overload;
  begin
    result := sgGeometry.MatrixToString(m);
  end;

  function PointAdd(const pt1: Point2D; const pt2: Point2D): Point2D; overload;
  begin
    result := sgGeometry.PointAdd(pt1,pt2);
  end;

  function PointAt(x: Single; y: Single): Point2D; overload;
  begin
    result := sgGeometry.PointAt(x,y);
  end;

  function PointAt(const startPoint: Point2D; const offset: Vector): Point2D; overload;
  begin
    result := sgGeometry.PointAt(startPoint,offset);
  end;

  function PointInCircle(const pt: Point2D; const c: Circle): Boolean; overload;
  begin
    result := sgGeometry.PointInCircle(pt,c);
  end;

  function PointInCircle(ptX: Single; ptY: Single; cX: Single; cY: Single; radius: Single): Boolean; overload;
  begin
    result := sgGeometry.PointInCircle(ptX,ptY,cX,cY,radius);
  end;

  function PointInRect(const pt: Point2D; const rect: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.PointInRect(pt,rect);
  end;

  function PointInRect(x: Single; y: Single; const rect: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.PointInRect(x,y,rect);
  end;

  function PointInRect(const pt: Point2D; x: Single; y: Single; w: Single; h: Single): Boolean; overload;
  begin
    result := sgGeometry.PointInRect(pt,x,y,w,h);
  end;

  function PointInRect(ptX: Single; ptY: Single; x: Single; y: Single; w: Single; h: Single): Boolean; overload;
  begin
    result := sgGeometry.PointInRect(ptX,ptY,x,y,w,h);
  end;

  function PointInTriangle(const pt: Point2D; const tri: Triangle): Boolean; overload;
  begin
    result := sgGeometry.PointInTriangle(pt,tri);
  end;

  function PointLineDistance(x: Single; y: Single; const line: LineSegment): Single; overload;
  begin
    result := sgGeometry.PointLineDistance(x,y,line);
  end;

  function PointLineDistance(const pt: Point2D; const line: LineSegment): Single; overload;
  begin
    result := sgGeometry.PointLineDistance(pt,line);
  end;

  function PointOnLine(const pt: Point2D; const line: LineSegment): Boolean; overload;
  begin
    result := sgGeometry.PointOnLine(pt,line);
  end;

  function PointOnPoint(const pt1: Point2D; const pt2: Point2D): Boolean; overload;
  begin
    result := sgGeometry.PointOnPoint(pt1,pt2);
  end;

  function PointPointDistance(const pt1: Point2D; const pt2: Point2D): Single; overload;
  begin
    result := sgGeometry.PointPointDistance(pt1,pt2);
  end;

  function PointToString(const pt: Point2D): String; overload;
  begin
    result := sgGeometry.PointToString(pt);
  end;

  function PointsFrom(const rect: Rectangle): Point2DArray; overload;
  begin
    result := sgGeometry.PointsFrom(rect);
  end;

  function PointsFrom(const line: LineSegment): Point2DArray; overload;
  begin
    result := sgGeometry.PointsFrom(line);
  end;

  function RandomScreenPoint(): Point2D; overload;
  begin
    result := sgGeometry.RandomScreenPoint();
  end;

  function RayCircleIntersectDistance(const ray_origin: Point2D; const ray_heading: Vector; const c: Circle): Single; overload;
  begin
    result := sgGeometry.RayCircleIntersectDistance(ray_origin,ray_heading,c);
  end;

  function RayIntersectionPoint(const fromPt: Point2D; const heading: Vector; const line: LineSegment; out pt: Point2D): Boolean; overload;
  begin
    result := sgGeometry.RayIntersectionPoint(fromPt,heading,line,pt);
  end;

  function RectangleAfterMove(const rect: Rectangle; const mv: Vector): Rectangle; overload;
  begin
    result := sgGeometry.RectangleAfterMove(rect,mv);
  end;

  function RectangleBottom(const rect: Rectangle): Single; overload;
  begin
    result := sgGeometry.RectangleBottom(rect);
  end;

  function RectangleBottomLeft(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleBottomLeft(rect);
  end;

  function RectangleBottomRight(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleBottomRight(rect);
  end;

  function RectangleCenter(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleCenter(rect);
  end;

  function RectangleCenterBottom(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleCenterBottom(rect);
  end;

  function RectangleCenterLeft(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleCenterLeft(rect);
  end;

  function RectangleCenterRight(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleCenterRight(rect);
  end;

  function RectangleCenterTop(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleCenterTop(rect);
  end;

  function RectangleFrom(x: Single; y: Single; w: Longint; h: Longint): Rectangle; overload;
  begin
    result := sgGeometry.RectangleFrom(x,y,w,h);
  end;

  function RectangleFrom(const tri: Triangle): Rectangle; overload;
  begin
    result := sgGeometry.RectangleFrom(tri);
  end;

  function RectangleFrom(const lines: LinesArray): Rectangle; overload;
  begin
    result := sgGeometry.RectangleFrom(lines);
  end;

  function RectangleFrom(const line: LineSegment): Rectangle; overload;
  begin
    result := sgGeometry.RectangleFrom(line);
  end;

  function RectangleFrom(const c: Circle): Rectangle; overload;
  begin
    result := sgGeometry.RectangleFrom(c);
  end;

  function RectangleFrom(const pt1: Point2D; const pt2: Point2D): Rectangle; overload;
  begin
    result := sgGeometry.RectangleFrom(pt1,pt2);
  end;

  function RectangleFrom(const pt: Point2D; width: Longint; height: Longint): Rectangle; overload;
  begin
    result := sgGeometry.RectangleFrom(pt,width,height);
  end;

  function RectangleLeft(const rect: Rectangle): Single; overload;
  begin
    result := sgGeometry.RectangleLeft(rect);
  end;

  function RectangleOffset(const rect: Rectangle; const vec: Vector): Rectangle; overload;
  begin
    result := sgGeometry.RectangleOffset(rect,vec);
  end;

  function RectangleRight(const rect: Rectangle): Single; overload;
  begin
    result := sgGeometry.RectangleRight(rect);
  end;

  function RectangleToString(const rect: Rectangle): String; overload;
  begin
    result := sgGeometry.RectangleToString(rect);
  end;

  function RectangleTop(const rect: Rectangle): Single; overload;
  begin
    result := sgGeometry.RectangleTop(rect);
  end;

  function RectangleTopLeft(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleTopLeft(rect);
  end;

  function RectangleTopRight(const rect: Rectangle): Point2D; overload;
  begin
    result := sgGeometry.RectangleTopRight(rect);
  end;

  function RectanglesIntersect(const rect1: Rectangle; const rect2: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.RectanglesIntersect(rect1,rect2);
  end;

  function RotationMatrix(deg: Single): Matrix2D; overload;
  begin
    result := sgGeometry.RotationMatrix(deg);
  end;

  function ScaleMatrix(scale: Single): Matrix2D; overload;
  begin
    result := sgGeometry.ScaleMatrix(scale);
  end;

  function ScaleMatrix(const scale: Point2D): Matrix2D; overload;
  begin
    result := sgGeometry.ScaleMatrix(scale);
  end;

  function ScaleRotateTranslateMatrix(const scale: Point2D; deg: Single; const translate: Point2D): Matrix2D; overload;
  begin
    result := sgGeometry.ScaleRotateTranslateMatrix(scale,deg,translate);
  end;

  function Sine(angle: Single): Single; overload;
  begin
    result := sgGeometry.Sine(angle);
  end;

  function SubtractVectors(const v1: Vector; const v2: Vector): Vector; overload;
  begin
    result := sgGeometry.SubtractVectors(v1,v2);
  end;

  function Tangent(angle: Single): Single; overload;
  begin
    result := sgGeometry.Tangent(angle);
  end;

  function TangentPoints(const fromPt: Point2D; const c: Circle; out p1: Point2D; out p2: Point2D): Boolean; overload;
  begin
    result := sgGeometry.TangentPoints(fromPt,c,p1,p2);
  end;

  function TranslationMatrix(dx: Single; dy: Single): Matrix2D; overload;
  begin
    result := sgGeometry.TranslationMatrix(dx,dy);
  end;

  function TranslationMatrix(const pt: Point2D): Matrix2D; overload;
  begin
    result := sgGeometry.TranslationMatrix(pt);
  end;

  function TriangleBarycenter(const tri: Triangle): Point2D; overload;
  begin
    result := sgGeometry.TriangleBarycenter(tri);
  end;

  function TriangleFrom(ax: Single; ay: Single; bx: Single; by: Single; cx: Single; cy: Single): Triangle; overload;
  begin
    result := sgGeometry.TriangleFrom(ax,ay,bx,by,cx,cy);
  end;

  function TriangleFrom(const a: Point2D; const b: Point2D; const c: Point2D): Triangle; overload;
  begin
    result := sgGeometry.TriangleFrom(a,b,c);
  end;

  function TriangleRectangleIntersect(const tri: Triangle; const rect: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.TriangleRectangleIntersect(tri,rect);
  end;

  function TriangleToString(const tri: Triangle): String; overload;
  begin
    result := sgGeometry.TriangleToString(tri);
  end;

  function TrianglesRectangleIntersect(const tri: TriangleArray; const rect: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.TrianglesRectangleIntersect(tri,rect);
  end;

  function UnitVector(const v: Vector): Vector; overload;
  begin
    result := sgGeometry.UnitVector(v);
  end;

  function VectorAngle(const v: Vector): Single; overload;
  begin
    result := sgGeometry.VectorAngle(v);
  end;

  function VectorFromAngle(angle: Single; magnitude: Single): Vector; overload;
  begin
    result := sgGeometry.VectorFromAngle(angle,magnitude);
  end;

  function VectorFromPointToRect(const pt: Point2D; const rect: Rectangle): Vector; overload;
  begin
    result := sgGeometry.VectorFromPointToRect(pt,rect);
  end;

  function VectorFromPointToRect(x: Single; y: Single; const rect: Rectangle): Vector; overload;
  begin
    result := sgGeometry.VectorFromPointToRect(x,y,rect);
  end;

  function VectorFromPointToRect(x: Single; y: Single; rectX: Single; rectY: Single; rectWidth: Longint; rectHeight: Longint): Vector; overload;
  begin
    result := sgGeometry.VectorFromPointToRect(x,y,rectX,rectY,rectWidth,rectHeight);
  end;

  function VectorFromPoints(const p1: Point2D; const p2: Point2D): Vector; overload;
  begin
    result := sgGeometry.VectorFromPoints(p1,p2);
  end;

  function VectorInRect(const v: Vector; const rect: Rectangle): Boolean; overload;
  begin
    result := sgGeometry.VectorInRect(v,rect);
  end;

  function VectorInRect(const v: Vector; x: Single; y: Single; w: Single; h: Single): Boolean; overload;
  begin
    result := sgGeometry.VectorInRect(v,x,y,w,h);
  end;

  function VectorIsZero(const v: Vector): Boolean; overload;
  begin
    result := sgGeometry.VectorIsZero(v);
  end;

  function VectorMagnitude(const v: Vector): Single; overload;
  begin
    result := sgGeometry.VectorMagnitude(v);
  end;

  function VectorMagnitudeSq(const v: Vector): Single; overload;
  begin
    result := sgGeometry.VectorMagnitudeSq(v);
  end;

  function VectorMultiply(const v: Vector; s: Single): Vector; overload;
  begin
    result := sgGeometry.VectorMultiply(v,s);
  end;

  function VectorNormal(const v: Vector): Vector; overload;
  begin
    result := sgGeometry.VectorNormal(v);
  end;

  function VectorOutOfCircleFromCircle(const src: Circle; const bounds: Circle; const velocity: Vector): Vector; overload;
  begin
    result := sgGeometry.VectorOutOfCircleFromCircle(src,bounds,velocity);
  end;

  function VectorOutOfCircleFromPoint(const pt: Point2D; const c: Circle; const velocity: Vector): Vector; overload;
  begin
    result := sgGeometry.VectorOutOfCircleFromPoint(pt,c,velocity);
  end;

  function VectorOutOfRectFromCircle(const c: Circle; const rect: Rectangle; const velocity: Vector): Vector; overload;
  begin
    result := sgGeometry.VectorOutOfRectFromCircle(c,rect,velocity);
  end;

  function VectorOutOfRectFromPoint(const pt: Point2D; const rect: Rectangle; const velocity: Vector): Vector; overload;
  begin
    result := sgGeometry.VectorOutOfRectFromPoint(pt,rect,velocity);
  end;

  function VectorOutOfRectFromRect(const src: Rectangle; const bounds: Rectangle; const velocity: Vector): Vector; overload;
  begin
    result := sgGeometry.VectorOutOfRectFromRect(src,bounds,velocity);
  end;

  function VectorOverLinesFromCircle(const c: Circle; const lines: LinesArray; const velocity: Vector; out maxIdx: Longint): Vector; overload;
  begin
    result := sgGeometry.VectorOverLinesFromCircle(c,lines,velocity,maxIdx);
  end;

  function VectorOverLinesFromLines(const srcLines: LinesArray; const boundLines: LinesArray; const velocity: Vector; out maxIdx: Longint): Vector; overload;
  begin
    result := sgGeometry.VectorOverLinesFromLines(srcLines,boundLines,velocity,maxIdx);
  end;

  function VectorTo(x: Single; y: Single): Vector; overload;
  begin
    result := sgGeometry.VectorTo(x,y);
  end;

  function VectorTo(x: Single; y: Single; invertY: Boolean): Vector; overload;
  begin
    result := sgGeometry.VectorTo(x,y,invertY);
  end;

  function VectorToPoint(const p1: Point2D): Vector; overload;
  begin
    result := sgGeometry.VectorToPoint(p1);
  end;

  function VectorsEqual(const v1: Vector; const v2: Vector): Boolean; overload;
  begin
    result := sgGeometry.VectorsEqual(v1,v2);
  end;

  function VectorsNotEqual(const v1: Vector; const v2: Vector): Boolean; overload;
  begin
    result := sgGeometry.VectorsNotEqual(v1,v2);
  end;

  procedure WidestPoints(const c: Circle; const along: Vector; out pt1: Point2D; out pt2: Point2D); overload;
  begin
    sgGeometry.WidestPoints(c,along,pt1,pt2);
  end;

  function AvailableResolutions(): ResolutionArray; overload;
  begin
    result := sgGraphics.AvailableResolutions();
  end;

  function BlueOf(c: Color): Byte; overload;
  begin
    result := sgGraphics.BlueOf(c);
  end;

  function BrightnessOf(c: Color): Single; overload;
  begin
    result := sgGraphics.BrightnessOf(c);
  end;

  procedure ChangeScreenSize(width: Longint; height: Longint); overload;
  begin
    sgGraphics.ChangeScreenSize(width,height);
  end;

  procedure ClearScreen(); overload;
  begin
    sgGraphics.ClearScreen();
  end;

  procedure ClearScreen(toColor: Color); overload;
  begin
    sgGraphics.ClearScreen(toColor);
  end;

  procedure ColorComponents(c: Color; out r: Byte; out g: Byte; out b: Byte; out a: Byte); overload;
  begin
    sgGraphics.ColorComponents(c,r,g,b,a);
  end;

  function ColorFrom(bmp: Bitmap; apiColor: Color): Color; overload;
  begin
    result := sgGraphics.ColorFrom(bmp,apiColor);
  end;

  function ColorToString(c: Color): String; overload;
  begin
    result := sgGraphics.ColorToString(c);
  end;

  function CurrentClip(): Rectangle; overload;
  begin
    result := sgGraphics.CurrentClip();
  end;

  function CurrentClip(bmp: Bitmap): Rectangle; overload;
  begin
    result := sgGraphics.CurrentClip(bmp);
  end;

  procedure DrawCircle(clr: Color; x: Single; y: Single; radius: Single); overload;
  begin
    sgGraphics.DrawCircle(clr,x,y,radius);
  end;

  procedure DrawCircle(clr: Color; const c: Circle); overload;
  begin
    sgGraphics.DrawCircle(clr,c);
  end;

  procedure DrawCircle(clr: Color; const c: Circle; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawCircle(clr,c,opts);
  end;

  procedure DrawCircle(clr: Color; x: Single; y: Single; radius: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawCircle(clr,x,y,radius,opts);
  end;

  procedure DrawEllipse(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single); overload;
  begin
    sgGraphics.DrawEllipse(clr,xPos,yPos,width,height);
  end;

  procedure DrawEllipse(clr: Color; const rec: Rectangle); overload;
  begin
    sgGraphics.DrawEllipse(clr,rec);
  end;

  procedure DrawEllipse(clr: Color; const rec: Rectangle; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawEllipse(clr,rec,opts);
  end;

  procedure DrawEllipse(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawEllipse(clr,xPos,yPos,width,height,opts);
  end;

  procedure DrawLine(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single); overload;
  begin
    sgGraphics.DrawLine(clr,x1,y1,x2,y2);
  end;

  procedure DrawLine(clr: Color; const l: LineSegment); overload;
  begin
    sgGraphics.DrawLine(clr,l);
  end;

  procedure DrawLine(clr: Color; const l: LineSegment; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawLine(clr,l,opts);
  end;

  procedure DrawLine(clr: Color; xPosStart: Single; yPosStart: Single; xPosEnd: Single; yPosEnd: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawLine(clr,xPosStart,yPosStart,xPosEnd,yPosEnd,opts);
  end;

  procedure DrawPixel(clr: Color; const position: Point2D); overload;
  begin
    sgGraphics.DrawPixel(clr,position);
  end;

  procedure DrawPixel(clr: Color; x: Single; y: Single); overload;
  begin
    sgGraphics.DrawPixel(clr,x,y);
  end;

  procedure DrawPixel(dest: Bitmap; clr: Color; const position: Point2D); overload;
  begin
    sgGraphics.DrawPixel(dest,clr,position);
  end;

  procedure DrawPixel(dest: Bitmap; clr: Color; x: Longint; y: Longint); overload;
  begin
    sgGraphics.DrawPixel(dest,clr,x,y);
  end;

  procedure DrawPixelOnScreen(clr: Color; const position: Point2D); overload;
  begin
    sgGraphics.DrawPixelOnScreen(clr,position);
  end;

  procedure DrawPixelOnScreen(clr: Color; x: Longint; y: Longint); overload;
  begin
    sgGraphics.DrawPixelOnScreen(clr,x,y);
  end;

  procedure DrawRectangle(clr: Color; x: Single; y: Single; width: Single; height: Single); overload;
  begin
    sgGraphics.DrawRectangle(clr,x,y,width,height);
  end;

  procedure DrawRectangle(clr: Color; const rect: Rectangle); overload;
  begin
    sgGraphics.DrawRectangle(clr,rect);
  end;

  procedure DrawRectangle(clr: Color; const rect: Rectangle; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawRectangle(clr,rect,opts);
  end;

  procedure DrawRectangle(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawRectangle(clr,xPos,yPos,width,height,opts);
  end;

  procedure DrawTriangle(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single); overload;
  begin
    sgGraphics.DrawTriangle(clr,x1,y1,x2,y2,x3,y3);
  end;

  procedure DrawTriangle(clr: Color; const tri: Triangle); overload;
  begin
    sgGraphics.DrawTriangle(clr,tri);
  end;

  procedure DrawTriangle(clr: Color; const tri: Triangle; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawTriangle(clr,tri,opts);
  end;

  procedure DrawTriangle(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.DrawTriangle(clr,x1,y1,x2,y2,x3,y3,opts);
  end;

  procedure FillCircle(clr: Color; x: Single; y: Single; radius: Single); overload;
  begin
    sgGraphics.FillCircle(clr,x,y,radius);
  end;

  procedure FillCircle(clr: Color; const c: Circle); overload;
  begin
    sgGraphics.FillCircle(clr,c);
  end;

  procedure FillCircle(clr: Color; const c: Circle; const opts: DrawingOptions); overload;
  begin
    sgGraphics.FillCircle(clr,c,opts);
  end;

  procedure FillCircle(clr: Color; x: Single; y: Single; radius: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.FillCircle(clr,x,y,radius,opts);
  end;

  procedure FillEllipse(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single); overload;
  begin
    sgGraphics.FillEllipse(clr,xPos,yPos,width,height);
  end;

  procedure FillEllipse(clr: Color; const rec: Rectangle); overload;
  begin
    sgGraphics.FillEllipse(clr,rec);
  end;

  procedure FillEllipse(clr: Color; const rec: Rectangle; const opts: DrawingOptions); overload;
  begin
    sgGraphics.FillEllipse(clr,rec,opts);
  end;

  procedure FillEllipse(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.FillEllipse(clr,xPos,yPos,width,height,opts);
  end;

  procedure FillRectangle(clr: Color; x: Single; y: Single; width: Single; height: Single); overload;
  begin
    sgGraphics.FillRectangle(clr,x,y,width,height);
  end;

  procedure FillRectangle(clr: Color; const rect: Rectangle); overload;
  begin
    sgGraphics.FillRectangle(clr,rect);
  end;

  procedure FillRectangle(clr: Color; const rect: Rectangle; const opts: DrawingOptions); overload;
  begin
    sgGraphics.FillRectangle(clr,rect,opts);
  end;

  procedure FillRectangle(clr: Color; xPos: Single; yPos: Single; width: Single; height: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.FillRectangle(clr,xPos,yPos,width,height,opts);
  end;

  procedure FillTriangle(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single); overload;
  begin
    sgGraphics.FillTriangle(clr,x1,y1,x2,y2,x3,y3);
  end;

  procedure FillTriangle(clr: Color; const tri: Triangle); overload;
  begin
    sgGraphics.FillTriangle(clr,tri);
  end;

  procedure FillTriangle(clr: Color; const tri: Triangle; const opts: DrawingOptions); overload;
  begin
    sgGraphics.FillTriangle(clr,tri,opts);
  end;

  procedure FillTriangle(clr: Color; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; const opts: DrawingOptions); overload;
  begin
    sgGraphics.FillTriangle(clr,x1,y1,x2,y2,x3,y3,opts);
  end;

  function GetPixel(bmp: Bitmap; x: Longint; y: Longint): Color; overload;
  begin
    result := sgGraphics.GetPixel(bmp,x,y);
  end;

  function GetPixelFromScreen(x: Longint; y: Longint): Color; overload;
  begin
    result := sgGraphics.GetPixelFromScreen(x,y);
  end;

  function GreenOf(c: Color): Byte; overload;
  begin
    result := sgGraphics.GreenOf(c);
  end;

  function HSBColor(hue: Single; saturation: Single; brightness: Single): Color; overload;
  begin
    result := sgGraphics.HSBColor(hue,saturation,brightness);
  end;

  procedure HSBValuesOf(c: Color; out h: Single; out s: Single; out b: Single); overload;
  begin
    sgGraphics.HSBValuesOf(c,h,s,b);
  end;

  function HueOf(c: Color): Single; overload;
  begin
    result := sgGraphics.HueOf(c);
  end;

  function NumberOfResolutions(): Longint; overload;
  begin
    result := sgGraphics.NumberOfResolutions();
  end;

  procedure OpenGraphicsWindow(caption: String); overload;
  begin
    sgGraphics.OpenGraphicsWindow(caption);
  end;

  procedure OpenGraphicsWindow(caption: String; width: Longint; height: Longint); overload;
  begin
    sgGraphics.OpenGraphicsWindow(caption,width,height);
  end;

  procedure PopClip(); overload;
  begin
    sgGraphics.PopClip();
  end;

  procedure PopClip(bmp: Bitmap); overload;
  begin
    sgGraphics.PopClip(bmp);
  end;

  procedure PushClip(const r: Rectangle); overload;
  begin
    sgGraphics.PushClip(r);
  end;

  procedure PushClip(bmp: Bitmap; const r: Rectangle); overload;
  begin
    sgGraphics.PushClip(bmp,r);
  end;

  procedure PushClip(x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgGraphics.PushClip(x,y,w,h);
  end;

  procedure PutPixel(bmp: Bitmap; value: Color; x: Longint; y: Longint); overload;
  begin
    sgGraphics.PutPixel(bmp,value,x,y);
  end;

  function RGBAColor(red: Byte; green: Byte; blue: Byte; alpha: Byte): Color; overload;
  begin
    result := sgGraphics.RGBAColor(red,green,blue,alpha);
  end;

  function RGBAFloatColor(r: Single; g: Single; b: Single; a: Single): Color; overload;
  begin
    result := sgGraphics.RGBAFloatColor(r,g,b,a);
  end;

  function RGBColor(red: Byte; green: Byte; blue: Byte): Color; overload;
  begin
    result := sgGraphics.RGBColor(red,green,blue);
  end;

  function RGBFloatColor(r: Single; g: Single; b: Single): Color; overload;
  begin
    result := sgGraphics.RGBFloatColor(r,g,b);
  end;

  function RandomColor(): Color; overload;
  begin
    result := sgGraphics.RandomColor();
  end;

  function RandomRGBColor(alpha: Byte): Color; overload;
  begin
    result := sgGraphics.RandomRGBColor(alpha);
  end;

  function RedOf(c: Color): Byte; overload;
  begin
    result := sgGraphics.RedOf(c);
  end;

  procedure RefreshScreen(); overload;
  begin
    sgGraphics.RefreshScreen();
  end;

  procedure RefreshScreen(TargetFPS: Longword); overload;
  begin
    sgGraphics.RefreshScreen(TargetFPS);
  end;

  procedure ResetClip(); overload;
  begin
    sgGraphics.ResetClip();
  end;

  procedure ResetClip(bmp: Bitmap); overload;
  begin
    sgGraphics.ResetClip(bmp);
  end;

  function SaturationOf(c: Color): Single; overload;
  begin
    result := sgGraphics.SaturationOf(c);
  end;

  function ScreenHeight(): Longint; overload;
  begin
    result := sgGraphics.ScreenHeight();
  end;

  function ScreenWidth(): Longint; overload;
  begin
    result := sgGraphics.ScreenWidth();
  end;

  procedure SetClip(const r: Rectangle); overload;
  begin
    sgGraphics.SetClip(r);
  end;

  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;
  begin
    sgGraphics.SetClip(bmp,r);
  end;

  procedure SetClip(x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgGraphics.SetClip(x,y,w,h);
  end;

  procedure SetClip(bmp: Bitmap; x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgGraphics.SetClip(bmp,x,y,w,h);
  end;

  procedure SetIcon(filename: String); overload;
  begin
    sgGraphics.SetIcon(filename);
  end;

  procedure ShowSwinGameSplashScreen(); overload;
  begin
    sgGraphics.ShowSwinGameSplashScreen();
  end;

  procedure TakeScreenshot(basename: String); overload;
  begin
    sgGraphics.TakeScreenshot(basename);
  end;

  procedure ToggleFullScreen(); overload;
  begin
    sgGraphics.ToggleFullScreen();
  end;

  procedure ToggleWindowBorder(); overload;
  begin
    sgGraphics.ToggleWindowBorder();
  end;

  function TransparencyOf(c: Color): Byte; overload;
  begin
    result := sgGraphics.TransparencyOf(c);
  end;

  function BitmapCellCircle(bmp: Bitmap; const pt: Point2D): Circle; overload;
  begin
    result := sgImages.BitmapCellCircle(bmp,pt);
  end;

  function BitmapCellCircle(bmp: Bitmap; x: Longint; y: Longint): Circle; overload;
  begin
    result := sgImages.BitmapCellCircle(bmp,x,y);
  end;

  function BitmapCellColumns(bmp: Bitmap): Longint; overload;
  begin
    result := sgImages.BitmapCellColumns(bmp);
  end;

  function BitmapCellCount(bmp: Bitmap): Longint; overload;
  begin
    result := sgImages.BitmapCellCount(bmp);
  end;

  function BitmapCellHeight(bmp: Bitmap): Longint; overload;
  begin
    result := sgImages.BitmapCellHeight(bmp);
  end;

  function BitmapCellOf(bmp: Bitmap; cell: Longint): BitmapCell; overload;
  begin
    result := sgImages.BitmapCellOf(bmp,cell);
  end;

  function BitmapCellRectangle(bmp: Bitmap): Rectangle; overload;
  begin
    result := sgImages.BitmapCellRectangle(bmp);
  end;

  function BitmapCellRectangle(const pt: Point2D; bmp: Bitmap): Rectangle; overload;
  begin
    result := sgImages.BitmapCellRectangle(pt,bmp);
  end;

  function BitmapCellRectangle(x: Single; y: Single; bmp: Bitmap): Rectangle; overload;
  begin
    result := sgImages.BitmapCellRectangle(x,y,bmp);
  end;

  function BitmapCellRows(bmp: Bitmap): Longint; overload;
  begin
    result := sgImages.BitmapCellRows(bmp);
  end;

  function BitmapCellWidth(bmp: Bitmap): Longint; overload;
  begin
    result := sgImages.BitmapCellWidth(bmp);
  end;

  function BitmapCircle(bmp: Bitmap; const pt: Point2D): Circle; overload;
  begin
    result := sgImages.BitmapCircle(bmp,pt);
  end;

  function BitmapCircle(bmp: Bitmap; x: Longint; y: Longint): Circle; overload;
  begin
    result := sgImages.BitmapCircle(bmp,x,y);
  end;

  function BitmapFilename(bmp: Bitmap): String; overload;
  begin
    result := sgImages.BitmapFilename(bmp);
  end;

  function BitmapHeight(const bmp: BitmapCell): Longint; overload;
  begin
    result := sgImages.BitmapHeight(bmp);
  end;

  function BitmapHeight(bmp: Bitmap): Longint; overload;
  begin
    result := sgImages.BitmapHeight(bmp);
  end;

  function BitmapName(bmp: Bitmap): String; overload;
  begin
    result := sgImages.BitmapName(bmp);
  end;

  function BitmapNamed(name: String): Bitmap; overload;
  begin
    result := sgImages.BitmapNamed(name);
  end;

  function BitmapRectangle(bmp: Bitmap): Rectangle; overload;
  begin
    result := sgImages.BitmapRectangle(bmp);
  end;

  function BitmapRectangle(x: Single; y: Single; bmp: Bitmap): Rectangle; overload;
  begin
    result := sgImages.BitmapRectangle(x,y,bmp);
  end;

  function BitmapRectangleOfCell(src: Bitmap; cell: Longint): Rectangle; overload;
  begin
    result := sgImages.BitmapRectangleOfCell(src,cell);
  end;

  procedure BitmapSetCellDetails(bmp: Bitmap; width: Longint; height: Longint; columns: Longint; rows: Longint; count: Longint); overload;
  begin
    sgImages.BitmapSetCellDetails(bmp,width,height,columns,rows,count);
  end;

  function BitmapWidth(bmp: Bitmap): Longint; overload;
  begin
    result := sgImages.BitmapWidth(bmp);
  end;

  function BitmapWidth(const bmp: BitmapCell): Longint; overload;
  begin
    result := sgImages.BitmapWidth(bmp);
  end;

  function BitmapsInterchangable(bmp1: Bitmap; bmp2: Bitmap): Boolean; overload;
  begin
    result := sgImages.BitmapsInterchangable(bmp1,bmp2);
  end;

  procedure ClearSurface(dest: Bitmap); overload;
  begin
    sgImages.ClearSurface(dest);
  end;

  procedure ClearSurface(dest: Bitmap; toColor: Color); overload;
  begin
    sgImages.ClearSurface(dest,toColor);
  end;

  function CombineIntoGrid(const bitmaps: BitmapArray; cols: Longint): Bitmap; overload;
  begin
    result := sgImages.CombineIntoGrid(bitmaps,cols);
  end;

  function CreateBitmap(width: Longint; height: Longint): Bitmap; overload;
  begin
    result := sgImages.CreateBitmap(width,height);
  end;

  function CreateBitmap(name: String; width: Longint; height: Longint): Bitmap; overload;
  begin
    result := sgImages.CreateBitmap(name,width,height);
  end;

  procedure DrawBitmap(src: Bitmap; x: Single; y: Single); overload;
  begin
    sgImages.DrawBitmap(src,x,y);
  end;

  procedure DrawBitmap(name: String; x: Single; y: Single); overload;
  begin
    sgImages.DrawBitmap(name,x,y);
  end;

  procedure DrawBitmap(name: String; const position: Point2D); overload;
  begin
    sgImages.DrawBitmap(name,position);
  end;

  procedure DrawBitmap(src: Bitmap; const position: Point2D); overload;
  begin
    sgImages.DrawBitmap(src,position);
  end;

  procedure DrawBitmap(dest: Bitmap; src: Bitmap; const position: Point2D); overload;
  begin
    sgImages.DrawBitmap(dest,src,position);
  end;

  procedure DrawBitmap(name: String; x: Longint; y: Longint; const opts: DrawingOptions); overload;
  begin
    sgImages.DrawBitmap(name,x,y,opts);
  end;

  procedure DrawBitmap(dest: Bitmap; src: Bitmap; x: Longint; y: Longint); overload;
  begin
    sgImages.DrawBitmap(dest,src,x,y);
  end;

  procedure DrawBitmap(src: Bitmap; x: Longint; y: Longint; const opts: DrawingOptions); overload;
  begin
    sgImages.DrawBitmap(src,x,y,opts);
  end;

  procedure DrawBitmapCell(const src: BitmapCell; const position: Point2D); overload;
  begin
    sgImages.DrawBitmapCell(src,position);
  end;

  procedure DrawBitmapCell(const src: BitmapCell; x: Single; y: Single); overload;
  begin
    sgImages.DrawBitmapCell(src,x,y);
  end;

  procedure DrawBitmapCell(dest: Bitmap; const src: BitmapCell; const position: Point2D); overload;
  begin
    sgImages.DrawBitmapCell(dest,src,position);
  end;

  procedure DrawBitmapCell(dest: Bitmap; const src: BitmapCell; x: Longint; y: Longint); overload;
  begin
    sgImages.DrawBitmapCell(dest,src,x,y);
  end;

  procedure DrawBitmapCellOnScreen(const src: BitmapCell; const position: Point2D); overload;
  begin
    sgImages.DrawBitmapCellOnScreen(src,position);
  end;

  procedure DrawBitmapCellOnScreen(const src: BitmapCell; x: Longint; y: Longint); overload;
  begin
    sgImages.DrawBitmapCellOnScreen(src,x,y);
  end;

  procedure DrawBitmapOnScreen(src: Bitmap; const position: Point2D); overload;
  begin
    sgImages.DrawBitmapOnScreen(src,position);
  end;

  procedure DrawBitmapOnScreen(src: Bitmap; x: Longint; y: Longint); overload;
  begin
    sgImages.DrawBitmapOnScreen(src,x,y);
  end;

  procedure DrawBitmapPart(src: Bitmap; const source: Rectangle; const position: Point2D); overload;
  begin
    sgImages.DrawBitmapPart(src,source,position);
  end;

  procedure DrawBitmapPart(src: Bitmap; const source: Rectangle; x: Single; y: Single); overload;
  begin
    sgImages.DrawBitmapPart(src,source,x,y);
  end;

  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; const position: Point2D); overload;
  begin
    sgImages.DrawBitmapPart(dest,src,source,position);
  end;

  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; x: Longint; y: Longint); overload;
  begin
    sgImages.DrawBitmapPart(dest,src,source,x,y);
  end;

  procedure DrawBitmapPart(src: Bitmap; srcX: Longint; srcY: Longint; srcW: Longint; srcH: Longint; x: Single; y: Single); overload;
  begin
    sgImages.DrawBitmapPart(src,srcX,srcY,srcW,srcH,x,y);
  end;

  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; srcX: Longint; srcY: Longint; srcW: Longint; srcH: Longint; x: Longint; y: Longint); overload;
  begin
    sgImages.DrawBitmapPart(dest,src,srcX,srcY,srcW,srcH,x,y);
  end;

  procedure DrawBitmapPartOnScreen(src: Bitmap; const source: Rectangle; const position: Point2D); overload;
  begin
    sgImages.DrawBitmapPartOnScreen(src,source,position);
  end;

  procedure DrawBitmapPartOnScreen(src: Bitmap; const source: Rectangle; x: Longint; y: Longint); overload;
  begin
    sgImages.DrawBitmapPartOnScreen(src,source,x,y);
  end;

  procedure DrawBitmapPartOnScreen(src: Bitmap; srcX: Longint; srcY: Longint; srcW: Longint; srcH: Longint; x: Longint; y: Longint); overload;
  begin
    sgImages.DrawBitmapPartOnScreen(src,srcX,srcY,srcW,srcH,x,y);
  end;

  procedure DrawCell(src: Bitmap; cell: Longint; const position: Point2D); overload;
  begin
    sgImages.DrawCell(src,cell,position);
  end;

  procedure DrawCell(dest: Bitmap; src: Bitmap; cell: Longint; const position: Point2D); overload;
  begin
    sgImages.DrawCell(dest,src,cell,position);
  end;

  procedure DrawCell(src: Bitmap; cell: Longint; x: Single; y: Single); overload;
  begin
    sgImages.DrawCell(src,cell,x,y);
  end;

  procedure DrawCell(dest: Bitmap; src: Bitmap; cell: Longint; x: Single; y: Single); overload;
  begin
    sgImages.DrawCell(dest,src,cell,x,y);
  end;

  procedure DrawCellOnScreen(src: Bitmap; cell: Longint; const position: Point2D); overload;
  begin
    sgImages.DrawCellOnScreen(src,cell,position);
  end;

  procedure DrawCellOnScreen(src: Bitmap; cell: Longint; x: Single; y: Single); overload;
  begin
    sgImages.DrawCellOnScreen(src,cell,x,y);
  end;

  procedure FreeBitmap(var bitmapToFree: Bitmap); overload;
  begin
    sgImages.FreeBitmap(bitmapToFree);
  end;

  function HasBitmap(name: String): Boolean; overload;
  begin
    result := sgImages.HasBitmap(name);
  end;

  function LoadBitmap(filename: String): Bitmap; overload;
  begin
    result := sgImages.LoadBitmap(filename);
  end;

  function LoadBitmap(filename: String; transparent: Boolean; transparentColor: Color): Bitmap; overload;
  begin
    result := sgImages.LoadBitmap(filename,transparent,transparentColor);
  end;

  function LoadBitmapNamed(name: String; filename: String): Bitmap; overload;
  begin
    result := sgImages.LoadBitmapNamed(name,filename);
  end;

  function LoadTransparentBitmap(filename: String; transparentColor: Color): Bitmap; overload;
  begin
    result := sgImages.LoadTransparentBitmap(filename,transparentColor);
  end;

  function LoadTransparentBitmapNamed(name: String; filename: String; transparentColor: Color): Bitmap; overload;
  begin
    result := sgImages.LoadTransparentBitmapNamed(name,filename,transparentColor);
  end;

  procedure MakeOpaque(bmp: Bitmap); overload;
  begin
    sgImages.MakeOpaque(bmp);
  end;

  procedure MakeTransparent(bmp: Bitmap); overload;
  begin
    sgImages.MakeTransparent(bmp);
  end;

  procedure OptimiseBitmap(surface: Bitmap); overload;
  begin
    sgImages.OptimiseBitmap(surface);
  end;

  function PixelDrawnAtPoint(bmp: Bitmap; x: Longint; y: Longint): Boolean; overload;
  begin
    result := sgImages.PixelDrawnAtPoint(bmp,x,y);
  end;

  procedure ReleaseAllBitmaps(); overload;
  begin
    sgImages.ReleaseAllBitmaps();
  end;

  procedure ReleaseBitmap(name: String); overload;
  begin
    sgImages.ReleaseBitmap(name);
  end;

  function RotateScaleBitmap(src: Bitmap; degRot: Single; scale: Single): Bitmap; overload;
  begin
    result := sgImages.RotateScaleBitmap(src,degRot,scale);
  end;

  function SameBitmapCell(const bmp1: BitmapCell; const bmp2: BitmapCell): Boolean; overload;
  begin
    result := sgImages.SameBitmapCell(bmp1,bmp2);
  end;

  procedure SaveBitmap(src: Bitmap; filepath: String); overload;
  begin
    sgImages.SaveBitmap(src,filepath);
  end;

  procedure SaveToPNG(bmp: Bitmap; filename: String); overload;
  begin
    sgImages.SaveToPNG(bmp,filename);
  end;

  procedure SetOpacity(bmp: Bitmap; pct: Single); overload;
  begin
    sgImages.SetOpacity(bmp,pct);
  end;

  procedure SetTransparentColor(src: Bitmap; clr: Color); overload;
  begin
    sgImages.SetTransparentColor(src,clr);
  end;

  procedure SetupBitmapForCollisions(src: Bitmap); overload;
  begin
    sgImages.SetupBitmapForCollisions(src);
  end;

  function AccelerometerThreshold(): Single; overload;
  begin
    result := sgInput.AccelerometerThreshold();
  end;

  procedure AccelerometerThreshold(value: Single); overload;
  begin
    sgInput.AccelerometerThreshold(value);
  end;

  function AnyKeyPressed(): Boolean; overload;
  begin
    result := sgInput.AnyKeyPressed();
  end;

  function DeviceMovedInXAxis(): Single; overload;
  begin
    result := sgInput.DeviceMovedInXAxis();
  end;

  function DeviceMovedInYAxis(): Single; overload;
  begin
    result := sgInput.DeviceMovedInYAxis();
  end;

  function DeviceMovedInZAxis(): Single; overload;
  begin
    result := sgInput.DeviceMovedInZAxis();
  end;

  function EndReadingText(): String; overload;
  begin
    result := sgInput.EndReadingText();
  end;

  function FingersOnScreen(): FingerArray; overload;
  begin
    result := sgInput.FingersOnScreen();
  end;

  procedure HideKeyboard(); overload;
  begin
    sgInput.HideKeyboard();
  end;

  procedure HideMouse(); overload;
  begin
    sgInput.HideMouse();
  end;

  function KeyDown(key: KeyCode): Boolean; overload;
  begin
    result := sgInput.KeyDown(key);
  end;

  function KeyName(key: KeyCode): String; overload;
  begin
    result := sgInput.KeyName(key);
  end;

  function KeyReleased(key: KeyCode): Boolean; overload;
  begin
    result := sgInput.KeyReleased(key);
  end;

  function KeyTyped(key: KeyCode): Boolean; overload;
  begin
    result := sgInput.KeyTyped(key);
  end;

  function KeyUp(key: KeyCode): Boolean; overload;
  begin
    result := sgInput.KeyUp(key);
  end;

  function KeyboardShown(): Boolean; overload;
  begin
    result := sgInput.KeyboardShown();
  end;

  function MouseClicked(button: MouseButton): Boolean; overload;
  begin
    result := sgInput.MouseClicked(button);
  end;

  function MouseDown(button: MouseButton): Boolean; overload;
  begin
    result := sgInput.MouseDown(button);
  end;

  function MouseMovement(): Vector; overload;
  begin
    result := sgInput.MouseMovement();
  end;

  function MousePosition(): Point2D; overload;
  begin
    result := sgInput.MousePosition();
  end;

  function MousePositionAsVector(): Vector; overload;
  begin
    result := sgInput.MousePositionAsVector();
  end;

  function MouseShown(): Boolean; overload;
  begin
    result := sgInput.MouseShown();
  end;

  function MouseUp(button: MouseButton): Boolean; overload;
  begin
    result := sgInput.MouseUp(button);
  end;

  function MouseX(): Single; overload;
  begin
    result := sgInput.MouseX();
  end;

  function MouseY(): Single; overload;
  begin
    result := sgInput.MouseY();
  end;

  procedure MoveMouse(const point: Point2D); overload;
  begin
    sgInput.MoveMouse(point);
  end;

  procedure MoveMouse(x: Longint; y: Longint); overload;
  begin
    sgInput.MoveMouse(x,y);
  end;

  function NumberOfFingersOnScreen(): Longint; overload;
  begin
    result := sgInput.NumberOfFingersOnScreen();
  end;

  procedure ProcessEvents(); overload;
  begin
    sgInput.ProcessEvents();
  end;

  function ReadingText(): Boolean; overload;
  begin
    result := sgInput.ReadingText();
  end;

  function ScreenTouched(): Boolean; overload;
  begin
    result := sgInput.ScreenTouched();
  end;

  procedure ShowKeyboard(); overload;
  begin
    sgInput.ShowKeyboard();
  end;

  procedure ShowMouse(); overload;
  begin
    sgInput.ShowMouse();
  end;

  procedure ShowMouse(show: Boolean); overload;
  begin
    sgInput.ShowMouse(show);
  end;

  procedure StartReadingText(textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    sgInput.StartReadingText(textColor,maxLength,theFont,area);
  end;

  procedure StartReadingText(textColor: Color; maxLength: Longint; theFont: Font; x: Longint; y: Longint); overload;
  begin
    sgInput.StartReadingText(textColor,maxLength,theFont,x,y);
  end;

  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; const pt: Point2D); overload;
  begin
    sgInput.StartReadingTextWithText(text,textColor,maxLength,theFont,pt);
  end;

  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    sgInput.StartReadingTextWithText(text,textColor,maxLength,theFont,area);
  end;

  procedure StartReadingTextWithText(text: String; textColor: Color; backGroundColor: Color; maxLength: Longint; theFont: Font; const area: Rectangle); overload;
  begin
    sgInput.StartReadingTextWithText(text,textColor,backGroundColor,maxLength,theFont,area);
  end;

  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: Longint; theFont: Font; x: Longint; y: Longint); overload;
  begin
    sgInput.StartReadingTextWithText(text,textColor,maxLength,theFont,x,y);
  end;

  function TextEntryCancelled(): Boolean; overload;
  begin
    result := sgInput.TextEntryCancelled();
  end;

  function TextReadAsASCII(): String; overload;
  begin
    result := sgInput.TextReadAsASCII();
  end;

  procedure ToggleKeyboard(); overload;
  begin
    sgInput.ToggleKeyboard();
  end;

  function WindowCloseRequested(): Boolean; overload;
  begin
    result := sgInput.WindowCloseRequested();
  end;

  function AcceptTCPConnection(): Longint; overload;
  begin
    result := sgNetworking.AcceptTCPConnection();
  end;

  procedure BroadcastTCPMessage(aMsg: String); overload;
  begin
    sgNetworking.BroadcastTCPMessage(aMsg);
  end;

  procedure BroadcastUDPMessage(aMsg: String); overload;
  begin
    sgNetworking.BroadcastUDPMessage(aMsg);
  end;

  procedure ClearMessageQueue(aConnection: Connection); overload;
  begin
    sgNetworking.ClearMessageQueue(aConnection);
  end;

  procedure CloseAllConnections(); overload;
  begin
    sgNetworking.CloseAllConnections();
  end;

  procedure CloseAllSockets(); overload;
  begin
    sgNetworking.CloseAllSockets();
  end;

  procedure CloseAllTCPHostSockets(); overload;
  begin
    sgNetworking.CloseAllTCPHostSockets();
  end;

  procedure CloseAllUDPSockets(); overload;
  begin
    sgNetworking.CloseAllUDPSockets();
  end;

  function CloseConnection(var aConnection: Connection): Boolean; overload;
  begin
    result := sgNetworking.CloseConnection(aConnection);
  end;

  function CloseTCPHostSocket(aPort: Longint): Boolean; overload;
  begin
    result := sgNetworking.CloseTCPHostSocket(aPort);
  end;

  function CloseUDPSocket(aPort: Longint): Boolean; overload;
  begin
    result := sgNetworking.CloseUDPSocket(aPort);
  end;

  function ConnectionCount(): Longint; overload;
  begin
    result := sgNetworking.ConnectionCount();
  end;

  function ConnectionIP(aConnection: Connection): Longword; overload;
  begin
    result := sgNetworking.ConnectionIP(aConnection);
  end;

  function ConnectionPort(aConnection: Connection): Longint; overload;
  begin
    result := sgNetworking.ConnectionPort(aConnection);
  end;

  function ConnectionQueueSize(): Longint; overload;
  begin
    result := sgNetworking.ConnectionQueueSize();
  end;

  function CreateTCPConnection(aIP: String; aPort: Longint): Connection; overload;
  begin
    result := sgNetworking.CreateTCPConnection(aIP,aPort);
  end;

  function CreateTCPHost(aPort: Longint): Boolean; overload;
  begin
    result := sgNetworking.CreateTCPHost(aPort);
  end;

  function CreateUDPConnection(aDestIP: String; aDestPort: Longint; aInPort: Longint): Connection; overload;
  begin
    result := sgNetworking.CreateUDPConnection(aDestIP,aDestPort,aInPort);
  end;

  function CreateUDPHost(aPort: Longint): Longint; overload;
  begin
    result := sgNetworking.CreateUDPHost(aPort);
  end;

  function DecToHex(aDec: Longword): String; overload;
  begin
    result := sgNetworking.DecToHex(aDec);
  end;

  procedure EnqueueMessage(aMsg: String; aConnection: Connection); overload;
  begin
    sgNetworking.EnqueueMessage(aMsg,aConnection);
  end;

  procedure EnqueueNewConnection(aConnection: Connection); overload;
  begin
    sgNetworking.EnqueueNewConnection(aConnection);
  end;

  function FetchConnection(): Connection; overload;
  begin
    result := sgNetworking.FetchConnection();
  end;

  procedure FreeConnection(var aConnection: Connection); overload;
  begin
    sgNetworking.FreeConnection(aConnection);
  end;

  function HexStrToIPv4(aHex: String): String; overload;
  begin
    result := sgNetworking.HexStrToIPv4(aHex);
  end;

  function HexToDecString(aHex: String): String; overload;
  begin
    result := sgNetworking.HexToDecString(aHex);
  end;

  function IPv4ToDec(aIP: String): Longword; overload;
  begin
    result := sgNetworking.IPv4ToDec(aIP);
  end;

  function MessageCount(aConnection: Connection): Longint; overload;
  begin
    result := sgNetworking.MessageCount(aConnection);
  end;

  function MyIP(): String; overload;
  begin
    result := sgNetworking.MyIP();
  end;

  function ReadLastMessage(aConnection: Connection): String; overload;
  begin
    result := sgNetworking.ReadLastMessage(aConnection);
  end;

  function ReadMessage(aConnection: Connection): String; overload;
  begin
    result := sgNetworking.ReadMessage(aConnection);
  end;

  procedure ReleaseAllConnections(); overload;
  begin
    sgNetworking.ReleaseAllConnections();
  end;

  function RetreiveConnection(aConnectionAt: Longint): Connection; overload;
  begin
    result := sgNetworking.RetreiveConnection(aConnectionAt);
  end;

  function SendTCPMessage(aMsg: String; aConnection: Connection): Connection; overload;
  begin
    result := sgNetworking.SendTCPMessage(aMsg,aConnection);
  end;

  function SendUDPMessage(aMsg: String; aConnection: Connection): Boolean; overload;
  begin
    result := sgNetworking.SendUDPMessage(aMsg,aConnection);
  end;

  function TCPMessageReceived(): Boolean; overload;
  begin
    result := sgNetworking.TCPMessageReceived();
  end;

  function UDPMessageReceived(): Boolean; overload;
  begin
    result := sgNetworking.UDPMessageReceived();
  end;

  function BitmapCollision(bmp1: Bitmap; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D): Boolean; overload;
  begin
    result := sgPhysics.BitmapCollision(bmp1,pt1,bmp2,pt2);
  end;

  function BitmapCollision(bmp1: Bitmap; x1: Longint; y1: Longint; bmp2: Bitmap; x2: Longint; y2: Longint): Boolean; overload;
  begin
    result := sgPhysics.BitmapCollision(bmp1,x1,y1,bmp2,x2,y2);
  end;

  function BitmapCollision(bmp1: Bitmap; const pt1: Point2D; const part1: Rectangle; bmp2: Bitmap; const pt2: Point2D; const part2: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.BitmapCollision(bmp1,pt1,part1,bmp2,pt2,part2);
  end;

  function BitmapPartPointCollision(bmp: Bitmap; x: Longint; y: Longint; const part: Rectangle; const pt: Point2D): Boolean; overload;
  begin
    result := sgPhysics.BitmapPartPointCollision(bmp,x,y,part,pt);
  end;

  function BitmapPartPointCollision(bmp: Bitmap; x: Longint; y: Longint; const part: Rectangle; ptX: Single; ptY: Single): Boolean; overload;
  begin
    result := sgPhysics.BitmapPartPointCollision(bmp,x,y,part,ptX,ptY);
  end;

  function BitmapPointCollision(bmp: Bitmap; x: Longint; y: Longint; const pt: Point2D): Boolean; overload;
  begin
    result := sgPhysics.BitmapPointCollision(bmp,x,y,pt);
  end;

  function BitmapPointCollision(bmp: Bitmap; x: Longint; y: Longint; ptX: Single; ptY: Single): Boolean; overload;
  begin
    result := sgPhysics.BitmapPointCollision(bmp,x,y,ptX,ptY);
  end;

  function BitmapRectCollision(bmp: Bitmap; const pt: Point2D; const part: Rectangle; const rect: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.BitmapRectCollision(bmp,pt,part,rect);
  end;

  function BitmapRectCollision(bmp: Bitmap; x: Longint; y: Longint; const rect: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.BitmapRectCollision(bmp,x,y,rect);
  end;

  function BitmapRectCollision(bmp: Bitmap; x: Longint; y: Longint; const part: Rectangle; const rect: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.BitmapRectCollision(bmp,x,y,part,rect);
  end;

  function BitmapRectCollision(bmp: Bitmap; x: Longint; y: Longint; rectX: Longint; rectY: Longint; rectWidth: Longint; rectHeight: Longint): Boolean; overload;
  begin
    result := sgPhysics.BitmapRectCollision(bmp,x,y,rectX,rectY,rectWidth,rectHeight);
  end;

  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D): Boolean; overload;
  begin
    result := sgPhysics.CellBitmapCollision(bmp1,cell,pt1,bmp2,pt2);
  end;

  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D; const part: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.CellBitmapCollision(bmp1,cell,pt1,bmp2,pt2,part);
  end;

  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; x1: Longint; y1: Longint; bmp2: Bitmap; x2: Longint; y2: Longint): Boolean; overload;
  begin
    result := sgPhysics.CellBitmapCollision(bmp1,cell,x1,y1,bmp2,x2,y2);
  end;

  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; x1: Longint; y1: Longint; bmp2: Bitmap; x2: Longint; y2: Longint; const part: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.CellBitmapCollision(bmp1,cell,x1,y1,bmp2,x2,y2,part);
  end;

  function CellCollision(bmp1: Bitmap; cell1: Longint; const pt1: Point2D; bmp2: Bitmap; cell2: Longint; const pt2: Point2D): Boolean; overload;
  begin
    result := sgPhysics.CellCollision(bmp1,cell1,pt1,bmp2,cell2,pt2);
  end;

  function CellCollision(bmp1: Bitmap; cell1: Longint; x1: Longint; y1: Longint; bmp2: Bitmap; cell2: Longint; x2: Longint; y2: Longint): Boolean; overload;
  begin
    result := sgPhysics.CellCollision(bmp1,cell1,x1,y1,bmp2,cell2,x2,y2);
  end;

  function CellRectCollision(bmp: Bitmap; cell: Longint; const pt: Point2D; const rect: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.CellRectCollision(bmp,cell,pt,rect);
  end;

  function CellRectCollision(bmp: Bitmap; cell: Longint; x: Longint; y: Longint; const rect: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.CellRectCollision(bmp,cell,x,y,rect);
  end;

  function CircleCircleCollision(const c1: Circle; const c2: Circle): Boolean; overload;
  begin
    result := sgPhysics.CircleCircleCollision(c1,c2);
  end;

  function CircleLineCollision(s: Sprite; const line: LineSegment): Boolean; overload;
  begin
    result := sgPhysics.CircleLineCollision(s,line);
  end;

  function CircleLinesCollision(const c: Circle; const lines: LinesArray): Boolean; overload;
  begin
    result := sgPhysics.CircleLinesCollision(c,lines);
  end;

  function CircleRectCollision(const c: Circle; const rect: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.CircleRectCollision(c,rect);
  end;

  function CircleTriangleCollision(const c: Circle; const tri: Triangle): Boolean; overload;
  begin
    result := sgPhysics.CircleTriangleCollision(c,tri);
  end;

  procedure CollideCircleCircle(s: Sprite; const c: Circle); overload;
  begin
    sgPhysics.CollideCircleCircle(s,c);
  end;

  procedure CollideCircleLine(s: Sprite; const line: LineSegment); overload;
  begin
    sgPhysics.CollideCircleLine(s,line);
  end;

  procedure CollideCircleLines(s: Sprite; const lines: LinesArray); overload;
  begin
    sgPhysics.CollideCircleLines(s,lines);
  end;

  procedure CollideCircleRectangle(s: Sprite; const rect: Rectangle); overload;
  begin
    sgPhysics.CollideCircleRectangle(s,rect);
  end;

  procedure CollideCircles(s1: Sprite; s2: Sprite); overload;
  begin
    sgPhysics.CollideCircles(s1,s2);
  end;

  function RectLineCollision(s: Sprite; const line: LineSegment): Boolean; overload;
  begin
    result := sgPhysics.RectLineCollision(s,line);
  end;

  function RectLineCollision(const rect: Rectangle; const line: LineSegment): Boolean; overload;
  begin
    result := sgPhysics.RectLineCollision(rect,line);
  end;

  function SideForCollisionTest(const velocity: Vector): CollisionSide; overload;
  begin
    result := sgPhysics.SideForCollisionTest(velocity);
  end;

  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D): Boolean; overload;
  begin
    result := sgPhysics.SpriteBitmapCollision(s,bmp,pt);
  end;

  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; x: Single; y: Single): Boolean; overload;
  begin
    result := sgPhysics.SpriteBitmapCollision(s,bmp,x,y);
  end;

  function SpriteCollision(s1: Sprite; s2: Sprite): Boolean; overload;
  begin
    result := sgPhysics.SpriteCollision(s1,s2);
  end;

  function SpriteRectCollision(s: Sprite; const r: Rectangle): Boolean; overload;
  begin
    result := sgPhysics.SpriteRectCollision(s,r);
  end;

  function SpriteRectCollision(s: Sprite; x: Single; y: Single; width: Longint; height: Longint): Boolean; overload;
  begin
    result := sgPhysics.SpriteRectCollision(s,x,y,width,height);
  end;

  function TriangleLineCollision(const tri: Triangle; const ln: LineSegment): Boolean; overload;
  begin
    result := sgPhysics.TriangleLineCollision(tri,ln);
  end;

  function AppPath(): String; overload;
  begin
    result := sgResources.AppPath();
  end;

  function FilenameToResource(name: String; kind: ResourceKind): String; overload;
  begin
    result := sgResources.FilenameToResource(name,kind);
  end;

  function HasResourceBundle(name: String): Boolean; overload;
  begin
    result := sgResources.HasResourceBundle(name);
  end;

  procedure LoadResourceBundle(name: String); overload;
  begin
    sgResources.LoadResourceBundle(name);
  end;

  procedure LoadResourceBundle(name: String; showProgress: Boolean); overload;
  begin
    sgResources.LoadResourceBundle(name,showProgress);
  end;

  procedure LoadResourceBundleNamed(name: String; filename: String; showProgress: Boolean); overload;
  begin
    sgResources.LoadResourceBundleNamed(name,filename,showProgress);
  end;

  function PathToResource(filename: String): String; overload;
  begin
    result := sgResources.PathToResource(filename);
  end;

  function PathToResource(filename: String; kind: ResourceKind): String; overload;
  begin
    result := sgResources.PathToResource(filename,kind);
  end;

  function PathToResource(filename: String; subdir: String): String; overload;
  begin
    result := sgResources.PathToResource(filename,subdir);
  end;

  function PathToResource(filename: String; kind: ResourceKind; const subPaths: StringArray): String; overload;
  begin
    result := sgResources.PathToResource(filename,kind,subPaths);
  end;

  function PathToResource(filename: String; kind: ResourceKind; subdir: String): String; overload;
  begin
    result := sgResources.PathToResource(filename,kind,subdir);
  end;

  function PathToResourceWithBase(path: String; filename: String): String; overload;
  begin
    result := sgResources.PathToResourceWithBase(path,filename);
  end;

  function PathToResourceWithBase(path: String; filename: String; kind: ResourceKind): String; overload;
  begin
    result := sgResources.PathToResourceWithBase(path,filename,kind);
  end;

  procedure RegisterFreeNotifier(fn: FreeNotifier); overload;
  begin
    sgResources.RegisterFreeNotifier(fn);
  end;

  procedure ReleaseAllResources(); overload;
  begin
    sgResources.ReleaseAllResources();
  end;

  procedure ReleaseResourceBundle(name: String); overload;
  begin
    sgResources.ReleaseResourceBundle(name);
  end;

  procedure SetAppPath(path: String); overload;
  begin
    sgResources.SetAppPath(path);
  end;

  procedure SetAppPath(path: String; withExe: Boolean); overload;
  begin
    sgResources.SetAppPath(path,withExe);
  end;

  procedure CallForAllSprites(fn: SpriteFunction); overload;
  begin
    sgSprites.CallForAllSprites(fn);
  end;

  procedure CallOnSpriteEvent(handler: SpriteEventHandler); overload;
  begin
    sgSprites.CallOnSpriteEvent(handler);
  end;

  function CenterPoint(s: Sprite): Point2D; overload;
  begin
    result := sgSprites.CenterPoint(s);
  end;

  function CreateSprite(layer: Bitmap): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layer);
  end;

  function CreateSprite(const layers: BitmapArray): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layers);
  end;

  function CreateSprite(layer: Bitmap; layerName: String): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layer,layerName);
  end;

  function CreateSprite(layer: Bitmap; pt: Point2D): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layer,pt);
  end;

  function CreateSprite(bitmapName: String; animationName: String): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(bitmapName,animationName);
  end;

  function CreateSprite(name: String; layer: Bitmap): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(name,layer);
  end;

  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layers,layerNames);
  end;

  function CreateSprite(layer: Bitmap; ani: AnimationScript): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layer,ani);
  end;

  function CreateSprite(name: String; const layers: BitmapArray): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(name,layers);
  end;

  function CreateSprite(const layers: BitmapArray; ani: AnimationScript): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layers,ani);
  end;

  function CreateSprite(name: String; layer: Bitmap; ani: AnimationScript): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(name,layer,ani);
  end;

  function CreateSprite(layer: Bitmap; ani: AnimationScript; pt: Point2D): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layer,ani,pt);
  end;

  function CreateSprite(name: String; layer: Bitmap; layerName: String): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(name,layer,layerName);
  end;

  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray; ani: AnimationScript): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layers,layerNames,ani);
  end;

  function CreateSprite(layer: Bitmap; layerName: String; ani: AnimationScript): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layer,layerName,ani);
  end;

  function CreateSprite(name: String; const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(name,layers,layerNames);
  end;

  function CreateSprite(name: String; const layers: BitmapArray; ani: AnimationScript): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(name,layers,ani);
  end;

  function CreateSprite(layer: Bitmap; x: Single; y: Single): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layer,x,y);
  end;

  function CreateSprite(name: String; layer: Bitmap; layerName: String; ani: AnimationScript): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(name,layer,layerName,ani);
  end;

  function CreateSprite(layer: Bitmap; ani: AnimationScript; x: Single; y: Single): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(layer,ani,x,y);
  end;

  function CreateSprite(name: String; const layers: BitmapArray; const layerNames: StringArray; ani: AnimationScript): Sprite; overload;
  begin
    result := sgSprites.CreateSprite(name,layers,layerNames,ani);
  end;

  procedure CreateSpritePack(name: String); overload;
  begin
    sgSprites.CreateSpritePack(name);
  end;

  function CurrentSpritePack(): String; overload;
  begin
    result := sgSprites.CurrentSpritePack();
  end;

  procedure DrawAllSprites(); overload;
  begin
    sgSprites.DrawAllSprites();
  end;

  procedure DrawSprite(s: Sprite); overload;
  begin
    sgSprites.DrawSprite(s);
  end;

  procedure DrawSprite(s: Sprite; const position: Point2D); overload;
  begin
    sgSprites.DrawSprite(s,position);
  end;

  procedure DrawSprite(s: Sprite; xOffset: Longint; yOffset: Longint); overload;
  begin
    sgSprites.DrawSprite(s,xOffset,yOffset);
  end;

  procedure FreeSprite(var s: Sprite); overload;
  begin
    sgSprites.FreeSprite(s);
  end;

  function HasSprite(name: String): Boolean; overload;
  begin
    result := sgSprites.HasSprite(name);
  end;

  function HasSpritePack(name: String): Boolean; overload;
  begin
    result := sgSprites.HasSpritePack(name);
  end;

  procedure MoveSprite(s: Sprite); overload;
  begin
    sgSprites.MoveSprite(s);
  end;

  procedure MoveSprite(s: Sprite; const distance: Vector); overload;
  begin
    sgSprites.MoveSprite(s,distance);
  end;

  procedure MoveSprite(s: Sprite; pct: Single); overload;
  begin
    sgSprites.MoveSprite(s,pct);
  end;

  procedure MoveSprite(s: Sprite; const distance: Vector; pct: Single); overload;
  begin
    sgSprites.MoveSprite(s,distance,pct);
  end;

  procedure MoveSpriteTo(s: Sprite; x: Longint; y: Longint); overload;
  begin
    sgSprites.MoveSpriteTo(s,x,y);
  end;

  procedure ReleaseAllSprites(); overload;
  begin
    sgSprites.ReleaseAllSprites();
  end;

  procedure ReleaseSprite(name: String); overload;
  begin
    sgSprites.ReleaseSprite(name);
  end;

  procedure SelectSpritePack(name: String); overload;
  begin
    sgSprites.SelectSpritePack(name);
  end;

  function SpriteAddLayer(s: Sprite; newLayer: Bitmap; layerName: String): Longint; overload;
  begin
    result := sgSprites.SpriteAddLayer(s,newLayer,layerName);
  end;

  procedure SpriteAddToVelocity(s: Sprite; const value: Vector); overload;
  begin
    sgSprites.SpriteAddToVelocity(s,value);
  end;

  procedure SpriteAddValue(s: Sprite; name: String); overload;
  begin
    sgSprites.SpriteAddValue(s,name);
  end;

  procedure SpriteAddValue(s: Sprite; name: String; initVal: Single); overload;
  begin
    sgSprites.SpriteAddValue(s,name,initVal);
  end;

  function SpriteAnimationHasEnded(s: Sprite): Boolean; overload;
  begin
    result := sgSprites.SpriteAnimationHasEnded(s);
  end;

  function SpriteAnimationName(s: Sprite): String; overload;
  begin
    result := sgSprites.SpriteAnimationName(s);
  end;

  procedure SpriteBringLayerForward(s: Sprite; visibleLayer: Longint); overload;
  begin
    sgSprites.SpriteBringLayerForward(s,visibleLayer);
  end;

  procedure SpriteBringLayerToFront(s: Sprite; visibleLayer: Longint); overload;
  begin
    sgSprites.SpriteBringLayerToFront(s,visibleLayer);
  end;

  procedure SpriteCallOnEvent(s: Sprite; handler: SpriteEventHandler); overload;
  begin
    sgSprites.SpriteCallOnEvent(s,handler);
  end;

  function SpriteCircle(s: Sprite): Circle; overload;
  begin
    result := sgSprites.SpriteCircle(s);
  end;

  function SpriteCollisionBitmap(s: Sprite): Bitmap; overload;
  begin
    result := sgSprites.SpriteCollisionBitmap(s);
  end;

  function SpriteCollisionCircle(s: Sprite): Circle; overload;
  begin
    result := sgSprites.SpriteCollisionCircle(s);
  end;

  function SpriteCollisionKind(s: Sprite): CollisionTestKind; overload;
  begin
    result := sgSprites.SpriteCollisionKind(s);
  end;

  function SpriteCollisionRectangle(s: Sprite): Rectangle; overload;
  begin
    result := sgSprites.SpriteCollisionRectangle(s);
  end;

  function SpriteCurrentCell(s: Sprite): Longint; overload;
  begin
    result := sgSprites.SpriteCurrentCell(s);
  end;

  function SpriteCurrentCellRectangle(s: Sprite): Rectangle; overload;
  begin
    result := sgSprites.SpriteCurrentCellRectangle(s);
  end;

  function SpriteDX(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteDX(s);
  end;

  function SpriteDY(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteDY(s);
  end;

  function SpriteHeading(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteHeading(s);
  end;

  function SpriteHeight(s: Sprite): Longint; overload;
  begin
    result := sgSprites.SpriteHeight(s);
  end;

  procedure SpriteHideLayer(s: Sprite; name: String); overload;
  begin
    sgSprites.SpriteHideLayer(s,name);
  end;

  procedure SpriteHideLayer(s: Sprite; id: Longint); overload;
  begin
    sgSprites.SpriteHideLayer(s,id);
  end;

  function SpriteLayer(s: Sprite; idx: Longint): Bitmap; overload;
  begin
    result := sgSprites.SpriteLayer(s,idx);
  end;

  function SpriteLayer(s: Sprite; name: String): Bitmap; overload;
  begin
    result := sgSprites.SpriteLayer(s,name);
  end;

  function SpriteLayerCircle(s: Sprite; idx: Longint): Circle; overload;
  begin
    result := sgSprites.SpriteLayerCircle(s,idx);
  end;

  function SpriteLayerCircle(s: Sprite; name: String): Circle; overload;
  begin
    result := sgSprites.SpriteLayerCircle(s,name);
  end;

  function SpriteLayerCount(s: Sprite): Longint; overload;
  begin
    result := sgSprites.SpriteLayerCount(s);
  end;

  function SpriteLayerHeight(s: Sprite; name: String): Longint; overload;
  begin
    result := sgSprites.SpriteLayerHeight(s,name);
  end;

  function SpriteLayerHeight(s: Sprite; idx: Longint): Longint; overload;
  begin
    result := sgSprites.SpriteLayerHeight(s,idx);
  end;

  function SpriteLayerIndex(s: Sprite; name: String): Longint; overload;
  begin
    result := sgSprites.SpriteLayerIndex(s,name);
  end;

  function SpriteLayerName(s: Sprite; idx: Longint): String; overload;
  begin
    result := sgSprites.SpriteLayerName(s,idx);
  end;

  function SpriteLayerOffset(s: Sprite; name: String): Point2D; overload;
  begin
    result := sgSprites.SpriteLayerOffset(s,name);
  end;

  function SpriteLayerOffset(s: Sprite; idx: Longint): Point2D; overload;
  begin
    result := sgSprites.SpriteLayerOffset(s,idx);
  end;

  function SpriteLayerOffsets(s: Sprite): Point2DArray; overload;
  begin
    result := sgSprites.SpriteLayerOffsets(s);
  end;

  function SpriteLayerRectangle(s: Sprite; name: String): Rectangle; overload;
  begin
    result := sgSprites.SpriteLayerRectangle(s,name);
  end;

  function SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle; overload;
  begin
    result := sgSprites.SpriteLayerRectangle(s,idx);
  end;

  function SpriteLayerWidth(s: Sprite; name: String): Longint; overload;
  begin
    result := sgSprites.SpriteLayerWidth(s,name);
  end;

  function SpriteLayerWidth(s: Sprite; idx: Longint): Longint; overload;
  begin
    result := sgSprites.SpriteLayerWidth(s,idx);
  end;

  function SpriteLayers(s: Sprite): BitmapArray; overload;
  begin
    result := sgSprites.SpriteLayers(s);
  end;

  function SpriteMass(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteMass(s);
  end;

  procedure SpriteMoveTo(s: Sprite; const pt: Point2D; takingSeconds: Longint); overload;
  begin
    sgSprites.SpriteMoveTo(s,pt,takingSeconds);
  end;

  function SpriteName(sprt: Sprite): String; overload;
  begin
    result := sgSprites.SpriteName(sprt);
  end;

  function SpriteNamed(name: String): Sprite; overload;
  begin
    result := sgSprites.SpriteNamed(name);
  end;

  function SpriteOffscreen(s: Sprite): Boolean; overload;
  begin
    result := sgSprites.SpriteOffscreen(s);
  end;

  function SpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;
  begin
    result := sgSprites.SpriteOnScreenAt(s,pt);
  end;

  function SpriteOnScreenAt(s: Sprite; x: Longint; y: Longint): Boolean; overload;
  begin
    result := sgSprites.SpriteOnScreenAt(s,x,y);
  end;

  function SpritePosition(s: Sprite): Point2D; overload;
  begin
    result := sgSprites.SpritePosition(s);
  end;

  procedure SpriteReplayAnimation(s: Sprite); overload;
  begin
    sgSprites.SpriteReplayAnimation(s);
  end;

  procedure SpriteReplayAnimation(s: Sprite; withSound: Boolean); overload;
  begin
    sgSprites.SpriteReplayAnimation(s,withSound);
  end;

  function SpriteRotation(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteRotation(s);
  end;

  function SpriteScale(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteScale(s);
  end;

  function SpriteScreenRectangle(s: Sprite): Rectangle; overload;
  begin
    result := sgSprites.SpriteScreenRectangle(s);
  end;

  procedure SpriteSendLayerBackward(s: Sprite; visibleLayer: Longint); overload;
  begin
    sgSprites.SpriteSendLayerBackward(s,visibleLayer);
  end;

  procedure SpriteSendLayerToBack(s: Sprite; visibleLayer: Longint); overload;
  begin
    sgSprites.SpriteSendLayerToBack(s,visibleLayer);
  end;

  procedure SpriteSetCollisionBitmap(s: Sprite; bmp: Bitmap); overload;
  begin
    sgSprites.SpriteSetCollisionBitmap(s,bmp);
  end;

  procedure SpriteSetCollisionKind(s: Sprite; value: CollisionTestKind); overload;
  begin
    sgSprites.SpriteSetCollisionKind(s,value);
  end;

  procedure SpriteSetDX(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetDX(s,value);
  end;

  procedure SpriteSetDY(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetDY(s,value);
  end;

  procedure SpriteSetHeading(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetHeading(s,value);
  end;

  procedure SpriteSetLayerOffset(s: Sprite; name: String; const value: Point2D); overload;
  begin
    sgSprites.SpriteSetLayerOffset(s,name,value);
  end;

  procedure SpriteSetLayerOffset(s: Sprite; idx: Longint; const value: Point2D); overload;
  begin
    sgSprites.SpriteSetLayerOffset(s,idx,value);
  end;

  procedure SpriteSetLayerOffsets(s: Sprite; const values: Point2DArray); overload;
  begin
    sgSprites.SpriteSetLayerOffsets(s,values);
  end;

  procedure SpriteSetMass(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetMass(s,value);
  end;

  procedure SpriteSetPosition(s: Sprite; const value: Point2D); overload;
  begin
    sgSprites.SpriteSetPosition(s,value);
  end;

  procedure SpriteSetRotation(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetRotation(s,value);
  end;

  procedure SpriteSetScale(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetScale(s,value);
  end;

  procedure SpriteSetSpeed(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetSpeed(s,value);
  end;

  procedure SpriteSetValue(s: Sprite; name: String; val: Single); overload;
  begin
    sgSprites.SpriteSetValue(s,name,val);
  end;

  procedure SpriteSetValue(s: Sprite; idx: Longint; val: Single); overload;
  begin
    sgSprites.SpriteSetValue(s,idx,val);
  end;

  procedure SpriteSetVelocity(s: Sprite; const value: Vector); overload;
  begin
    sgSprites.SpriteSetVelocity(s,value);
  end;

  procedure SpriteSetX(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetX(s,value);
  end;

  procedure SpriteSetY(s: Sprite; value: Single); overload;
  begin
    sgSprites.SpriteSetY(s,value);
  end;

  function SpriteShowLayer(s: Sprite; id: Longint): Longint; overload;
  begin
    result := sgSprites.SpriteShowLayer(s,id);
  end;

  function SpriteShowLayer(s: Sprite; name: String): Longint; overload;
  begin
    result := sgSprites.SpriteShowLayer(s,name);
  end;

  function SpriteSpeed(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteSpeed(s);
  end;

  procedure SpriteStartAnimation(s: Sprite; idx: Longint); overload;
  begin
    sgSprites.SpriteStartAnimation(s,idx);
  end;

  procedure SpriteStartAnimation(s: Sprite; named: String); overload;
  begin
    sgSprites.SpriteStartAnimation(s,named);
  end;

  procedure SpriteStartAnimation(s: Sprite; named: String; withSound: Boolean); overload;
  begin
    sgSprites.SpriteStartAnimation(s,named,withSound);
  end;

  procedure SpriteStartAnimation(s: Sprite; idx: Longint; withSound: Boolean); overload;
  begin
    sgSprites.SpriteStartAnimation(s,idx,withSound);
  end;

  procedure SpriteStopCallingOnEvent(s: Sprite; handler: SpriteEventHandler); overload;
  begin
    sgSprites.SpriteStopCallingOnEvent(s,handler);
  end;

  procedure SpriteToggleLayerVisible(s: Sprite; id: Longint); overload;
  begin
    sgSprites.SpriteToggleLayerVisible(s,id);
  end;

  procedure SpriteToggleLayerVisible(s: Sprite; name: String); overload;
  begin
    sgSprites.SpriteToggleLayerVisible(s,name);
  end;

  function SpriteValue(s: Sprite; index: Longint): Single; overload;
  begin
    result := sgSprites.SpriteValue(s,index);
  end;

  function SpriteValue(s: Sprite; name: String): Single; overload;
  begin
    result := sgSprites.SpriteValue(s,name);
  end;

  function SpriteValueCount(s: Sprite): Longint; overload;
  begin
    result := sgSprites.SpriteValueCount(s);
  end;

  function SpriteValueNames(s: Sprite): StringArray; overload;
  begin
    result := sgSprites.SpriteValueNames(s);
  end;

  function SpriteVelocity(s: Sprite): Vector; overload;
  begin
    result := sgSprites.SpriteVelocity(s);
  end;

  function SpriteVisibleIndexOfLayer(s: Sprite; id: Longint): Longint; overload;
  begin
    result := sgSprites.SpriteVisibleIndexOfLayer(s,id);
  end;

  function SpriteVisibleIndexOfLayer(s: Sprite; name: String): Longint; overload;
  begin
    result := sgSprites.SpriteVisibleIndexOfLayer(s,name);
  end;

  function SpriteVisibleLayer(s: Sprite; idx: Longint): Longint; overload;
  begin
    result := sgSprites.SpriteVisibleLayer(s,idx);
  end;

  function SpriteVisibleLayerCount(s: Sprite): Longint; overload;
  begin
    result := sgSprites.SpriteVisibleLayerCount(s);
  end;

  function SpriteVisibleLayerIds(s: Sprite): LongintArray; overload;
  begin
    result := sgSprites.SpriteVisibleLayerIds(s);
  end;

  function SpriteWidth(s: Sprite): Longint; overload;
  begin
    result := sgSprites.SpriteWidth(s);
  end;

  function SpriteX(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteX(s);
  end;

  function SpriteY(s: Sprite): Single; overload;
  begin
    result := sgSprites.SpriteY(s);
  end;

  procedure StopCallingOnSpriteEvent(handler: SpriteEventHandler); overload;
  begin
    sgSprites.StopCallingOnSpriteEvent(handler);
  end;

  procedure UpdateAllSprites(); overload;
  begin
    sgSprites.UpdateAllSprites();
  end;

  procedure UpdateAllSprites(pct: Single); overload;
  begin
    sgSprites.UpdateAllSprites(pct);
  end;

  procedure UpdateSprite(s: Sprite); overload;
  begin
    sgSprites.UpdateSprite(s);
  end;

  procedure UpdateSprite(s: Sprite; withSound: Boolean); overload;
  begin
    sgSprites.UpdateSprite(s,withSound);
  end;

  procedure UpdateSprite(s: Sprite; pct: Single); overload;
  begin
    sgSprites.UpdateSprite(s,pct);
  end;

  procedure UpdateSprite(s: Sprite; pct: Single; withSound: Boolean); overload;
  begin
    sgSprites.UpdateSprite(s,pct,withSound);
  end;

  procedure UpdateSpriteAnimation(s: Sprite); overload;
  begin
    sgSprites.UpdateSpriteAnimation(s);
  end;

  procedure UpdateSpriteAnimation(s: Sprite; withSound: Boolean); overload;
  begin
    sgSprites.UpdateSpriteAnimation(s,withSound);
  end;

  procedure UpdateSpriteAnimation(s: Sprite; pct: Single); overload;
  begin
    sgSprites.UpdateSpriteAnimation(s,pct);
  end;

  procedure UpdateSpriteAnimation(s: Sprite; pct: Single; withSound: Boolean); overload;
  begin
    sgSprites.UpdateSpriteAnimation(s,pct,withSound);
  end;

  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector; overload;
  begin
    result := sgSprites.VectorFromCenterSpriteToPoint(s,pt);
  end;

  function VectorFromTo(s1: Sprite; s2: Sprite): Vector; overload;
  begin
    result := sgSprites.VectorFromTo(s1,s2);
  end;

  procedure DrawFramerate(x: Longint; y: Longint); overload;
  begin
    sgText.DrawFramerate(x,y);
  end;

  procedure DrawFramerate(x: Longint; y: Longint; font: Font); overload;
  begin
    sgText.DrawFramerate(x,y,font);
  end;

  procedure DrawFramerate(x: Longint; y: Longint; name: String); overload;
  begin
    sgText.DrawFramerate(x,y,name);
  end;

  procedure DrawFramerate(x: Longint; y: Longint; name: String; size: Longint); overload;
  begin
    sgText.DrawFramerate(x,y,name,size);
  end;

  procedure DrawText(theText: String; textColor: Color; const pt: Point2D); overload;
  begin
    sgText.DrawText(theText,textColor,pt);
  end;

  procedure DrawText(theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    sgText.DrawText(theText,textColor,theFont,pt);
  end;

  procedure DrawText(theText: String; textColor: Color; x: Single; y: Single); overload;
  begin
    sgText.DrawText(theText,textColor,x,y);
  end;

  procedure DrawText(theText: String; textColor: Color; name: String; const pt: Point2D); overload;
  begin
    sgText.DrawText(theText,textColor,name,pt);
  end;

  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; const pt: Point2D); overload;
  begin
    sgText.DrawText(dest,theText,textColor,name,pt);
  end;

  procedure DrawText(theText: String; textColor: Color; theFont: Font; x: Single; y: Single); overload;
  begin
    sgText.DrawText(theText,textColor,theFont,x,y);
  end;

  procedure DrawText(theText: String; textColor: Color; name: String; x: Single; y: Single); overload;
  begin
    sgText.DrawText(theText,textColor,name,x,y);
  end;

  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; x: Single; y: Single); overload;
  begin
    sgText.DrawText(dest,theText,textColor,x,y);
  end;

  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    sgText.DrawText(dest,theText,textColor,theFont,pt);
  end;

  procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  begin
    sgText.DrawText(theText,textColor,name,size,pt);
  end;

  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  begin
    sgText.DrawText(dest,theText,textColor,name,size,pt);
  end;

  procedure DrawText(theText: String; textColor: Color; name: String; size: Longint; x: Single; y: Single); overload;
  begin
    sgText.DrawText(theText,textColor,name,size,x,y);
  end;

  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; theFont: Font; x: Longint; y: Longint); overload;
  begin
    sgText.DrawText(dest,theText,textColor,theFont,x,y);
  end;

  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; x: Longint; y: Longint); overload;
  begin
    sgText.DrawText(dest,theText,textColor,name,x,y);
  end;

  procedure DrawText(dest: Bitmap; theText: String; textColor: Color; name: String; size: Longint; x: Longint; y: Longint); overload;
  begin
    sgText.DrawText(dest,theText,textColor,name,size,x,y);
  end;

  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; x: Single; y: Single; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLines(theText,textColor,backColor,name,size,align,x,y,w,h);
  end;

  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLines(dest,theText,textColor,backColor,name,align,x,y,w,h);
  end;

  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLines(dest,theText,textColor,backColor,theFont,align,x,y,w,h);
  end;

  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLines(dest,theText,textColor,backColor,name,size,align,x,y,w,h);
  end;

  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLines(theText,textColor,backColor,name,align,withinRect);
  end;

  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLines(theText,textColor,backColor,theFont,align,withinRect);
  end;

  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLines(dest,theText,textColor,backColor,theFont,align,withinRect);
  end;

  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLines(dest,theText,textColor,backColor,name,align,withinRect);
  end;

  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLines(theText,textColor,backColor,name,size,align,withinRect);
  end;

  procedure DrawTextLines(dest: Bitmap; theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLines(dest,theText,textColor,backColor,name,size,align,withinRect);
  end;

  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; x: Single; y: Single; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLines(theText,textColor,backColor,theFont,align,x,y,w,h);
  end;

  procedure DrawTextLines(theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; x: Single; y: Single; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLines(theText,textColor,backColor,name,align,x,y,w,h);
  end;

  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLinesOnScreen(theText,textColor,backColor,name,size,align,x,y,w,h);
  end;

  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLinesOnScreen(theText,textColor,backColor,theFont,align,withinRect);
  end;

  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLinesOnScreen(theText,textColor,backColor,name,align,withinRect);
  end;

  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; name: String; size: Longint; align: FontAlignment; const withinRect: Rectangle); overload;
  begin
    sgText.DrawTextLinesOnScreen(theText,textColor,backColor,name,size,align,withinRect);
  end;

  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; theFont: Font; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLinesOnScreen(theText,textColor,backColor,theFont,align,x,y,w,h);
  end;

  procedure DrawTextLinesOnScreen(theText: String; textColor: Color; backColor: Color; name: String; align: FontAlignment; x: Longint; y: Longint; w: Longint; h: Longint); overload;
  begin
    sgText.DrawTextLinesOnScreen(theText,textColor,backColor,name,align,x,y,w,h);
  end;

  procedure DrawTextOnScreen(theText: String; textColor: Color; x: Single; y: Single); overload;
  begin
    sgText.DrawTextOnScreen(theText,textColor,x,y);
  end;

  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; const pt: Point2D); overload;
  begin
    sgText.DrawTextOnScreen(theText,textColor,name,pt);
  end;

  procedure DrawTextOnScreen(theText: String; textColor: Color; theFont: Font; const pt: Point2D); overload;
  begin
    sgText.DrawTextOnScreen(theText,textColor,theFont,pt);
  end;

  procedure DrawTextOnScreen(theText: String; textColor: Color; theFont: Font; x: Longint; y: Longint); overload;
  begin
    sgText.DrawTextOnScreen(theText,textColor,theFont,x,y);
  end;

  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; x: Longint; y: Longint); overload;
  begin
    sgText.DrawTextOnScreen(theText,textColor,name,x,y);
  end;

  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; size: Longint; const pt: Point2D); overload;
  begin
    sgText.DrawTextOnScreen(theText,textColor,name,size,pt);
  end;

  procedure DrawTextOnScreen(theText: String; textColor: Color; name: String; size: Longint; x: Longint; y: Longint); overload;
  begin
    sgText.DrawTextOnScreen(theText,textColor,name,size,x,y);
  end;

  function DrawTextTo(font: Font; str: String; clrFg: Color; backgroundColor: Color): Bitmap; overload;
  begin
    result := sgText.DrawTextTo(font,str,clrFg,backgroundColor);
  end;

  function FontFontStyle(font: Font): FontStyle; overload;
  begin
    result := sgText.FontFontStyle(font);
  end;

  function FontNameFor(fontName: String; size: Longint): String; overload;
  begin
    result := sgText.FontNameFor(fontName,size);
  end;

  function FontNamed(name: String): Font; overload;
  begin
    result := sgText.FontNamed(name);
  end;

  function FontNamed(name: String; size: Longint): Font; overload;
  begin
    result := sgText.FontNamed(name,size);
  end;

  procedure FontSetStyle(font: Font; value: FontStyle); overload;
  begin
    sgText.FontSetStyle(font,value);
  end;

  procedure FreeFont(var fontToFree: Font); overload;
  begin
    sgText.FreeFont(fontToFree);
  end;

  function HasFont(name: String): Boolean; overload;
  begin
    result := sgText.HasFont(name);
  end;

  function LoadFont(fontName: String; size: Longint): Font; overload;
  begin
    result := sgText.LoadFont(fontName,size);
  end;

  function LoadFontNamed(name: String; filename: String; size: Longint): Font; overload;
  begin
    result := sgText.LoadFontNamed(name,filename,size);
  end;

  procedure ReleaseAllFonts(); overload;
  begin
    sgText.ReleaseAllFonts();
  end;

  procedure ReleaseFont(name: String); overload;
  begin
    sgText.ReleaseFont(name);
  end;

  function TextAlignmentFrom(str: String): FontAlignment; overload;
  begin
    result := sgText.TextAlignmentFrom(str);
  end;

  function TextHeight(theFont: Font; theText: String): Longint; overload;
  begin
    result := sgText.TextHeight(theFont,theText);
  end;

  function TextWidth(theFont: Font; theText: String): Longint; overload;
  begin
    result := sgText.TextWidth(theFont,theText);
  end;

  function CreateTimer(): Timer; overload;
  begin
    result := sgTimers.CreateTimer();
  end;

  function CreateTimer(name: String): Timer; overload;
  begin
    result := sgTimers.CreateTimer(name);
  end;

  procedure FreeTimer(var toFree: Timer); overload;
  begin
    sgTimers.FreeTimer(toFree);
  end;

  procedure PauseTimer(toPause: Timer); overload;
  begin
    sgTimers.PauseTimer(toPause);
  end;

  procedure PauseTimer(name: String); overload;
  begin
    sgTimers.PauseTimer(name);
  end;

  procedure ReleaseAllTimers(); overload;
  begin
    sgTimers.ReleaseAllTimers();
  end;

  procedure ReleaseTimer(name: String); overload;
  begin
    sgTimers.ReleaseTimer(name);
  end;

  procedure ResetTimer(name: String); overload;
  begin
    sgTimers.ResetTimer(name);
  end;

  procedure ResetTimer(tmr: Timer); overload;
  begin
    sgTimers.ResetTimer(tmr);
  end;

  procedure ResumeTimer(name: String); overload;
  begin
    sgTimers.ResumeTimer(name);
  end;

  procedure ResumeTimer(toUnpause: Timer); overload;
  begin
    sgTimers.ResumeTimer(toUnpause);
  end;

  procedure StartTimer(name: String); overload;
  begin
    sgTimers.StartTimer(name);
  end;

  procedure StartTimer(toStart: Timer); overload;
  begin
    sgTimers.StartTimer(toStart);
  end;

  procedure StopTimer(toStop: Timer); overload;
  begin
    sgTimers.StopTimer(toStop);
  end;

  procedure StopTimer(name: String); overload;
  begin
    sgTimers.StopTimer(name);
  end;

  function TimerNamed(name: String): Timer; overload;
  begin
    result := sgTimers.TimerNamed(name);
  end;

  function TimerTicks(name: String): Longword; overload;
  begin
    result := sgTimers.TimerTicks(name);
  end;

  function TimerTicks(toGet: Timer): Longword; overload;
  begin
    result := sgTimers.TimerTicks(toGet);
  end;

  procedure CalculateFramerate(out average: String; out highest: String; out lowest: String; out textColor: Color); overload;
  begin
    sgUtils.CalculateFramerate(average,highest,lowest,textColor);
  end;

  procedure Delay(time: Longword); overload;
  begin
    sgUtils.Delay(time);
  end;

  function ExceptionMessage(): String; overload;
  begin
    result := sgUtils.ExceptionMessage();
  end;

  function ExceptionOccured(): Boolean; overload;
  begin
    result := sgUtils.ExceptionOccured();
  end;

  function GetFramerate(): Longint; overload;
  begin
    result := sgUtils.GetFramerate();
  end;

  function GetTicks(): Longword; overload;
  begin
    result := sgUtils.GetTicks();
  end;

  function Rnd(): Single; overload;
  begin
    result := sgUtils.Rnd();
  end;

  function Rnd(ubound: Longint): Longint; overload;
  begin
    result := sgUtils.Rnd(ubound);
  end;

  function SwinGameVersion(): String; overload;
  begin
    result := sgUtils.SwinGameVersion();
  end;

  procedure ActivatePanel(p: Panel); overload;
  begin
    sgUserInterface.ActivatePanel(p);
  end;

  function ActiveRadioButton(grp: GUIRadioGroup): Region; overload;
  begin
    result := sgUserInterface.ActiveRadioButton(grp);
  end;

  function ActiveRadioButton(id: String): Region; overload;
  begin
    result := sgUserInterface.ActiveRadioButton(id);
  end;

  function ActiveRadioButton(pnl: Panel; id: String): Region; overload;
  begin
    result := sgUserInterface.ActiveRadioButton(pnl,id);
  end;

  function ActiveRadioButtonIndex(RadioGroup: GUIRadioGroup): Longint; overload;
  begin
    result := sgUserInterface.ActiveRadioButtonIndex(RadioGroup);
  end;

  function ActiveRadioButtonIndex(id: String): Longint; overload;
  begin
    result := sgUserInterface.ActiveRadioButtonIndex(id);
  end;

  function ActiveRadioButtonIndex(pnl: Panel; id: String): Longint; overload;
  begin
    result := sgUserInterface.ActiveRadioButtonIndex(pnl,id);
  end;

  function ActiveTextBoxParent(): Panel; overload;
  begin
    result := sgUserInterface.ActiveTextBoxParent();
  end;

  function ActiveTextIndex(): Longint; overload;
  begin
    result := sgUserInterface.ActiveTextIndex();
  end;

  function ButtonClicked(name: String): Boolean; overload;
  begin
    result := sgUserInterface.ButtonClicked(name);
  end;

  function ButtonClicked(r: Region): Boolean; overload;
  begin
    result := sgUserInterface.ButtonClicked(r);
  end;

  function CheckboxFromRegion(r: Region): GUICheckbox; overload;
  begin
    result := sgUserInterface.CheckboxFromRegion(r);
  end;

  procedure CheckboxSetState(chk: GUICheckbox; val: Boolean); overload;
  begin
    sgUserInterface.CheckboxSetState(chk,val);
  end;

  procedure CheckboxSetState(id: String; val: Boolean); overload;
  begin
    sgUserInterface.CheckboxSetState(id,val);
  end;

  procedure CheckboxSetState(r: Region; val: Boolean); overload;
  begin
    sgUserInterface.CheckboxSetState(r,val);
  end;

  procedure CheckboxSetState(pnl: Panel; id: String; val: Boolean); overload;
  begin
    sgUserInterface.CheckboxSetState(pnl,id,val);
  end;

  function CheckboxState(r: Region): Boolean; overload;
  begin
    result := sgUserInterface.CheckboxState(r);
  end;

  function CheckboxState(s: String): Boolean; overload;
  begin
    result := sgUserInterface.CheckboxState(s);
  end;

  function CheckboxState(chk: GUICheckbox): Boolean; overload;
  begin
    result := sgUserInterface.CheckboxState(chk);
  end;

  function CheckboxState(p: Panel; s: String): Boolean; overload;
  begin
    result := sgUserInterface.CheckboxState(p,s);
  end;

  procedure DeactivatePanel(p: Panel); overload;
  begin
    sgUserInterface.DeactivatePanel(p);
  end;

  procedure DeactivateTextBox(); overload;
  begin
    sgUserInterface.DeactivateTextBox();
  end;

  function DialogCancelled(): Boolean; overload;
  begin
    result := sgUserInterface.DialogCancelled();
  end;

  function DialogComplete(): Boolean; overload;
  begin
    result := sgUserInterface.DialogComplete();
  end;

  function DialogPath(): String; overload;
  begin
    result := sgUserInterface.DialogPath();
  end;

  procedure DialogSetPath(fullname: String); overload;
  begin
    sgUserInterface.DialogSetPath(fullname);
  end;

  procedure DrawGUIAsVectors(b: Boolean); overload;
  begin
    sgUserInterface.DrawGUIAsVectors(b);
  end;

  procedure DrawInterface(); overload;
  begin
    sgUserInterface.DrawInterface();
  end;

  procedure FinishReadingText(); overload;
  begin
    sgUserInterface.FinishReadingText();
  end;

  procedure FreePanel(var pnl: Panel); overload;
  begin
    sgUserInterface.FreePanel(pnl);
  end;

  function GUIClicked(): Boolean; overload;
  begin
    result := sgUserInterface.GUIClicked();
  end;

  procedure GUISetActiveTextbox(r: Region); overload;
  begin
    sgUserInterface.GUISetActiveTextbox(r);
  end;

  procedure GUISetActiveTextbox(name: String); overload;
  begin
    sgUserInterface.GUISetActiveTextbox(name);
  end;

  procedure GUISetActiveTextbox(t: GUITextbox); overload;
  begin
    sgUserInterface.GUISetActiveTextbox(t);
  end;

  procedure GUISetBackgroundColor(c: Color); overload;
  begin
    sgUserInterface.GUISetBackgroundColor(c);
  end;

  procedure GUISetBackgroundColorInactive(c: Color); overload;
  begin
    sgUserInterface.GUISetBackgroundColorInactive(c);
  end;

  procedure GUISetForegroundColor(c: Color); overload;
  begin
    sgUserInterface.GUISetForegroundColor(c);
  end;

  procedure GUISetForegroundColorInactive(c: Color); overload;
  begin
    sgUserInterface.GUISetForegroundColorInactive(c);
  end;

  function GUITextBoxOfTextEntered(): GUITextbox; overload;
  begin
    result := sgUserInterface.GUITextBoxOfTextEntered();
  end;

  function GUITextEntryComplete(): Boolean; overload;
  begin
    result := sgUserInterface.GUITextEntryComplete();
  end;

  function HasPanel(name: String): Boolean; overload;
  begin
    result := sgUserInterface.HasPanel(name);
  end;

  procedure HidePanel(name: String); overload;
  begin
    sgUserInterface.HidePanel(name);
  end;

  procedure HidePanel(p: Panel); overload;
  begin
    sgUserInterface.HidePanel(p);
  end;

  function IndexOfLastUpdatedTextBox(): Longint; overload;
  begin
    result := sgUserInterface.IndexOfLastUpdatedTextBox();
  end;

  function IsDragging(): Boolean; overload;
  begin
    result := sgUserInterface.IsDragging();
  end;

  function IsDragging(pnl: Panel): Boolean; overload;
  begin
    result := sgUserInterface.IsDragging(pnl);
  end;

  function LabelAlignment(r: Region): FontAlignment; overload;
  begin
    result := sgUserInterface.LabelAlignment(r);
  end;

  function LabelAlignment(lbl: GUILabel): FontAlignment; overload;
  begin
    result := sgUserInterface.LabelAlignment(lbl);
  end;

  function LabelFont(r: Region): Font; overload;
  begin
    result := sgUserInterface.LabelFont(r);
  end;

  function LabelFont(l: GUILabel): Font; overload;
  begin
    result := sgUserInterface.LabelFont(l);
  end;

  function LabelFromRegion(r: Region): GUILabel; overload;
  begin
    result := sgUserInterface.LabelFromRegion(r);
  end;

  procedure LabelSetAlignment(r: Region; align: FontAlignment); overload;
  begin
    sgUserInterface.LabelSetAlignment(r,align);
  end;

  procedure LabelSetAlignment(tb: GUILabel; align: FontAlignment); overload;
  begin
    sgUserInterface.LabelSetAlignment(tb,align);
  end;

  procedure LabelSetFont(l: GUILabel; s: String); overload;
  begin
    sgUserInterface.LabelSetFont(l,s);
  end;

  procedure LabelSetText(lb: GUILabel; newString: String); overload;
  begin
    sgUserInterface.LabelSetText(lb,newString);
  end;

  procedure LabelSetText(r: Region; newString: String); overload;
  begin
    sgUserInterface.LabelSetText(r,newString);
  end;

  procedure LabelSetText(id: String; newString: String); overload;
  begin
    sgUserInterface.LabelSetText(id,newString);
  end;

  procedure LabelSetText(pnl: Panel; id: String; newString: String); overload;
  begin
    sgUserInterface.LabelSetText(pnl,id,newString);
  end;

  function LabelText(r: Region): String; overload;
  begin
    result := sgUserInterface.LabelText(r);
  end;

  function LabelText(lb: GUILabel): String; overload;
  begin
    result := sgUserInterface.LabelText(lb);
  end;

  function LabelText(id: String): String; overload;
  begin
    result := sgUserInterface.LabelText(id);
  end;

  function LabelText(pnl: Panel; id: String): String; overload;
  begin
    result := sgUserInterface.LabelText(pnl,id);
  end;

  function ListActiveItemIndex(lst: GUIList): Longint; overload;
  begin
    result := sgUserInterface.ListActiveItemIndex(lst);
  end;

  function ListActiveItemIndex(id: String): Longint; overload;
  begin
    result := sgUserInterface.ListActiveItemIndex(id);
  end;

  function ListActiveItemIndex(r: Region): Longint; overload;
  begin
    result := sgUserInterface.ListActiveItemIndex(r);
  end;

  function ListActiveItemIndex(pnl: Panel; id: String): Longint; overload;
  begin
    result := sgUserInterface.ListActiveItemIndex(pnl,id);
  end;

  function ListActiveItemText(list: GUIList): String; overload;
  begin
    result := sgUserInterface.ListActiveItemText(list);
  end;

  function ListActiveItemText(r: Region): String; overload;
  begin
    result := sgUserInterface.ListActiveItemText(r);
  end;

  function ListActiveItemText(ID: String): String; overload;
  begin
    result := sgUserInterface.ListActiveItemText(ID);
  end;

  function ListActiveItemText(pnl: Panel; ID: String): String; overload;
  begin
    result := sgUserInterface.ListActiveItemText(pnl,ID);
  end;

  procedure ListAddItem(lst: GUIList; img: Bitmap); overload;
  begin
    sgUserInterface.ListAddItem(lst,img);
  end;

  procedure ListAddItem(id: String; img: Bitmap); overload;
  begin
    sgUserInterface.ListAddItem(id,img);
  end;

  procedure ListAddItem(lst: GUIList; const img: BitmapCell); overload;
  begin
    sgUserInterface.ListAddItem(lst,img);
  end;

  procedure ListAddItem(lst: GUIList; text: String); overload;
  begin
    sgUserInterface.ListAddItem(lst,text);
  end;

  procedure ListAddItem(id: String; text: String); overload;
  begin
    sgUserInterface.ListAddItem(id,text);
  end;

  procedure ListAddItem(r: Region; img: Bitmap); overload;
  begin
    sgUserInterface.ListAddItem(r,img);
  end;

  procedure ListAddItem(r: Region; const img: BitmapCell); overload;
  begin
    sgUserInterface.ListAddItem(r,img);
  end;

  procedure ListAddItem(id: String; const img: BitmapCell); overload;
  begin
    sgUserInterface.ListAddItem(id,img);
  end;

  procedure ListAddItem(r: Region; text: String); overload;
  begin
    sgUserInterface.ListAddItem(r,text);
  end;

  procedure ListAddItem(pnl: Panel; id: String; img: Bitmap); overload;
  begin
    sgUserInterface.ListAddItem(pnl,id,img);
  end;

  procedure ListAddItem(r: Region; const img: BitmapCell; text: String); overload;
  begin
    sgUserInterface.ListAddItem(r,img,text);
  end;

  procedure ListAddItem(lst: GUIList; img: Bitmap; text: String); overload;
  begin
    sgUserInterface.ListAddItem(lst,img,text);
  end;

  procedure ListAddItem(pnl: Panel; id: String; const img: BitmapCell); overload;
  begin
    sgUserInterface.ListAddItem(pnl,id,img);
  end;

  procedure ListAddItem(id: String; img: Bitmap; text: String); overload;
  begin
    sgUserInterface.ListAddItem(id,img,text);
  end;

  procedure ListAddItem(r: Region; img: Bitmap; text: String); overload;
  begin
    sgUserInterface.ListAddItem(r,img,text);
  end;

  procedure ListAddItem(lst: GUIList; const img: BitmapCell; text: String); overload;
  begin
    sgUserInterface.ListAddItem(lst,img,text);
  end;

  procedure ListAddItem(pnl: Panel; id: String; text: String); overload;
  begin
    sgUserInterface.ListAddItem(pnl,id,text);
  end;

  procedure ListAddItem(id: String; const img: BitmapCell; text: String); overload;
  begin
    sgUserInterface.ListAddItem(id,img,text);
  end;

  procedure ListAddItem(pnl: Panel; id: String; const img: BitmapCell; text: String); overload;
  begin
    sgUserInterface.ListAddItem(pnl,id,img,text);
  end;

  procedure ListAddItem(pnl: Panel; id: String; img: Bitmap; text: String); overload;
  begin
    sgUserInterface.ListAddItem(pnl,id,img,text);
  end;

  function ListBitmapIndex(lst: GUIList; img: Bitmap): Longint; overload;
  begin
    result := sgUserInterface.ListBitmapIndex(lst,img);
  end;

  function ListBitmapIndex(lst: GUIList; const img: BitmapCell): Longint; overload;
  begin
    result := sgUserInterface.ListBitmapIndex(lst,img);
  end;

  procedure ListClearItems(id: String); overload;
  begin
    sgUserInterface.ListClearItems(id);
  end;

  procedure ListClearItems(lst: GUIList); overload;
  begin
    sgUserInterface.ListClearItems(lst);
  end;

  procedure ListClearItems(r: Region); overload;
  begin
    sgUserInterface.ListClearItems(r);
  end;

  procedure ListClearItems(pnl: Panel; id: String); overload;
  begin
    sgUserInterface.ListClearItems(pnl,id);
  end;

  function ListFont(lst: GUIList): Font; overload;
  begin
    result := sgUserInterface.ListFont(lst);
  end;

  function ListFont(r: Region): Font; overload;
  begin
    result := sgUserInterface.ListFont(r);
  end;

  function ListFontAlignment(lst: GUIList): FontAlignment; overload;
  begin
    result := sgUserInterface.ListFontAlignment(lst);
  end;

  function ListFontAlignment(r: Region): FontAlignment; overload;
  begin
    result := sgUserInterface.ListFontAlignment(r);
  end;

  function ListFromRegion(r: Region): GUIList; overload;
  begin
    result := sgUserInterface.ListFromRegion(r);
  end;

  function ListItemCount(lst: GUIList): Longint; overload;
  begin
    result := sgUserInterface.ListItemCount(lst);
  end;

  function ListItemCount(r: Region): Longint; overload;
  begin
    result := sgUserInterface.ListItemCount(r);
  end;

  function ListItemCount(id: String): Longint; overload;
  begin
    result := sgUserInterface.ListItemCount(id);
  end;

  function ListItemCount(pnl: Panel; id: String): Longint; overload;
  begin
    result := sgUserInterface.ListItemCount(pnl,id);
  end;

  function ListItemText(id: String; idx: Longint): String; overload;
  begin
    result := sgUserInterface.ListItemText(id,idx);
  end;

  function ListItemText(lst: GUIList; idx: Longint): String; overload;
  begin
    result := sgUserInterface.ListItemText(lst,idx);
  end;

  function ListItemText(r: Region; idx: Longint): String; overload;
  begin
    result := sgUserInterface.ListItemText(r,idx);
  end;

  function ListItemText(pnl: Panel; id: String; idx: Longint): String; overload;
  begin
    result := sgUserInterface.ListItemText(pnl,id,idx);
  end;

  function ListLargestStartIndex(lst: GUIList): Longint; overload;
  begin
    result := sgUserInterface.ListLargestStartIndex(lst);
  end;

  procedure ListRemoveActiveItem(r: Region); overload;
  begin
    sgUserInterface.ListRemoveActiveItem(r);
  end;

  procedure ListRemoveActiveItem(id: String); overload;
  begin
    sgUserInterface.ListRemoveActiveItem(id);
  end;

  procedure ListRemoveActiveItem(pnl: Panel; id: String); overload;
  begin
    sgUserInterface.ListRemoveActiveItem(pnl,id);
  end;

  procedure ListRemoveItem(id: String; idx: Longint); overload;
  begin
    sgUserInterface.ListRemoveItem(id,idx);
  end;

  procedure ListRemoveItem(lst: GUIList; idx: Longint); overload;
  begin
    sgUserInterface.ListRemoveItem(lst,idx);
  end;

  procedure ListRemoveItem(pnl: Panel; id: String; idx: Longint); overload;
  begin
    sgUserInterface.ListRemoveItem(pnl,id,idx);
  end;

  function ListScrollIncrement(lst: GUIList): Longint; overload;
  begin
    result := sgUserInterface.ListScrollIncrement(lst);
  end;

  procedure ListSetActiveItemIndex(id: String; idx: Longint); overload;
  begin
    sgUserInterface.ListSetActiveItemIndex(id,idx);
  end;

  procedure ListSetActiveItemIndex(lst: GUIList; idx: Longint); overload;
  begin
    sgUserInterface.ListSetActiveItemIndex(lst,idx);
  end;

  procedure ListSetActiveItemIndex(pnl: Panel; id: String; idx: Longint); overload;
  begin
    sgUserInterface.ListSetActiveItemIndex(pnl,id,idx);
  end;

  procedure ListSetFont(lst: GUIList; f: Font); overload;
  begin
    sgUserInterface.ListSetFont(lst,f);
  end;

  procedure ListSetFontAlignment(r: Region; align: FontAlignment); overload;
  begin
    sgUserInterface.ListSetFontAlignment(r,align);
  end;

  procedure ListSetFontAlignment(lst: GUIList; align: FontAlignment); overload;
  begin
    sgUserInterface.ListSetFontAlignment(lst,align);
  end;

  procedure ListSetStartAt(r: Region; idx: Longint); overload;
  begin
    sgUserInterface.ListSetStartAt(r,idx);
  end;

  procedure ListSetStartAt(lst: GUIList; idx: Longint); overload;
  begin
    sgUserInterface.ListSetStartAt(lst,idx);
  end;

  function ListStartAt(lst: GUIList): Longint; overload;
  begin
    result := sgUserInterface.ListStartAt(lst);
  end;

  function ListStartAt(r: Region): Longint; overload;
  begin
    result := sgUserInterface.ListStartAt(r);
  end;

  function ListTextIndex(lst: GUIList; value: String): Longint; overload;
  begin
    result := sgUserInterface.ListTextIndex(lst,value);
  end;

  function LoadPanel(filename: String): Panel; overload;
  begin
    result := sgUserInterface.LoadPanel(filename);
  end;

  function LoadPanelNamed(name: String; filename: String): Panel; overload;
  begin
    result := sgUserInterface.LoadPanelNamed(name,filename);
  end;

  procedure MovePanel(p: Panel; const mvmt: Vector); overload;
  begin
    sgUserInterface.MovePanel(p,mvmt);
  end;

  function NewPanel(pnlName: String): Panel; overload;
  begin
    result := sgUserInterface.NewPanel(pnlName);
  end;

  function PanelActive(pnl: Panel): Boolean; overload;
  begin
    result := sgUserInterface.PanelActive(pnl);
  end;

  function PanelAtPoint(const pt: Point2D): Panel; overload;
  begin
    result := sgUserInterface.PanelAtPoint(pt);
  end;

  function PanelClicked(): Panel; overload;
  begin
    result := sgUserInterface.PanelClicked();
  end;

  function PanelClicked(pnl: Panel): Boolean; overload;
  begin
    result := sgUserInterface.PanelClicked(pnl);
  end;

  function PanelDraggable(p: Panel): Boolean; overload;
  begin
    result := sgUserInterface.PanelDraggable(p);
  end;

  function PanelFilename(pnl: Panel): String; overload;
  begin
    result := sgUserInterface.PanelFilename(pnl);
  end;

  function PanelHeight(p: Panel): Longint; overload;
  begin
    result := sgUserInterface.PanelHeight(p);
  end;

  function PanelHeight(name: String): Longint; overload;
  begin
    result := sgUserInterface.PanelHeight(name);
  end;

  function PanelName(pnl: Panel): String; overload;
  begin
    result := sgUserInterface.PanelName(pnl);
  end;

  function PanelNamed(name: String): Panel; overload;
  begin
    result := sgUserInterface.PanelNamed(name);
  end;

  procedure PanelSetDraggable(p: Panel; b: Boolean); overload;
  begin
    sgUserInterface.PanelSetDraggable(p,b);
  end;

  function PanelVisible(p: Panel): Boolean; overload;
  begin
    result := sgUserInterface.PanelVisible(p);
  end;

  function PanelWidth(name: String): Longint; overload;
  begin
    result := sgUserInterface.PanelWidth(name);
  end;

  function PanelWidth(p: Panel): Longint; overload;
  begin
    result := sgUserInterface.PanelWidth(p);
  end;

  function PanelX(p: Panel): Single; overload;
  begin
    result := sgUserInterface.PanelX(p);
  end;

  function PanelY(p: Panel): Single; overload;
  begin
    result := sgUserInterface.PanelY(p);
  end;

  function PointInRegion(const pt: Point2D; p: Panel): Boolean; overload;
  begin
    result := sgUserInterface.PointInRegion(pt,p);
  end;

  function PointInRegion(const pt: Point2D; p: Panel; kind: GUIElementKind): Boolean; overload;
  begin
    result := sgUserInterface.PointInRegion(pt,p,kind);
  end;

  function RadioGroupFromId(id: String): GUIRadioGroup; overload;
  begin
    result := sgUserInterface.RadioGroupFromId(id);
  end;

  function RadioGroupFromId(pnl: Panel; id: String): GUIRadioGroup; overload;
  begin
    result := sgUserInterface.RadioGroupFromId(pnl,id);
  end;

  function RadioGroupFromRegion(r: Region): GUIRadioGroup; overload;
  begin
    result := sgUserInterface.RadioGroupFromRegion(r);
  end;

  function RegionActive(forRegion: Region): Boolean; overload;
  begin
    result := sgUserInterface.RegionActive(forRegion);
  end;

  function RegionAtPoint(p: Panel; const pt: Point2D): Region; overload;
  begin
    result := sgUserInterface.RegionAtPoint(p,pt);
  end;

  function RegionClicked(): Region; overload;
  begin
    result := sgUserInterface.RegionClicked();
  end;

  function RegionClickedID(): String; overload;
  begin
    result := sgUserInterface.RegionClickedID();
  end;

  function RegionHeight(r: Region): Longint; overload;
  begin
    result := sgUserInterface.RegionHeight(r);
  end;

  function RegionID(r: Region): String; overload;
  begin
    result := sgUserInterface.RegionID(r);
  end;

  function RegionOfLastUpdatedTextBox(): Region; overload;
  begin
    result := sgUserInterface.RegionOfLastUpdatedTextBox();
  end;

  function RegionPanel(r: Region): Panel; overload;
  begin
    result := sgUserInterface.RegionPanel(r);
  end;

  function RegionWidth(r: Region): Longint; overload;
  begin
    result := sgUserInterface.RegionWidth(r);
  end;

  function RegionWithID(ID: String): Region; overload;
  begin
    result := sgUserInterface.RegionWithID(ID);
  end;

  function RegionWithID(pnl: Panel; ID: String): Region; overload;
  begin
    result := sgUserInterface.RegionWithID(pnl,ID);
  end;

  function RegionX(r: Region): Single; overload;
  begin
    result := sgUserInterface.RegionX(r);
  end;

  function RegionY(r: Region): Single; overload;
  begin
    result := sgUserInterface.RegionY(r);
  end;

  procedure RegisterEventCallback(r: Region; callback: GUIEventCallback); overload;
  begin
    sgUserInterface.RegisterEventCallback(r,callback);
  end;

  procedure ReleaseAllPanels(); overload;
  begin
    sgUserInterface.ReleaseAllPanels();
  end;

  procedure ReleasePanel(name: String); overload;
  begin
    sgUserInterface.ReleasePanel(name);
  end;

  procedure SelectRadioButton(r: Region); overload;
  begin
    sgUserInterface.SelectRadioButton(r);
  end;

  procedure SelectRadioButton(id: String); overload;
  begin
    sgUserInterface.SelectRadioButton(id);
  end;

  procedure SelectRadioButton(rGroup: GUIRadioGroup; r: Region); overload;
  begin
    sgUserInterface.SelectRadioButton(rGroup,r);
  end;

  procedure SelectRadioButton(pnl: Panel; id: String); overload;
  begin
    sgUserInterface.SelectRadioButton(pnl,id);
  end;

  procedure SelectRadioButton(rGroup: GUIRadioGroup; idx: Longint); overload;
  begin
    sgUserInterface.SelectRadioButton(rGroup,idx);
  end;

  procedure SetRegionActive(forRegion: Region; b: Boolean); overload;
  begin
    sgUserInterface.SetRegionActive(forRegion,b);
  end;

  procedure ShowOpenDialog(); overload;
  begin
    sgUserInterface.ShowOpenDialog();
  end;

  procedure ShowOpenDialog(select: FileDialogSelectType); overload;
  begin
    sgUserInterface.ShowOpenDialog(select);
  end;

  procedure ShowPanel(p: Panel); overload;
  begin
    sgUserInterface.ShowPanel(p);
  end;

  procedure ShowPanel(name: String); overload;
  begin
    sgUserInterface.ShowPanel(name);
  end;

  procedure ShowPanelDialog(p: Panel); overload;
  begin
    sgUserInterface.ShowPanelDialog(p);
  end;

  procedure ShowSaveDialog(); overload;
  begin
    sgUserInterface.ShowSaveDialog();
  end;

  procedure ShowSaveDialog(select: FileDialogSelectType); overload;
  begin
    sgUserInterface.ShowSaveDialog(select);
  end;

  function TextBoxFont(tb: GUITextbox): Font; overload;
  begin
    result := sgUserInterface.TextBoxFont(tb);
  end;

  function TextBoxFont(r: Region): Font; overload;
  begin
    result := sgUserInterface.TextBoxFont(r);
  end;

  function TextBoxFromID(id: String): GUITextbox; overload;
  begin
    result := sgUserInterface.TextBoxFromID(id);
  end;

  function TextBoxFromRegion(r: Region): GUITextbox; overload;
  begin
    result := sgUserInterface.TextBoxFromRegion(r);
  end;

  function TextBoxText(tb: GUITextbox): String; overload;
  begin
    result := sgUserInterface.TextBoxText(tb);
  end;

  function TextBoxText(id: String): String; overload;
  begin
    result := sgUserInterface.TextBoxText(id);
  end;

  function TextBoxText(r: Region): String; overload;
  begin
    result := sgUserInterface.TextBoxText(r);
  end;

  function TextBoxText(pnl: Panel; id: String): String; overload;
  begin
    result := sgUserInterface.TextBoxText(pnl,id);
  end;

  function TextboxAlignment(r: Region): FontAlignment; overload;
  begin
    result := sgUserInterface.TextboxAlignment(r);
  end;

  function TextboxAlignment(tb: GUITextbox): FontAlignment; overload;
  begin
    result := sgUserInterface.TextboxAlignment(tb);
  end;

  procedure TextboxSetAlignment(r: Region; align: FontAlignment); overload;
  begin
    sgUserInterface.TextboxSetAlignment(r,align);
  end;

  procedure TextboxSetAlignment(tb: GUITextbox; align: FontAlignment); overload;
  begin
    sgUserInterface.TextboxSetAlignment(tb,align);
  end;

  procedure TextboxSetFont(Tb: GUITextbox; f: Font); overload;
  begin
    sgUserInterface.TextboxSetFont(Tb,f);
  end;

  procedure TextboxSetText(id: String; i: Longint); overload;
  begin
    sgUserInterface.TextboxSetText(id,i);
  end;

  procedure TextboxSetText(id: String; single: Single); overload;
  begin
    sgUserInterface.TextboxSetText(id,single);
  end;

  procedure TextboxSetText(r: Region; s: String); overload;
  begin
    sgUserInterface.TextboxSetText(r,s);
  end;

  procedure TextboxSetText(tb: GUITextbox; s: String); overload;
  begin
    sgUserInterface.TextboxSetText(tb,s);
  end;

  procedure TextboxSetText(id: String; s: String); overload;
  begin
    sgUserInterface.TextboxSetText(id,s);
  end;

  procedure TextboxSetText(r: Region; i: Longint); overload;
  begin
    sgUserInterface.TextboxSetText(r,i);
  end;

  procedure TextboxSetText(tb: GUITextbox; i: Longint); overload;
  begin
    sgUserInterface.TextboxSetText(tb,i);
  end;

  procedure TextboxSetText(tb: GUITextbox; single: Single); overload;
  begin
    sgUserInterface.TextboxSetText(tb,single);
  end;

  procedure TextboxSetText(r: Region; single: Single); overload;
  begin
    sgUserInterface.TextboxSetText(r,single);
  end;

  procedure TextboxSetText(pnl: Panel; id: String; s: String); overload;
  begin
    sgUserInterface.TextboxSetText(pnl,id,s);
  end;

  procedure TextboxSetText(pnl: Panel; id: String; i: Longint); overload;
  begin
    sgUserInterface.TextboxSetText(pnl,id,i);
  end;

  procedure TextboxSetText(pnl: Panel; id: String; single: Single); overload;
  begin
    sgUserInterface.TextboxSetText(pnl,id,single);
  end;

  procedure ToggleActivatePanel(p: Panel); overload;
  begin
    sgUserInterface.ToggleActivatePanel(p);
  end;

  procedure ToggleCheckboxState(id: String); overload;
  begin
    sgUserInterface.ToggleCheckboxState(id);
  end;

  procedure ToggleCheckboxState(c: GUICheckbox); overload;
  begin
    sgUserInterface.ToggleCheckboxState(c);
  end;

  procedure ToggleCheckboxState(pnl: Panel; id: String); overload;
  begin
    sgUserInterface.ToggleCheckboxState(pnl,id);
  end;

  procedure ToggleRegionActive(forRegion: Region); overload;
  begin
    sgUserInterface.ToggleRegionActive(forRegion);
  end;

  procedure ToggleShowPanel(p: Panel); overload;
  begin
    sgUserInterface.ToggleShowPanel(p);
  end;

  procedure UpdateInterface(); overload;
  begin
    sgUserInterface.UpdateInterface();
  end;

  function ArduinoDeviceNamed(name: String): ArduinoDevice; overload;
  begin
    result := sgArduino.ArduinoDeviceNamed(name);
  end;

  function ArduinoHasData(dev: ArduinoDevice): Boolean; overload;
  begin
    result := sgArduino.ArduinoHasData(dev);
  end;

  function ArduinoReadByte(dev: ArduinoDevice): Byte; overload;
  begin
    result := sgArduino.ArduinoReadByte(dev);
  end;

  function ArduinoReadByte(dev: ArduinoDevice; timeout: Longint): Byte; overload;
  begin
    result := sgArduino.ArduinoReadByte(dev,timeout);
  end;

  function ArduinoReadLine(dev: ArduinoDevice): String; overload;
  begin
    result := sgArduino.ArduinoReadLine(dev);
  end;

  function ArduinoReadLine(dev: ArduinoDevice; timeout: Longint): String; overload;
  begin
    result := sgArduino.ArduinoReadLine(dev,timeout);
  end;

  procedure ArduinoSendByte(dev: ArduinoDevice; value: Byte); overload;
  begin
    sgArduino.ArduinoSendByte(dev,value);
  end;

  procedure ArduinoSendString(dev: ArduinoDevice; value: String); overload;
  begin
    sgArduino.ArduinoSendString(dev,value);
  end;

  procedure ArduinoSendStringLine(dev: ArduinoDevice; value: String); overload;
  begin
    sgArduino.ArduinoSendStringLine(dev,value);
  end;

  function CreateArduinoDevice(port: String; baud: Longint): ArduinoDevice; overload;
  begin
    result := sgArduino.CreateArduinoDevice(port,baud);
  end;

  function CreateArduinoDevice(name: String; port: String; baud: Longint): ArduinoDevice; overload;
  begin
    result := sgArduino.CreateArduinoDevice(name,port,baud);
  end;

  procedure FreeArduinoDevice(var dev: ArduinoDevice); overload;
  begin
    sgArduino.FreeArduinoDevice(dev);
  end;

  function HasArduinoDevice(name: String): Boolean; overload;
  begin
    result := sgArduino.HasArduinoDevice(name);
  end;

  procedure ReleaseAllArduinoDevices(); overload;
  begin
    sgArduino.ReleaseAllArduinoDevices();
  end;

  procedure ReleaseArduinoDevice(name: String); overload;
  begin
    sgArduino.ReleaseArduinoDevice(name);
  end;

end.
