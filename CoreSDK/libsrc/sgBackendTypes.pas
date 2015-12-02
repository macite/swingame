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
    BITMAP_PTR = 'BMP*';
    SPRITE_PTR = 'SPRT';
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

    BitmapPtr = ^BitmapData;

    /// Bitmap data stores the data associated with a Bitmap. Each bitmap contains
    /// a pointer to the bitmap color information (surface), its width, height,
    /// and a mask storing the non-transparent pixels that is used for pixel level
    /// collision checking.
    ///
    /// @note Do not use BitmapData directly, use Bitmap.
    /// @struct BitmapData
    /// @via_pointer
    BitmapData = packed record
      id                 : PointerIdentifier;
      filename, name     : String;      // Used for locating bitmaps during load/freeing
      surface            : Pointer;     // The actual bitmap image
      
      width              : Longint;     //  The width of the bitmap
      height             : Longint;     //  The height of the bitmap
      TextureWidthRatio  : Single;      //  bmp width / texture width
      TextureHeightRatio : Single;      //  bmp height /texture height
      
      //Used for bitmaps that are made up of cells
      cellW                : Longint;    // The width of a cell
      cellH                : Longint;    // The height of a cell
      cellCols             : Longint;    // The columns of cells in the bitmap
      cellRows             : Longint;    // The rows of cells in the bitmap
      cellCount            : Longint;    // The total number of cells in the bitmap
      
      nonTransparentPixels : Array of Array of Boolean;  // Pixel mask used for pixel level collisions
      clipStack            : Array of Rectangle;         // The clipping rectangle history for the bitmap
    end;

    SpritePtr = ^SpriteData;

    SpriteData = packed record
      id:               PointerIdentifier;
      name:             String;               // The name of the sprite for resource management
      
      layerIds:         NamedIndexCollection; // The name <-> ids mapping for layers
      layers:           BitmapArray;          // Layers of the sprites
      visibleLayers:    LongintArray;         // The indexes of the visible layers
      layerOffsets:     Point2DArray;         // Offsets from drawing the layers
      
      values:           SingleArray;          // Values associated with this sprite
      valueIds:         NamedIndexCollection; // The name <-> ids mappings for values
      
      animationInfo:    Animation;            // The data used to animate this sprite
      animationScript:  AnimationScript;      // The template for this sprite's animations
      
      position:         Point2D;              // The game location of the sprite
      velocity:         Vector;               // The velocity of the sprite
      
      collisionKind:    CollisionTestKind;    //The kind of collisions used by this sprite
      collisionBitmap:  Bitmap;               // The bitmap used for collision testing (default to first image)
        
      backupCollisionBitmap:  Bitmap;         // Cache for rotated sprites
      cacheImage:             Bitmap;         // ...
      
      isMoving:     Boolean;                  // Used for events to indicate the sprite is moving
      destination:  Point2D;                  // The destination the sprite is moving to
      movingVec:    Vector;                   // The sprite's movement vector
      arriveInSec:  Single;                   // Amount of time in seconds to arrive at point
      lastUpdate:   Longint;                  // Time of last update

      announcedAnimationEnd: Boolean;         // Used to avoid multiple announcements of an end of an animation

      evts: SpriteEventHandlerArray;          // The call backs listening for sprite events

      pack: Pointer;                          // Points the the SpritePack that contains this sprite        

      //add later -> 
      //collisionShape: Shape;                // This can be used in place of pixel level collisions for a Shape
    end;


  function ToSoundEffectPtr(s: SoundEffect): SoundEffectPtr;
  function ToMusicPtr(m: Music): MusicPtr;
  function ToAnimationScriptPtr(a: AnimationScript): AnimationScriptPtr;
  function ToAnimationPtr(a: Animation): AnimationPtr;
  function ToBitmapPtr(b: Bitmap): BitmapPtr;
  function ToSpritePtr(s: Sprite): SpritePtr;

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

  function ToBitmapPtr(b: Bitmap): BitmapPtr;
  begin
    result := BitmapPtr(b);
    if Assigned(result) and (result^.id <> BITMAP_PTR) then
    begin
      RaiseWarning('Attempted to access a Bitmap that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToSpritePtr(s: Sprite): SpritePtr;
  begin
    result := SpritePtr(s);
    if Assigned(result) and (result^.id <> SPRITE_PTR) then
    begin
      RaiseWarning('Attempted to access a Sprite that appears to be an invalid pointer');
      result := nil;
    end;
  end;
end.
