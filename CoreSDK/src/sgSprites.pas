//=============================================================================
// sgSprites.pas
//=============================================================================
//
// Create and manage sprites in SwinGame.
//
//=============================================================================



/// SwinGame Sprites are game elements that can be moved, and animated. Sprites are
/// located at a position in the game, have a velocity, and an animation. The 
/// Sprite can also have arbitary data associated with it for game specific purposes.
///
/// @module Sprites
unit sgSprites;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  
//---------------------------------------------------------------------------
// Sprite creation routines
//---------------------------------------------------------------------------
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animations, and have one layer named 'layer1'.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateBasicSprite
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s
  function CreateSprite(layer: Bitmap): Sprite; overload;

  /// Creates a sprite. The bitmapName is used to indicate the bitmap the sprite will use, and the 
  /// animationName is used to indicate which AnimationScript to use.
  ///
  /// @lib CreateSpriteWithBitmapAndAnimationName
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmapNamed:%s animationScriptNamed:%s
  function CreateSprite(const bitmapName, animationName: String): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, the specified animation template, the layer have name 'layer1'.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateSpriteWithAnimation
  /// @sn createSpriteWithLayer:%s animationScript:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s animationScript:%s
  function CreateSprite(layer: Bitmap; ani: AnimationScript): Sprite; overload;

  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, the layer have name 'layer1'.
  /// 
  /// @lib CreateBasicSpriteNamed
  /// @sn createSpriteNamed:%s layer:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initNamed:%s withBitmap:%s
  function CreateSprite(const name: String; layer: Bitmap): Sprite; overload;

  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, the specified animation template, the layer have name 'layer1'.
  /// 
  /// @lib CreateSpriteWithAnimationNamed
  /// @sn createSpriteNamed:%s layer:%s animationScript:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initNamed:%s withBitmap:%s animationScript:%s
  function CreateSprite(const name: String; layer: Bitmap; ani: AnimationScript): Sprite; overload;
  
  /// Free the resources associated with a sprite.
  /// 
  /// @lib
  /// @class Sprite
  /// @dispose
  procedure FreeSprite(var s : Sprite);
  
  
  
//---------------------------------------------------------------------------
// Sprite Resource Management code
//---------------------------------------------------------------------------
  
  /// Determines if SwinGame has a sprite for the supplied name.
  /// This checks against all sprites, those loaded without a name
  /// are assigned a default.
  ///
  /// @lib
  function HasSprite(const name: String): Boolean;
  
  /// Returns the `Sprite` with the specified name,
  /// see `CreateBasicSprite`.
  ///
  /// @lib
  function SpriteNamed(const name: String): Sprite;
  
  /// Releases the SwinGame resources associated with the sprite of the
  /// specified ``name``.
  ///
  /// @lib
  procedure ReleaseSprite(const name: String);
  
  /// Releases all of the sprites that have been loaded.
  ///
  /// @lib
  procedure ReleaseAllSprites();
  
//---------------------------------------------------------------------------
// Event Code
//---------------------------------------------------------------------------

  /// Register a procedure to be called when an events occur on any sprite.
  ///
  /// @lib
  /// @sn callOnSpriteEvent:%s
  procedure CallOnSpriteEvent(handler: SpriteEventHandler);
  
  /// Removes an global event handler, stopping events calling the indicated procedure.
  ///
  /// @lib
  /// @sn stopCallingOnSpriteEvent:%s
  procedure StopCallingOnSpriteEvent(handler: SpriteEventHandler);

  /// Register a procedure to call when events occur on the sprite.
  ///
  /// @lib
  /// @sn sprite:%s callOnEvent:%s
  ///
  /// @class Sprite
  /// @method CallOnEvent
  /// @csn callOnEvent:%s
  procedure SpriteCallOnEvent(s: Sprite; handler: SpriteEventHandler);

  /// Removes an event handler from the sprite, stopping events from this 
  /// Sprite calling the indicated method.
  ///
  /// @lib
  /// @sn sprite:%s stopCallingOnEvent:%s
  ///
  /// @class Sprite
  /// @method StopCallingOnEvent
  /// @csn stopCallingOnEvent:%s
  procedure SpriteStopCallingOnEvent(s: Sprite; handler: SpriteEventHandler);

  
//---------------------------------------------------------------------------
// Layer code
//---------------------------------------------------------------------------
  
  /// Adds a new layer to the sprite.
  /// 
  /// @lib
  /// @sn sprite:%s addLayer:%s named:%s
  ///
  /// @class Sprite
  /// @method AddLayer
  /// @csn addLayer:%s named:%s
  function SpriteAddLayer(s: Sprite; newLayer: Bitmap; const layerName: String): Longint;
  
  /// Returns the bitmap of the indicated layer of the sprite.
  ///
  /// @lib SpriteLayerNamed
  /// @sn sprite:%s layerNamed:%s
  ///
  /// @class Sprite
  /// @method LayerNamed
  /// @csn layerNamed:%s
  function SpriteLayer(s: Sprite; const name: String): Bitmap; overload;
  
  /// Returns the bitmap of the indicated layer of the sprite.
  ///
  /// @lib SpriteLayerAtIdx
  /// @sn sprite:%s layerAtIdx:%s
  ///
  /// @class Sprite
  /// @method LayerAtIdx
  /// @csn layerAtIdx:%s
  function SpriteLayer(s: Sprite; idx: Longint): Bitmap; overload;
  
  /// Returns the index of the specified layer.
  ///
  /// @lib SpriteLayerIndex
  /// @sn sprite:%s indexOfLayer:%s
  ///
  /// @class Sprite
  /// @method IndexOfLayer
  /// @csn indexOfLayer:%s
  function SpriteLayerIndex(s: Sprite; const name: String): Longint;
  
  /// Returns the name of the specified layer.
  ///
  /// @lib SpriteLayerName
  /// @sn sprite:%s layerName:%s
  ///
  /// @class Sprite
  /// @method LayerName
  /// @csn layerName:%s
  function SpriteLayerName(s: Sprite; idx: Longint): String;
  
  /// Show the specified layer of the sprite.
  ///
  /// @lib SpriteShowLayerNamed
  /// @sn sprite:%s showLayerNamed:%s
  ///
  /// @class Sprite
  /// @overload ShowLayer ShowLayerNamed
  /// @csn showLayerNamed:%s
  function SpriteShowLayer(s: Sprite; const name: String): Longint; overload;
  
  /// Show the specified layer of the sprite.
  ///
  /// @lib SpriteShowLayer
  /// @sn sprite:%s showLayer:%s
  ///
  /// @class Sprite
  /// @method ShowLayer
  /// @csn showLayer:%s
  function SpriteShowLayer(s: Sprite; id: Longint): Longint; overload;
  
  /// Hide the specified layer of the sprite.
  ///
  /// @lib SpriteHideLayerNamed
  /// @sn sprite:%s hideLayerNamed:%s
  ///
  /// @class Sprite
  /// @overload HideLayer HideLayerNamed
  /// @csn hideLayerNamed:%s
  procedure SpriteHideLayer(s: Sprite; const name: String); overload;
  
  /// Hide the specified layer of the sprite.
  ///
  /// @lib SpriteHideLayer
  /// @sn sprite:%s hideLayer:%s
  ///
  /// @class Sprite
  /// @method HideLayer
  /// @csn hideLayer:%s
  procedure SpriteHideLayer(s: Sprite; id: Longint); overload;
  
  /// Toggle the visibility of the specified layer of the sprite.
  ///
  /// @lib SpriteToggleLayerNamedVisible
  /// @sn sprite:%s toggleVisibleLayerNamed:%s
  ///
  /// @class Sprite
  /// @overload ToggleLayerVisible ToggleLayerNamedVisible
  /// @csn toggleLayerNamedVisible:%s
  procedure SpriteToggleLayerVisible(s: Sprite; const name: String); overload;
  
  /// Toggle the visibility of the specified layer of the sprite.
  ///
  /// @lib SpriteToggleLayerVisible
  /// @sn sprite:%s toggleVisibleLayer:%s
  ///
  /// @class Sprite
  /// @method ToggleLayerVisible
  /// @csn toggleLayerVisible:%s
  procedure SpriteToggleLayerVisible(s: Sprite; id: Longint); overload;
  
  /// Returns the index (z-order) of the sprite's layer.
  ///
  /// @lib SpriteVisibleIndexOfLayerNamed
  /// @sn sprite:%s visibleIndexOfLayerNamed:%s
  ///
  /// @class Sprite
  /// @overload VisibleIndexOfLayer VisibleIndexOfLayerNamed
  /// @csn visibleIndexOfLayerNamed:%s
  function SpriteVisibleIndexOfLayer(s: Sprite; const name: String): Longint; overload;
  
  /// Returns the index (z-order) of the sprite's layer.
  ///
  /// @lib SpriteVisibleIndexOfLayer
  /// @sn sprite:%s visibleIndexOfLayer:%s
  ///
  /// @class Sprite
  /// @method VisibleIndexOfLayer
  /// @csn visibleIndexOfLayer:%s
  function SpriteVisibleIndexOfLayer(s: Sprite; id: Longint): Longint; overload;
  
  /// Returns the number of layers within the Sprite.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter LayerCount
  function SpriteLayerCount(s: Sprite): Longint;
  
  ///Returns the number of layers that are currently visible for the sprite.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter VisibleLayerCount
  function SpriteVisibleLayerCount(s: Sprite): Longint;
  
  /// Returns the id of the layer at index `idx` that is currently visible.
  /// Index 0 is the background, with larger indexes moving toward the foreground.
  /// This returns -1 if there are no visible layers.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @method VisibleLayerIdAt
  function SpriteVisibleLayerId(s: Sprite; idx: Longint) : Longint;
  
  /// Gets the offset of the specified layer.
  ///
  /// @lib SpriteLayerOffsetNamed
  /// @sn sprite:%s offsetOfLayerNamed:%s
  ///
  /// @class Sprite
  /// @overload LayerOffset LayerNamedOffset
  /// @csn offsetOfLayerNamed:%s 
  function SpriteLayerOffset(s: Sprite; const name: String): Point2D; overload;
  
  /// Gets the offset of the specified layer.
  ///
  /// @lib
  /// @sn sprite:%s offsetOfLayer:%s
  ///
  /// @class Sprite
  /// @method LayerOffset
  /// @csn offsetOfLayer:%s 
  function SpriteLayerOffset(s: Sprite; idx: Longint): Point2D; overload;
  
  /// Sets the offset of the specified layer.
  ///
  /// @lib SpriteSetLayerOffsetNamed
  /// @sn sprite:%s setOffsetOfLayerNamed:%s to:%s
  ///
  /// @class Sprite
  /// @overload SetLayerOffset SetLayerNamedOffset
  /// @csn layerNamed:%s setOffset:%s 
  procedure SpriteSetLayerOffset(s: Sprite; const name: String; const value: Point2D); overload;
  
  /// Sets the offset of the specified layer.
  ///
  /// @lib
  /// @sn sprite:%s setOffsetOfLayer:%s to:%s
  ///
  /// @class Sprite
  /// @overload SetLayerOffset SetLayerNamedOffset
  /// @csn layer:%s setOffset:%s 
  procedure SpriteSetLayerOffset(s: Sprite; idx: Longint; const value: Point2D); overload;
  
  /// Returns the index of the n'th (idx parameter) visible layer.
  /// 
  /// @lib
  /// @sn sprite:%s visibleLayer:%s
  ///
  /// @class Sprite
  /// @method VisibleLayer
  function SpriteVisibleLayer(s: Sprite; idx: Longint): Longint;
  
  /// Sends the layer specified to the back in the visible layer order.
  /// 
  /// @lib
  /// @sn sprite:%s sendLayerToBack:%s
  ///
  /// @class Sprite
  /// @method SendLayerToBack
  procedure SpriteSendLayerToBack(s: Sprite; visibleLayer: Longint);
  
  /// Sends the layer specified backward in the visible layer order.
  /// 
  /// @lib
  /// @sn sprite:%s sendLayerToBackward:%s
  ///
  /// @class Sprite
  /// @method SendLayerToBackward
  procedure SpriteSendLayerBackward(s: Sprite; visibleLayer: Longint);
  
  /// Sends the layer specified forward in the visible layer order.
  /// 
  /// @lib
  /// @sn sprite:%s sendLayerForward:%s
  ///
  /// @class Sprite
  /// @method SendLayerForward
  procedure SpriteBringLayerForward(s: Sprite; visibleLayer: Longint);
  
  /// Sends the layer specified to the front in the visible layer order.
  /// 
  /// @lib
  /// @sn sprite:%s sendLayerToFront:%s
  ///
  /// @class Sprite
  /// @method SendLayerToFront
  procedure SpriteBringLayerToFront(s: Sprite; visibleLayer: Longint);
  
  /// Gets a rectangle that surrounds the indicated layer.
  ///
  /// @lib SpriteLayerNamedRectangle
  /// @sn sprite:%s rectangleForLayerNamed:%s
  /// 
  /// @class Sprite
  /// @method RectangleForLayerNamed
  function SpriteLayerRectangle(s: Sprite; const name: String): Rectangle; overload;
  
  /// Gets a rectangle that surrounds the indicated layer.
  ///
  /// @lib
  /// @sn sprite:%s rectangleForLayer:%s
  /// 
  /// @class Sprite
  /// @method RectangleForLayer
  function SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle; overload;
  
  /// Returns the collision rectangle for the specified sprite.
  ///
  /// @lib
  /// 
  /// @class Sprite
  /// @getter CollisionRectangle
  function SpriteCollisionRectangle(s: Sprite): Rectangle;
  
  /// Gets a circle in the bounds of the indicated layer.
  ///
  /// @lib SpriteLayerNamedCircle
  /// @sn sprite:%s circleForLayerNamed:%s
  /// 
  /// @class Sprite
  /// @method CircleForLayerNamed
  function SpriteLayerCircle(s: Sprite; const name: String): Circle; overload;
  
  /// Gets a circle in the bounds of the indicated layer.
  ///
  /// @lib
  /// @sn sprite:%s circleForLayer:%s
  /// 
  /// @class Sprite
  /// @method CircleForLayer
  function SpriteLayerCircle(s: Sprite; idx: Longint): Circle; overload;
  
  /// Gets a circle in the bounds of the base layer of the indicated sprite.
  ///
  /// @lib
  /// @sn spriteCircle:%s
  /// 
  /// @class Sprite
  /// @method Circle
  function SpriteCircle(s: Sprite): Circle; overload;
  
  /// Gets a circle in the bounds of the indicated sprite's collision rectangle.
  ///
  /// @lib
  /// 
  /// @class Sprite
  /// @method CollisionCircle
  function SpriteCollisionCircle(s: Sprite): Circle;
  
  /// Returns a matrix that can be used to transform points into the coordinate space
  /// of the passed in sprite.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter LocationMatrix
  /// @csn locationMatrix
  function SpriteLocationMatrix(s: Sprite): Matrix2D;
  
//---------------------------------------------------------------------------
// Sprite Animation code
//---------------------------------------------------------------------------
  
  /// Restart the sprite's current animation, this will play a sound if the
  /// first cell of the animation is associated with a sound effect.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @method ReplayAnimation
  procedure SpriteReplayAnimation(s : Sprite);
  
  /// Restart the sprite's current animation, this will play a sound if
  /// withSound is true and the first cell of the animation is associated with a sound effect.
  /// 
  /// @lib ReplayAnimationWithSound
  /// @sn sprite:%s replayAnimationWithSound:%s
  /// 
  /// @class Sprite
  /// @overload ReplayAnimation ReplayAnimationWithSound
  /// @csn replayAnimationWithSound:%s
  procedure SpriteReplayAnimation(s: Sprite; withSound: Boolean);
  
  /// Start playing an animation from the sprite's animation template.
  /// This will play a sound effect if the first cell of the animation
  /// has a sound.
  /// 
  /// @lib SpriteStartAnimationNamed
  /// @sn sprite:%s startAnimationNamed:%s
  /// 
  /// @class Sprite
  /// @overload StartAnimation StartAnimationNamed
  /// @csn startAnimationNamed:%s
  procedure SpriteStartAnimation(s: Sprite; const named: String); overload;
  
  /// Start playing an animation from the sprite's animation template.
  /// The withSound parameter determines whether to play a sound effect 
  /// if the first cell of the animation has a sound.
  /// 
  /// @lib SpriteStartAnimationNamedWithSound
  /// @sn sprite:%s startAnimationNamed:%s withSound:%s
  /// 
  /// @class Sprite
  /// @overload StartAnimation StartAnimationNamedWithSound
  /// @csn startAnimationNamed:%s withSound:%s
  procedure SpriteStartAnimation(s: Sprite; const named: String; withSound: Boolean); overload;
  
  /// Start playing an animation from the sprite's animation template.
  /// This will play a sound effect if the first cell of the animation
  /// has a sound.
  /// 
  /// @lib SpriteStartAnimation
  /// @sn sprite:%s startAnimation:%s
  /// 
  /// @class Sprite
  /// @method StartAnimation
  /// @csn startAnimation:%s
  procedure SpriteStartAnimation(s: Sprite; idx: Longint); overload;
  
  /// Start playing an animation from the sprite's animation template.
  /// The withSound parameter determines whether to play a sound effect 
  /// if the first cell of the animation has a sound.
  /// 
  /// @lib SpriteStartAnimationWithSound
  /// @sn sprite:%s startAnimation:%s withSound:%s
  /// 
  /// @class Sprite
  /// @overload StartAnimation StartAnimationWithSound
  /// @csn startAnimation:%s withSound:%s
  procedure SpriteStartAnimation(s: Sprite; idx: Longint; withSound: Boolean); overload;
  
  /// Returns the name of the Sprite's current animation.
  /// 
  /// @lib SpriteAnimationName
  /// @sn spriteAnimationName:%s
  /// 
  /// @class Sprite
  /// @method animationName
  function SpriteAnimationName(s: Sprite): String;



//---------------------------------------------------------------------------
// Sprite Update code
//---------------------------------------------------------------------------
    
  /// Update the position and animation details of the Sprite.
  /// This will play a sound effect if the new cell of the animation
  /// has a sound.
  ///
  /// @lib UpdateSpritePctWithSound(s, 1.0, true)
  /// @uname UpdateSprite
  /// @sn updateSprite:%s
  ///
  /// @class Sprite
  /// @method Update
  procedure UpdateSprite(s: Sprite); overload;
  
  /// Update the position and animation details of the Sprite.
  /// This will play a sound effect if the new cell of the animation
  /// has a sound and withSound is true.
  ///
  /// @lib UpdateSpritePctWithSound(s, 1.0, withSound)
  /// @uname UpdateSpriteWithSound
  /// @sn updateSprite:%s withSound:%s
  /// 
  /// @class Sprite
  /// @overload Update UpdateWithSound
  /// @csn updateWithSound:%s
  procedure UpdateSprite(s: Sprite; withSound:Boolean); overload;
  
  /// Update the position and animation details of the Sprite by a 
  /// given percentage of a single unit of movement/animation.
  /// This will play a sound effect if the new cell of the animation
  /// has a sound.
  ///  
  /// @lib UpdateSpritePctWithSound(s, pct, true)
  /// @uname UpdateSpritePercent
  /// @sn updateSprite:%s percent:%s
  ///
  /// @class Sprite
  /// @overload Update UpdatePercent
  /// @csn updatePercent:%s
  procedure UpdateSprite(s: Sprite; pct: Single); overload;
  
  /// Update the position and animation details of the Sprite by a 
  /// given percentage of a single unit of movement/animation.
  /// This will play a sound effect if the new cell of the animation
  /// has a sound and withSound is true.
  ///  
  /// @lib UpdateSpritePctWithSound
  /// @sn updateSprite:%s percent:%s withSound:%s
  /// 
  /// @class Sprite
  /// @overload Update UpdatePercentWithSound
  /// @csn updatePercent:%s withSound:%s
  procedure UpdateSprite(s: Sprite; pct: Single; withSound: Boolean); overload;
  
  /// Updates the animation details of the sprite. 
  /// This will play a sound effect if the new cell of the animation
  /// has a sound.
  ///
  /// @lib UpdateSpriteAnimationPctWithSound(s, 1.0, true)
  /// @uname UpdateSpriteAnimation
  ///
  /// @class Sprite
  /// @method UpdateAnimation
  procedure UpdateSpriteAnimation(s: Sprite); overload;
  
  /// Update the animation details of the Sprite.
  /// This will play a sound effect if the new cell of the animation
  /// has a sound and withSound is true.
  ///  
  /// @lib UpdateSpriteAnimationPctWithSound(s, 1.0, withSound)
  /// @uname UpdateSpriteAnimationWithSound
  /// @sn sprite:%s updateAnimationWithSound:%s
  ///
  /// @class Sprite
  /// @overload UpdateAnimation UpdateAnimationWithSound
  /// @csn updateAnimationWithSound:%s
  procedure UpdateSpriteAnimation(s: Sprite; withSound: Boolean); overload;
  
  /// Update the animation details of the Sprite by a 
  /// given percentage of a single unit of movement/animation.
  /// This will play a sound effect if the new cell of the animation
  /// has a sound.
  ///  
  /// @lib UpdateSpriteAnimationPctWithSound(s, pct, true)
  /// @uname UpdateSpriteAnimationPercent
  /// @sn sprite:%s updateAnimationPct:%s
  ///
  /// @class Sprite
  /// @overload UpdateAnimation UpdateAnimationPct
  /// @csn updateAnimationPct:%s
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single); overload;
  
  /// Update the position and animation details of the Sprite by a 
  /// given percentage of a single unit of movement/animation.
  /// This will play a sound effect if the new cell of the animation
  /// has a sound and withSound is true.
  ///
  /// @lib UpdateSpriteAnimationPctWithSound
  /// @sn sprite:%s updateAnimationPct:%s withSound:%s
  ///
  /// @class Sprite
  /// @overload UpdateAnimation UpdateAnimationPctWithSound
  /// @csn updateAnimationPct:%s withSound:%s
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single; withSound: Boolean); overload;
  
  /// Indicates if the sprites animation has ended.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter AnimationHasEnded
  function SpriteAnimationHasEnded(s: Sprite): Boolean;
  
  
  
//---------------------------------------------------------------------------
// Positioning code
//---------------------------------------------------------------------------
  
  /// Returns a `Vector` that is the difference in the position of two sprites
  /// (``s1`` and ``s2``).
  /// 
  /// @lib
  /// @sn vectorFromSprite:%s toSprite:%s
  /// 
  /// @class Sprite
  /// @method VectorTo
  /// @csn vectorToSprite:%s
  function VectorFromTo(s1, s2: Sprite): Vector;
  
  /// Returns a `Vector` that is the difference in location from the center of
  /// the sprite ``s`` to the point ``pt``.
  ///
  /// @lib
  /// @sn vectorFromCenterOfSprite:%s toPoint:%s
  ///
  /// @class Sprite
  /// @overload VectorTo VectorToPoint
  /// @csn vectorToPoint:%s
  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector;
  
  
  
//---------------------------------------------------------------------------
// Drawing code
//---------------------------------------------------------------------------
  
  /// Draws the sprite at its location in the world. This is effected by the
  /// position of the camera and the sprites current location.
  ///
  /// This is the standard routine for drawing sprites to the screen and should be
  /// used in most cases.
  ///
  /// @lib DrawSpriteOffsetXY(s, 0, 0)
  /// @uname DrawSprite
  /// 
  /// @class Sprite
  /// @method Draw
  procedure DrawSprite(s : Sprite); overload;

  /// Draws the sprite at its position in the game offset by a given amount. Only
  /// use this method when you want to draw the sprite displaced from its location
  /// in your game. Otherwise you should change the sprite's location and then
  /// use the standard ''DrawSprite'' routine.
  ///
  /// @lib DrawSpriteOffsetXY
  /// @sn sprite:%s drawOffsetX:%s y:%s
  ///
  /// @class Sprite
  /// @overload Draw DrawOffsetXY
  /// @csn drawOffsetX:%s y:%s
  procedure DrawSprite(s : Sprite; xOffset, yOffset: Longint); overload;
  
  /// Draws the sprite at its position in the game offset by a given amount. Only
  /// use this method when you want to draw the sprite displaced from its location
  /// in your game. Otherwise you should change the sprite's location and then
  /// use the standard ''DrawSprite'' routine.
  ///
  /// @lib DrawSpriteOffsetPoint
  /// @sn sprite:%s drawOffset:%s
  ///
  /// @class Sprite
  /// @overload Draw DrawOffsetPoint
  /// @csn drawOffset:%s
  procedure DrawSprite(s : Sprite; const position: Point2D); overload;  
  
  
//---------------------------------------------------------------------------
// Movement code
//---------------------------------------------------------------------------
  
  /// Moves the sprite as indicated by its velocity. You can call this directly ot 
  /// alternatively, this action is performed when the sprite is updated using
  /// the ''UpdateSprite'' routine.
  ///
  /// @lib MoveSpritePct(s, 1.0)
  /// @uname MoveSprite
  /// 
  /// @class Sprite
  /// @method Move
  procedure MoveSprite(s: Sprite); overload;
  
  /// Moves the sprite as indicated by a percentage of its velocity. You can call 
  /// this directly ot alternatively, this action is performed when the sprite is
  /// updated using the ''UpdateSprite'' routines that require a percentage.
  ///
  /// @lib MoveSpritePct
  /// @sn sprite:%s movePct:%s
  /// 
  /// @class Sprite
  /// @overload Move MovePct
  /// @csn movePct:%s
  procedure MoveSprite(s: Sprite; pct: Single); overload;
  
  /// Moves the sprite a given distance based on the value passed in rather than
  /// based on the sprite's velocity. Typically this method is used to apply
  /// other movement actions to the sprite and the velocity of the sprite is
  /// used the intended movement of the sprite.
  /// 
  /// @lib MoveSpriteVecPct(s, distance, 1.0)
  /// @uname MoveSpriteVec
  /// @sn sprite:%s move:%s
  ///
  /// @class Sprite
  /// @overload Move MoveVec
  /// @csn move:%s
  procedure MoveSprite(s : Sprite; const distance: Vector); overload;
  
  
  /// Moves the sprite a percentage of a given distance based on the value 
  /// passed in rather than based on the sprite's velocity. Typically this 
  /// method is used to apply other movement actions to the sprite and the
  /// velocity of the sprite is used the intended movement of the sprite.
  /// 
  /// @lib MoveSpriteVecPct
  /// @sn sprite:%s move:%s pct:%s
  ///
  /// @class Sprite
  /// @overload Move MoveVecPct
  /// @csn move:%s pct:%s
  procedure MoveSprite(s : Sprite; const distance: Vector; pct: Single); overload;
  
  /// This method moves a sprite to a given position in the game.
  /// 
  /// @lib
  /// @sn sprite:%s moveToX:%s y:%s
  ///
  /// @class Sprite
  /// @method MoveTo
  /// @csn moveToX:%s y:%s
  procedure MoveSpriteTo(s : Sprite; x,y : Longint);


  /// This procedure starts the sprite moving to the indicated
  /// destination point, over a specified number of seconds. When the 
  /// sprite arrives it will raise the SpriteArrived event.
  ///
  /// @lib
  /// @sn sprite:%s moveTo:%s takingSeconds:%s
  ///
  /// @class Sprite
  /// @overload MoveTo MoveToTakingSeconds
  /// @csn moveTo:%s takingSeconds:%s
  procedure SpriteMoveTo(s: Sprite; const pt: Point2D; takingSeconds: Longint);
  
  
//---------------------------------------------------------------------------
// Sprite Screen Position Tests 
//---------------------------------------------------------------------------
  
  /// Returns True if a pixel of the `Sprite` ``s`` is at the screen location
  /// specified (``x`` and ``y``) which is converted to a world location.
  ///
  /// @lib
  /// @sn sprite:%s onScreenAtX:%s y:%s
  /// 
  /// @class Sprite
  /// @method OnScreenAt
  /// @csn onScreenAtX:%s y:%s
  function SpriteOnScreenAt(s: Sprite; x, y: Longint): Boolean; overload;
  
  /// Returns True if a pixel of the `Sprite` ``s`` is at the screen location
  /// specified (``pt``), which is converted to a world location.
  ///
  /// @lib SpriteOnScreenAtPoint
  /// @sn sprite:%s onScreenAt:%s
  ///
  /// @class Sprite
  /// @overload OnScreenAt OnScreenAtPoint
  /// @csn onScreenAt:%s
  function SpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;
  
  /// Returns True if the sprite is entirely off the screen.
  /// 
  /// @lib
  ///
  /// @class Sprite
  /// @method Offscreen
  function SpriteOffscreen(s : Sprite): Boolean;
  
  
  
//---------------------------------------------------------------------------
// Sprite Width and Heigth - CenterPoint
//---------------------------------------------------------------------------
  
  /// The current Height of the sprite (aligned to the Y axis).
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter Height
  function SpriteHeight(s: Sprite): Longint;
  
  /// The height of a given layer of the Sprite (aligned to the Y axis).
  ///
  /// @lib SpriteLayerNamedHeight
  /// @sn sprite:%s heightOfLayerNamed:%s
  ///
  /// @class Sprite
  /// @overload LayerHeight LayerNamedHeight
  /// @csn heightOfLayerNamed:%s
  function SpriteLayerHeight(s: Sprite; const name: String): Longint; overload;
  
  /// The height of a given layer of the Sprite (aligned to the Y axis).
  ///
  /// @lib SpriteLayerHeight
  /// @sn sprite:%s heightOfLayer:%s
  ///
  /// @class Sprite
  /// @method LayerHeight
  /// @csn heightOfLayer:%s
  function SpriteLayerHeight(s: Sprite; idx: Longint): Longint; overload;
  
  /// The current Width of the sprite (aligned to the X axis).
  /// 
  /// @lib
  /// @class Sprite
  /// @getter Width
  function SpriteWidth(s: Sprite): Longint;
  
  /// The width of a given layer of the Sprite (aligned to the X axis).
  ///
  /// @lib SpriteLayerNamedWidth
  /// @sn sprite:%s widthOfLayerNamed:%s
  ///
  /// @class Sprite
  /// @overload LayerWidth LayerNamedWidth
  /// @csn widthOfLayerNamed:%s
  function SpriteLayerWidth(s: Sprite; const name: String): Longint; overload;
  
  /// The width of a given layer of the Sprite (aligned to the X axis).
  ///
  /// @lib SpriteLayerWidth
  /// @sn sprite:%s widthOfLayer:%s
  ///
  /// @class Sprite
  /// @method LayerWidth
  /// @csn widthOfLayer:%s
  function SpriteLayerWidth(s: Sprite; idx: Longint): Longint; overload;
  
  /// Returns the center point of the passed in Sprite. This is based on the Sprite's 
  /// Position, Width and Height.
  ///
  /// @lib CenterPoint
  ///
  /// @class Sprite
  /// @getter CenterPoint
  function CenterPoint(s: Sprite): Point2D; overload;
  
  /// Returns the anchor point of the sprite. This is the point around which the sprite rotates.
  /// This is in sprite coordinates, so as if the Sprite is drawn at 0,0.
  /// 
  /// @lib
  ///
  /// @class Sprite
  /// @getter AnchorPoint
  function SpriteAnchorPoint(s: Sprite): Point2D;

  /// Allows you to set the anchor point for the sprite. This is the point around
  /// which the sprite rotates. This is in sprite coordinates, so as if the Sprite
  /// is drawn at 0,0.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @setter AnchorPoint
  procedure SpriteSetAnchorPoint(s: Sprite; pt: Point2D);


  /// Indicates if the sprite is moved from its anchor point, or from its top left.
  /// When this returns true the location of the Sprite will indicate its anchor point.
  /// When this returns false the location of the Sprite is its top left corner.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter MoveFromAnchorPoint
  function SpriteMoveFromAnchorPoint(s: Sprite): Boolean;

  /// Allows you to indicate if the sprite is moved from its anchor point, or from its
  /// top left.
  /// When set to true the location of the Sprite will be its anchor point.
  /// When set to false the location of the Sprite is its top left corner.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @setter MoveFromAnchorPoint
  procedure SpriteSetMoveFromAnchorPoint(s: Sprite; value: Boolean);

  
  
//---------------------------------------------------------------------------
// Sprite velocity
//---------------------------------------------------------------------------
  
  /// Returns the current velocity of the Sprite. When the Sprite is updated (see ``UpdateSprite``)
  /// this vector is used to move the Sprite.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter Velocity
  function SpriteVelocity(s: Sprite): Vector;
  
  /// Sets the current velocity of the Sprite. When the Sprite is updated (see ``UpdateSprite``)
  /// this vector is used to move the Sprite.
  /// 
  /// @lib
  /// @sn sprite:%s setVelocity:%s
  /// 
  /// @class Sprite
  /// @setter Velocity
  procedure SpriteSetVelocity(s: Sprite; const value: Vector);
  
  /// Alters the current velocity of the Sprite, adding the passed in vector to the current velocity.
  ///
  /// When the Sprite is updated (see ``UpdateSprite``)
  /// this vector is used to move the Sprite.
  /// 
  /// @lib
  /// @sn sprite:%s addToVelocity:%s
  /// 
  /// @class Sprite
  /// @method AddToVelocity
  procedure SpriteAddToVelocity(s: Sprite; const value: Vector);
  
  
//---------------------------------------------------------------------------
// Sprite CellCount
//---------------------------------------------------------------------------
  
  /// Returns a rectangle of the current cell within the Sprite's image. This is used
  /// to determine what part of the bitmap should be used when the Sprite is drawn.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter CurrentCellRectangle
  function SpriteCurrentCellRectangle(s: Sprite): Rectangle;
  
  /// Returns the rectangle representing the location of the Sprite on the
  /// screen.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter ScreenRectangle
  function SpriteScreenRectangle(s: Sprite): Rectangle;
  
  
  
//---------------------------------------------------------------------------
// Sprite X,Y
//---------------------------------------------------------------------------
  
  /// Sets the X position of the Sprite.
  /// 
  /// @lib
  /// @sn sprite:%s setX:%s
  ///
  /// @class Sprite
  /// @setter X
  procedure SpriteSetX(s: Sprite; value: Single);
  
  /// Returns the X position of the Sprite.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter X
  function SpriteX(s: Sprite): Single;
  
  /// Sets the Y position of the Sprite.
  /// 
  /// @lib
  /// @sn sprite:%s setY:%s
  /// 
  /// @class Sprite
  /// @setter Y
  procedure SpriteSetY(s: Sprite; value: Single);
  
  /// Returns the Y position of the Sprite.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter Y
  function SpriteY(s: Sprite): Single;
  
  
  
//---------------------------------------------------------------------------
// Sprite position
//---------------------------------------------------------------------------
  
  /// Returns the Sprite's position.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter Position
  function SpritePosition(s: Sprite): Point2D;
  
  /// Sets the Sprite's position.
  /// 
  /// @lib
  /// @sn sprite:%s setPosition:%s
  /// 
  /// @class Sprite
  /// @setter Position
  procedure SpriteSetPosition(s: Sprite; const value: Point2D);
  
  
  
//---------------------------------------------------------------------------
// Sprite DX,DY
//---------------------------------------------------------------------------
  
  /// Sets the X value of the Sprite's velocity.
  /// 
  /// @lib
  /// @sn sprite:%s setDX:%s
  /// 
  /// @class Sprite
  /// @setter DX
  procedure SpriteSetDX(s: Sprite; value: Single);
  
  /// Returns the X value of the Sprite's velocity.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter DX
  function SpriteDX(s: Sprite): Single;
  
  /// Sets the Y value of the Sprite's velocity.
  /// 
  /// @lib
  /// @sn sprite:%s setDY:%s
  /// 
  /// @class Sprite
  /// @setter DY
  procedure SpriteSetDY(s: Sprite; value: Single);
  
  /// Returns the Y value of the Sprite's velocity.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter DY
  function SpriteDY(s: Sprite): Single;
  
  
  
//---------------------------------------------------------------------------
// Sprite speed and heading
//---------------------------------------------------------------------------
  
  /// Returns the current speed (distance travelled per update) of the Sprite.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter Speed
  function SpriteSpeed(s: Sprite): Single;
  
  /// Alters the speed of the Sprite without effecting the direction.
  /// 
  /// @lib
  /// @sn sprite:%s setSpeed:%s
  /// 
  /// @class Sprite
  /// @setter Speed
  procedure SpriteSetSpeed(s: Sprite; value: Single);
  
  /// Returns the direction the Sprite is heading in degrees.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter Heading
  function SpriteHeading(s: Sprite): Single;
  
  /// Alters the direction the Sprite is heading without changing the speed.
  /// 
  /// @lib
  /// @sn sprite:%s setHeading:%s
  /// 
  /// @class Sprite
  /// @setter Heading
  procedure SpriteSetHeading(s: Sprite; value: Single);
  
//---------------------------------------------------------------------------
// Sprite Current Frame
//---------------------------------------------------------------------------
  
  /// Returns the current animation cell for an Animated Sprite. The cell is
  /// updated when the sprite's animation data is updated.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter CurrentCell
  function SpriteCurrentCell(s: Sprite): Longint;
  
  
  
//---------------------------------------------------------------------------
// Sprite collision details
//---------------------------------------------------------------------------
  
  /// Returns the bitmap used by the Sprite to determine if it has collided with
  /// other objects in the game.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter CollisionBitmap
  function SpriteCollisionBitmap(s: Sprite): Bitmap;
  
  /// Sets the bitmap used by the Sprite to determine if it has collided with
  /// other objects in the game. By default the CollisionBitmap is set to the
  /// bitmap from the Sprite's first layer.
  /// 
  /// @lib
  /// @sn sprite:%s setCollisionBitmap:%s
  ///
  /// @class Sprite
  /// @setter CollisionBitmap
  procedure SpriteSetCollisionBitmap(s: Sprite; bmp: Bitmap);
  
  /// Returns the kind of collision used with this Sprite. This is used when
  /// determining if the Sprite has collided with other objects in the game.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter CollisionKind
  function SpriteCollisionKind(s: Sprite): CollisionTestKind;
  
  /// Sets the kind of collision used with this Sprite. This is used when
  /// determining if the Sprite has collided with other objects in the game.
  /// 
  /// @lib
  /// @sn sprite:%s setCollisionKind:%s
  /// 
  /// @class Sprite
  /// @setter CollisionKind
  procedure SpriteSetCollisionKind(s: Sprite; value: CollisionTestKind);
  
  
  
//---------------------------------------------------------------------------
// Sprite mass
//---------------------------------------------------------------------------
  
  /// This indicates the mass of the Sprite for any of the collide methods from
  /// Physics. The mass of two colliding sprites will determine the relative
  /// velocitys after the collision.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter Mass
  function SpriteMass(s: Sprite): Single;
  
  /// Allows you to change the mass of a Sprite.
  /// 
  /// @lib
  /// @sn sprite:%s sestMass:%s
  /// 
  /// @class Sprite
  /// @setter Mass
  procedure SpriteSetMass(s: Sprite; value: Single);
  
  
  
//---------------------------------------------------------------------------
// Sprite rotation
//---------------------------------------------------------------------------
  
  /// This indicates the angle of rotation of the Sprite. This will rotate any 
  /// images of the sprite before drawing, which can be very slow. Avoid using
  /// this method with bitmap based Sprites where possible.
  /// 
  /// @lib
  /// 
  /// @class Sprite
  /// @getter Rotation
  function SpriteRotation(s: Sprite): Single;

  /// Allows you to change the rotation of a Sprite.
  /// 
  /// @lib
  /// @sn sprite:%s setRotation:%s
  /// 
  /// @class Sprite
  /// @setter Rotation
  procedure SpriteSetRotation(s: Sprite; value: Single);
  
  
  
//---------------------------------------------------------------------------
// Sprite scale
//---------------------------------------------------------------------------
  
  /// This indicates the scale of the Sprite. This will scale any 
  /// images of the sprite before drawing, which can be very slow. Avoid using
  /// this method with bitmap based Sprites where possible.
  /// 
  /// @lib
  /// @class Sprite
  /// @getter Scale
  function SpriteScale(s: Sprite): Single;
  
  /// Allows you to change the scale of a Sprite.
  /// 
  /// @lib
  /// @sn sprite:%s setScale:%s
  /// 
  /// @class Sprite
  /// @setter Scale
  procedure SpriteSetScale(s: Sprite; value: Single);
  
  
  
//---------------------------------------------------------------------------
// Sprite value code
//---------------------------------------------------------------------------
  
  /// Returns the count of sprite's values.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter ValueCount 
  function SpriteValueCount(s: Sprite) : Longint;
  
  /// Returns the names of all of the values of the sprite
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @method ValueNameAt
  function SpriteValueName(s: Sprite; idx: Longint): String; 
  
  /// Returns the sprite's value at the index specified
  ///
  /// @lib
  /// @sn sprite:%s valueAt:%s
  ///
  /// @class Sprite
  /// @overload Value ValueAt
  function SpriteValue(s: Sprite; index: Longint): Single; overload;
  
  /// Returns the indicated value of the sprite
  ///
  /// @lib SpriteValueNamed
  /// @sn sprite:%s valueOf:%s
  ///
  /// @class Sprite
  /// @method Value
  function SpriteValue(s: Sprite; const name: String): Single; overload;
  
  /// Adds a new kind of value to the Sprite
  /// 
  /// @lib SpriteAddValue
  /// @sn sprite:%s addValue:%s
  /// 
  /// @class Sprite
  /// @method AddValue
  procedure SpriteAddValue(s: Sprite; const name: String);
  
  /// Adds a new kind of value to the Sprite, setting the initial value
  /// to the value passed in.
  /// 
  /// @lib SpriteAddValueWithInitialValue
  /// @sn sprite:%s addValue:%s initally:%s
  /// 
  /// @class Sprite
  /// @overload AddValue AddValueWithDefault
  /// @csn addValue:%s initally:%s
  procedure SpriteAddValue(s: Sprite; const name: String; initVal: Single);
  
  /// Assigns a value to the Sprite.
  ///
  /// @lib SpriteSetValueNamed
  /// @sn sprite:%s setValueNamed:%s to:%s
  ///
  /// @class Sprite
  /// @overload SetValue SetValueNamed
  /// @csn setValueNamed:%s to:%s
  procedure SpriteSetValue(s: Sprite; const name: String; val: Single); overload;
  
  /// Assigns a value to the Sprite.
  ///
  /// @lib SpriteSetValue
  /// @sn sprite:%s setValue:%s to:%s
  ///
  /// @class Sprite
  /// @method SetValue
  /// @csn setValue:%s to:%s
  procedure SpriteSetValue(s: Sprite; idx: Longint; val: Single); overload;
  
  
  
//---------------------------------------------------------------------------
// Sprite name
//---------------------------------------------------------------------------
  
  /// Returns the name of the sprite. This name is used for resource management
  /// and can be used to interact with the sprite in various routines.
  ///
  /// @lib SpriteName
  /// 
  /// @class Sprite
  /// @getter Name
  function SpriteName(sprt: Sprite): String;
  

//---------------------------------------------------------------------------
// Sprite Packs
//---------------------------------------------------------------------------

  /// Draws all of the sprites in the current Sprite pack. Packs can be
  /// switched to select between different sets of sprites.
  ///
  /// @lib 
  procedure DrawAllSprites();

  /// Update all of the sprites in the current Sprite pack. 
  ///
  /// @lib
  procedure UpdateAllSprites(); overload;

  /// Update all of the sprites in the current Sprite pack, passing in a
  /// percentage value to indicate the percentage to update.
  ///
  /// @lib UpdateAllSpritesPct
  procedure UpdateAllSprites(pct: Single); overload;

  /// Call the supplied function for all sprites.
  ///
  /// @lib
  procedure CallForAllSprites(fn: SpriteFunction);

  /// Create a new SpritePack with a given name. This pack can then be 
  /// selected and used to control which sprites are drawn/updated in
  /// the calls to DrawAllSprites and UpdateAllSprites.
  ///
  /// @lib
  procedure CreateSpritePack(const name: String);

  /// Indicates if a given SpritePack has already been created.
  ///
  /// @lib
  function HasSpritePack(const name: String): Boolean;

  /// Selects the named SpritePack (if it has been created). The
  /// selected SpritePack determines which sprites are drawn and updated
  /// with the DrawAllSprites and UpdateAllSprites code.
  ///
  /// @lib
  procedure SelectSpritePack(const name: String);

  /// Returns the name of the currently selected SpritePack.
  ///
  /// @lib
  function CurrentSpritePack(): String;

  
  
//=============================================================================
implementation
  uses
    Classes, SysUtils, Math, // System
    stringhash, sgBackendTypes,
    sgNamedIndexCollection, SpritePack, //libsrc
    sgAnimations, sgGraphics, sgGeometry, sgPhysics, sgInput, sgCamera, sgShared, sgResources, sgImages, sgTrace, sgTimers, sgDrawingOptions; //SwinGame
//=============================================================================

  var
    _GlobalSpriteEventHandlers: array of SpriteEventHandler;
    _Sprites: TStringHash;
    _SpritePacks: TStringHash;
    _spriteTimer: Timer;
    _CurrentPack: TSpritePack;
  
  const
    MASS_IDX      = 0;  // The index of the sprite's mass value
    ROTATION_IDX  = 1;  // The index of the sprite's rotation value
    SCALE_IDX     = 2;  // The index of the sprite's scale value
    // WIDTH_IDX     = 3;  // The index of the sprite's width value
    // HEIGHT_IDX    = 4;  // The index of the sprite's height value



  //-----------------------------------------------------------------------------
  // Event Utility Code
  //-----------------------------------------------------------------------------
  
  //
  // Loop through all event listeners and notify them of the event
  //
  procedure SpriteRaiseEvent(s: Sprite; evt: SpriteEventKind);
  var
    i: Integer;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    if not Assigned(sp) then exit;

    // this sprite's event handlers
    for i := 0 to High(sp^.evts) do
    begin
      SpriteEventHandler(sp^.evts[i])(s, evt);
    end;

    // global sprite event handlers
    for i := 0 to High(_GlobalSpriteEventHandlers) do
    begin
      SpriteEventHandler(_GlobalSpriteEventHandlers[i])(s, evt);
    end;
  end;
  

  //-----------------------------------------------------------------------------
  // ...
  //-----------------------------------------------------------------------------

  function VectorFromTo(s1, s2: Sprite): Vector;
  begin
    result := VectorFromPoints(CenterPoint(s1), CenterPoint(s2));
  end;

  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector;
  begin
    result := VectorFromPoints(CenterPoint(s), pt);
  end;
  
  function CreateSprite(layer: Bitmap): Sprite; overload;
  begin
    result := CreateSprite(layer, AnimationScript(nil));
  end;
    
  function CreateSprite(const bitmapName, animationName: String): Sprite; overload;
  begin
    result := CreateSprite(BitmapNamed(bitmapName), AnimationScriptNamed(animationName));
  end;
  
  function CreateSprite(layer: Bitmap; ani: AnimationScript): Sprite; overload;
  begin
    result := CreateSprite('Sprite', layer, ani);
  end;
  
  function CreateSprite(const name: String; layer: Bitmap): Sprite; overload;
  begin
    result := CreateSprite(name, layer, nil);
  end;
  
  function CreateSprite(const name: String; layer: Bitmap; ani: AnimationScript): Sprite; overload;
  var
    sn: String;
    idx: Longint;
    obj: tResourceContainer;
    sp: SpritePtr;
    layerNames: StringArray;
  begin
    result := nil; 

    // Find a unique name for this sprite
    idx := 0;
    sn := name;
    
    while HasSprite(sn) do
    begin
      sn := name + IntToStr(idx);
      idx += 1;
    end;

    SetLength(layerNames, 1);
    layerNames[0] := 'BaseLayer';
    
    //Allocate the space for the sprite
    New(sp);
    sp^.id := SPRITE_PTR;
    sp^.name := sn;
    
    //Set lengths of the layer arrays
    SetLength(sp^.layers, 1);
    SetLength(sp^.layerOffsets, 1);
    
    sp^.layers[0] := layer;
    sp^.layerOffsets[0] := PointAt(0,0);

    sp^.anchorPoint := PointAt(BitmapWidth(layer) / 2, BitmapHeight(layer) / 2);
    sp^.positionAtAnchorPoint := false;
        
    // Setup the layer name <-> id mapping
    InitNamedIndexCollection(sp^.layerIds, layerNames);
    
    // Set the first layer as visible.
    SetLength(sp^.visibleLayers, 1);
    sp^.visibleLayers[0] := 0;                //The first layer (at idx 0) is drawn
    
    // Setup the values
    SetLength(sp^.values, 3);
    InitNamedIndexCollection(sp^.valueIds);
    
    AddName(sp^.valueIds, 'mass');            //idx 0 = mass, default to 1
    sp^.values[MASS_IDX]      := 1;
    
    AddName(sp^.valueIds, 'rotation');        //idx 1 = rotation, default to 0
    sp^.values[ROTATION_IDX]  := 0;
    
    AddName(sp^.valueIds, 'scale');           //idx 2 = scale, default to 1
    sp^.values[SCALE_IDX]       := 1;
    
    // Position the sprite
    sp^.position                := PointAt(0,0);
    
    // Initialise sprite movement
    sp^.velocity                := VectorTo(0,0);
    
    // Setup animation detials
    sp^.animationScript         := ani;
    sp^.animationInfo           := nil;
    
    // Setup collision details
    sp^.collisionKind           := PixelCollisions;
    sp^.collisionBitmap         := sp^.layers[0];
    
    // Event details
    sp^.announcedAnimationEnd := false;
    sp^.isMoving := false;
    sp^.destination := PointAt(0,0);
    sp^.movingVec := VectorTo(0,0);
    sp^.arriveInSec := 0;
    sp^.lastUpdate := TimerTicks(_spriteTimer);
    SetLength(sp^.evts, 0);

    // Register in _Sprites
    obj := tResourceContainer.Create(sp);
    // WriteLn('Adding for ', name, ' ', HexStr(obj));
    if not _Sprites.setValue(sn, obj) then
    begin
        RaiseWarning('** Leaking: Caused by loading Sprite created twice, ' + sn);
        result := nil;
        exit;
    end;

    sp^.pack := _CurrentPack;
    _CurrentPack.AddSprite(sp);

    result := sp;
  end;
  
  function HasSprite(const name: String): Boolean;
  begin
    result := _Sprites.containsKey(name);
  end;

  function SpriteNamed(const name: String): Sprite;
  var
    tmp : TObject;
  begin
    tmp := _Sprites.values[name];
    if Assigned(tmp) then
      result := Sprite(tResourceContainer(tmp).Resource)
    else 
      result := nil;
  end;

  procedure ReleaseSprite(const name: String);
  var
    sprt: Sprite;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'ReleaseSprite', 'name = ' + name);
    {$ENDIF}

    sprt := SpriteNamed(name);
    if (Assigned(sprt)) then
    begin
      FreeSprite(sprt);
    end;
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'ReleaseSprite');
    {$ENDIF}
  end;

  procedure ReleaseAllSprites();
  begin
    ReleaseAll(_Sprites, @ReleaseSprite);
  end;
  
  procedure FreeSprite(var s : Sprite);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then
    begin
      // Free arrays
      SetLength(sp^.layers, 0);
      SetLength(sp^.visibleLayers, 0);
      SetLength(sp^.values, 0);
      SetLength(sp^.layerOffsets, 0);
      
      // Free the name <-> id maps
      FreeNamedIndexCollection(sp^.layerIds);
      FreeNamedIndexCollection(sp^.valueIds);
      
      // Free pointers
      FreeAnimation(sp^.animationInfo);
      
      // Nil pointers to resources managed by sgResources
      sp^.animationScript := nil;
      
      //Free buffered rotation image
      sp^.collisionBitmap := nil;
      
      TSpritePack(sp^.pack).RemoveSprite(s);
      
      // Remove from hashtable
      // WriteLn('Freeing Sprite named: ', s^.name);
      _Sprites.remove(sp^.name).Free();
      
      //Dispose sprite
      CallFreeNotifier(s);
      
      sp^.id := NONE_PTR;
      Dispose(sp);
    end;
    
    s := nil;
  end;
  
  function SpriteAddLayer(s: Sprite; newLayer: Bitmap; const layerName: String): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if newLayer = nil then begin RaiseWarning('Cannot add non-existing bitmap as layer to Sprite'); exit; end;
    if sp = nil then begin RaiseWarning('No sprite to add layer to'); exit; end;
    
    result := AddName(sp^.layerIds, layerName);
    if (result <> Length(sp^.layers)) then begin RaiseException('Error adding layer ' + layerName); exit; end;
    
    //Resize the array
    SetLength(sp^.layers, Length(sp^.layers) + 1);
    SetLength(sp^.layerOffsets, Length(sp^.layerOffsets) + 1);
    
    //Add the values to the array
    sp^.layers[result] := newLayer;
    sp^.layerOffsets[result] := PointAt(0,0);
  end;
  
  procedure SpriteReplayAnimation(s: Sprite);
  begin
    SpriteReplayAnimation(s, true);
  end;
  
  procedure SpriteReplayAnimation(s: Sprite; withSound: Boolean);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if sp = nil then exit;
    
    RestartAnimation(sp^.animationInfo, withSound);
    if not SpriteAnimationHasEnded(s) then sp^.announcedAnimationEnd := false;
  end;
  
  procedure SpriteStartAnimation(s: Sprite; const named: String);
  begin
    SpriteStartAnimation(s, named, true);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; const named: String; withSound: Boolean);
  var
  	idx: Integer;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

  	if not Assigned(sp) then exit;
  	if not Assigned(sp^.animationScript) then exit;
  	
  	idx := AnimationIndex(sp^.animationScript, named);
  	if (idx < 0) or (idx >= AnimationCount(sp^.animationScript)) then
    begin
  		RaiseWarning('Unable to create animation "' + named + '" for sprite ' + sp^.name + ' from script ' + AnimationScriptName(sp^.animationScript));
  		exit;
  	end;

    SpriteStartAnimation(s, idx, withSound);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; idx: Longint);
  begin
    SpriteStartAnimation(s, idx, true);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; idx: Longint; withSound: Boolean);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;
    if not Assigned(sp^.animationScript) then exit;
    if (idx < 0) or (idx >= AnimationCount(sp^.animationScript)) then
    begin
		  RaiseWarning('Unable to create animation no. ' + IntToStr(idx) + ' for sprite ' + sp^.name + ' from script ' + AnimationScriptName(sp^.animationScript));
		  exit;
    end;
    
    if Assigned(sp^.animationInfo) then
      AssignAnimation(sp^.animationInfo, idx, sp^.animationScript, withSound)
    else
      sp^.animationInfo := CreateAnimation(idx, sp^.animationScript, withSound);

    if not SpriteAnimationHasEnded(s) then sp^.announcedAnimationEnd := false;
    // WriteLn('Sprite Animation: ', HexStr(sp^.animationInfo));
  end;

  function SpriteAnimationName(s: Sprite): String;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then result := AnimationName(sp^.animationInfo)
    else result := '';
  end;
  
  procedure UpdateSpriteAnimation(s: Sprite); overload;
  begin
    UpdateSpriteAnimation(s, 1.0, true);
  end;
  
  procedure UpdateSpriteAnimation(s: Sprite; withSound: Boolean); overload;
  begin
    UpdateSpriteAnimation(s, 1.0, withSound);
  end;
  
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single); overload;
  begin
    UpdateSpriteAnimation(s, pct, true);
  end;
  
  procedure UpdateSpriteAnimation(s: Sprite; pct: Single; withSound: Boolean); overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;
    
    UpdateAnimation(sp^.animationInfo, pct, withSound);
    // WriteLn('Move ', PointToString(AnimationCurrentVector(s^.animationInfo)), ' ', pct);
    MoveSprite(s, AnimationCurrentVector(sp^.animationInfo), pct);
  end;

  procedure UpdateSprite(s: Sprite); overload;
  begin
    UpdateSprite(s, 1.0, true);
  end;
  
  procedure UpdateSprite(s: Sprite; withSound: Boolean); overload;
  begin
    UpdateSprite(s, 1.0, withSound);
  end;
  
  procedure UpdateSprite(s: Sprite; pct: Single); overload;
  begin
    UpdateSprite(s, pct, true);
  end;
  
  procedure UpdateSprite(s: Sprite; pct: Single; withSound: Boolean); overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then
    begin
      MoveSprite(s, pct);
      UpdateSpriteAnimation(s, pct, withSound);

      {$IFDEF IOS}
        if MouseClicked(LeftButton) and CircleCircleCollision(SpriteCollisionCircle(s), CircleAt(MouseX(), MouseY(), 17)) then
        begin
          SpriteRaiseEvent(s, SpriteTouchedEvent);
        end;
      {$ELSE}
        if MouseClicked(LeftButton) and CircleCircleCollision(SpriteCollisionCircle(s), CircleAt(MouseX(), MouseY(), 1)) then
        begin
          SpriteRaiseEvent(s, SpriteClickedEvent);  
        end;
      {$ENDIF}

      if SpriteAnimationHasEnded(s) and (not sp^.announcedAnimationEnd) then
      begin
        sp^.announcedAnimationEnd := true;
        SpriteRaiseEvent(s, SpriteAnimationEndedEvent);
      end;
    end;
  end;
  
  procedure DrawSprite(s: Sprite); overload;
  begin
    DrawSprite(s, 0, 0);
  end;

  procedure DrawSprite(s : Sprite; const position: Point2D); overload;
  begin
    DrawSprite(s, Round(position.x), Round(position.y));
  end;

  procedure DrawSprite(s: Sprite; xOffset, yOffset: Longint); overload;
  var
    i, idx: Longint;
    angle, scale: Single;
    sp: SpritePtr;
    opts: DrawingOptions;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;
    
    angle := SpriteRotation(s);
    if angle <> 0 then
      opts := OptionRotateBmp( angle, sp^.anchorPoint.x - SpriteLayerWidth(s, 0) / 2, sp^.anchorPoint.y  - SpriteLayerHeight(s, 0) / 2 )
    else
      opts := OptionDefaults();

    scale := SpriteScale(s);
    if scale <> 1 then
      opts := OptionScaleBmp( scale, scale, opts );

    for i := 0 to High(sp^.visibleLayers) do
    begin
      idx := sp^.visibleLayers[i];
      DrawCell(SpriteLayer(s, idx), SpriteCurrentCell(s), 
        Round(sp^.position.x + xOffset + sp^.layerOffsets[idx].x), 
        Round(sp^.position.y + yOffset + sp^.layerOffsets[idx].y),
        opts);
    end;
  end;

  function SpriteOffscreen(s : Sprite): Boolean;
  begin
    if s = nil then 
      result := false
    else
      result := not RectOnScreen(SpriteLayerRectangle(s, 0));
  end;

  procedure MoveSprite(s : Sprite; const distance : Vector); overload;
  begin
    MoveSprite(s, distance, 1.0);
  end;

  procedure MoveSprite(s : Sprite; const distance : Vector; pct: Single); overload;
  var
    mvmt: Vector;
    trans: Matrix2D;
    sp: SpritePtr;
    angle: Single;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then begin RaiseWarning('No sprite supplied to MoveSprite'); exit; end;
    
    angle := SpriteRotation(s);

    if angle <> 0 then
    begin
      trans := RotationMatrix(angle);
      mvmt := MatrixMultiply(trans, distance);
    end
    else  
      mvmt := distance;
       
    sp^.position.x := sp^.position.x + (pct * mvmt.x);
    sp^.position.y := sp^.position.y + (pct * mvmt.y);

    if sp^.isMoving then
    begin
      pct := (TimerTicks(_spriteTimer) - sp^.lastUpdate) / 1000;
      if pct <= 0 then exit;

      sp^.lastUpdate := TimerTicks(_spriteTimer);

      sp^.position.x += pct * sp^.movingVec.x;
      sp^.position.y += pct * sp^.movingVec.y;

      sp^.arriveInSec -= pct;
      if sp^.arriveInSec <= 0 then 
      begin
        sp^.isMoving := false;
        sp^.arriveInSec := 0;

        SpriteRaiseEvent(s, SpriteArrivedEvent);
      end;
    end;
  end;

  procedure MoveSpriteTo(s : Sprite; x,y : Longint);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if sp = nil then begin RaiseException('No sprite supplied'); exit; end;
  
    sp^.position.x := x;
    sp^.position.y := y;

    if sp^.positionAtAnchorPoint then
      sp^.position += sp^.anchorPoint;
  end;

  procedure MoveSprite(s: Sprite); overload;
  begin
    MoveSprite(s, 1.0);
  end;  

  procedure MoveSprite(s: Sprite; pct: Single); overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    if Assigned(sp) then
      MoveSprite(s, sp^.velocity, pct);
  end;

  function SpriteOnScreenAt(s: Sprite; x, y: Longint): Boolean; overload;
  var
    cellRect: Rectangle;
    wx, wy: Single;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    result := false;
    
    if not Assigned(sp) then exit;
    
    wx := ToWorldX(x);
    wy := ToWorldY(y);
    
    if wy > sp^.position.y + SpriteHeight(s) then      result := false
    else if wy < sp^.position.y then                   result := false
    else if wx > sp^.position.x + SpriteWidth(s) then  result := false
    else if wx < sp^.position.x then                   result := false
    else if sp^.collisionKind = AABBCollisions then    result := true
    else
    begin
      cellRect := SpriteCurrentCellRectangle(s);
      result := PixelDrawnAtPoint(sp^.collisionBitmap, Round(wx - sp^.position.x + cellRect.x), Round(wy - sp^.position.y + cellRect.y));
    end;
  end;

  function SpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;
  begin
    result := SpriteOnScreenAt(s, Round(pt.x), Round(pt.y));
  end;

  function SpriteVelocity(s: Sprite): Vector;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then 
    begin 
      result := VectorTo(0,0);
      exit;
    end;

    result := sp^.velocity;
  end;

  procedure SpriteSetVelocity(s: Sprite; const value: Vector);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then 
    begin 
      exit;
    end;

    sp^.velocity := value;
  end;
  
  procedure SpriteAddToVelocity(s: Sprite; const value: Vector);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then 
    begin 
      exit;
    end;

    sp^.velocity := AddVectors(sp^.velocity, value);
  end;
  
  function SpriteCurrentCellRectangle(s: Sprite): Rectangle;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if (not Assigned(sp)) then
      result := RectangleFrom(0,0,0,0)
    else
      result := BitmapRectangleOfCell(sp^.layers[0], AnimationCurrentCell(sp^.animationInfo));
  end;
  
  function SpriteScreenRectangle(s: Sprite): Rectangle;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if (not Assigned(sp)) or (not Assigned(sp^.animationInfo)) then
      result := RectangleFrom(0,0,0,0)
    else
      result := ToScreen(SpriteLayerRectangle(s, 0));
  end;
  
  procedure SpriteSetX(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then 
    begin 
      exit;
    end;

    sp^.position.x := value;
  end;
    
  function SpriteX(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then 
    begin 
      result := 0;
      exit;
    end;

    result := sp^.position.x;
  end;
  
  procedure SpriteSetY(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then 
    begin 
      exit;
    end;

    sp^.position.y := value;
  end;
    
  function SpriteY(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then 
    begin 
      result := 0;
      exit;
    end;

    result := sp^.position.y;
  end;
  
  function SpriteLayer(s: Sprite; const name: String): Bitmap;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := nil
    else result := SpriteLayer(s, IndexOf(sp^.layerIds, name));
  end;
  
  function SpriteLayer(s: Sprite; idx: Longint): Bitmap;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := nil
    else if (idx < 0) or (idx > High(sp^.layers)) then begin result := nil; RaiseException('Sprite layer index out of range - ' + IntToStr(idx)); exit; end
    else result := sp^.layers[idx];
  end;
  
  function SpriteLayerIndex(s: Sprite; const name: String): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := -1
    else result := IndexOf(sp^.layerIds, name);
  end;
  
  function SpriteLayerName(s: Sprite; idx: Longint): String;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := ''
    else result := NameAt(sp^.layerIds, idx);
  end;
  
  function SpriteShowLayer(s: Sprite; const name: String): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := -1
    else result := SpriteShowLayer(s, IndexOf(sp^.layerIds, name));
  end;
  
  function SpriteShowLayer(s: Sprite; id: Longint): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then 
      result := -1
    else
    begin
      if (id < 0) or (id > Length(sp^.layers)) then begin result := -1; RaiseWarning('Cannot show layer ' + IntToStr(id) + ' of sprite.'); exit; end;
      
      //Scan for the current ID
      result := SpriteVisibleIndexOfLayer(s, id);
      if result >= 0 then exit;
      
      //Extend layers and add index
      SetLength(sp^.visibleLayers, Length(sp^.visibleLayers) + 1);
      result := High(sp^.visibleLayers);
      sp^.visibleLayers[result] := id;
    end;
  end;
  
  procedure SpriteHideLayer(s: Sprite; const name: String);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit
    else SpriteHideLayer(s, IndexOf(sp^.layerIds, name));
  end;
  
  procedure SpriteHideLayer(s: Sprite; id: Longint);
  var
    i, idx: Longint;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;

    idx := SpriteVisibleIndexOfLayer(s, id);
    if idx < 0 then exit; // The layer is not shown
    
    //Shift all later layers back over removed layer
    for i := idx to High(sp^.visibleLayers) - 1 do
    begin
      sp^.visibleLayers[i] := sp^.visibleLayers[i + 1];
    end;
    
    //Resize the array to remove element
    SetLength(sp^.visibleLayers, Length(sp^.visibleLayers) - 1);
  end;
  
  procedure SpriteToggleLayerVisible(s: Sprite; const name: String); overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit
    else SpriteToggleLayerVisible(s, IndexOf(sp^.layerIds, name));
  end;
  
  procedure SpriteToggleLayerVisible(s: Sprite; id: Longint); overload;
  begin
    if SpriteVisibleIndexOfLayer(s, id) < 0 then 
      SpriteShowLayer(s, id)
    else
      SpriteHideLayer(s, id);
  end;
  
  function SpriteLayerCount(s: Sprite): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := Length(sp^.layers);
  end;
  
  function SpriteVisibleLayerCount(s: Sprite): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := Length(sp^.visibleLayers);
  end;
  
  function SpriteVisibleLayerId(s: Sprite; idx: Longint) : Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if (not Assigned(sp)) or (idx < 0) or (idx > High(sp^.visibleLayers)) then result := -1
    else result := sp^.visibleLayers[idx];
  end;
  
  function SpriteLayerOffsets(s: Sprite): Point2DArray;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then begin result := nil; exit; end;
    result := sp^.layerOffsets;
  end;
  
  procedure SpriteSetLayerOffsets(s: Sprite; const values: Point2DArray);
  var
    i: Longint;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    if not Assigned(s) then exit;
    if not Length(values) = Length(sp^.layerOffsets) then begin RaiseException('Unable to set sprite layer offsets as lengths are not equal.'); exit; end;
    
    for i := 0 to High(values) do
    begin
      sp^.layerOffsets[i] := values[i];
    end;
  end;
  
  function SpriteLayerOffset(s: Sprite; const name: String): Point2D;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := PointAt(0,0)
    else result := SpriteLayerOffset(s, IndexOf(sp^.layerIds, name));
  end;
  
  function SpriteLayerOffset(s: Sprite; idx: Longint): Point2D;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := PointAt(0,0)
    else if (idx < 0) or (idx >= Length(sp^.layerOffsets)) then begin RaiseException('Error fetching layer offset out of range.'); result := PointAt(0,0); exit; end
    else result := sp^.layerOffsets[idx];
  end;
  
  procedure SpriteSetLayerOffset(s: Sprite; const name: String; const value: Point2D);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then
      SpriteSetLayerOffset(s, IndexOf(sp^.layerIds, name), value);
  end;
  
  procedure SpriteSetLayerOffset(s: Sprite; idx: Longint; const value: Point2D);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then
      sp^.layerOffsets[idx] := value;
  end;
  
  function SpriteVisibleIndexOfLayer(s: Sprite; const name: String): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := -1
    else result := SpriteVisibleIndexOfLayer(s, IndexOf(sp^.layerIds, name));
  end;
  
  function SpriteVisibleIndexOfLayer(s: Sprite; id: Longint): Longint;
  var
    i: Longint;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    result := -1;
    if not Assigned(sp) then exit
    else
    begin
      for i := 0 to High(sp^.visibleLayers) do
      begin
        if sp^.visibleLayers[i] = id then 
        begin
          result := i;
          exit;
        end;
      end;
    end;
  end;
  
  function SpriteVisibleLayer(s: Sprite; idx: Longint): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    result := -1;
    if not Assigned(sp) then exit
    else result := sp^.visibleLayers[idx];
  end;
  
  procedure Swap(var val1, val2: Longint);
  var
    temp: Longint;
  begin
    temp := val1;
    val1 := val2;
    val2 := temp;
  end;
  
  procedure SpriteSendLayerToBack(s: Sprite; visibleLayer: Longint);
  var
    i: Longint;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(sp^.visibleLayers) - 1) then exit;
    
    // Bubble layer up
    for i := visibleLayer to High(sp^.visibleLayers) - 1 do
    begin
      Swap(sp^.visibleLayers[i], sp^.visibleLayers[i + 1]);
    end;
  end;
  
  procedure SpriteSendLayerBackward(s: Sprite; visibleLayer: Longint);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(s) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(sp^.visibleLayers) - 1) then exit;
    
    Swap(sp^.visibleLayers[visibleLayer], sp^.visibleLayers[visibleLayer + 1]);
  end;
  
  procedure SpriteBringLayerForward(s: Sprite; visibleLayer: Longint);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;
    //Check not first or lower
    if (visibleLayer < 1) or (visibleLayer >= Length(sp^.visibleLayers)) then exit;
    
    Swap(sp^.visibleLayers[visibleLayer], sp^.visibleLayers[visibleLayer - 1]);
  end;
  
  procedure SpriteBringLayerToFront(s: Sprite; visibleLayer: Longint);
  var
    i: Longint;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(s) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(sp^.visibleLayers) - 1) then exit;
    
    // Bubble layer down
    for i := visibleLayer downto 1 do
    begin
      Swap(sp^.visibleLayers[i], sp^.visibleLayers[i - 1]);
    end;
  end;
  
  function SpriteLayerRectangle(s: Sprite; const name: String): Rectangle; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := RectangleFrom(0,0,0,0)
    else result := SpriteLayerRectangle(s, IndexOf(sp^.layerIds, name));
  end;
  
  function SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle', '');
    {$ENDIF}
    
    if not Assigned(sp) then result := RectangleFrom(0,0,0,0)
    else result := BitmapCellRectangle(sp^.position.x + sp^.layerOffsets[idx].x, sp^.position.y + sp^.layerOffsets[idx].y, sp^.layers[idx]);
      
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle', '');
    {$ENDIF}
  end;
  
  function SpriteCollisionRectangle(s: Sprite): Rectangle;
  var
    sp: SpritePtr;
    pts: array [0..3] of Point2D;
    cw, ch: Single;
    m: Matrix2D;
    minX, minY, maxX, maxY: Single;
    i: Integer;
  begin
    sp := ToSpritePtr(s);

    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'SpriteCollisionRectangle(s: Sprite): Rectangle', '');
    {$ENDIF}
    
    if not Assigned(s) then 
      result := RectangleFrom(0,0,0,0)
    else if (SpriteRotation(s) = 0) and (SpriteScale(s) = 1) then 
      result := BitmapCellRectangle(sp^.position.x, sp^.position.y, sp^.collisionBitmap)
    else
    begin
      cw := BitmapCellWidth(sp^.collisionBitmap);
      ch := BitmapCellHeight(sp^.collisionBitmap);

      pts[0] := PointAt(0, 0);
      pts[1] := PointAt(0, ch - 1);
      pts[2] := PointAt(cw - 1, 0);
      pts[3] := PointAt(cw - 1, ch - 1);

      m := SpriteLocationMatrix(s);

      for i := 0 to 3 do
      begin
        pts[i] := MatrixMultiply(m, pts[i]);
      end;
      
      minX := pts[0].x;
      maxX := pts[0].x;
      minY := pts[0].y;
      maxY := pts[0].y;

      for i := 1 to 3 do
      begin
        if pts[i].x < minX then minX := pts[i].x
        else if pts[i].x > maxX then maxX := pts[i].x;

        if pts[i].y < minY then minY := pts[i].y
        else if pts[i].y > maxY then maxY := pts[i].y;
      end;
      
      result := RectangleFrom(minX, minY, maxX - minX, maxY - minY);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'SpriteCollisionRectangle(s: Sprite): Rectangle', '');
    {$ENDIF}
  end;
  
  function SpriteCircle(s: Sprite): Circle; overload;
  begin
    result := SpriteLayerCircle(s, 0);
  end;
  
  function SpriteLayerCircle(s: Sprite; const name: String): Circle; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := CircleAt(0,0,0)
    else result := SpriteLayerCircle(s, IndexOf(sp^.layerIds, name));
  end;
  
  function SpriteLayerCircle(s: Sprite; idx: Longint): Circle; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'SpriteLayerCircle(s: Sprite): Circle', '');
    {$ENDIF}
    
    if not Assigned(sp) then result := CircleAt(0, 0, 0)
    else if (idx < 0) or (idx > High(sp^.layers)) then result := CircleAt(0,0,0)
    else
    begin
      result := BitmapCellCircle(sp^.layers[idx], CenterPoint(s), SpriteScale(s));
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'SpriteLayerCircle(s: Sprite): Circle', '');
    {$ENDIF}
  end;
  
  function SpriteCollisionCircle(s: Sprite): Circle;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'SpriteLayerCircle(s: Sprite): Circle', '');
    {$ENDIF}
    
    if (not Assigned(sp)) or (not Assigned(sp^.collisionBitmap)) then result := CircleAt(0, 0, 0)
    else result := BitmapCellCircle(sp^.collisionBitmap, CenterPoint(s), SpriteScale(s));
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'SpriteLayerCircle(s: Sprite): Circle', '');
    {$ENDIF}
    
  end;
  
  
//---------------------------------------------------------------------------
// Sprite position
//---------------------------------------------------------------------------
  
  function SpritePosition(s: Sprite): Point2D;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := PointAt(0,0)
    else result := sp^.position;
  end;
  
  procedure SpriteSetPosition(s: Sprite; const value: Point2D);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then sp^.position := value;
  end;
  
//---------------------------------------------------------------------------
// Sprite DX,DY
//---------------------------------------------------------------------------
  
  procedure SpriteSetDX(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then sp^.velocity.x := value;
  end;
  
  function SpriteDX(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := sp^.velocity.x;
  end;
  
  procedure SpriteSetDY(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then sp^.velocity.y := value;
  end;
  
  function SpriteDY(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := sp^.velocity.y;
  end;
  
//---------------------------------------------------------------------------
// Sprite speed and heading
//---------------------------------------------------------------------------
  
  function SpriteSpeed(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := VectorMagnitude(sp^.velocity);
  end;
  
  procedure SpriteSetSpeed(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then sp^.velocity := VectorMultiply(UnitVector(sp^.velocity), value);
  end;
  
  function SpriteHeading(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := VectorAngle(sp^.velocity);
  end;
  
  procedure SpriteSetHeading(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then sp^.velocity := VectorFromAngle(value, VectorMagnitude(sp^.velocity));
  end;
  
//---------------------------------------------------------------------------
// Sprite Current cell
//---------------------------------------------------------------------------
  
  function SpriteCurrentCell(s: Sprite): Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := -1
    else result := AnimationCurrentCell(sp^.animationInfo);
  end;
  
  function SpriteAnimationHasEnded(s: Sprite): Boolean;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := false
    else result := AnimationEnded(sp^.animationInfo);
  end;
  
//---------------------------------------------------------------------------
// Sprite width/height
//---------------------------------------------------------------------------
  
  function SpriteLayerHeight(s: Sprite; const name: String): Longint; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(s) then result := 0
    else result := SpriteLayerHeight(s, IndexOf(sp^.layerIds, name));
  end;
  
  function SpriteLayerHeight(s: Sprite; idx: Longint): Longint; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else if (idx < 0) or (idx >= Length(sp^.layers)) then result := 0
    else result := BitmapCellHeight(sp^.layers[idx]);
  end;
  
  function SpriteLayerWidth(s: Sprite; const name: String): Longint; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := SpriteLayerWidth(s, IndexOf(sp^.layerIds, name));
  end;
  
  function SpriteLayerWidth(s: Sprite; idx: Longint): Longint; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else if (idx < 0) or (idx >= Length(sp^.layers)) then result := 0
    else result := BitmapCellWidth(sp^.layers[idx]);
  end;
  
  function SpriteWidth(s: Sprite): Longint;
  begin
    result := SpriteLayerWidth(s, 0);
  end;
  
  function SpriteHeight(s: Sprite): Longint;
  begin
    result := SpriteLayerHeight(s, 0);
  end;

  function SpriteAnchorPoint(s: Sprite): Point2D;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    if Assigned(sp) then
    begin
      result := sp^.anchorPoint;
    end
    else
    begin
      result := PointAt(0,0);
    end;
  end;

  procedure SpriteSetAnchorPoint(s: Sprite; pt: Point2D);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    if Assigned(sp) then
    begin
      sp^.anchorPoint := pt;
    end;
  end;

  function SpriteMoveFromAnchorPoint(s: Sprite): Boolean;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    if Assigned(sp) then
    begin
      result := sp^.positionAtAnchorPoint;
    end
    else
    begin
      result := false;
    end;
  end;
  
  procedure SpriteSetMoveFromAnchorPoint(s: Sprite; value: Boolean);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    if Assigned(sp) then
    begin
      sp^.positionAtAnchorPoint := value;
    end;
  end;
  
//---------------------------------------------------------------------------
// Sprite mass
//---------------------------------------------------------------------------
  
  function SpriteMass(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := sp^.values[MASS_IDX];
  end;
  
  procedure SpriteSetMass(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then sp^.values[MASS_IDX] := value;
  end;
  
//---------------------------------------------------------------------------
// Sprite rotation
//---------------------------------------------------------------------------
  
  function SpriteRotation(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := sp^.values[ROTATION_IDX];
  end;
  
  procedure SpriteSetRotation(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then 
    begin
      if value < 0 then
      begin
        value := 360 + (value + Abs(Trunc(value / 360) * 360));
      end;
      
      if value > 360 then
      begin
        value := value - Trunc(value / 360) * 360;
      end;

      sp^.values[ROTATION_IDX] := value;
    end;
  end;
  
//---------------------------------------------------------------------------
// Sprite scale
//---------------------------------------------------------------------------
  
  function SpriteScale(s: Sprite): Single;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := 0
    else result := sp^.values[SCALE_IDX];
  end;
  
  procedure SpriteSetScale(s: Sprite; value: Single);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then 
    begin
      sp^.values[SCALE_IDX] := value;
    end;
  end;
  
//---------------------------------------------------------------------------
// Sprite center point
//---------------------------------------------------------------------------
  
  function CenterPoint(s: Sprite): Point2D;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'CenterPoint(s: Sprite): Point2D', '');
    {$ENDIF}

    if not Assigned(sp) then
    begin
      result := PointAt(0,0);
      exit;
    end;

    result.x := sp^.position.x + SpriteWidth(s) / 2;
    result.y := sp^.position.y + SpriteHeight(s) / 2;
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'CenterPoint(s: Sprite): Point2D', '');
    {$ENDIF}
  end;
  
//---------------------------------------------------------------------------
// Sprite collision details
//---------------------------------------------------------------------------
  
  function SpriteCollisionKind(s: Sprite): CollisionTestKind;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := AABBCollisions
    else result := sp^.collisionKind;
  end;
  
  procedure SpriteSetCollisionKind(s: Sprite; value: CollisionTestKind);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then sp^.collisionKind := value;
  end;
  
  function SpriteCollisionBitmap(s: Sprite): Bitmap;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then result := nil
    else result := sp^.collisionBitmap;
  end;
    
  procedure SpriteSetCollisionBitmap(s: Sprite; bmp: Bitmap);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then sp^.collisionBitmap := bmp;
  end;
  
//---------------------------------------------------------------------------
// Sprite value code
//---------------------------------------------------------------------------
  
  function SpriteValueCount(s: Sprite) : Longint;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    result := -1;
    if not Assigned(sp) then exit;
    
    result := NameCount(sp^.valueIds);
  end;
  
  function SpriteValueName(s: Sprite; idx: Longint) : String; 
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);
    result := '';

    if not Assigned(sp) then exit;
    
    result := NameAt(sp^.valueIds, idx);
  end;
  
  function SpriteValue(s: Sprite; index: Longint): Single; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    result := 0;
    if not Assigned(sp) then exit;
    
    result := sp^.values[index];
  end;
  
  function SpriteValue(s: Sprite; const name: String): Single; overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    result := 0;
    if not Assigned(sp) then exit;
    
    result := SpriteValue(s, IndexOf(sp^.valueIds, name));
  end;
  
  procedure SpriteAddValue(s: Sprite; const name: String);
  begin
    SpriteAddValue(s, name, 0);
  end;
  
  procedure SpriteAddValue(s: Sprite; const name: String; initVal: Single);
  var
    idx: Longint;
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;
    if HasName(sp^.valueIds, name) then exit;
    
    idx := AddName(sp^.valueIds, name);
    SetLength(sp^.values, Length(sp^.values) + 1);
    sp^.values[idx] := initVal;
  end;
  
  procedure SpriteSetValue(s: Sprite; const name: String; val: Single); overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;
    
    SpriteSetValue(s, IndexOf(sp^.valueIds, name), val);
  end;
  
  procedure SpriteSetValue(s: Sprite; idx: Longint; val: Single); overload;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not Assigned(sp) then exit;
    if (idx < 0) or (idx > High(sp^.values)) then exit;
    
    sp^.values[idx] := val;
  end;
  
  function SpriteName(sprt: Sprite): String;
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(sprt);

    result := '';
    if not Assigned(sp) then exit;
    result := sp^.name;
  end;

  procedure SpriteMoveTo(s: Sprite; const pt: Point2D; takingSeconds: Longint);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if not assigned(sp) then exit;

    sp^.destination := pt;
    sp^.arriveInSec := takingSeconds;
    sp^.isMoving := true;
    sp^.movingVec := VectorMultiply(UnitVector(VectorFromPoints(CenterPoint(s), pt)), PointPointDistance(CenterPoint(s), pt) / takingSeconds);
    sp^.lastUpdate := TimerTicks(_spriteTimer);
  end;

  function SpriteLocationMatrix(s: Sprite): Matrix2D;
  var
    scale, newX, newY, w, h: Single;
    anchorMatrix: Matrix2D;
    // sp: SpritePtr;
  begin
    result := IdentityMatrix();

    // sp := ToSpritePtr(s);
    // if not Assigned(sp) then exit;
    
    scale := SpriteScale(s);
    w := SpriteLayerWidth(s, 0);
    h := SpriteLayerHeight(s, 0);
    
    anchorMatrix := TranslationMatrix(SpriteAnchorPoint(s));

    result := MatrixMultiply(MatrixInverse(anchorMatrix), result);
    result := MatrixMultiply(RotationMatrix(SpriteRotation(s)), result);
    result := MatrixMultiply(anchorMatrix, result);

    newX := SpriteX(s) - (w * scale / 2.0) + (w / 2.0);
    newY := SpriteY(s) - (h * scale / 2.0) + (h / 2.0);
    result := MatrixMultiply(TranslationMatrix(newX / scale, newY / scale), result);

    result := MatrixMultiply(ScaleMatrix(scale), result); 
  end;

//---------------------------------------------------------------------------
// Event Code
//---------------------------------------------------------------------------

  procedure _AddSpriteEventHandler(var arr: SpriteEventHandlerArray; handler: SpriteEventHandler);
  begin
    if Assigned(handler) then
    begin
      SetLength(arr, Length(arr) + 1);
      arr[High(arr)] := handler;
    end;
  end;

  function _IndexOfSpriteEventHandler(const arr: SpriteEventHandlerArray; handler: SpriteEventHandler): Integer;
  var
    i: Integer;
  begin
    for i := 0 to High(arr) do
    begin
      if arr[i] = handler then
      begin
        result := i;
        exit;
      end;
    end;
    result := -1;
  end;

  procedure _RemoveSpriteEventHandler(var arr: SpriteEventHandlerArray; handler: SpriteEventHandler);
  var
    i, idx: Integer;
  begin
    idx := _IndexOfSpriteEventHandler(arr, handler);
    
    if idx < 0 then exit;
    
    for i := idx to High(arr) - 1 do
    begin
      arr[i] := arr[i + 1];
    end;

    SetLength(arr, Length(arr) - 1);
  end;

  procedure CallOnSpriteEvent(handler: SpriteEventHandler);
  begin
    _AddSpriteEventHandler(_GlobalSpriteEventHandlers, handler);
  end;
  
  procedure StopCallingOnSpriteEvent(handler: SpriteEventHandler);
  begin
    _RemoveSpriteEventHandler(_GlobalSpriteEventHandlers, handler);
  end;

  procedure SpriteCallOnEvent(s: Sprite; handler: SpriteEventHandler);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then
    begin
      _AddSpriteEventHandler(sp^.evts, handler);
    end;
  end;

  procedure SpriteStopCallingOnEvent(s: Sprite; handler: SpriteEventHandler);
  var
    sp: SpritePtr;
  begin
    sp := ToSpritePtr(s);

    if Assigned(sp) then
    begin
      _RemoveSpriteEventHandler(sp^.evts, handler);
    end;
  end;

//---------------------------------------------------------------------------
// Sprite Packs
//---------------------------------------------------------------------------

  // Duplicate for the UpdateSprite for use in UpdateAllSprites... as it
  // currently does not select the correct overload.
  procedure _UpdateSpritePct(s: Sprite; pct: Single);
  begin
    UpdateSprite(s, pct);  
  end;

  procedure DrawAllSprites();
  begin
    _CurrentPack.CallForAllSprites(@DrawSprite);
  end;

  procedure UpdateAllSprites(pct: Single); overload;
  begin
    _CurrentPack.CallForAllSprites(@_UpdateSpritePct, pct);
  end;

  procedure CallForAllSprites(fn: SpriteFunction);
  begin
    _CurrentPack.CallForAllSprites(fn);
  end;

  procedure UpdateAllSprites(); overload;
  begin
    UpdateAllSprites(1.0);
  end;

  procedure CreateSpritePack(const name: String);
  begin
    if not HasSpritePack(name) then
    begin
      _CurrentPack := TSpritePack.Create(name);
      _CurrentPack.AddPackTo(_SpritePacks);
    end
    else
    begin
      RaiseWarning('The SpritePack ' + name + ' already exists');
    end;
  end;

  function HasSpritePack(const name: String): Boolean;
  begin
    result := _SpritePacks.ContainsKey(name);
  end;

  function CurrentSpritePack(): String;
  begin
    result := _CurrentPack.Name;
  end;

  function SpritePackNamed(const name: String): TSpritePack;
  begin
    if HasSpritePack(name) then
      result := TSpritePack(_SpritePacks.Values[name])
    else
      result := nil;
  end;

  procedure SelectSpritePack(const name: String);
  begin
    if HasSpritePack(name) then
    begin
      _CurrentPack := TSpritePack(_SpritePacks.Values[name]);
    end
    else
    begin
      RaiseWarning('No SpritePack named ' + name + ' to select.');
    end;
  end;

  procedure ReleaseSpritePack(const name: String);
  var
    pack: TSpritePack;
  begin
    pack := SpritePackNamed(name);
    if (Assigned(pack)) then
    begin
      FreeAndNil(pack);
    end;
  end;

  procedure ReleaseAllSpritePacks();
  begin
    ReleaseAll(_SpritePacks, nil);
  end;

//=============================================================================
  initialization
  begin
    InitialiseSwinGame();
    
    _Sprites := TStringHash.Create(False, 16384);
    _SpritePacks := TStringHash.Create(False, 50);

    _CurrentPack := TSpritePack.Create('Default');
    _CurrentPack.AddPackTo(_SpritePacks);

    // Sprite movement timer - all sprites movement is timed from this
    _spriteTimer := CreateTimer('*SG* SpriteTimer');
    StartTimer(_spriteTimer);

    // Sprite Event Handlers
    SetLength(_GlobalSpriteEventHandlers, 0);
  end;
  
  finalization
  begin
    ReleaseAllSprites();
    ReleaseAllSpritePacks();
    SetLength(_GlobalSpriteEventHandlers, 0);
    FreeAndNil(_Sprites); 
    FreeAndNil(_SpritePacks);
  end;
  
end.