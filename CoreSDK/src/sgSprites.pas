//=============================================================================
// sgSprites.pas
//=============================================================================
//
// Create and manage sprites in SwinGame.
//
// Change History:
//
// Version 3.0:
// - 2010-12-31: Andrew : Added name to sprite
//                      : Added standard resource management code
// - 2009-12-21: Andrew : Added the ability to toggle visible layers.
//                      : Added width/height of layers and layer offsets
//                      : Added rectangle calculation code for sprite layers
// - 2009-12-20: Andrew : Added code to manage sprite layers (show, hide, reorder, etc)
// - 2009-12-18: Andrew : Moved to new sprite format.
// - 2009-11-10: Andrew : Changed sn and csn tags
// - 2009-10-16: Andrew : Called the free notifier to ensure Sprites are freed
// - 2009-07-06: Andrew : Added property access routines for Sprite data
// - 2009-06-29: Andrew : Renamed CurrentWidth to SpriteWidth
//                      : Renamed CurrentHeight to SpriteHeight
//                      : Renamed IsSpriteOnScreenAt to SpriteOnScreenAt
// - 2009-06-20: Andrew : Created Sprites unit.
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
  /// pixel level collisions, no animation, the layer have name 'layer1'.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateBasicSprite
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s
  function CreateSprite(layer: Bitmap): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, the layer have name 'layer1', and position the sprite at the
  /// given x,y location.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateBasicSpriteXY
  /// @sn createSpriteWithLayer:%s x:%s y:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s x:%s y:%s
  function CreateSprite(layer: Bitmap; x, y: Single): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, the layer have name 'layer1', and position the sprite at the
  /// given location.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateBasicSpritePt
  /// @sn createSpriteWithLayer:%s atPoint:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s position:%s
  function CreateSprite(layer: Bitmap; pt: Point2D): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, and the specified layer have name.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateSpriteWithLayer
  /// @sn createSpriteWithLayer:%s layerNamed:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s layerNamed:%s
  function CreateSprite(layer: Bitmap; layerName: String): Sprite; overload;
  
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
  /// pixel level collisions, the specified animation template, the layer have name 'layer1', at the given
  /// x, y location.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateSpriteWithAnimationXY
  /// @sn createSpriteWithLayer:%s animationScript:%s x:%s y:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s animationScript:%s x:%s y:%s
  function CreateSprite(layer: Bitmap; ani: AnimationScript; x, y: Single): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, the specified animation template, the layer have name 'layer1', at the given
  /// location.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateSpriteWithAnimationPt
  /// @sn createSpriteWithLayer:%s animationScript:%s position:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s animationScript:%s position:%s
  function CreateSprite(layer: Bitmap; ani: AnimationScript; pt: Point2D): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, the specified animation template, and layer name.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateSpriteWithLayerAndAnimation
  /// @sn createSpriteWithLayer:%s layerNamed:%s animationScript:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmap:%s layerNamed:%s animationScript:%s
  function CreateSprite(layer: Bitmap; layerName: String; ani: AnimationScript): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, the layer names 'layer1', 'layer2',... .
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateLayeredSprite
  /// @sn createSpriteWithLayers:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmaps:%s
  function CreateSprite(const layers: BitmapArray): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, and the specified layer names.
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateLayeredSpriteWithLayerNames
  /// @sn createSpriteWithLayers:%s layersNamed:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmaps:%s layerNames:%s
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, the specified animation template, the layer names 'layer1', 'layer2',... .
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateLayeredSpriteWithAnimationScript
  /// @sn createSpriteWithLayers:%s animationScript:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmaps:%s animationScript:%s
  function CreateSprite(const layers: BitmapArray; ani: AnimationScript): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, the layer names 'layer1', 'layer2',... .
  ///
  /// This version of the constructor will assign a default name to the sprite for resource management purposes.
  /// 
  /// @lib CreateLayeredSpriteWithLayerNamesAndAnimationScript
  /// @sn createSpriteWithLayers:%s layersNamed:%s animationScript:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initWithBitmaps:%s layerNames:%s animationScript:%s
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray; ani: AnimationScript): Sprite; overload;
  
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
  function CreateSprite(name: String; layer: Bitmap): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, and the specified layer have name.
  /// 
  /// @lib CreateSpriteWithLayerNamed
  /// @sn createSpriteNamed:%s layer:%s layerNamed:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initNamed:%s withBitmap:%s layerNamed:%s
  function CreateSprite(name: String; layer: Bitmap; layerName: String): Sprite; overload;
  
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
  function CreateSprite(name: String; layer: Bitmap; ani: AnimationScript): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap image. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, the specified animation template, and layer name.
  /// 
  /// @lib CreateSpriteWithLayerAndAnimationNamed
  /// @sn createSpriteNamed:%s layer:%s layerNamed:%s animationScript:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initNamed:%s withBitmap:%s layerNamed:%s animationScript:%s
  function CreateSprite(name: String; layer: Bitmap; layerName: String; ani: AnimationScript): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, the layer names 'layer1', 'layer2',... .
  /// 
  /// @lib CreateLayeredSpriteNamed
  /// @sn createSpriteNamed:%s layers:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initNamed:%s withBitmaps:%s
  function CreateSprite(name: String; const layers: BitmapArray): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, and the specified layer names.
  /// 
  /// @lib CreateLayeredSpriteWithLayerNamesNamed
  /// @sn createSpriteNamed:%s layers:%s layersNamed:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initNamed:%s withBitmaps:%s layerNames:%s
  function CreateSprite(name: String; const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, the specified animation template, the layer names 'layer1', 'layer2',... .
  /// 
  /// @lib CreateLayeredSpriteWithAnimationScriptNamed
  /// @sn createSpriteNamed:%s layers:%s animationScript:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initNamed: %s withBitmaps:%s animationScript:%s
  function CreateSprite(name: String; const layers: BitmapArray; ani: AnimationScript): Sprite; overload;
  
  /// Creates a sprite for the passed in bitmap images. The sprite will use the cell information within the 
  /// sprite if it is animated at a later stage. This version of CreateSprite will initialise the sprite to use
  /// pixel level collisions, no animation, the layer names 'layer1', 'layer2',... .
  /// 
  /// @lib CreateLayeredSpriteWithLayerNamesAndAnimationScriptNamed
  /// @sn createSpriteNamed:%s layers:%s layersNamed:%s animationScript:%s
  /// 
  /// @class Sprite
  /// @constructor
  /// @csn initNamed:%s withBitmaps:%s layerNames:%s animationScript:%s
  function CreateSprite(name: String; const layers: BitmapArray; const layerNames: StringArray; ani: AnimationScript): Sprite; overload;
  
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
  function HasSprite(name: String): Boolean;
  
  /// Returns the `Sprite` with the specified name,
  /// see `CreateBasicSprite`.
  ///
  /// @lib
  function SpriteNamed(name: String): Sprite;
  
  /// Releases the SwinGame resources associated with the sprite of the
  /// specified ``name``.
  ///
  /// @lib
  procedure ReleaseSprite(name: String);
  
  /// Releases all of the sprites that have been loaded.
  ///
  /// @lib
  procedure ReleaseAllSprites();
  
  
  
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
  function SpriteAddLayer(s: Sprite; newLayer: Bitmap; layerName: String): Longint;
  
  /// Returns the bitmap of the indicated layer of the sprite.
  ///
  /// @lib SpriteLayerNamed
  /// @sn sprite:%s layerNamed:%s
  ///
  /// @class Sprite
  /// @method LayerNamed
  /// @csn layerNamed:%s
  function SpriteLayer(s: Sprite; name: String): Bitmap; overload;
  
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
  function SpriteLayerIndex(s: Sprite; name: String): Longint;
  
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
  function SpriteShowLayer(s: Sprite; name: String): Longint; overload;
  
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
  procedure SpriteHideLayer(s: Sprite; name: String); overload;
  
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
  procedure SpriteToggleLayerVisible(s: Sprite; name: String); overload;
  
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
  function SpriteVisibleIndexOfLayer(s: Sprite; name: String): Longint; overload;
  
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
  
  /// Returns the ids of the layers that are currently visible. In order back to front.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter VisibleLayerIds
  /// @length SpriteVisibleLayerCount
  function SpriteVisibleLayerIds(s: Sprite) : LongintArray;
  
  /// Returns the bitmaps of the layers in the Sprite.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter Layers
  /// @length SpriteLayerCount
  function SpriteLayers(s: Sprite): BitmapArray;
  
  /// Returns the offets of the layers in the Sprite.
  ///
  /// @lib
  ///
  /// @class Sprite
  /// @getter LayerOffsets
  /// @length SpriteLayerCount
  function SpriteLayerOffsets(s: Sprite): Point2DArray;
  
  /// Sets the layer offsets for the sprite.
  ///
  /// @lib
  /// @sn sprite:%s setLayerOffsets:%s
  ///
  /// @class Sprite
  /// @setter LayerOffsets
  /// @length SpriteLayerCount
  procedure SpriteSetLayerOffsets(s: Sprite; const values: Point2DArray);
  
  /// Gets the offset of the specified layer.
  ///
  /// @lib SpriteLayerOffsetNamed
  /// @sn sprite:%s offsetOfLayerNamed:%s
  ///
  /// @class Sprite
  /// @overload LayerOffset LayerNamedOffset
  /// @csn offsetOfLayerNamed:%s 
  function SpriteLayerOffset(s: Sprite; name: String): Point2D; overload;
  
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
  procedure SpriteSetLayerOffset(s: Sprite; name: String; const value: Point2D); overload;
  
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
  function SpriteLayerRectangle(s: Sprite; name: String): Rectangle; overload;
  
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
  function SpriteLayerCircle(s: Sprite; name: String): Circle; overload;
  
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
  procedure SpriteStartAnimation(s: Sprite; named: String); overload;
  
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
  procedure SpriteStartAnimation(s: Sprite; named: String; withSound: Boolean); overload;
  
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
  function SpriteLayerHeight(s: Sprite; name: String): Longint; overload;
  
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
  function SpriteLayerWidth(s: Sprite; name: String): Longint; overload;
  
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
  /// @getter ValueNames
  /// @length SpriteValueCount
  function SpriteValueNames(s: Sprite) : StringArray; 
  
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
  function SpriteValue(s: Sprite; name: String): Single; overload;
  
  /// Adds a new kind of value to the Sprite
  /// 
  /// @lib SpriteAddValue
  /// @sn sprite:%s addValue:%s
  /// 
  /// @class Sprite
  /// @method AddValue
  procedure SpriteAddValue(s: Sprite; name: String);
  
  /// Adds a new kind of value to the Sprite, setting the initial value
  /// to the value passed in.
  /// 
  /// @lib SpriteAddValueWithInitialValue
  /// @sn sprite:%s addValue:%s initally:%s
  /// 
  /// @class Sprite
  /// @overload AddValue AddValueWithDefault
  /// @csn addValue:%s initally:%s
  procedure SpriteAddValue(s: Sprite; name: String; initVal: Single);
  
  /// Assigns a value to the Sprite.
  ///
  /// @lib SpriteSetValueNamed
  /// @sn sprite:%s setValueNamed:%s to:%s
  ///
  /// @class Sprite
  /// @overload SetValue SetValueNamed
  /// @csn setValueNamed:%s to:%s
  procedure SpriteSetValue(s: Sprite; name: String; val: Single); overload;
  
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
  
  
  
//=============================================================================
implementation
  uses
    Classes, SysUtils, Math, // System
    stringhash,
    sgNamedIndexCollection, //libsrc
    sgAnimations, sgGraphics, sgGeometry, sgCamera, sgShared, sgResources, sgImages, sgTrace; //SwinGame
//=============================================================================

  var
    _Sprites: TStringHash;
  
  const
    MASS_IDX      = 0;  // The index of the sprite's mass value
    ROTATION_IDX  = 1;  // The index of the sprite's rotation value
    SCALE_IDX     = 2;  // The index of the sprite's scale value
    // WIDTH_IDX     = 3;  // The index of the sprite's width value
    // HEIGHT_IDX    = 4;  // The index of the sprite's height value
    
  
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
    result := CreateSprite(layer, 'layer0', nil);
  end;
  
  function CreateSprite(layer: Bitmap; x, y: Single): Sprite; overload;
  begin
    result := CreateSprite(layer);
    SpriteSetX(result, x);
    SpriteSetY(result, y);
  end;
  
  function CreateSprite(layer: Bitmap; pt: Point2D): Sprite; overload;
  begin
    result := CreateSprite(layer);
    SpriteSetPosition(result, pt);
  end;
  
  function CreateSprite(layer: Bitmap; layerName: String): Sprite; overload;
  begin
    result := CreateSprite(layer, layerName, nil);
  end;
  
  function CreateSprite(layer: Bitmap; ani: AnimationScript): Sprite; overload;
  begin
    result := CreateSprite(layer, 'layer0', ani);
  end;
  
  function CreateSprite(layer: Bitmap; ani: AnimationScript; x, y: Single): Sprite; overload;
  begin
    result := CreateSprite(layer, ani);
    SpriteSetX(result, x);
    SpriteSetY(result, y);
  end;
  
  function CreateSprite(layer: Bitmap; ani: AnimationScript; pt: Point2D): Sprite; overload;
  begin
    result := CreateSprite(layer);
    SpriteSetPosition(result, pt);
  end;
  
  function CreateSprite(layer: Bitmap; layerName: String; ani: AnimationScript): Sprite; overload;
  var
    layerNames: StringArray;
    layers: BitmapArray;
  begin
    SetLength(layerNames, 1);
    SetLength(layers, 1);
    
    layerNames[0] := layerName;
    layers[0] := layer;
    
    result := CreateSprite(layers, layerNames, ani);
  end;
  
  function CreateSprite(const layers: BitmapArray): Sprite; overload;
  begin
    result := CreateSprite(layers, AnimationScript(nil));
  end;
  
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;
  begin
    result := CreateSprite(layers, layerNames, nil);
  end;
  
  function CreateSprite(const layers: BitmapArray; ani: AnimationScript): Sprite; overload;
  var
    layerNames: StringArray;
    i: Longint;
  begin
    SetLength(layerNames, Length(layers));
    for i := 0 to High(layers) do
    begin
      layerNames[i] := 'layer' + IntToStr(i);
    end;
    
    result := CreateSprite(layers, layerNames, ani);
  end;
  
  function CreateSprite(const layers: BitmapArray; const layerNames: StringArray; ani: AnimationScript): Sprite; overload;
  begin
    result := CreateSprite('Sprite', layers, layerNames, ani);
  end;
  
  function CreateSprite(name: String; layer: Bitmap): Sprite; overload;
  begin
    result := CreateSprite(name, layer, 'layer0', nil);
  end;
  
  function CreateSprite(name: String; layer: Bitmap; layerName: String): Sprite; overload;
  begin
    result := CreateSprite(name, layer, layerName, nil);
  end;
  
  function CreateSprite(name: String; layer: Bitmap; ani: AnimationScript): Sprite; overload;
  begin
    result := CreateSprite(name, layer, 'layer0', ani);
  end;
  
  function CreateSprite(name: String; layer: Bitmap; layerName: String; ani: AnimationScript): Sprite; overload;
  var
    layerNames: StringArray;
    layers: BitmapArray;
  begin
    SetLength(layerNames, 1);
    SetLength(layers, 1);
    
    layerNames[0] := layerName;
    layers[0] := layer;
    
    result := CreateSprite(name, layers, layerNames, ani);
  end;
  
  function CreateSprite(name: String; const layers: BitmapArray): Sprite; overload;
  begin
    result := CreateSprite(name, layers, AnimationScript(nil));
  end;
  
  function CreateSprite(name: String; const layers: BitmapArray; const layerNames: StringArray): Sprite; overload;
  begin
    result := CreateSprite(name, layers, layerNames, nil);
  end;
  
  function CreateSprite(name: String; const layers: BitmapArray; ani: AnimationScript): Sprite; overload;
  var
    layerNames: StringArray;
    i: Longint;
  begin
    SetLength(layerNames, Length(layers));
    for i := 0 to High(layers) do
    begin
      layerNames[i] := 'layer' + IntToStr(i);
    end;
    
    result := CreateSprite(name, layers, layerNames, ani);
  end;
  
  function CreateSprite(name: String; const layers: BitmapArray; const layerNames: StringArray; ani: AnimationScript): Sprite; overload;
  var
    nameBase: String;
    i, idx, count, cellCount: Longint;
    obj: tResourceContainer;
  begin
    result := nil; 
    count := Length(layers);
    
    if count <> Length(layerNames) then begin RaiseException('The number of layers and layer names do not match.'); exit; end;
    if count = 0 then begin exit; end;
    
    // Find a unique name for this sprite
    idx := 0;
    nameBase := name;
    
    while HasSprite(name) do
    begin
      name := nameBase + IntToStr(idx);
      idx += 1;
    end;
    
    //Allocate the space for the sprite
    New(result);
    
    result^.name := name;
    
    //Set lengths of the layer arrays
    SetLength(result^.layers, count);
    SetLength(result^.layerOffsets, count);
    
    //Get the number of cells from the first layer
    cellCount := BitmapCellCount(layers[0]);
    
    //Copy across the layers...
    for i := 0 to High(result^.layers) do
    begin
      result^.layers[i] := layers[i];
      result^.layerOffsets[i] := PointAt(0,0);
      
      // Make sure that this image can be used interchangably with the other layers of the
      // sprite.
      if BitmapCellCount(result^.layers[i]) <> cellCount then
      begin
        Dispose(result);
        result := nil;
        RaiseException('Layer ' + IntToStr(i + 1) + ' does not have the same number of cells as the other layers in the sprite ' + name);
        exit;
      end;
    end;
    
    // Setup the layer name <-> id mapping
    InitNamedIndexCollection(result^.layerIds, layerNames);
    
    // Set the first layer as visible.
    SetLength(result^.visibleLayers, 1);
    result^.visibleLayers[0] := 0;                //The first layer (at idx 0) is drawn
    
    // Setup the values
    SetLength(result^.values, 3);
    InitNamedIndexCollection(result^.valueIds);
    
    AddName(result^.valueIds, 'mass');            //idx 0 = mass, default to 1
    result^.values[MASS_IDX]      := 1;
    
    AddName(result^.valueIds, 'rotation');        //idx 1 = rotation, default to 0
    result^.values[ROTATION_IDX]  := 0;
    
    AddName(result^.valueIds, 'scale');           //idx 2 = scale, default to 1
    result^.values[SCALE_IDX]       := 1;
    
    // Position the sprite
    result^.position                := PointAt(0,0);
    
    // Initialise sprite movement
    result^.velocity                := VectorTo(0,0);
    
    // Setup animation detials
    result^.animationScript         := ani;
    result^.animationInfo           := nil;
    
    // Setup collision details
    result^.collisionKind           := PixelCollisions;
    result^.collisionBitmap         := result^.layers[0];
    
    // Setup cache details
    result^.backupCollisionBitmap   := nil;
    result^.cacheImage              := nil;
    
    obj := tResourceContainer.Create(result);
    // WriteLn('Adding for ', name, ' ', HexStr(obj));
    if not _Sprites.setValue(name, obj) then
    begin
        RaiseException('** Leaking: Caused by loading Sprite created twice, ' + name);
        result := nil;
        exit;
    end;
  end;
  
  function HasSprite(name: String): Boolean;
  begin
    result := _Sprites.containsKey(name);
  end;

  function SpriteNamed(name: String): Sprite;
  var
    tmp : TObject;
  begin
    tmp := _Sprites.values[name];
    if Assigned(tmp) then
      result := Sprite(tResourceContainer(tmp).Resource)
    else 
      result := nil;
  end;

  procedure ReleaseSprite(name: String);
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
  
  
  //
  // Update the buffered image for rotation and scaling of a bitmap based sprite.
  //
  procedure _UpdateSpriteImageCache(s: Sprite);
  var
    dest, bmp: Bitmap; //temporary surface
    cells: BitmapArray;
    i, idx, currentCell, numCells: Longint; //for image parts
  begin
    if (s^.cacheImage <> nil) then FreeBitmap(s^.cacheImage);
    if (s^.backupCollisionBitmap <> nil) then
    begin
      FreeBitmap(s^.collisionBitmap); // cache is the copy...
      s^.collisionBitmap := s^.backupCollisionBitmap;
      s^.backupCollisionBitmap := nil;
    end; 
    if (SpriteRotation(s) = 0) and (SpriteScale(s) = 1) then exit; //no need to transform
    
    numCells := BitmapCellCount(s^.layers[0]);
    if numCells = 0 then exit;
    
    SetLength(cells, numCells);
    
    //Draw non-transformed bitmap onto temp surface
    for currentCell := 0 to High(cells) do
    begin
      dest := CreateBitmap('sprite-rot-cell-' + IntToStr(currentCell) + '-' + SpriteName(s), SpriteWidth(s), SpriteHeight(s));
      for i := 0 to High(s^.visibleLayers) do
      begin
        idx := s^.visibleLayers[i];
        bmp := s^.layers[idx]; // the bitmap to draw + rotate
        MakeOpaque(bmp);
        DrawCell(dest, bmp, currentCell, Round(s^.layerOffsets[idx].x), Round(s^.layerOffsets[idx].y));
        MakeTransparent(bmp);
      end;
      cells[currentCell] := RotateScaleBitmap(dest, SpriteRotation(s), SpriteScale(s));
      FreeBitmap(dest);
    end;
    
    // Make into a new image...
    s^.cacheImage := CombineIntoGrid(cells, 6);
    
    for currentCell := 0 to High(cells) do FreeBitmap(cells[currentCell]);
    
    // Repeat for cacheCollisionBitmap
    s^.backupCollisionBitmap := s^.collisionBitmap;
    bmp := s^.collisionBitmap;
    numCells := BitmapCellCount(bmp);
    
    if numCells = 0 then exit;
    SetLength(cells, numCells);
    
    //Draw non-transformed bitmap onto temp surface
    for currentCell := 0 to High(cells) do
    begin
      dest := CreateBitmap('collision-cache-cell' + IntToStr(currentCell) + '-' + BitmapName(bmp), BitmapCellWidth(bmp), BitmapCellHeight(bmp));
      MakeOpaque(bmp);
      DrawCell(dest, bmp, currentCell, 0, 0);
      MakeTransparent(bmp);
      cells[currentCell] := RotateScaleBitmap(dest, SpriteRotation(s), SpriteScale(s));
      FreeBitmap(dest);
    end;
    
    s^.collisionBitmap := CombineIntoGrid(cells, 6);
    for currentCell := 0 to High(cells) do FreeBitmap(cells[currentCell]);
    
    SetupBitmapForCollisions(s^.collisionBitmap);
  end;
  
  procedure FreeSprite(var s : Sprite);
  begin
    if Assigned(s) then
    begin
      // Free arrays
      SetLength(s^.layers, 0);
      SetLength(s^.visibleLayers, 0);
      SetLength(s^.values, 0);
      SetLength(s^.layerOffsets, 0);
      
      // Free the name <-> id maps
      FreeNamedIndexCollection(s^.layerIds);
      FreeNamedIndexCollection(s^.valueIds);
      
      // Free pointers
      FreeAnimation(s^.animationInfo);
      
      // Nil pointers to resources managed by sgResources
      s^.animationScript := nil;
      
      //Free buffered rotation image
      if s^.cacheImage <> nil then FreeBitmap(s^.cacheImage);
      if s^.backupCollisionBitmap <> nil then FreeBitmap(s^.collisionBitmap);
      s^.cacheImage := nil;
      s^.collisionBitmap := nil;
      s^.backupCollisionBitmap := nil;
      
      // Remove from hashtable
      // WriteLn('Freeing Sprite named: ', s^.name);
      _Sprites.remove(s^.name).Free();
      
      //Dispose sprite
      CallFreeNotifier(s);
      
      Dispose(s);
    end;
    
    s := nil;
  end;
  
  function SpriteAddLayer(s: Sprite; newLayer: Bitmap; layerName: String): Longint;
  begin
    if newLayer = nil then begin RaiseWarning('Cannot add non-existing bitmap as layer to Sprite'); exit; end;
    if s = nil then begin RaiseWarning('No sprite to add layer to'); exit; end;
    
    result := AddName(s^.layerIds, layerName);
    if (result <> Length(s^.layers)) then begin RaiseException('Error adding layer ' + layerName); exit; end;
    
    //Resize the array
    SetLength(s^.layers, Length(s^.layers) + 1);
    SetLength(s^.layerOffsets, Length(s^.layerOffsets) + 1);
    
    //Add the values to the array
    s^.layers[result] := newLayer;
    s^.layerOffsets[result] := PointAt(0,0);
    
    _UpdateSpriteImageCache(s);
  end;
  
  procedure SpriteReplayAnimation(s: Sprite);
  begin
    SpriteReplayAnimation(s, true);
  end;
  
  procedure SpriteReplayAnimation(s: Sprite; withSound: Boolean);
  begin
    if s = nil then exit;
    
    RestartAnimation(s^.animationInfo, withSound);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; named: String);
  begin
    SpriteStartAnimation(s, named, true);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; named: String; withSound: Boolean);
  var
	idx: Integer;
  begin
	if not Assigned(s) then exit;
	if not Assigned(s^.animationScript) then exit;
	
	idx := AnimationIndex(s^.animationScript, named);
	if (idx < 0) or (idx > High(s^.animationScript^.animations)) then
    begin
		RaiseWarning('Unable to create animation "' + named + '" for sprite ' + s^.name + ' from script ' + s^.animationScript^.name);
		exit;
	end;

    SpriteStartAnimation(s, idx, withSound);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; idx: Longint);
  begin
    SpriteStartAnimation(s, idx, true);
  end;
  
  procedure SpriteStartAnimation(s: Sprite; idx: Longint; withSound: Boolean);
  begin
    if not Assigned(s) then exit;
    if not Assigned(s^.animationScript) then exit;
	if (idx < 0) or (idx > High(s^.animationScript^.animations)) then
    begin
		RaiseWarning('Unable to create animation no. ' + IntToStr(idx) + ' for sprite ' + s^.name + ' from script ' + s^.animationScript^.name);
		exit;
	end;
    
    if Assigned(s^.animationInfo) then
      AssignAnimation(s^.animationInfo, idx, s^.animationScript, withSound)
    else
      s^.animationInfo := CreateAnimation(idx, s^.animationScript, withSound);
    // WriteLn('Sprite Animation: ', HexStr(s^.animationInfo));
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
  begin
    if not Assigned(s) then exit;
    
    UpdateAnimation(s^.animationInfo, pct, withSound);
    // WriteLn('Move ', PointToString(AnimationCurrentVector(s^.animationInfo)), ' ', pct);
    MoveSprite(s, AnimationCurrentVector(s^.animationInfo), pct);
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
  begin
    MoveSprite(s, pct);
    UpdateSpriteAnimation(s, pct, withSound);
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
  begin
    if not Assigned(s) then begin RaiseException('No sprite supplied'); exit; end;
    
    if Assigned(s^.cacheImage) then 
    begin
      DrawCell(s^.cacheImage, SpriteCurrentCell(s), 
        Round(s^.position.x + xOffset), 
        Round(s^.position.y + yOffset));
      exit;
    end;
    
    for i := 0 to High(s^.visibleLayers) do
    begin
      idx := s^.visibleLayers[i];
      DrawCell(SpriteLayer(s, idx), SpriteCurrentCell(s), 
        Round(s^.position.x + xOffset + s^.layerOffsets[idx].x), 
        Round(s^.position.y + yOffset + s^.layerOffsets[idx].y));
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
  begin
    if not Assigned(s) then begin RaiseException('No sprite supplied'); exit; end;
    
    if SpriteRotation(s) <> 0 then
    begin
      trans := RotationMatrix(-SpriteRotation(s));
      mvmt := MatrixMultiply(trans, distance);
    end
    else  
      mvmt := distance;
       
    s^.position.x := s^.position.x + (pct * mvmt.x);
    s^.position.y := s^.position.y + (pct * mvmt.y);
  end;

  procedure MoveSpriteTo(s : Sprite; x,y : Longint);
  begin
    if s = nil then begin RaiseException('No sprite supplied'); exit; end;
  
    s^.position.x := x;
    s^.position.y := y;
  end;

  procedure MoveSprite(s: Sprite); overload;
  begin
    MoveSprite(s, 1.0);
  end;  

  procedure MoveSprite(s: Sprite; pct: Single); overload;
  begin
    MoveSprite(s, s^.velocity, pct);
  end;

  function SpriteOnScreenAt(s: Sprite; x, y: Longint): Boolean; overload;
  var
    cellRect: Rectangle;
    wx, wy: Single;
  begin
    result := false;
    
    if not Assigned(s) then exit;
    
    wx := ToWorldX(x);
    wy := ToWorldY(y);
    
    if wy > s^.position.y + SpriteHeight(s) then      result := false
    else if wy < s^.position.y then                   result := false
    else if wx > s^.position.x + SpriteWidth(s) then  result := false
    else if wx < s^.position.x then                   result := false
    else if s^.collisionKind = AABBCollisions then    result := true
    else
    begin
      cellRect := SpriteCurrentCellRectangle(s);
      result := PixelDrawnAtPoint(s^.collisionBitmap, Round(wx - s^.position.x + cellRect.x), Round(wy - s^.position.y + cellRect.y));
    end;
  end;

  function SpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;
  begin
    result := SpriteOnScreenAt(s, Round(pt.x), Round(pt.y));
  end;

  function SpriteVelocity(s: Sprite): Vector;
  begin
    if not Assigned(s) then 
    begin 
      result := VectorTo(0,0);
      exit;
    end;

    result := s^.velocity;
  end;

  procedure SpriteSetVelocity(s: Sprite; const value: Vector);
  begin
    if not Assigned(s) then 
    begin 
      exit;
    end;

    s^.velocity := value;
  end;
  
  procedure SpriteAddToVelocity(s: Sprite; const value: Vector);
  begin
    if not Assigned(s) then 
    begin 
      exit;
    end;

    s^.velocity := AddVectors(s^.velocity, value);
  end;
  
  function SpriteCurrentCellRectangle(s: Sprite): Rectangle;
  begin
    if (not Assigned(s)) then
      result := RectangleFrom(0,0,0,0)
    else
      result := BitmapRectangleOfCell(s^.layers[0], AnimationCurrentCell(s^.animationInfo));
  end;
  
  function SpriteScreenRectangle(s: Sprite): Rectangle;
  begin
    if (not Assigned(s)) or (not Assigned(s^.animationInfo)) then
      result := RectangleFrom(0,0,0,0)
    else
      result := ToScreen(SpriteLayerRectangle(s, 0));
  end;
  
  procedure SpriteSetX(s: Sprite; value: Single);
  begin
    if not Assigned(s) then 
    begin 
      exit;
    end;

    s^.position.x := value;
  end;
    
  function SpriteX(s: Sprite): Single;
  begin
    if not Assigned(s) then 
    begin 
      result := 0;
      exit;
    end;

    result := s^.position.x;
  end;
  
  procedure SpriteSetY(s: Sprite; value: Single);
  begin
    if not Assigned(s) then 
    begin 
      exit;
    end;

    s^.position.y := value;
  end;
    
  function SpriteY(s: Sprite): Single;
  begin
    if not Assigned(s) then 
    begin 
      result := 0;
      exit;
    end;

    result := s^.position.y;
  end;
  
  function SpriteLayer(s: Sprite; name: String): Bitmap;
  begin
    if not Assigned(s) then result := nil
    else result := SpriteLayer(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteLayer(s: Sprite; idx: Longint): Bitmap;
  begin
    if not Assigned(s) then result := nil
    else if (idx < 0) or (idx > High(s^.layers)) then begin result := nil; RaiseException('Sprite layer index out of range - ' + IntToStr(idx)); exit; end
    else result := s^.layers[idx];
  end;
  
  function SpriteLayerIndex(s: Sprite; name: String): Longint;
  begin
    if not Assigned(s) then result := -1
    else result := IndexOf(s^.layerIds, name);
  end;
  
  function SpriteLayerName(s: Sprite; idx: Longint): String;
  begin
    if not Assigned(s) then result := ''
    else result := NameAt(s^.layerIds, idx);
  end;
  
  function SpriteShowLayer(s: Sprite; name: String): Longint;
  begin
    if not Assigned(s) then result := -1
    else result := SpriteShowLayer(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteShowLayer(s: Sprite; id: Longint): Longint;
  begin
    if not Assigned(s) then result := -1
    else
    begin
      if (id < 0) or (id > Length(s^.layers)) then begin result := -1; RaiseWarning('Cannot show layer ' + IntToStr(id) + ' of sprite.'); exit; end;
      
      //Scan for the current ID
      result := SpriteVisibleIndexOfLayer(s, id);
      if result >= 0 then exit;
      
      //Extend layers and add index
      SetLength(s^.visibleLayers, Length(s^.visibleLayers) + 1);
      result := High(s^.visibleLayers);
      s^.visibleLayers[result] := id;
      
      _UpdateSpriteImageCache(s);
    end;
  end;
  
  procedure SpriteHideLayer(s: Sprite; name: String);
  begin
    if not Assigned(s) then exit
    else SpriteHideLayer(s, IndexOf(s^.layerIds, name));
  end;
  
  procedure SpriteHideLayer(s: Sprite; id: Longint);
  var
    i, idx: Longint;
  begin
    if not Assigned(s) then exit;

    idx := SpriteVisibleIndexOfLayer(s, id);
    if idx < 0 then exit; // The layer is not shown
    
    //Shift all later layers back over removed layer
    for i := idx to High(s^.visibleLayers) - 1 do
    begin
      s^.visibleLayers[i] := s^.visibleLayers[i + 1];
    end;
    
    //Resize the array to remove element
    SetLength(s^.visibleLayers, Length(s^.visibleLayers) - 1);
    _UpdateSpriteImageCache(s);
  end;
  
  procedure SpriteToggleLayerVisible(s: Sprite; name: String); overload;
  begin
    if not Assigned(s) then exit
    else SpriteToggleLayerVisible(s, IndexOf(s^.layerIds, name));
  end;
  
  procedure SpriteToggleLayerVisible(s: Sprite; id: Longint); overload;
  begin
    if SpriteVisibleIndexOfLayer(s, id) < 0 then 
      SpriteShowLayer(s, id)
    else
      SpriteHideLayer(s, id);
  end;
  
  function SpriteLayerCount(s: Sprite): Longint;
  begin
    if not Assigned(s) then result := 0
    else result := Length(s^.layers);
  end;
  
  function SpriteVisibleLayerCount(s: Sprite): Longint;
  begin
    if not Assigned(s) then result := 0
    else result := Length(s^.visibleLayers);
  end;
  
  function SpriteVisibleLayerIds(s: Sprite) : LongintArray;
  begin
    if not Assigned(s) then result := nil
    else result := s^.visibleLayers;
  end;
  
  function SpriteLayers(s: Sprite): BitmapArray;
  begin
    if not Assigned(s) then result := nil
    else result := s^.layers;
  end;
  
  function SpriteLayerOffsets(s: Sprite): Point2DArray;
  begin
    if not Assigned(s) then begin result := nil; exit; end;
    result := s^.layerOffsets;
  end;
  
  procedure SpriteSetLayerOffsets(s: Sprite; const values: Point2DArray);
  var
    i: Longint;
  begin
    if not Assigned(s) then exit;
    if not Length(values) = Length(s^.layerOffsets) then begin RaiseException('Unable to set sprite layer offsets as lengths are not equal.'); exit; end;
    
    for i := 0 to High(values) do
    begin
      s^.layerOffsets[i] := values[i];
    end;
    _UpdateSpriteImageCache(s);
  end;
  
  function SpriteLayerOffset(s: Sprite; name: String): Point2D;
  begin
    if not Assigned(s) then result := PointAt(0,0)
    else result := SpriteLayerOffset(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteLayerOffset(s: Sprite; idx: Longint): Point2D;
  begin
    if not Assigned(s) then result := PointAt(0,0)
    else if (idx < 0) or (idx >= Length(s^.layerOffsets)) then begin RaiseException('Error fetching layer offset out of range.'); result := PointAt(0,0); exit; end
    else result := s^.layerOffsets[idx];
  end;
  
  procedure SpriteSetLayerOffset(s: Sprite; name: String; const value: Point2D);
  begin
    if Assigned(s) then
      SpriteSetLayerOffset(s, IndexOf(s^.layerIds, name), value);
  end;
  
  procedure SpriteSetLayerOffset(s: Sprite; idx: Longint; const value: Point2D);
  begin
    if Assigned(s) then
      s^.layerOffsets[idx] := value;
  end;
  
  function SpriteVisibleIndexOfLayer(s: Sprite; name: String): Longint;
  begin
    if not Assigned(s) then result := -1
    else result := SpriteVisibleIndexOfLayer(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteVisibleIndexOfLayer(s: Sprite; id: Longint): Longint;
  var
    i: Longint;
  begin
    result := -1;
    if not Assigned(s) then exit
    else
    begin
      for i := 0 to High(s^.visibleLayers) do
      begin
        if s^.visibleLayers[i] = id then 
        begin
          result := i;
          exit;
        end;
      end;
    end;
  end;
  
  function SpriteVisibleLayer(s: Sprite; idx: Longint): Longint;
  begin
    result := -1;
    if not Assigned(s) then exit
    else result := s^.visibleLayers[idx];
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
  begin
    if not Assigned(s) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(s^.visibleLayers) - 1) then exit;
    
    // Bubble layer up
    for i := visibleLayer to High(s^.visibleLayers) - 1 do
    begin
      Swap(s^.visibleLayers[i], s^.visibleLayers[i + 1]);
    end;
    
    _UpdateSpriteImageCache(s);
  end;
  
  procedure SpriteSendLayerBackward(s: Sprite; visibleLayer: Longint);
  begin
    if not Assigned(s) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(s^.visibleLayers) - 1) then exit;
    
    Swap(s^.visibleLayers[visibleLayer], s^.visibleLayers[visibleLayer + 1]);
    _UpdateSpriteImageCache(s);
  end;
  
  procedure SpriteBringLayerForward(s: Sprite; visibleLayer: Longint);
  begin
    if not Assigned(s) then exit;
    //Check not first or lower
    if (visibleLayer < 1) or (visibleLayer >= Length(s^.visibleLayers)) then exit;
    
    Swap(s^.visibleLayers[visibleLayer], s^.visibleLayers[visibleLayer - 1]);
    _UpdateSpriteImageCache(s);
  end;
  
  procedure SpriteBringLayerToFront(s: Sprite; visibleLayer: Longint);
  var
    i: Longint;
  begin
    if not Assigned(s) then exit;
    //Check not last or beyond in array
    if (visibleLayer < 0) or (visibleLayer >= Length(s^.visibleLayers) - 1) then exit;
    
    // Bubble layer down
    for i := visibleLayer downto 1 do
    begin
      Swap(s^.visibleLayers[i], s^.visibleLayers[i - 1]);
    end;
    
    _UpdateSpriteImageCache(s);
  end;
  
  function SpriteLayerRectangle(s: Sprite; name: String): Rectangle; overload;
  begin
    if not Assigned(s) then result := RectangleFrom(0,0,0,0)
    else result := SpriteLayerRectangle(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle', '');
    {$ENDIF}
    
    if not Assigned(s) then result := RectangleFrom(0,0,0,0)
    else result := BitmapCellRectangle(s^.position.x + s^.layerOffsets[idx].x, s^.position.y + s^.layerOffsets[idx].y, s^.layers[idx]);
      
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'SpriteLayerRectangle(s: Sprite; idx: Longint): Rectangle', '');
    {$ENDIF}
  end;
  
  function SpriteCollisionRectangle(s: Sprite): Rectangle;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'SpriteCollisionRectangle(s: Sprite): Rectangle', '');
    {$ENDIF}
    
    if not Assigned(s) then result := RectangleFrom(0,0,0,0)
    else result := BitmapCellRectangle(s^.position.x, s^.position.y, s^.collisionBitmap);
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'SpriteCollisionRectangle(s: Sprite): Rectangle', '');
    {$ENDIF}
  end;
  
  function SpriteCircle(s: Sprite): Circle; overload;
  begin
    result := SpriteLayerCircle(s, 0);
  end;
  
  function SpriteLayerCircle(s: Sprite; name: String): Circle; overload;
  begin
    if not Assigned(s) then result := CircleAt(0,0,0)
    else result := SpriteLayerCircle(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteLayerCircle(s: Sprite; idx: Longint): Circle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'SpriteLayerCircle(s: Sprite): Circle', '');
    {$ENDIF}
    
    if not Assigned(s) then result := CircleAt(0, 0, 0)
    else if (idx < 0) or (idx > High(s^.layers)) then begin RaiseException('Layer out of range in SpriteLayerCircle.'); result := CircleAt(0,0,0); exit; end
    else
    begin
      result := BitmapCellCircle(s^.layers[idx], CenterPoint(s));
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'SpriteLayerCircle(s: Sprite): Circle', '');
    {$ENDIF}
  end;
  
  function SpriteCollisionCircle(s: Sprite): Circle;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'SpriteLayerCircle(s: Sprite): Circle', '');
    {$ENDIF}
    
    if (not Assigned(s)) or (not Assigned(s^.collisionBitmap)) then result := CircleAt(0, 0, 0)
    else result := BitmapCellCircle(s^.collisionBitmap, CenterPoint(s));
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'SpriteLayerCircle(s: Sprite): Circle', '');
    {$ENDIF}
    
  end;
  
  
//---------------------------------------------------------------------------
// Sprite position
//---------------------------------------------------------------------------
  
  function SpritePosition(s: Sprite): Point2D;
  begin
    if not Assigned(s) then result := PointAt(0,0)
    else result := s^.position;
  end;
  
  procedure SpriteSetPosition(s: Sprite; const value: Point2D);
  begin
    if Assigned(s) then s^.position := value;
  end;
  
//---------------------------------------------------------------------------
// Sprite DX,DY
//---------------------------------------------------------------------------
  
  procedure SpriteSetDX(s: Sprite; value: Single);
  begin
    if Assigned(s) then s^.velocity.x := value;
  end;
  
  function SpriteDX(s: Sprite): Single;
  begin
    if not Assigned(s) then result := 0
    else result := s^.velocity.x;
  end;
  
  procedure SpriteSetDY(s: Sprite; value: Single);
  begin
    if Assigned(s) then s^.velocity.y := value;
  end;
  
  function SpriteDY(s: Sprite): Single;
  begin
    if not Assigned(s) then result := 0
    else result := s^.velocity.y;
  end;
  
//---------------------------------------------------------------------------
// Sprite speed and heading
//---------------------------------------------------------------------------
  
  function SpriteSpeed(s: Sprite): Single;
  begin
    if not Assigned(s) then result := 0
    else result := VectorMagnitude(s^.velocity);
  end;
  
  procedure SpriteSetSpeed(s: Sprite; value: Single);
  begin
    if Assigned(s) then s^.velocity := VectorMultiply(UnitVector(s^.velocity), value);
  end;
  
  function SpriteHeading(s: Sprite): Single;
  begin
    if not Assigned(s) then result := 0
    else result := VectorAngle(s^.velocity);
  end;
  
  procedure SpriteSetHeading(s: Sprite; value: Single);
  begin
    if Assigned(s) then s^.velocity := VectorFromAngle(value, VectorMagnitude(s^.velocity));
  end;
  
//---------------------------------------------------------------------------
// Sprite Current cell
//---------------------------------------------------------------------------
  
  function SpriteCurrentCell(s: Sprite): Longint;
  begin
    if not Assigned(s) then result := -1
    else result := AnimationCurrentCell(s^.animationInfo);
  end;
  
  function SpriteAnimationHasEnded(s: Sprite): Boolean;
  begin
    if not Assigned(s) then result := false
    else result := AnimationEnded(s^.animationInfo);
  end;
  
//---------------------------------------------------------------------------
// Sprite width/height
//---------------------------------------------------------------------------
  
  function SpriteLayerHeight(s: Sprite; name: String): Longint; overload;
  begin
    if not Assigned(s) then result := 0
    else result := SpriteLayerHeight(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteLayerHeight(s: Sprite; idx: Longint): Longint; overload;
  begin
    if not Assigned(s) then result := 0
    else if (idx < 0) or (idx >= Length(s^.layers)) then result := 0
    else result := s^.layers[idx]^.cellH;
  end;
  
  function SpriteLayerWidth(s: Sprite; name: String): Longint; overload;
  begin
    if not Assigned(s) then result := 0
    else result := SpriteLayerWidth(s, IndexOf(s^.layerIds, name));
  end;
  
  function SpriteLayerWidth(s: Sprite; idx: Longint): Longint; overload;
  begin
    if not Assigned(s) then result := 0
    else if (idx < 0) or (idx >= Length(s^.layers)) then result := 0
    else result := s^.layers[idx]^.cellW;
  end;  
  
  function SpriteWidth(s: Sprite): Longint;
  begin
    result := SpriteLayerWidth(s, 0);
  end;
  
  function SpriteHeight(s: Sprite): Longint;
  begin
    result := SpriteLayerHeight(s, 0);
  end;
  
  
//---------------------------------------------------------------------------
// Sprite mass
//---------------------------------------------------------------------------
  
  function SpriteMass(s: Sprite): Single;
  begin
    if not Assigned(s) then result := 0
    else result := s^.values[MASS_IDX];
  end;
  
  procedure SpriteSetMass(s: Sprite; value: Single);
  begin
    if Assigned(s) then s^.values[MASS_IDX] := value;
  end;
  
//---------------------------------------------------------------------------
// Sprite rotation
//---------------------------------------------------------------------------
  
  function SpriteRotation(s: Sprite): Single;
  begin
    if not Assigned(s) then result := 0
    else result := s^.values[ROTATION_IDX];
  end;
  
  procedure SpriteSetRotation(s: Sprite; value: Single);
  begin
    if Assigned(s) then 
    begin
      s^.values[ROTATION_IDX] := value;
      _UpdateSpriteImageCache(s);
    end;
  end;
  
//---------------------------------------------------------------------------
// Sprite scale
//---------------------------------------------------------------------------
  
  function SpriteScale(s: Sprite): Single;
  begin
    if not Assigned(s) then result := 0
    else result := s^.values[SCALE_IDX];
  end;
  
  procedure SpriteSetScale(s: Sprite; value: Single);
  begin
    if Assigned(s) then 
    begin
      s^.values[SCALE_IDX] := value;
      _UpdateSpriteImageCache(s);
    end;
  end;
  
//---------------------------------------------------------------------------
// Sprite center point
//---------------------------------------------------------------------------
  
  function CenterPoint(s: Sprite): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgSprites', 'CenterPoint(s: Sprite): Point2D', '');
    {$ENDIF}

    if not Assigned(s) then
    begin
      result := PointAt(0,0);
      exit;
    end;

    result.x := s^.position.x + SpriteWidth(s) / 2;
    result.y := s^.position.y + SpriteHeight(s) / 2;
    
    {$IFDEF TRACE}
      TraceExit('sgSprites', 'CenterPoint(s: Sprite): Point2D', '');
    {$ENDIF}
  end;
  
//---------------------------------------------------------------------------
// Sprite collision details
//---------------------------------------------------------------------------
  
  function SpriteCollisionKind(s: Sprite): CollisionTestKind;
  begin
    if not Assigned(s) then result := AABBCollisions
    else result := s^.collisionKind;
  end;
  
  procedure SpriteSetCollisionKind(s: Sprite; value: CollisionTestKind);
  begin
    if Assigned(s) then s^.collisionKind := value;
  end;
  
  function SpriteCollisionBitmap(s: Sprite): Bitmap;
  begin
    if not Assigned(s) then result := nil
    else result := s^.collisionBitmap;
  end;
    
  procedure SpriteSetCollisionBitmap(s: Sprite; bmp: Bitmap);
  begin
    if Assigned(s) then s^.collisionBitmap := bmp;
  end;
  
//---------------------------------------------------------------------------
// Sprite value code
//---------------------------------------------------------------------------
  
  function SpriteValueCount(s: Sprite) : Longint;
  begin
    result := -1;
    if not Assigned(s) then exit;
    
    result := NameCount(s^.valueIds);
  end;
  
  function SpriteValueNames(s: Sprite) : StringArray; 
  begin
    SetLength(result, 0);
    if not Assigned(s) then exit;
    
    result := NamesOf(s^.valueIds);
  end;
  
  function SpriteValue(s: Sprite; index: Longint): Single; overload;
  begin
    result := 0;
    if not Assigned(s) then exit;
    
    result := s^.values[index];
  end;
  
  function SpriteValue(s: Sprite; name: String): Single; overload;
  begin
    result := 0;
    if not Assigned(s) then exit;
    
    result := SpriteValue(s, IndexOf(s^.valueIds, name));
  end;
  
  procedure SpriteAddValue(s: Sprite; name: String);
  begin
    SpriteAddValue(s, name, 0);
  end;
  
  procedure SpriteAddValue(s: Sprite; name: String; initVal: Single);
  var
    idx: Longint;
  begin
    if not Assigned(s) then exit;
    if HasName(s^.valueIds, name) then exit;
    
    idx := AddName(s^.valueIds, name);
    SetLength(s^.values, Length(s^.values) + 1);
    s^.values[idx] := initVal;
  end;
  
  procedure SpriteSetValue(s: Sprite; name: String; val: Single); overload;
  begin
    if not Assigned(s) then exit;
    
    SpriteSetValue(s, IndexOf(s^.valueIds, name), val);
  end;
  
  procedure SpriteSetValue(s: Sprite; idx: Longint; val: Single); overload;
  begin
    if not Assigned(s) then exit;
    if (idx < 0) or (idx > High(s^.values)) then exit;
    
    s^.values[idx] := val;
  end;
  
  function SpriteName(sprt: Sprite): String;
  begin
    result := '';
    if not Assigned(sprt) then exit;
    result := sprt^.name;
  end;


//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
    
    _Sprites := TStringHash.Create(False, 16384);
  end;
  
  finalization
  begin
    ReleaseAllSprites();
    FreeAndNil(_Sprites);
  end;
  
end.