//=============================================================================
// sgPhysics.pas
//=============================================================================
//
// Responsible for performing collisions and vector maths.
//
// Change History:
//
// Version 3.0:
// - 2009-12-18: Andrew : Updated to use new sprite format
// - 2009-12-17: Andrew : Removed bitmap bbox collision tests (use rect versions)
//                      : Added Cell collision functions
// - 2009-06-29: Andrew : Removed all need for Collision Side
//                      : Changed to use Circle Type
// - 2009-06-26: Andrew : Added CircleRectCollision
//                      : Added CircleLinesCollision
//                      : Added CircleCircleCollision
//                      : Added CollideCircleCircle
// - 2009-06-25: Andrew : Moved VectorInRect, VectorFrom... functions to Geometry
// - 2009-06-24: Andrew : Added BitmapPointCollision 
// - 2009-06-23: Clinton: Renamed VectorFrom to VectorFrom
//                      : Renamed HaveSpritesCollided to SpritesCollided
//                      : Move Vector/Angle/Matrix code to sgMath.pas unit
// - 2009-06-17: Clinton: Comment cleanup (moved to interface) and new comments
//                      : General parameter name cleanup/normalisation
//                      : Renamed GetUnitVector to UnitVector
//                      : Optimised LimitMagnitude (see renamed)
//                      : Optimised VectorNormal
//                      : Renamed GetVectorFromAngle to VectorFromAngle
//                      : Renamed MultiplyVector to VectorMultiply
//                      : Renamed Multiply to MatrixMultiply
//                      : Renamed CalculateVectorFromTo to VectorFromTo
//                      : Renamed PointToVector to VectorFromPoint
//                      : Renamed CalculateAngleBetween to CalculateAngle
//                      : Renamed LimitMagnitude to LimitVector
//                      : Renamed VectorIsWithinRect to VectorInRect
//                      : Renamed RectangleHasCollidedWithLine to RectLineCollision
//                      : Renamed IsZeroVector to VectorIsZero
//                      : Renamed HasSpriteCollidedWithRect to SpriteRectCollision
//                      : Renamed HasSpriteCollidedWithBitmap to SpriteBitmapCollision
//                      : Renamed bounded (params) to bbox (or BBox in method)
//                      : Renamed HasBitmapCollidedWithRect with BitmapRectCollision
//                      : Renamed HasBitmapPartCollidedWithRect to BitmapPartRectCollision
//                      : Renamed VectorFromPointToRectangle to VectorFromPointToRect
//                      : Renamed CircleHasCollidedWithLine to CircleLineCollision
//                      : Removed VectorCollision (was renamed to CircleCollision)
//                      : Renamed CircleCollisionWithLine to CollideCircleLine
//                      : Renamed CircularCollision to CollideCircles
//                      : Renamed Magnitude to VectorMagnitude
//                      : Optimised VectorOutOfCircleFromPoint (slightly)
// 
// - 2009-06-15: Andrew: Added meta tags
//
// Version 2.0:
// - 2008-12-10: Andrew: Moved types to Core
//
// Version 1.1:
// - 2008-01-30: Andrew: Fixed rectangle collision with bitmap
//                     : Fixed vector out for 0, 90, 180, 270 deg
//                     : Fixed GetSideForCollisionTest for same deg of movement
// - 2008-01-25: Andrew: Fixed compiler hints
// - 2008-01-22: Andrew: Correct Circular Collision to handle situations where 
//                       the balls have overlaped.
// - 2008-01-21: Andrew: General refactoring, adding new collision routines
//               using Rectangle and Point2D.
// - 2008-01-18: Aki, Andrew, Stephen: Refactor
//
// Version 1.0:
// - Various
//=============================================================================



/// The physics contains code to check and perform collisions between bitmaps and shapes
/// in SwinGame. This code will help you determine if two sprites have hit each other, 
/// or allow you to bounce one sprite off another or off a stationary shape. 
/// 
/// The functions that contain the text "collision" are used to determine if two 
/// ``things`` have collided, for example `BitmapPointCollision` is used to determine 
/// if a bitmap (with a specified location) has collided with a given point.
///
/// The procedures that contain the text "collide" are used to perform a "bounce"
/// style collision, where a sprite is bounced off some other ``thing``. For example
/// `CollideCircleLine` is used to bounce a sprite (which should be treated like a circle)
/// off a given line.
/// 
/// @module Physics
/// @static
unit sgPhysics;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  
//---------------------------------------------------------------------------
// Sprite <-> Sprite Collision Detection
//---------------------------------------------------------------------------
  
  /// Returns ``true`` if the specifed sprites (``s1`` and ``s2``) have
  /// collided. Will use simple bounding box tests first, and low-level pixel
  /// tests if needed.
  ///
  /// @lib
  /// @sn sprite:%s collisionWithSprite:%s
  ///
  /// @class Sprite
  /// @method CollisionWithSprite
  function SpriteCollision(s1, s2: Sprite): Boolean;
  
  
  
//---------------------------------------------------------------------------
// Sprite <-> Rectangle Collision Detection
//---------------------------------------------------------------------------
  
  /// Determined if a sprite has collided with a given rectangle. The rectangles
  /// coordinates are expressed in "world" coordinates.
  ///
  /// @lib
  /// @sn sprite:%s collisionWithRectX:%s y:%s width:%s height:%s
  ///
  /// @class Sprite
  /// @method RectCollision
  /// @csn collisionWithRectX:%s y:%s width:%s height:%s
  function SpriteRectCollision(s: Sprite; x, y: Single; width, height: Longint): Boolean; overload;
  
  /// Returns true if the sprite has collided with a rectangle.
  /// 
  /// @lib SpriteRectangleCollision
  /// @sn sprite:%s collisionWithRect:%s
  /// 
  /// @class Sprite
  /// @overload RectCollision RectangleCollision
  /// @csn collisionWithRect:%s
  function SpriteRectCollision(s: Sprite; const r: Rectangle): Boolean; overload;
  
  
  
//---------------------------------------------------------------------------
// Sprite <-> Bitmap Collision Detection
//---------------------------------------------------------------------------
  
  /// Determines if the `Sprite` ``s`` has collided with the bitmap ``bmp`` using
  /// pixel level testing if required.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  ///
  /// @lib
  /// @sn sprite:%s collisionWithBitmap:%s x:%s y:%s
  ///
  /// @uname SpriteBitmapCollision
  /// @class Sprite
  /// @method BitmapCollision
  /// @csn collisionWithBitmap:%s x:%s y:%s
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; x, y: Single): Boolean; overload;
  
  /// Determines if the `Sprite` ``s`` has collided with the bitmap ``bmp`` using
  /// pixel level testing if required.
  /// The ``pt`` (`Point2D`) value specifies the world location of the bitmap.
  /// 
  /// @lib SpriteBitmapAtPointCollision
  /// @sn sprite:%s collisionWithBitmap:%s at:%s
  /// 
  /// @class Sprite
  /// @overload BitmapCollision BitmapAtPointCollision
  /// @csn collisionWithBitmap:%s at:%s
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D): Boolean; overload;
  
  
  
//---------------------------------------------------------------------------
// Bitmap <-> Rectangle Collision Tests
//---------------------------------------------------------------------------
  
  /// Returns True if the bitmap ``bmp`` has collided with the rectangle
  /// specified using pixel level testing if required.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The rectangles world position (``rectX`` and ``rectY``) and size
  /// (``rectWidth`` and ``rectHeight``) need to be provided.
  /// 
  /// @lib BitmapRectCollision
  /// @sn bitmap:%s collisionAtX:%s y:%s withRectX:%s y:%s width:%s height:%s
  /// 
  /// @class Bitmap
  /// @method RectCollision
  /// @csn collisionAtX:%s y:%s withRectX:%s y:%s width:%s height:%s
  function BitmapRectCollision(bmp: Bitmap; x, y, rectX, rectY, rectWidth, rectHeight: Longint): Boolean; overload;
  
  /// Returns True if the bitmap ``bmp`` has collided with the rectangle
  /// specified using pixel level testing if required.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The rectangle ``rect`` needs to be provided in world coordinates.
  /// 
  /// @lib BitmapRectangleCollision
  /// @sn bitmap:%s collisionAtX:%s y:%s withRect:%s
  /// 
  /// @class Bitmap
  /// @overload RectCollision RectangleCollision
  /// @csn collisionAtX:%s y:%s withRect:%s
  function BitmapRectCollision(bmp: Bitmap; x, y: Longint; const rect: Rectangle): Boolean; overload;
  
  /// Returns True if the indicated part of the bitmap has collided with the specified
  /// rectangle.
  ///
  /// @lib BitmapPartRectCollision
  /// @sn bitmap:%s collisionAtX:%s y:%s part:%s withRect:%s
  ///
  /// @class Bitmap
  /// @overload RectCollision RectPartCollision
  /// @csn collisionAtX:%s y:%s part:%s withRect:%s
  function BitmapRectCollision(bmp: Bitmap; x, y: Longint; const part, rect: Rectangle): Boolean; overload;
  
  /// Returns True if the indicated part of the bitmap has collided with the specified
  /// rectangle.
  ///
  /// @lib BitmapPartAtPtRectCollision
  /// @sn bitmap:%s atPt:%s part:%s collisionWithRect:%s
  ///
  /// @class Bitmap
  /// @overload RectCollision RectPartCollisionAtPoint
  /// @csn atPt:%s part:%s collisionWithRect:%s
  function BitmapRectCollision(bmp: Bitmap; const pt:Point2D; const part, rect: Rectangle): Boolean; overload;
  
  
  
//---------------------------------------------------------------------------
// Bitmap <-> Point
//---------------------------------------------------------------------------
  
  /// Returns True if a point (``ptX``,``ptY``) is located within the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The ``ptX`` and ``ptY`` needs to be provided in world coordinates.
  /// 
  /// @lib BitmapPointCollision
  /// @sn bitmap:%s atX:%s y:%s collisionWithPtX:%s y:%s
  /// 
  /// @class Bitmap
  /// @method PointCollision
  /// @csn atX:%s y:%s collisionWithPtX:%s y:%s
  function BitmapPointCollision(bmp: Bitmap; x, y: Longint; ptX, ptY: Single): Boolean; overload;
  
  /// Returns True if a point (``pt``) is located within the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The point ``pt`` needs to be provided in world coordinates.
  ///
  /// @lib BitmapPointPtCollision
  /// @sn bitmap:%s atX:%s y:%s collisionWithPt:%s
  /// 
  /// @class Bitmap
  /// @overload PointCollision PointPtCollision
  /// @csn atX:%s y:%s collisionWithPt:%s
  function BitmapPointCollision(bmp: Bitmap; x, y: Longint; const pt: Point2D): Boolean; overload;
  
  /// Returns True if a point (``ptX``,``ptY``) is located within the ``part`` (rectangle) of the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions. For bounding box collisions
  /// use the rectangle collision functions.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The ``ptX`` and ``ptY`` needs to be provided in world coordinates.
  ///
  /// @lib
  /// @sn bitmap:%s atX:%s y:%s part:%s collisionWithPtX:%s y:%s
  ///
  /// @class Bitmap
  /// @overload PointCollision PointPartCollision
  /// @csn atX:%s y:%s part:%s collisionWithPtX:%s y:%s
  function BitmapPartPointCollision(bmp: Bitmap; x, y: Longint; const part: Rectangle; ptX, ptY: Single): Boolean; overload;
  
  /// Returns True if a point (``pt``) is located within the ``part`` (rectangle) of the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions. For bounding box collisions
  /// use the rectangle collision functions.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The point ``pt`` needs to be provided in world coordinates.
  ///  
  /// @lib  BitmapPartPointXYCollision
  /// @sn bitmap:%s atX:%s y:%s part:%s collisionWithPt:%s
  /// 
  /// @class Bitmap
  /// @overload PointCollision PointPartCollisionWithPt
  /// @csn atX:%s y:%s part:%s collisionWithPt:%s
  function BitmapPartPointCollision(bmp: Bitmap; x, y: Longint; const part: Rectangle; const pt: Point2D): Boolean; overload;
  
  
  
//---------------------------------------------------------------------------
// Bitmap <-> Bitmap Collision Tests
//---------------------------------------------------------------------------
  
  /// Returns True if two bitmaps have collided using per pixel testing if required.
  /// The ``x`` and ``y`` parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``).
  /// 
  /// @lib BitmapCollision
  /// @sn bitmap:%s atX:%s y:%s collisionWithBitmap:%s atX:%s y:%s
  /// 
  /// @class Bitmap
  /// @method BitmapCollision
  /// @csn atX:%s y:%s collisionWithBitmap:%s atX:%s y:%s
  function BitmapCollision(bmp1: Bitmap; x1, y1: Longint; bmp2: Bitmap; x2, y2: Longint): Boolean; overload;
  
  /// Returns True if two bitmaps have collided using per pixel testing if required. 
  /// The ``pt1`` and ``pt2`` (`Point2D`) parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``).
  /// 
  /// @lib BitmapAtPointsCollision
  /// @sn bitmap:%s at:%s collisionWithBitmap:%s atPt:%s
  /// 
  /// @class Bitmap
  /// @overload BitmapCollision BitmapAtPointCollision
  /// @csn at:%s collisionWithBitmap:%s atPt:%s
  function BitmapCollision(bmp1: Bitmap; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D): Boolean; overload;
  
  /// Returns True if the specified parts (``part1`` and ``part2`` rectangles) of the two 
  /// bitmaps (``bmp1`` and ``bmpt2``) have collided, using pixel level collision if required. 
  /// The ``pt1`` and ``pt2`` (`Point2D`) parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``). 
  /// 
  /// @lib BitmapsPartsCollision
  /// @sn bitmap:%s at:%s part:%s collisionWith:%s at:%s part:%s
  /// 
  /// @class Bitmap
  /// @overload BitmapCollision BitmapPartCollision
  /// @csn at:%s part:%s collisionWith:%s at:%s part:%s
  function BitmapCollision(bmp1: Bitmap; const pt1: Point2D; const part1: Rectangle; bmp2: Bitmap; const pt2: Point2D; const part2: Rectangle): Boolean; overload;
  
  
  
//---------------------------------------------------------------------------
// Cell based Collision Tests
//---------------------------------------------------------------------------
  
  /// Returns true if the cells within the two bitmaps have collided at their specified x,y locations.
  ///
  /// @lib
  /// @sn bitmap:%s cell:%s atX:%s y:%s collisionWithBitmap:%s cell:%s atX:%s y:%s
  ///
  /// @class Bitmap
  /// @overload CellCollision CellCollisionXY
  /// @csn cell:%s atX:%s y:%s collisionWithBitmap:%s cell:%s atX:%s y:%s
  function CellCollision( bmp1: Bitmap; cell1, x1, y1: Longint; 
                          bmp2: Bitmap; cell2, x2, y2: Longint): Boolean; overload;
  
  /// Returns true if the cells within the two bitmaps have collided at the given points.
  ///
  /// @lib CellCollisionAtPt
  /// @sn bitmap:%s cell:%s at:%s collisionWithBitmap:%s cell:%s at:%s
  ///
  /// @class Bitmap
  /// @method CellCollision
  /// @csn cell:%s at:%s collisionWithBitmap:%s cell:%s at:%s
  function CellCollision( bmp1: Bitmap; cell1: Longint; const pt1: Point2D; 
                          bmp2: Bitmap; cell2: Longint; const pt2: Point2D): Boolean; overload;
  
  /// Returns true if the cell in the specified bitmap has collided with a bitmap.
  /// 
  /// @lib
  /// @sn bitmap:%s cell:%s atX:%s y:%s collisionWithBitmap:%s atX:%s y:%s
  /// 
  /// @class Bitmap
  /// @method CellBitmapCollision
  /// @csn cell:%s atX:%s y:%s collisionWithBitmap:%s atX:%s y:%s
  function CellBitmapCollision(bmp1: Bitmap; cell, x1, y1: Longint; 
                              bmp2: Bitmap; x2, y2: Longint): Boolean; overload;
  
  /// Returns true if the cell in the specified bitmap has collided with a bitmap.
  /// 
  /// @lib CellBitmapCollisionAtPt
  /// @sn bitmap:%s cell:%s at:%s collisionWithBitmap:%s at:%s
  /// 
  /// @class Bitmap
  /// @overload CellBitmapCollision CellBitmapCollisionAtPt
  /// @csn cell:%s at:%s collisionWithBitmap:%s at:%s
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; const pt1: Point2D; 
                              bmp2: Bitmap; const pt2: Point2D): Boolean; overload;
  
  /// Returns true if the cell in the specified bitmap has collided with a part of a bitmap.
  /// 
  /// @lib CellBitmapPartCollision
  /// @sn bitmap:%s cell:%s atX:%s y:%s collisionWithBitmap:%s atX:%s y:%s part:%s
  /// 
  /// @class Bitmap
  /// @overload CellBitmapCollision CellBitmapPartCollision
  /// @csn cell:%s atX:%s y:%s collisionWithBitmap:%s atX:%s y:%s part:%s
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; x1, y1: Longint;
                              bmp2: Bitmap; x2, y2: Longint; const part: Rectangle): Boolean; overload;
  
  /// Returns true if the cell in the specified bitmap has collided with a part of a bitmap.
  /// 
  /// @lib CellBitmapPartCollisionAtPt
  /// @sn bitmap:%s cell:%s at:%s collisionWithBitmap:%s at:%s part:%s
  /// 
  /// @class Bitmap
  /// @overload CellBitmapCollision CellBitmapPartCollisionAtPt
  /// @csn cell:%s at:%s collisionWithBitmap:%s at:%s part:%s
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; const pt1: Point2D;
                              bmp2: Bitmap; const pt2:Point2D; const part: Rectangle): Boolean; overload;
  
  /// Returns true if the cell of the bitmap has collided with a given rectangle.
  /// 
  /// @lib
  /// @sn bitmap:%s cell:%s atX:%s y:%s collisionWithRect:%s
  /// 
  /// @class Bitmap
  /// @method CellRectCollision
  /// @csn cell:%s atX:%s y:%s collisionWithRect:%s
  function CellRectCollision(bmp: Bitmap; cell, x, y: Longint; const rect: Rectangle): Boolean; overload;
  
  /// Returns true if the cell of the bitmap has collided with a given rectangle.
  /// 
  /// @lib CellRectCollisionAtPt
  /// @sn bitmap:%s cell:%s at:%s collisionWithRect:%s
  /// 
  /// @class Bitmap
  /// @method CellRectCollision
  /// @csn cell:%s at:%s collisionWithRect:%s
  function CellRectCollision(bmp: Bitmap; cell: Longint; const pt: Point2D; const rect: Rectangle): Boolean; overload;
  
  
  
//---------------------------------------------------------------------------
// Geometry Collision Tests
//---------------------------------------------------------------------------
  
  /// Returns True if the Circle collised with rectangle ``rect``.
  ///
  /// @lib
  /// @sn circle:%s collisionWithRect:%s
  function CircleRectCollision(const c: Circle; const rect: Rectangle): Boolean;
  
  /// Returns True if the circle has collided with any of the lines from the ``rect`` rectangle.
  ///
  /// @lib
  /// @sn circle:%s collisionWithLine:%s
  function CircleLinesCollision(const c: Circle; const lines: LinesArray): Boolean;
  
  /// Returns True if the circles have collided.
  ///
  /// @lib
  /// @sn circle:%s collisionWithCircle:%s
  function CircleCircleCollision(const c1, c2: Circle): Boolean;
  
  /// Returns True if the Circle has collided with the Triangle ``tri``.
  ///
  /// @lib
  /// @sn circle:%s collisionWithTriangle:%s
  function CircleTriangleCollision(const c: Circle; const tri: Triangle): Boolean;
  
  /// Returns true if the triangle and the line have collided.
  ///
  /// @lib
  /// @sn triangle:%s collisionWithLine:%s
  function TriangleLineCollision(const tri: Triangle; const ln: LineSegment): Boolean;
  
  
  
//---------------------------------------------------------------------------
// Sprite / Geometry Collision Tests
//---------------------------------------------------------------------------
  
  /// Returns True if the `Sprite` ``s``, represented by a bounding circle, has 
  /// collided with a ``line``. The diameter for the bounding circle is 
  /// based on the sprites width or height value -- whatever is largest.
  ///
  /// @lib SpriteCircleLineCollision
  /// @sn sprite:%s circleCollisionWithLine:%s
  function CircleLineCollision(s: Sprite; const line: LineSegment): Boolean;
  
  /// Returns True if the bounding rectangle of the `Sprite` ``s`` has collided 
  /// with the ``line`` specified.
  ///
  /// @lib SpriteRectLineCollision
  /// @sn sprite:%s rectCollisionWithLine:%s
  function RectLineCollision(s: Sprite; const line: LineSegment): Boolean; overload;
  
  /// Returns True if the rectangle ``rect`` provided has collided with the
  /// ``line``.
  ///
  /// @lib RectLineCollision
  /// @sn rectangle:%s collisionWithLine:%s
  function RectLineCollision(const rect: Rectangle; const line: LineSegment): Boolean; overload;
  
  
  
//---------------------------------------------------------------------------
// Side to check based on movement direction
//---------------------------------------------------------------------------
  
  /// Returns the side of that needs to be checked for collisions given the
  /// movement velocity.
  /// 
  /// @lib
  function SideForCollisionTest(const velocity: Vector): CollisionSide;
  
  
  
//---------------------------------------------------------------------------
// Collision Effect Application ( angle + energy/mass transfer)
//---------------------------------------------------------------------------
  
  /// Perform a physical collision with a circle bouncing off a line.
  /// 
  /// @lib
  /// @sn sprite:%s circleCollideWithLine:%s
  ///
  /// @class Sprite
  /// @method CircleCollideLine
  /// @csn circleCollideWithLine:%s
  procedure CollideCircleLine(s: Sprite; const line: LineSegment);
  
  /// Perform a physical collidion with a sprite circle bouncing off a
  /// stationary circle.
  ///
  /// @lib
  /// @sn sprite:%s circleCollideWithCircle:%s
  ///
  /// @class Sprite
  /// @method CircleCollideCircle
  /// @csn circleCollideWithCircle:%s
  procedure CollideCircleCircle(s: Sprite; const c: Circle);
  
  /// Perform a physical collision with a sprite as a circle bouncing off
  /// a stationary rectangle.
  ///
  /// @lib
  /// @sn sprite:%s circleCollideWithRect:%s
  ///
  /// @class Sprite
  /// @method CircleCollideRectangle
  /// @csn circleCollideWithRectangle:%s
  procedure CollideCircleRectangle(s: Sprite; const rect: Rectangle); overload;
  
  /// Perform a physical collision between two circular sprites.
  /// 
  /// @lib
  /// @sn sprite:%s circleCollide:%s
  /// 
  /// @class Sprite
  /// @method CirclesCollide
  /// @csn circlesCollide:%s
  procedure CollideCircles(s1, s2: Sprite);
  
  /// Perform a physical collision with a sprite as a circle bouncing off
  /// the closest line in the array of lines.
  /// 
  /// @lib
  /// @sn sprite:%s circleCollideWithLines:%s
  /// 
  /// @class Sprite
  /// @method CircleCollideWithLines
  /// @csn circleCollideWithLines:%s
  procedure CollideCircleLines(s: Sprite; const lines: LinesArray);
  
  
//=============================================================================
implementation
//=============================================================================

  uses
    SysUtils, sgTrace,
    sgGraphics, sgCamera, sgGeometry, sgSprites, sgShared, sgImages;


  //---------------------------------------------------------------------------

  function BitmapPartRectCollision(bmp: Bitmap; x, y: Longint; const part: Rectangle; const rect: Rectangle): Boolean;
  var
    i, j: Longint;
    left1, right1, left2, right2, overRight, overLeft: Longint;
    top1, bottom1, top2, bottom2, overTop, overBottom: Longint;
    yPixel1, xPixel1: Longint;
  begin
    result := false;
    
    if (not assigned(bmp)) or
       (not RectanglesIntersect(RectangleFrom(x, y, part.width, part.height), rect)) then exit;
    
    left1 := x;
    right1 := x + part.width - 1;
    top1 := y;
    bottom1 := y + part.height - 1;
    
    left2 := RoundInt(rect.x);
    right2 := RoundInt(rect.x) + rect.width - 1;
    top2 := RoundInt(rect.y);
    bottom2 := RoundInt(rect.y) + rect.height - 1;
    
    if bottom1 > bottom2 then overBottom := bottom2
    else overBottom := bottom1;
    
    if top1 < top2 then overTop := top2
    else overTop := top1;
    
    if right1 > right2 then overRight := right2
    else overRight := right1;
    
    if left1 < left2 then overLeft := left2
    else overLeft := left1;
    
    for i := overTop to overBottom do
    begin
      yPixel1 := i - top1 + RoundInt(part.y);
      
      for j := overLeft to overRight do
      begin
        xPixel1 := j - left1 + RoundInt(part.x);
        
        if PixelDrawnAtPoint(bmp, xPixel1, yPixel1) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;
  
  function BitmapRectCollision(bmp: Bitmap; x, y: Longint; const rect: Rectangle): Boolean; overload;
  begin
    result := BitmapPartRectCollision(bmp, x, y, BitmapRectangle(0, 0, bmp), rect);
  end;
  
  function BitmapRectCollision(bmp: Bitmap; x, y: Longint; const part, rect: Rectangle): Boolean; overload;
  begin
    result := BitmapPartRectCollision(bmp, x, y, part, rect);
  end;
  
  function BitmapRectCollision(bmp: Bitmap; const pt:Point2D; const part, rect: Rectangle): Boolean; overload;
  begin
    result := BitmapPartRectCollision(bmp, RoundInt(pt.x), RoundInt(pt.y), part, rect);
  end;
  
  function BitmapRectCollision(bmp: Bitmap; x, y, rectX, rectY, rectWidth, rectHeight: Longint): Boolean; overload;
  begin
    result := BitmapRectCollision(bmp, x, y, RectangleFrom(rectX, rectY, rectWidth, rectHeight));
  end;
  
  function SpriteRectCollision(s: Sprite; x, y: Single; width, height: Longint): Boolean; overload;
  begin
    result := SpriteRectCollision(s, RectangleFrom(x, y, width, height));
  end;

  function SpriteRectCollision(s: Sprite; const r: Rectangle): Boolean; overload;
  var
    rect: Rectangle;
  begin
    result := false;
    if s = nil then exit;

    rect := r;
    FixRectangle(rect);
   // if (width < 1) or (height < 1) then exit;
    
    if not RectanglesIntersect(SpriteCollisionRectangle(s), rect) then 
      exit;
    
    //  Check pixel level details
    if SpriteCollisionKind(s) = AABBCollisions then 
      result := true
    else
      result := CellRectCollision(s^.collisionBitmap, SpriteCurrentCell(s), RoundInt(s^.position.x), RoundInt(s^.position.y), rect);
  end;
  
  /// Performs a collision detection within two bitmaps at the given x, y
  /// locations. The bbox values indicate if each bitmap should use per
  /// pixel collision detection or a bbox collision detection. This version
  /// uses pixel based checking at all times.
  ///
  /// When both bitmaps are using bbox collision the routine checks to see
  /// if the bitmap rectangles intersect. If one is bbox and the other is
  /// pixel based the routine checks to see if a non-transparent pixel in the
  /// pixel based image intersects with the bounds of the bbox image. If
  /// both are pixel based, the routine checks to see if two non-transparent
  /// pixels collide.
  ///
  /// Note: Bitmaps do not need to actually be drawn on the screen.
  ///
  /// @param bmp1, bmp2: The bitmap images to check for collision
  /// @param x1, y1:        The x,y location of bmp 1
  /// @param bbox1:      Indicates if bmp1 should use bbox collision
  /// @param x2, y2:        The x,y location of bmp 2
  /// @param bbox2:      Indicates if bmp2 should use bbox collision
  ///
  /// @returns          True if the bitmaps collide.
  /// 
  function CollisionWithinBitmapImages(
             bmp1: Bitmap; x1, y1: Single; w1, h1: Longint; offsetX1, offsetY1: Single; //bbox1: Boolean;
             bmp2: Bitmap; x2, y2: Single; w2, h2: Longint; offsetX2, offsetY2: Single //bbox2: Boolean
           ): Boolean; overload;
  begin
    result := CollisionWithinBitmapImages(
      bmp1, RoundInt(x1), RoundInt(y1), w1, h1, RoundInt(offsetX1), RoundInt(offsetY1),
      bmp2, RoundInt(x2), RoundInt(y2), w2, h2, RoundInt(offsetX2), RoundInt(offsetY2));
  end;
  
  // offset... are the part 
  function CollisionWithinBitmapImages(
             bmp1: Bitmap; x1, y1, w1, h1, offsetX1, offsetY1: Longint; //bbox1: Boolean;
             bmp2: Bitmap; x2, y2, w2, h2, offsetX2, offsetY2: Longint //bbox2: Boolean
           ): Boolean; overload;
  var
    left1, left2, overLeft: Longint;
    right1, right2, overRight: Longint;
    top1, top2, overTop: Longint;
    bottom1, bottom2, overBottom: Longint;
    i, j, xPixel1, yPixel1, xPixel2, yPixel2: Longint;
  begin
    if (bmp1 = nil) or (bmp2 = nil) then begin RaiseException('One or both of the specified bitmaps are nil'); exit; end;
    if (w1 < 1) or (h1 < 1) or (w2 < 1) or (h2 < 1) then begin RaiseException('Bitmap width and height must be greater then 0'); exit; end;
    
    result := false;
 
    left1 := x1;
    right1 := x1 + w1 - 1;
    top1 := y1;
    bottom1 := y1 + h1 - 1;

    left2 := x2;
    right2 := x2 + w2 - 1;
    top2 := y2;
    bottom2 := y2 + h2 - 1;

    if bottom1 > bottom2 then overBottom := bottom2
    else overBottom := bottom1;

    if top1 < top2 then overTop := top2
    else overTop := top1;

    if right1 > right2 then overRight := right2
    else overRight := right1;

    if left1 < left2 then overLeft := left2
    else overLeft := left1;

    for i := overTop to overBottom do
    begin
      yPixel1 := i - top1 + offsetY1;
      yPixel2 := i - top2 + offsetY2;

      for j := overLeft to overRight do
      begin
        xPixel1 := j - left1 + offsetX1;
        xPixel2 := j - left2 + offsetX2;

        if PixelDrawnAtPoint(bmp1, xPixel1, yPixel1) and PixelDrawnAtPoint(bmp2, xPixel2, yPixel2) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;

  /// Performs a collision detection within two bitmaps at the given x, y
  /// locations using per pixel collision detection. This checks to see if
  /// two non-transparent pixels collide.
  function CollisionWithinBitmapImages(bmp1: Bitmap; x1, y1: Longint; bmp2: Bitmap; x2, y2: Longint): Boolean; overload;
  begin
    if (not Assigned(bmp1)) or (not Assigned(bmp2)) then result := False
    else result := CollisionWithinBitmapImages(
                bmp1, x1, y1, bmp1^.width, bmp1^.height, 0, 0, 
                bmp2, x2, y2, bmp2^.width, bmp2^.height, 0, 0);
  end;
  
  function CollisionWithinSpriteImages(s1, s2: Sprite): Boolean;
  var
    part1, part2: Rectangle;
  begin
    if (s1 = nil) or (s2 = nil) then begin RaiseException('One of the sprites specified is nil'); exit; end;
    
    // Check if either is not using pixel level collisions
    if SpriteCollisionKind(s1) = AABBCollisions then 
    begin
      result := SpriteRectCollision(s2, SpriteCollisionRectangle(s1));
      exit;
    end
    else if SpriteCollisionKind(s2) = AABBCollisions then 
    begin
      result := SpriteRectCollision(s1, SpriteCollisionRectangle(s2));
      exit;
    end;
    
    part1 := SpriteCurrentCellRectangle(s1);
    part2 := SpriteCurrentCellRectangle(s2);
    
    result := CollisionWithinBitmapImages(
                s1^.collisionBitmap, RoundInt(s1^.position.x), RoundInt(s1^.position.y), part1.width, part1.height, RoundInt(part1.x), RoundInt(part1.y), 
                s2^.collisionBitmap, RoundInt(s2^.position.x), RoundInt(s2^.position.y), part2.width, part2.height, RoundInt(part2.x), RoundInt(part2.y));
  end;

  function BitmapCollision(bmp1: Bitmap; x1, y1: Longint; bmp2: Bitmap; x2, y2: Longint): Boolean; overload;
  begin
    result := CollisionWithinBitmapImages(bmp1, x1, y1, BitmapWidth(bmp1), BitmapHeight(bmp1), 0, 0,
                                          bmp2, x2, y2, BitmapWidth(bmp2), BitmapHeight(bmp2), 0, 0);
  end;

  function BitmapCollision(bmp1: Bitmap; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D): Boolean; overload;
  begin
    result := BitmapCollision(bmp1, RoundInt(pt1.x), RoundInt(pt1.y), 
                              bmp2, RoundInt(pt2.x), RoundInt(pt2.y));
  end;
  
  function BitmapCollision(bmp1: Bitmap; const pt1: Point2D; const part1: Rectangle; bmp2: Bitmap; const pt2: Point2D; const part2: Rectangle): Boolean; overload;
  begin
    result := BitmapCollision(bmp1, RoundInt(pt1.x), RoundInt(pt1.y), 
                              bmp2, RoundInt(pt2.x), RoundInt(pt2.y));
  end;
  
  function SpriteCollision(s1, s2: Sprite): Boolean;
  begin
    if not SpriteRectCollision(s1, SpriteCollisionRectangle(s2)) then 
      result := false
    else if (SpriteCollisionKind(s1) = PixelCollisions) or (SpriteCollisionKind(s2) = PixelCollisions) then
      result := CollisionWithinSpriteImages(s1, s2)
    else
      result := true;
  end;
  
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D; const part: Rectangle): Boolean; overload;
  begin
    result := false;
    
    if not assigned(s) then exit;
    if (SpriteCollisionKind(s) = AABBCollisions) then
    begin
      result := BitmapRectCollision(bmp, pt, part, SpriteCollisionRectangle(s));
      exit;
    end;
    
    result := CellBitmapCollision(s^.collisionBitmap, SpriteCurrentCell(s), s^.position, bmp, pt, part);
  end;
  
  /// Determines if a sprite has collided with a bitmap using pixel level
  /// collision detection with the bitmap.
  ///
  /// @param s:     The sprite to check for collision
  /// @param bmp:     The bitmap image to check for collision
  /// @param x, y:           The x,y location of the bitmap
  /// @param bbox        Indicates if bmp should use bbox collision
  ///
  /// @returns               True if the bitmap has collided with the sprite.
  ///
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; x, y: Single): Boolean; overload;
  begin
    result := SpriteBitmapCollision(s, bmp, PointAt(x, y), BitmapRectangle(bmp));
  end;

  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D): Boolean; overload;
  begin
    result := SpriteBitmapCollision(s, bmp, pt, BitmapRectangle(bmp));
  end;
  
  function TriangleLineCollision(const tri: Triangle; const ln: LineSegment): Boolean;
  begin
    result := LineIntersectsLines(ln, LinesFrom(tri));
  end;
  
  function CircleLineCollision(s: Sprite; const line: LineSegment): Boolean;
  var
    r: Single;
    dist: Single;
  begin
    if not Assigned(s) then
    begin
      result := false;
      exit;
    end;

    if SpriteWidth(s) > SpriteHeight(s) then
      r := SpriteWidth(s) div 2
    else
      r := SpriteHeight(s) div 2;
      
    dist := PointLineDistance(s^.position.x + r, s^.position.y + r, line);
    result := dist < r;
  end;
  
  function RectLineCollision(const rect: Rectangle; const line: LineSegment): Boolean; overload;
  begin
    result := LineIntersectsLines(line, LinesFrom(rect)) or PointInRect(line.startPoint, rect);
  end;
    
  function RectLineCollision(s: Sprite; const line: LineSegment): Boolean; overload;
  begin
    result := RectLineCollision(SpriteCollisionRectangle(s), line);
  end;
  
  //You need to test for collisions on the ...
  function SideForCollisionTest (const velocity: Vector): CollisionSide;
  const SMALL = 0.01; //The delta for the check
  begin
    if velocity.x < -SMALL then //Going Left...
    begin
      if velocity.y < -SMALL then result := BottomRight
      else if velocity.y > SMALL then result := TopRight
      else result := Right;
    end
    else if velocity.x > SMALL then //Going Right
    begin
      if velocity.y < -SMALL then result := BottomLeft
      else if velocity.y > SMALL then result := TopLeft
      else result := Left;      
    end
    else // Going Up or Down
    begin
      if velocity.y < -SMALL then result := Bottom
      else if velocity.y > SMALL then result := Top
      else result := None;
    end;
  end;
  
//----------------------------------------------------------------------------
// Bitmap <--> Point collision detection
//----------------------------------------------------------------------------
  
  function BitmapPointCollision(bmp: Bitmap; x, y: Longint; bbox: Boolean; ptX, ptY: Single): Boolean; overload;
  begin
    if bbox then
      result := PointInRect(ptX, ptY, BitmapRectangle(x, y, bmp))
    else
      result := BitmapPointCollision(bmp, x, y, ptX, ptY);
  end;
  
  function BitmapPointCollision(bmp: Bitmap; x, y: Longint; ptX, ptY: Single): Boolean; overload;
  begin
    result := PixelDrawnAtPoint(bmp, RoundInt(ptX - x), RoundInt(ptY - y));
  end;
  
  function BitmapPointCollision(bmp: Bitmap; x, y: Longint; bbox: Boolean; const pt: Point2D): Boolean; overload;
  begin
    result := BitmapPointCollision(bmp, x, y, bbox, pt.x, pt.y);
  end;
  
  function BitmapPointCollision(bmp: Bitmap; x, y: Longint; const pt: Point2D): Boolean; overload;
  begin
    result := BitmapPointCollision(bmp, x, y, False, pt.x, pt.y);
  end;
  
  function BitmapPartPointCollision(bmp: Bitmap; x, y: Longint; const part: Rectangle; ptX, ptY: Single): Boolean; overload;
  begin
    result := BitmapPointCollision(bmp, x, y, False, ptX + part.x, ptY + part.y);
  end;
  
  function BitmapPartPointCollision(bmp: Bitmap; x, y: Longint; const part: Rectangle; const pt: Point2D): Boolean; overload;
  begin
    result := BitmapPointCollision(bmp, x, y, False, pt.x + part.x, pt.y + part.y);
  end;
  
  
//----------------------------------------------------------------------------
// Collision Effect Application (angle + mass/energy transfer)
//----------------------------------------------------------------------------

  procedure _CollideCircleLine(s: Sprite; const line: LineSegment);
  var
    npx, npy, dotPod: Single;
    toLine: Vector;
    intersect: Point2D;
  begin
    //TODO: fix collision pt.... cast back along velocity...
    intersect := ClosestPointOnLine(CenterPoint(s), line);

    //DrawSprite(s);
    //DrawCircle(ColorRed, intersect, 2);
    
    toLine := UnitVector(VectorFromCenterSpriteToPoint(s, intersect));
    // Project velocity across to-line
    dotPod := - DotProduct(toLine, s^.velocity);
    
    npx := dotPod * toLine.x;
    npy := dotPod * toLine.y;
    
    s^.velocity.x := s^.velocity.x + 2 * npx;
    s^.velocity.y := s^.velocity.y + 2 * npy;
    
    //DrawLine(ColorYellow, CenterPoint(s).x, CenterPoint(s).y, CenterPoint(s).x + (s^.velocity.x * 10), CenterPoint(s).y + (s^.velocity.y * 10));
    //RefreshScreen(1) ;
  end;
  
  procedure CollideCircleLine(s: Sprite; const line: LineSegment);
  var
    lines: LinesArray;
  begin
    SetLength(lines, 1);
    lines[0] := line;
    
    CollideCircleLines(s, lines);
  end;
  
  procedure CollideCircleLines(s: Sprite; const lines: LinesArray);
  var 
    outVec, mvmt: Vector;
    maxIdx: Longint;
    mvmtMag, prop: Single;
  begin
    if not Assigned(s) then exit;

    mvmt := s^.velocity;
    maxIdx := -1;
    outVec := VectorOverLinesFromCircle(SpriteCollisionCircle(s), lines, mvmt, maxIdx);
    if maxIdx < 0 then exit;
     
    MoveSprite(s, outVec);
    _CollideCircleLine(s, lines[maxIdx]);
    
    // do part velocity
    mvmtMag := VectorMagnitude(mvmt);
    prop := VectorMagnitude(outVec) / mvmtMag; //proportion of move "undone" by back out
    if prop > 0 then MoveSprite(s, prop); //TODO: Allow proportion of move to be passed in (overload)... then do velocity based on prop * pct
  end;
  
  
  procedure CollideCircles(s1, s2: Sprite);
  var
    c1, c2: Circle;
    colNormalAngle, a1, a2, optP, s1Mass, s2Mass: Single;
    n: Vector;
  begin
    s1Mass := SpriteMass(s1);
    s2Mass := SpriteMass(s2);
    if (s1Mass <= 0) or (s2Mass <= 0) then begin RaiseWarning('Collision with 0 or negative mass... ensure that mass is greater than 0'); exit; end;
    
    c1 := SpriteCollisionCircle(s1);
    c2 := SpriteCollisionCircle(s2);
    
    //if s1^.mass < s2^.mass then
    if VectorMagnitude(s1^.velocity) > VectorMagnitude(s2^.velocity) then
    begin
      //move s1 out
      n := VectorOutOfCircleFromCircle(c1, c2, s1^.velocity);
      s1^.position.x := s1^.position.x + n.x;
      s1^.position.y := s1^.position.y + n.y;
    end
    else
    begin
      //move s2 out
      n := VectorOutOfCircleFromCircle(c2, c1, s2^.velocity);
      s2^.position.x := s2^.position.x + n.x;
      s2^.position.y := s2^.position.y + n.y;
    end;
      
    colNormalAngle := CalculateAngle(s1, s2);
    // COLLISION RESPONSE
    // n = vector connecting the centers of the balls.
    // we are finding the components of the normalised vector n
    n := VectorTo(Cosine(colNormalAngle), Sine(colNormalAngle));
    // now find the length of the components of each velocity vectors
    // along n, by using dot product.
    a1 := DotProduct(s1^.velocity, n);
    // Local a1# = c.dx*nX  +  c.dy*nY
    a2 := DotProduct(s2^.velocity, n);
    // Local a2# = c2.dx*nX +  c2.dy*nY
    // optimisedP = 2(a1 - a2)
    // ----------
    // m1 + m2
    optP := (2.0 * (a1-a2)) / (s1Mass + s2Mass);
    // now find out the resultant vectors
    // Local r1% = c1.v - optimisedP * mass2 * n
    s1^.velocity.x := s1^.velocity.x - (optP * s2Mass * n.x);
    s1^.velocity.y := s1^.velocity.y - (optP * s2Mass * n.y);
    // Local r2% = c2.v - optimisedP * mass1 * n
    s2^.velocity.x := s2^.velocity.x + (optP * s1Mass * n.x);
    s2^.velocity.y := s2^.velocity.y + (optP * s1Mass * n.y);
  end;
  
  procedure CollideCircleCircle(s: Sprite; const c: Circle);
  var
    hitLine: LineSegment;
    outVec, mvmt, normal, colVec: Vector;
    mvmtMag, prop: Single;
    spriteCenter, hitPt: Point2D;
  begin
    if not Assigned(s) then exit;

    //TODO: what if height > width!!
    spriteCenter := CenterPoint(s);
    mvmt := s^.velocity;
    
    outVec := VectorOutOfCircleFromCircle(SpriteCollisionCircle(s), c, mvmt);
    // Back out of circle
    MoveSprite(s, outVec);
    
    // Normal of the collision...
    colVec := UnitVector(VectorFromPoints(c.center, spriteCenter));
    normal := VectorNormal(colVec);
    
    hitPt := AddVectors(c.center, VectorMultiply(colVec, Single(c.radius + 1.42)));
    hitLine := LineFromVector(AddVectors(hitPt, VectorMultiply(normal, 100)), VectorMultiply(normal, -200));
    
    // DrawSprite(s);
    // DrawLine(ColorWhite, hitLine);
    // RefreshScreen(1);
    
    _CollideCircleLine(s, hitLine);

    // do part velocity
    mvmtMag := VectorMagnitude(mvmt);
    prop := VectorMagnitude(outVec) / mvmtMag; //proportion of move "undone" by back out
    if prop > 0 then MoveSprite(s, prop); //TODO: Allow proportion of move to be passed in (overload)... then do velocity based on prop * pct
  end;
  
  
  //TODO: bounds based checking, need VectorIntoShape...
  procedure CollideCircleRectangle(s: Sprite; const rect: Rectangle; bounds: Boolean); overload;
  var
    hitIdx: Longint;
    lines: LinesArray;
    outVec, mvmt: Vector;
    mvmtMag, prop: Single;
  begin
    if not Assigned(s) then exit;
    
    mvmt := s^.velocity;
    hitIdx := -1;
    
    // Get the line hit...
    lines := LinesFrom(rect);
    outVec := VectorOverLinesFromCircle(SpriteCollisionCircle(s), lines, mvmt, hitIdx);
    
    if hitIdx = -1 then exit;
    
    // back out of rectangle
    MoveSprite(s, outVec);
    
    // bounce...
    _CollideCircleLine(s, lines[hitIdx]);
    
    // do part velocity
    mvmtMag := VectorMagnitude(mvmt);
    prop := VectorMagnitude(outVec) / mvmtMag; //proportion of move "undone" by back out
    if prop > 0 then MoveSprite(s, prop); //TODO: Allow proportion of move to be passed in (overload)... then do velocity based on prop * pct
  end;
  
  procedure CollideCircleRectangle(s: Sprite; const rect: Rectangle); overload;
  begin
    CollideCircleRectangle(s, rect, False);
  end;

  procedure CollideCircleRectangleBounds(s: Sprite; const rect: Rectangle);
  begin
    CollideCircleRectangle(s, rect, True);
  end;

  
  //----------------------------------------------------------------------------




//---------------------------------------------------------------------------
// Geometry Collision Tests
//---------------------------------------------------------------------------
  
  function CircleRectCollision(const c: Circle; const rect: Rectangle): Boolean;
  begin
    if CircleLinesCollision(c, LinesFrom(rect)) then result := True
    else result := PointInRect(c.center, rect.x, rect.y, rect.width, rect.height);
  end;
  
  function CircleLinesCollision(const c: Circle; const lines: LinesArray): Boolean;
  var
    pt: Point2D;
    i: Longint;
  begin
    result := False;
    
    for i := 0 to High(lines) do
    begin
      pt := ClosestPointOnLineFromCircle(c, lines[i]);

      if PointPointDistance(c.center, pt) <= c.radius then
      begin
        //DrawCircle(ColorGreen, pt, 2);
        result := True;
        exit;
      end;
    end;
  end;

  function CircleCircleCollision(const c1, c2: Circle): Boolean;
  begin
    result := PointPointDistance(c1.center, c2.center) < c1.radius + c2.radius;
  end;

  function CircleTriangleCollision(const c: Circle; const tri: Triangle): Boolean;
  var
    i: Longint;
  begin
    result := False;

    for i := 0 to 2 do
    begin
      // if the closest point on the circle is in the triangle, or the triangle pt is in the circle
      if PointInTriangle(ClosestPointOnCircle(tri.points[i], c), tri) or PointInCircle(tri.points[i], c) then
      begin
        result := True;
        exit;
      end;
    end;
  end;
  
  //---------------------------------------------------------------------------
  
  function CellCollision( bmp1: Bitmap; cell1, x1, y1: Longint; 
                          bmp2: Bitmap; cell2, x2, y2: Longint): Boolean;
  var
    r1, r2: Rectangle;
  begin
    r1 := BitmapRectangleOfCell(bmp1, cell1);
    r2 := BitmapRectangleOfCell(bmp2, cell2);
    
    result := 
      CollisionWithinBitmapImages(bmp1, x1, y1, r1.width, r1.height, RoundInt(r1.x), RoundInt(r1.y),
                                  bmp2, x2, y2, r2.width, r2.height, RoundInt(r2.x), RoundInt(r2.y));
  end;
  
  function CellCollision( bmp1: Bitmap; cell1: Longint; const pt1: Point2D; 
                          bmp2: Bitmap; cell2: Longint; const pt2: Point2D): Boolean;
  begin
    result := CellCollision(bmp1, cell1, RoundInt(pt1.x), RoundInt(pt1.y),
                            bmp2, cell2, RoundInt(pt2.x), RoundInt(pt2.y));
  end;
  
  function CellBitmapCollision(bmp1: Bitmap; cell, x1, y1: Longint; 
                              bmp2: Bitmap; x2, y2: Longint): Boolean; overload;
  begin
    result := CellBitmapCollision( bmp1, cell, x1, y1, 
                                  bmp2, x2, y2, BitmapRectangle(0, 0, bmp2));
  end;
  
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; const pt1: Point2D; 
                              bmp2: Bitmap; const pt2: Point2D): Boolean; overload;
  begin
    result := CellBitmapCollision( bmp1, cell, RoundInt(pt1.x), RoundInt(pt1.y),
                                  bmp2, RoundInt(pt2.x), RoundInt(pt2.y));
  end;
  
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; x1, y1: Longint;
                              bmp2: Bitmap; x2, y2: Longint; const part: Rectangle): Boolean; overload;
  var
    r1: Rectangle;
  begin
    if not assigned(bmp2) then 
    begin
      result := false;
      exit;
    end;
    
    r1 := BitmapRectangleOfCell(bmp1, cell);
    
    result := 
      CollisionWithinBitmapImages(bmp1, x1, y1, r1.width, r1.height, RoundInt(r1.x), RoundInt(r1.y),
                                  bmp2, x2, y2, part.width, part.height, RoundInt(part.x), RoundInt(part.y));
  end;
  
  function CellBitmapCollision(bmp1: Bitmap; cell: Longint; const pt1: Point2D;
                              bmp2: Bitmap; const pt2:Point2D; const part: Rectangle): Boolean; overload;
  begin
    result := CellBitmapCollision( bmp1, cell, RoundInt(pt1.x), RoundInt(pt1.y),
                                  bmp2, RoundInt(pt2.x), RoundInt(pt2.y), part);
  end;
  
  function CellRectCollision(bmp: Bitmap; cell, x, y: Longint; const rect: Rectangle): Boolean; overload;
  var
    r1: Rectangle;
  begin
    r1 := BitmapRectangleOfCell(bmp, cell);
    
    result := BitmapPartRectCollision(bmp, x, y, r1, rect);
  end;
  
  function CellRectCollision(bmp: Bitmap; cell: Longint; const pt: Point2D; const rect: Rectangle): Boolean; overload;
  begin
    result := CellRectCollision(bmp, cell, RoundInt(pt.x), RoundInt(pt.y), rect);
  end;
end.

