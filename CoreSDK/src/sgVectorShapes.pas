//=============================================================================
// sgVectorShapes.pas
//=============================================================================
//
// EXPERIMENTAL
//
// Contains code to create shapes - should allow vector graphics for 
// Sprites etc when complete... Needs lots of work.
//
// Change History:
// Version 3.0:
// - 2011-05-23: Andrew : Created. Removed this code from Geometry
//
//=============================================================================

///
/// @module VectorShapes
/// @static
unit sgVectorShapes;

interface
uses
  sgTypes;
  
  type
    /// The ShapeKind is used to configure the drawing method for a
    /// shape. Each of these options provides an alternate way of 
    /// rendering based upon the shapes points.
    ///
    /// @enum ShapeKind
    ShapeKind= (
        pkPoint,
        pkCircle,
        // pkEllipse,
        pkLine,
        pkTriangle,
        pkLineList,
        pkLineStrip,
        // pkPolygon,
        pkTriangleStrip,
        pkTriangleFan,
        pkTriangleList
      );
    
    /// @class ShapePrototype
    /// @pointer_wrapper
    /// @field pointer: pointer
    ShapePrototype = ^ShapePrototypeData;
    
    /// @class Shape
    /// @pointer_wrapper
    /// @field pointer: pointer
    Shape = ^ShapeData;
    
    /// The ShapeDrawingFn is a function pointer that points to a procedure
    /// that is capable of drawing a Shape. This is used when the shape
    /// is drawn be DrawShape and FillShape.
    ///
    /// @type ShapeDrawingFn
    ShapeDrawingFn = procedure(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);
    
    /// @struct ShapePrototypeData
    /// @via_pointer
    ShapePrototypeData = packed record
      points: Point2DArray;
      kind: ShapeKind;
      shapeCount: Longint;            //the number of shapes using the prototype
      drawWith: ShapeDrawingFn;
    end;
    
    /// @struct ShapeData
    /// @via_pointer
    ShapeData = packed record
      pt: Point2D;
      prototype: ShapePrototype;
      color: Color;
      scale: Point2D;
      angle: single;
      ptBuffer: Point2DArray;
      subShapes: array of Shape;
    end;
  
  /// Returns the number of lines in a given shape
  ///
  /// @lib
  ///
  /// @class Shape
  /// @getter LineCount
  function ShapeLineCount(s: Shape): Longint;
  
  /// Returns an array of lines from a given shape. These are the lines that represent
  /// the shape.
  /// 
  /// @lib LinesFromShape
  ///
  /// @class Shape
  /// @getter Lines
  /// @length ShapeLineCount
  function LinesFrom(s: shape): LinesArray; overload;
  
  /// Returns the vector needed to move shape ``s`` out of rectangle``bounds`` given the it was moving with the velocity specified.
  /// 
  /// @lib
  /// @sn shape:%s vectorOutOfRect:%s givenHeading:%s
  /// 
  /// @class Shape
  /// @method VectorOutOfRect
  /// @csn vectorOutOfRect:%s givenHeading:%s
  function ShapeVectorOutOfRect(s: shape; const bounds: Rectangle; const velocity: Vector  ): vector;

  //----------------------------------------------------------------------------
  // ShapePrototype creating functions
  //----------------------------------------------------------------------------

    /// Create a shape prototype for a given point.
    ///
    /// @lib
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initPointPrototypeAt:%s
    function PointPrototypeFrom(const pt: Point2D): ShapePrototype;

    /// Create a shape prototype of a circle with a given radius ``r``.
    ///
    /// @lib
    /// @sn circlePrototypeFromPt:%s radius:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initCirclePrototypeAt:%s withRadius:%s
    function CirclePrototypeFrom(const pt: Point2D; r: Single): ShapePrototype;

    // /// Create a shape prototype of an ellipse with a given width and height.
    // ///
    // /// @lib
    // /// @class ShapePrototype
    // /// @constructor
    // /// @csn initEllipsePrototypeAt:%s width:%s height:%s
    // function EllipsePrototypeFrom(const pt: Point2D; w, h: Single): ShapePrototype;

    /// Create a shape prototype for a line from ``startPt`` to ``endPt``.
    ///
    /// @lib
    /// @sn linePrototypeFromPt:%s toPt:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initLinePrototypeFrom:%s to:%s
    function LinePrototypeFrom(const startPt, endPt: Point2D): ShapePrototype;

    /// Create a shape prototype for a triangle made of points ``pt1``, ``pt2``, and ``pt3``.
    ///
    /// @lib
    /// @sn trianglePrototypeFromPt1:%s pt2:%s pt3:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initTrianglePrototype:%s point2:%s point3:%s
    function TrianglePrototypeFrom(const pt1, pt2, pt3: Point2D): ShapePrototype;

    /// Create a LineList prototype from the passed in points. There must be an
    /// even number of points, where each pair represents a line.
    ///
    /// @lib
    /// @sn lineListPrototypeFrom:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initLineListPrototype:%s
    function LineListPrototypeFrom(const points: Point2DArray): ShapePrototype;

    /// Creates a LineStrip prototype where the points in the array represent the 
    /// points on the line. The last point of the line does not join back to the starting
    /// point.
    ///
    /// @lib
    /// @sn lineStripPrototypeFrom:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initLineStripPrototype:%s
    function LineStripPrototypeFrom(const points: Point2DArray): ShapePrototype;

    // /// Creates a Polygon from the passed in points, the polygon is made up of
    // /// the points in the array and the last point does join back to the start
    // /// point.
    // ///
    // /// @lib
    // function PolygonPrototypeFrom(const points: Point2DArray): ShapePrototype;

    /// Creates a triangle strip from the passed in points. The passed in array
    /// must contain at least three points.
    ///
    /// @lib
    /// @sn triangleStripPrototypeFrom:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initTriangleStripPrototype:%s
    function TriangleStripPrototypeFrom(const points: Point2DArray): ShapePrototype;

    /// Creates a triangle fan from the passed in points. The fan is constructed from
    /// the first point combined with all other point pairs. The array must contain
    /// at least three points.
    ///
    /// @lib
    /// @sn triangleFanPrototypeFrom:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initTriangleFanPrototype:%s
    function TriangleFanPrototypeFrom(const points: Point2DArray): ShapePrototype;

    /// Creates a triangle list from the passed in points. The list is a set of 
    /// triangles. The number of points must be divisible by three.
    ///
    /// @lib
    /// @sn triangleListPrototypeFrom:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initTriangleListPrototype:%s
    function TriangleListPrototypeFrom(const points: Point2DArray): ShapePrototype;

    /// Creates a shape prototype from the data in the points array. The kind
    /// indicates the type of shape prototype to create.
    ///
    /// @lib
    /// @sn prototypeFrom:%s kind:%s
    ///
    /// @class ShapePrototype
    /// @constructor
    /// @csn initShapePropertyFrom:%s withKind:%s
    function PrototypeFrom(const points: Point2DArray; kind:ShapeKind): ShapePrototype;

    /// Free the resources used by a ShapePrototype.
    ///
    /// @lib
    ///
    /// @class ShapePrototype
    /// @dispose
    procedure FreePrototype(var p: ShapePrototype);



  //----------------------------------------------------------------------------
  // ShapePrototype functions and procedures
  //----------------------------------------------------------------------------

    /// Returns the minimum number of points required for the given shape kind.
    /// 
    /// @lib
    ///
    /// @class ShapePrototype
    /// @method MinimumPointsForKind
    /// @static
    function MinimumPointsForKind(k: ShapeKind): Longint;

    /// Returns the number of points allocated to this shape prototype.
    ///
    /// @lib
    ///
    /// @class ShapePrototype
    /// @getter PointCount
    function PrototypePointCount(p: ShapePrototype): Longint;

    /// Change the prototype's points to the passed in points. The number of points must
    /// be sufficient for the kind of shape being changed.
    ///
    /// @lib
    /// @sn prototype:%s setPointsTo:%s
    ///
    /// @class ShapePrototype
    /// @setter Points
    /// @length PrototypePointCount
    procedure PrototypeSetPoints(p: ShapePrototype; const points: Point2DArray);

    /// The prototype point locations.
    ///
    /// @lib
    ///
    /// @class ShapePrototype
    /// @getter Points
    /// @length PrototypePointCount
    function PrototypePoints(p: ShapePrototype): Point2DArray;

    /// Changes the prototype kind. This effects how shapes of this prototype
    /// are drawn to the screen, and the number of points required.
    /// 
    /// @lib
    /// @sn prototype:%s setKindTo:%s 
    ///
    /// @class ShapePrototype
    /// @setter Kind
    procedure PrototypeSetKind(p: ShapePrototype; kind: ShapeKind);

    /// The prototype kind.   This effects how shapes of this prototype
    /// are drawn to the screen, and the number of points required.
    ///
    /// @lib
    ///
    /// @class ShapePrototype
    /// @getter Kind
    function PrototypeKind(p: ShapePrototype): ShapeKind;



  //----------------------------------------------------------------------------
  // Shape Code
  //----------------------------------------------------------------------------

    /// Create a shape from a given prototype at a set location in the game.
    ///
    /// @lib
    /// @sn prototype:%s createShapeAtPt:%s
    ///
    /// @class Shape
    /// @constructor
    /// @csn initShapeWithPrototype:%s atPoint:%s
    function ShapeAtPoint(p: ShapePrototype; const pt: Point2D): Shape;

    /// Free the resources used by a Shape.
    ///
    /// @lib
    /// @class Shape
    /// @dispose
    procedure FreeShape(var s: Shape);

    /// Recalculate the points of the shape. This will be required when a Shape's
    /// prototype is changed.
    ///
    /// @lib
    /// @class Shape
    /// @method UpdatePoints
    procedure UpdateShapePoints(s: Shape);

    /// Returns the number of points on this shape.
    ///
    /// @lib
    /// @class Shape
    /// @getter PointCount
    function ShapePointCount(s: Shape): Longint;

    /// Returns all of the points for a shape, based on its kind, angle 
    /// and scale.
    ///
    /// @lib
    /// @class Shape
    /// @getter Points
    /// @length ShapePointCount
    function ShapePoints(s: Shape): Point2DArray;

    /// Change the angle of a Shape.
    ///
    /// @lib
    /// @sn shape:%s setAngleTo:%s
    ///
    /// @class Shape
    /// @setter Angle
    procedure ShapeSetAngle(s: Shape; angle: Single);

    /// Return the angle of a Shape.
    ///
    /// @lib
    ///
    /// @class Shape
    /// @getter Angle
    function ShapeAngle(s: Shape): Single;

    /// Change the scale of a Shape.
    ///
    /// @lib
    /// @sn shape:%s setScaleTo:%s
    ///
    /// @class Shape
    /// @setter Scale
    procedure ShapeSetScale(s: Shape; const scale: Point2D);

    /// Return the scale of a Shape.
    ///
    /// @lib
    ///
    /// @class Shape
    /// @getter Scale
    function ShapeScale(s: Shape): Point2D;

    /// Add a shape as a sub shape to a given parent shape.
    ///
    /// @lib
    /// @sn shape:%s addSubShape:%s
    ///
    /// @class Shape
    /// @method AddSubShape
    /// @csn addSubShape:%s
    procedure ShapeAddSubShape(parent, child: Shape);

    /// Gets the color of the shape s.
    ///
    /// @lib
    ///
    /// @class Shape
    /// @getter Color
    function ShapeColor(s: Shape): Color;

    /// Sets the color of the shape s.
    ///
    /// @lib
    /// @sn shape:%s setColorTo:%s
    ///
    /// @class Shape
    /// @setter Color
    procedure ShapeSetColor(s: Shape; c: Color);

    /// Sets the shape's prototype.
    /// 
    /// @lib
    /// @sn shape:%s setPrototypeTo:%s
    ///
    /// @class Shape
    /// @setter ShapePrototype
    procedure ShapeSetPrototype(s: Shape; p: ShapePrototype);

    /// Returns the shape's ShapePrototype.
    /// 
    /// @lib
    /// @sn shapeShapePrototype:%s
    /// 
    /// @class Shape
    /// @getter ShapePrototype
    function ShapeShapePrototype(s: Shape): ShapePrototype;

    /// Returns true if the shape and rectangle intersect
    /// 
    /// @lib
    /// @sn shape:%s intersectsRectangle:%s
    ///
    /// @class Shape
    /// @method IntersectsRectangle
    function ShapeRectangleIntersect(s: Shape; const rect: Rectangle): Boolean;

    /// Returns an axis aligned bounded box for the shape.
    /// 
    /// @lib
    ///
    /// @class Shape
    /// @getter AABB
    function ShapeAABB(s: Shape): Rectangle;

    /// Returns the kind of the shape.
    ///
    /// @lib
    ///
    /// @class Shape
    /// @getter ShapeKind
    function ShapeShapeKind(s: Shape): ShapeKind;

    /// Returns the number of triangles in the Shape.
    ///
    /// @lib
    /// 
    /// @class Shape
    /// @getter TriangleCount
    function ShapeTriangleCount(s: Shape): Longint;

    /// Returns the number of triangles in the Shape.
    ///
    /// @lib
    /// @sn shapeTriangleCount:%s kind:%s
    function ShapeKindTriangleCount(s: Shape; kind: ShapeKind): Longint;

    /// Returns the triangles for a triangle strip, list, or fan.
    ///
    /// @lib
    /// @sn shapeTriangles:%s
    /// @length ShapeTriangleCount
    ///
    /// @class Shape
    /// @method AsTriangles
    /// @csn triangles
    function ShapeTriangles(s: Shape): TriangleArray; overload;

    /// Returns the triangles for a triangle strip, list, or fan.
    ///
    /// @lib ShapeTrianglesForKind
    /// @sn shapeTriangles:%s forKind:%s
    /// @length ShapeKindTriangleCount
    ///
    /// @class Shape
    /// @overload AsTriangles AsTrianglesForKind
    /// @csn trianglesForKind:%s
    function ShapeTriangles(s: Shape; kind: ShapeKind): TriangleArray;

    /// Returns the triangles for a triangle strip, list, or fan with an offset.
    ///
    /// @lib ShapeTrianglesOffset
    /// @sn shapeTriangles:%s forKind:%s offset:%s
    /// @length ShapeKindTriangleCount
    ///
    /// @class Shape
    /// @overload AsTriangles AsTrianglesOffset
    /// @csn trianglesForKind:%s offset:%s
    function ShapeTriangles(s: Shape; kind: ShapeKind; const offset: Point2D): TriangleArray;

    /// Returns the number of lines in a given shape and kind.
    ///
    /// @lib
    /// @sn shapeLineCount:%s kind:%s
    function ShapeKindLineCount(s: Shape; kind: ShapeKind): Longint;

    /// Returns the lines for a triangle list or strip.
    ///
    /// @lib
    /// @sn shapeLines:%s
    /// @length ShapeLineCount
    ///
    /// @class Shape
    /// @method AsLines
    function ShapeLines(s: Shape): LinesArray; overload;

    /// Returns the lines for a triangle list or strip.
    ///
    /// @lib ShapeKindLines
    /// @sn shapeLines:%s forKind:%s
    /// @length ShapeKindLineCount
    ///
    /// @class Shape
    /// @overload AsLines AsLinesForKind
    /// @csn linesForKind:%s
    function ShapeLines(s: Shape; kind: ShapeKind): LinesArray; overload;

    /// Returns the lines for a triangle list or strip with offset.
    ///
    /// @lib ShapeLinesWithOffset
    /// @sn shapeLines:%s forKind:%s withOffset:%s
    /// @length ShapeLineCount
    ///
    /// @class Shape
    /// @overload AsLines AsLinesOffset
    /// @csn linesForKind:%s offset:%s
    function ShapeLines(s: Shape; kind: ShapeKind; const offset:Point2D): LinesArray; overload;

    /// Returns the triangle from the shapes details.
    /// 
    /// @lib
    /// 
    /// @class Shape
    /// @method AsTriangle
    function ShapeTriangle(s: Shape): Triangle;

    /// Returns the triangle from the shapes details with offset.
    /// 
    /// @lib ShapeTriangleWithOffset
    /// @sn shape:%s asTriangleOffset:%s
    /// 
    /// @class Shape
    /// @overload AsTriangle AsTriangleOffset
    function ShapeTriangle(s: Shape; const offset:Point2D): Triangle;

    /// Returns the line from the shapes details.
    /// 
    /// @lib
    /// 
    /// @class Shape
    /// @method AsLine
    function ShapeLine(s: Shape): LineSegment;

    /// Returns the line from the shapes details with offset.
    /// 
    /// @lib ShapeLineOffset
    /// @sn shape:%s asLineOffset:%s
    /// 
    /// @class Shape
    /// @overload AsLine AsLineOffset
    function ShapeLine(s: Shape; const offset:Point2D): LineSegment;

    /// Returns the circle from the shapes details.
    /// 
    /// @lib
    /// 
    /// @class Shape
    /// @method AsCircle
    function ShapeCircle(s: Shape): Circle;

    /// Returns the circle from the shapes details with offset.
    /// 
    /// @lib ShapeCircleOffset
    /// @sn circleShape:%s offset:%s
    /// 
    /// @class Shape
    /// @overload AsCircle AsCircleOffset
    function ShapeCircle(s: Shape; const offset:Point2D): Circle;



    /// Returns True if point 'pt' is in the shape.
    ///
    /// @lib
    /// @sn point:%s inShape:%s
    ///
    /// @class Point2D
    /// @method InShape
    function PointInShape(const pt: Point2d; s:Shape):Boolean;

    //---------------------------------------------------------------------------
    // Shape drawing code
    //---------------------------------------------------------------------------

      /// Draw the Shape s onto the destination bitmap. The filled Boolean indicates
      /// if the Shape should be filled.
      /// 
      /// @lib DrawOrFillShapeOnto
      /// @sn drawOnto:%s shape:%s filled:%s
      /// 
      /// @class Shape
      /// @overload Draw DrawOrFillOnto
      /// @self 2
      /// @csn drawOnto:%s filled:%s
      procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean); overload;

      /// Draw the Shape s onto the destination bitmap.
      /// 
      /// @lib DrawShapeOnto
      /// @sn drawOnto:%s shape:%s
      /// 
      /// @class Shape
      /// @overload Draw DrawOnto
      /// @self 2
      /// @csn drawOnto:%s
      procedure DrawShape(dest: Bitmap; s: Shape); overload;

      /// Fill the Shape s onto the destination bitmap.
      ///
      /// @lib FillShapeOnto
      /// @sn fillOnto:%s shape:%s
      /// 
      /// @class Shape
      /// @overload Fill FillOnto
      /// @self 2
      /// @csn fillOnto:%s
      procedure FillShape(dest: Bitmap; s: Shape); overload;

      /// Draw or fill the Shape s onto the screen at the 
      /// shapes game coordinates.
      /// 
      /// @lib DrawOrFillShape
      /// @sn drawShape:%s filled:%s
      /// 
      /// @class Shape
      /// @overload Draw DrawOrFill
      /// @csn drawFilled:%s
      procedure DrawShape(s: Shape; filled: Boolean); overload;

      /// Draw the Shape s onto the screen at the 
      /// shapes game coordinates.
      /// 
      /// @lib DrawShape
      /// 
      /// @class Shape
      /// @method Draw
      procedure DrawShape(s: Shape); overload;

      /// Fill the Shape s.
      /// 
      /// @lib FillShape
      /// 
      /// @class Shape
      /// @method Fill
      procedure FillShape(s: Shape); overload;

      /// Draw or fill the Shape s onto the screen using the
      /// Shape's location as screen coordinates.
      /// 
      /// @lib DrawOrFillShapeOnScreen
      /// @sn drawShapeOnScreen:%s filled:%s
      /// 
      /// @class Shape
      /// @overload DrawOnScreen DrawOrFillOnScreen
      /// @csn drawOnScreenFilled:%s
      procedure DrawShapeOnScreen(s: Shape; filled: Boolean); overload;

      /// Draw the Shape s onto the screen using the
      /// Shape's location as screen coordinates.
      ///
      /// @lib DrawShapeOnScreen
      ///
      /// @class Shape
      /// @method DrawOnScreen
      procedure DrawShapeOnScreen(s: Shape); overload;

      /// Fill the Shape s onto the screen using the
      /// Shape's location as screen coordinates.
      /// 
      /// @lib FillShapeOnScreen
      /// 
      /// @class Shape
      /// @method FillOnScreen
      procedure FillShapeOnScreen(s: Shape); overload;

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// Draws the first point of the shape as a pixel.
      ///
      /// @lib
      /// @sn drawOnto:%s pointShape:%s filled:%s offset:%s
      ///
      /// @class Shape
      /// @method DrawAsPoint
      /// @self 2
      /// @csn drawPointOnto:%s filled:%s offset:%s
      procedure DrawShapeAsPoint(dest: Bitmap; s:Shape; filled: Boolean; const offset:Point2D); overload;

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// draws the shape as a circle, centered on the first point with a radius defined
      /// by the distance to the second point.
      /// 
      /// @lib
      /// @sn drawOnto:%s circleShape:%s filled:%s offset:%s
      /// 
      /// @class Shape
      /// @method DrawAsCircle
      /// @self 2
      /// @csn drawCircleOnto:%s filled:%s offset:%s
      procedure DrawShapeAsCircle(dest: Bitmap; s:Shape; filled: Boolean; const offset:Point2D); overload;

      //   /// Draw the passed in shape to the specified bitmap. If filled the shape
      //   /// is drawn with a fill rather than drawn as a series of lines.
      //   ///
      //   /// @lib
      //   /// @class Shape
      //   /// @method DrawAsEllipse
      //   /// @self 2
      // procedure DrawShapeAsEllipse(dest: Bitmap; s:Shape; filled: Boolean); overload;

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// draws a line from the first point of the shape to the second point.
      /// 
      /// @lib
      /// @sn drawOnto:%s lineShape:%s filled:%s offset:%s
      /// 
      /// @class Shape
      /// @method DrawAsLine
      /// @self 2
      /// @csn drawLineOnto:%s filled:%s offset:%s
      procedure DrawShapeAsLine(dest: Bitmap; s:Shape; filled: Boolean; const offset:Point2D); overload;

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// draws the shape as a triangle based on the first three points of the shape.
      /// 
      /// @lib
      /// @sn drawOnto:%s triangleShape:%s filled:%s offset:%s
      ///
      /// @class Shape
      /// @method DrawAsTriangle
      /// @self 2
      /// @csn drawTriangleOnto:%s filled:%s offset:%s
      procedure DrawShapeAsTriangle(dest: Bitmap; s:Shape; filled: Boolean; const offset:Point2D); overload;

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// draws the points as a list of lines. A shape with 4 points has two lines in its
      /// list. If an odd numer of points are supplied then the extra point will be skipped.
      /// In this way a shape with 5 points also has 2 lines.
      /// 
      /// @lib
      /// @sn drawOnto:%s lineListShape:%s filled:%s offset:%s
      /// 
      /// @class Shape
      /// @method DrawAsLineList
      /// @self 2
      /// @csn drawLineListOnto:%s filled:%s offset:%s
      procedure DrawShapeAsLineList(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// draws the shape as a line strip. A shape with three points has two lines, one 
      /// from pt[0] to pt[1] and a second from pt[1] to pt[2].
      /// 
      /// @lib
      /// @sn drawOnto:%s lineStripShape:%s filled:%s offset:%s
      /// 
      /// @class Shape
      /// @method DrawAsLineStrip
      /// @self 2
      /// @csn drawLineStripOnto:%s filled:%s offset:%s
      procedure DrawShapeAsLineStrip(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);

      // / Draw the passed in shape to the specified bitmap. If filled the shape
      // / is drawn with a fill rather than drawn as a series of lines. This draws
      // / as a polygon where each point is connected to its neighbour and the
      // / first point is reconnected to the last point.
      // / 
      // / @lib
      // / @sn drawOnto:%s polygonShape:%s filled:%s offset:%s
      // / 
      // / @class Shape
      // / @method DrawAsPolygon
      // / @self 2
      // / @csn drawPolygonOnto:%s filled:%s offset:%s
      //procedure DrawShapeAsPolygon(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// draws the shape as a tan of triangles where each triangle is made up of
      /// the first point and two neighbouring points from the shape.
      /// 
      /// @lib
      /// @sn drawOnto:%s triangleFanShape:%s filled:%s offset:%s
      /// 
      /// @class Shape
      /// @method DrawAsTriangleFan
      /// @self 2
      /// @csn drawTriangleFonOnto:%s filled:%s offset:%s
      procedure DrawShapeAsTriangleFan(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// draws as a strip of triangles where each triangle is made up of the 
      /// three neighbouring points. In this way 4 points gives two triangles.
      /// 
      /// @lib
      /// @sn drawOnto:%s triangleStripShape:%s filled:%s offset:%s
      /// 
      /// @class Shape
      /// @method DrawAsTriangleStrip
      /// @self 2
      /// @csn drawTriangleStripOnto:%s filled:%s offset:%s
      procedure DrawShapeAsTriangleStrip(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);

      /// Draw the passed in shape to the specified bitmap. If filled the shape
      /// is drawn with a fill rather than drawn as a series of lines. This version
      /// draws as a triangle list, where each set of three points is drawn as an
      /// individual triangle and extra points are ignored. So 6, 7, and 8 points 
      /// all create 2 triangles (pt[0] + pt[1] + pt[2] and pt[3] + pt[4] + pt[5]). 
      /// 
      /// @lib
      /// @sn drawOnto:%s triangleListShape:%s filled:%s offset:%s
      /// 
      /// @class Shape
      /// @method DrawAsTriangleList
      /// @self 2
      /// @csn drawTriangleListOnto:%s filled:%s offset:%s
      procedure DrawShapeAsTriangleList(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);


  /// Returns true if the sprite and the shape have collided.
  /// 
  /// @lib SpriteShapeCollision
  /// @sn sprite:%s collisionWithShape:%s
  ///
  /// @class Sprite
  /// @method ShapeCollision
  function SpriteShapeCollision(s: Sprite; shp: Shape): Boolean; overload; 





implementation
    uses  SysUtils, sgGeometry, sgShared, sgPhysics, sgGraphics, sgCamera, sgSprites, sgTrace;

function PointOnLineList(const pt:point2d; const s:Shape):Boolean;
var
pts : Point2dArray;
i :Longint;
begin
  pts := ShapePoints(s);
  result:=False;
  for i := 0 to Length(pts) div 2 - 1 do
  begin
    if PointOnLine(pt, Linefrom(pts[i * 2], pts[i * 2 + 1])) then
    begin
      result:=True;
      exit;
    end
  end;
end;

function PointOnLineStrip(const pt:point2d; const s:Shape):Boolean;
var
pts : Point2dArray;
i :Longint;
begin
  pts := ShapePoints(s);
  result:=False;
  
  for i := 0 to Length(pts) - 2 do
  begin
    if PointOnLine(pt,Linefrom(pts[i], pts[i+ 1])) then
    begin
      result:=True;
      exit;
    end;
  end;
end;

function PointInTriangleList(const pt:point2d; const s:shape):Boolean;
var
pts : Point2dArray;
i :Longint;
begin
  pts := ShapePoints(s);
  result:=False;
  for i := 0 to Length(pts) div 3 - 1 do
  begin
    if PointInTriangle(pt, TriangleFrom(
        pts[i * 3].x, pts[i * 3].y,
        pts[i * 3 + 1].x, pts[i * 3 + 1].y,
        pts[i * 3 + 2].x, pts[i * 3 + 2].y)) then
    begin
      result:=True;
      exit;
    end
  end;
end;

function PointInTriangleFan(const pt:point2d; const s:shape):Boolean;
var
pts : Point2dArray;
i :Longint;
begin
  pts := ShapePoints(s);
  result:=False;
   for i := 0 to Length(pts) - 3 do
  begin
    if PointInTriangle(pt, TriangleFrom(
        pts[0].x,pts[0].y,
        pts[i + 1].x, pts[i + 1].y,
        pts[i + 2].x, pts[i + 2].y)) then
    begin
      result:=True;
      exit;
    end
  end;
end;

function PointInTriangleStrip(const pt:point2d; const s:shape):Boolean;
var
pts : Point2dArray;
i :Longint;
begin
  pts := ShapePoints(s);
  result:=False;
  for i := 0 to Length(pts) - 3 do
  begin
    if PointInTriangle(pt, TriangleFrom(
      pts[i].x,pts[i].y,
      pts[i + 1].x, pts[i + 1].y,
      pts[i + 2].x, pts[i + 2].y)) then
    begin
      result:=True;
      exit;
    end
  end;
end;

function PointInShape(const pt: Point2d; s:Shape):Boolean;
var
k : ShapeKind;
pts : Point2dArray;
begin
  k := PrototypeKind(s^.prototype);
  pts := ShapePoints(s);
  
  case k of
    pkPoint: result := PointOnPoint(pt, s^.pt);
    pkCircle: result := PointInCircle(pt, CircleAt(s^.pt,RoundInt(PointPointDistance(pts[0], pts[1]))));
    // pkEllipse: result := 2;
    pkLine: result := PointOnLine(pt, Linefrom(pts[0],pts[1]));
    pkTriangle: result := PointInTriangle(pt,TriangleFrom(pts[0],pts[1],pts[2]));
    pkLineList: result := PointOnLineList(pt,s);
    pkLineStrip: result := PointOnLineStrip(pt,s);
    //pkPolygon: result := 3;
    pkTriangleStrip: result := PointInTriangleStrip(pt,s);
    pkTriangleFan: result := PointInTriangleFan(pt,s);
    pkTriangleList: result := PointInTriangleList(pt,s);
    else
      begin
        RaiseException('Shape not Recognized when checking if point is in shape.');
        exit;
      end;
  end;
end;

function LinesFromShapeAsTriangleStrip(s: shape): LinesArray; overload;
var
  pts:Point2DArray;
  i:Longint;
begin
  pts:= ShapePoints(s);

  //WriteLn('ShapeLineCount: ', ShapeLineCount(s));
  SetLength(result, ShapeLineCount(s));
  if Length(result) <= 0 then exit;
  
  for i := 0 to Length(pts) - 3 do
  begin
    result[i * 2]     := LineFrom(pts[i], pts[i + 1]);
    result[i * 2 + 1] := LineFrom(pts[i], pts[i + 2]);
    //WriteLn('  Line ', LineToString(result[i*2]));
    //WriteLn('  Line ', LineToString(result[i*2 + 1]));
  end;

  result[high(result)] := LineFrom(pts[High(pts) - 1], pts[High(pts)]);
  //WriteLn('  Line ', LineToString(result[High(result)]));
end;

function LinesFrom(s: shape): LinesArray; overload;
var
k: ShapeKind;
pts: Point2DArray;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'LinesFrom(const tri: Triangle): LinesArray', '');
  {$ENDIF}
  k := PrototypeKind(s^.prototype);
  pts := ShapePoints(s);
  
  case k of
    pkCircle: SetLength(result, 0);
    //pkLine: result := PointOnLine(pt, Linefrom(pts[0],pts[1]));
    pkTriangle: result := LinesFrom(TriangleFrom(pts[0],pts[1],pts[2]));
    //pkLineList: result := PointOnLineList(pt,s);
    //pkLineStrip: result := PointOnLineStrip(pt,s);
    pkTriangleStrip: result := LinesFromShapeAsTriangleStrip(s);
    //pkTriangleFan: result := PointInTriangleFan(pt,s);
    //pkTriangleList: result := PointInTriangleList(pt,s);
    else
      begin
        RaiseException('LinesFrom not implemented with this kind of shape.');
        exit;
      end;
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'LinesFrom(const tri: Triangle): LinesArray', '');
  {$ENDIF}
end;

function ShapeAABB(s: Shape): Rectangle;
var
  pts : Point2DArray;
  minPt, maxPt: Point2D;
  i, max: Longint;
  r, l, t, b, temp: Single;
  subRect: Rectangle;
  kind: ShapeKind;
begin
  pts :=    ShapePoints(s);
  result := RectangleFrom(0,0,0,0);
  kind :=   ShapeShapeKind(s);
  if (Longint(kind) = -1) or (Length(pts) < MinimumPointsForKind(kind)) then exit;
  
  minPt := pts[0];
  maxPt := minPt;
  
  if kind = pkPoint then 
    //do nothing as the point is the maximum
  else if kind = pkCircle then
  begin
    temp := PointPointDistance(pts[0], pts[1]);
    minPt.x := minPt.x - temp;
    minPt.y := minPt.y - temp;
    maxPt.x := maxPt.x + temp;
    maxPt.y := maxPt.y + temp;
  end
  else
  begin
    
    case kind of
      pkLine: max := 1;         // line from 0 to 1
      pkTriangle: max := 2;     // triangle 0,1,2
      pkLineList: max := High(pts) - (Length(pts) mod 2); // even numbers
      pkLineStrip: max := High(pts); // all points in strip
      pkTriangleList: max := High(pts) - (Length(pts) mod 3); //groups of 3
      pkTriangleStrip: max := High(pts);
      pkTriangleFan: max := High(pts);
    else max := High(pts);
    end;
    
    for i := low(pts) + 1 to max do
    begin
      if pts[i].x < minPt.x then minPt.x := pts[i].x
      else if pts[i].x > maxPt.x then  maxPt.x := pts[i].x;
    
      if pts[i].y < minPt.y then minPt.y := pts[i].y
      else if pts[i].Y > maxPt.y then maxPt.y := pts[i].y;
    end;
  end;
  
  for i := Low(s^.subShapes) to High(s^.subShapes) do
  begin
    subRect := ShapeAABB(s^.subShapes[i]);
    
    r := RectangleRight(subRect);
    l := RectangleLeft(subRect);
    t := RectangleTop(subRect);
    b := RectangleBottom(subRect);
    
    if l < minPt.x then minPt.x := l;
    if r > maxPt.x then maxPt.x := r;
    
    if t < minPt.y then minPt.y := t;
    if b > maxPt.y then maxPt.y := b;
  end;
  
  result := RectangleFrom(minPt, maxPt);
  //DrawRectangle(colorYellow, result);
end;

function ShapeCircle(s: Shape): Circle;
begin
  result := ShapeCircle(s, PointAt(0,0));
end;

function ShapeCircle(s: Shape; const offset: Point2D): Circle;
var
  pts: Point2DArray;
begin
  pts := ShapePoints(s);
  if length(pts) < 2 then begin result := CircleAt(0,0,0); exit; end;
    
  result := CircleAt(PointAdd(pts[0], offset), RoundInt(PointPointDistance(pts[0], pts[1])));
end;

function ShapeLine(s: Shape): LineSegment;
begin
  result := ShapeLine(s, PointAt(0,0));
end;

function ShapeLine(s: Shape; const offset: Point2D): LineSegment;
var
  pts: Point2DArray;
begin
  pts := ShapePoints(s);
  if length(pts) < 2 then begin result := LineFrom(0,0,0,0); exit; end;
    
  result := LineFrom(PointAdd(pts[0], offset), PointAdd(pts[1], offset));
end;

function ShapeTriangle(s: Shape): Triangle;
begin
  result := ShapeTriangle(s, PointAt(0,0));
end;

function ShapeTriangle(s: Shape; const offset: Point2D): Triangle;
var
  pts: Point2DArray;
begin
  pts := ShapePoints(s);
  if length(pts) < 3 then begin result := TriangleFrom(0,0,0,0,0,0); exit; end;
    
  result := TriangleFrom( PointAdd(pts[0], offset), 
                          PointAdd(pts[1], offset), 
                          PointAdd(pts[2], offset));
end;

function ShapeTriangleCount(s: Shape): Longint;
begin
  result := ShapeKindTriangleCount(s, ShapeShapeKind(s));
end;

function ShapeKindTriangleCount(s: Shape; kind: ShapeKind): Longint;
var
  pts: Longint;
begin
  result := 0;

  pts := ShapePointCount(s);

  case kind of
    pkCircle: result := 0;
    pkLine: result := 0;
    pkTriangle: result := 1;
    pkLineList: result := 0;
    pkLineStrip: result := 0;
    //pkPolygon: result := 3;
    pkTriangleStrip, pkTriangleFan: result := pts - 2;
    pkTriangleList: result := pts div 3;
    else
      begin
        RaiseException('Shape not Recognized when getting line out of shape.');
        exit;
      end;
  end;
end;

function ShapeLineCount(s: Shape): Longint;
begin
  if assigned(s) then
    result := ShapeKindLineCount(s, ShapeShapeKind(s))
  else
    result := 0;
end;

function ShapeKindLineCount(s: Shape; kind: ShapeKind): Longint;
var
  pts: Longint;
begin
  result := 0;
  
  pts := ShapePointCount(s);
  
  case kind of
    pkCircle: result := 0;
    pkLine: result := 1;
    pkTriangle: result := 3;
    pkLineList: result := pts div 2;
    pkLineStrip: result := pts - 1;
    //pkPolygon: result := 3;
    pkTriangleStrip, pkTriangleFan: 
    begin
      result := 2 * pts - 3;
      if result < 0 then result := 0;
    end;
    pkTriangleList: result := (pts div 3) * 3;
    else
      begin
        RaiseException('Shape not Recognized when getting line out of shape.');
        exit;
      end;
  end;
end;

function ShapeLines(s: Shape): LinesArray;
begin
  if assigned(s) then
    result := ShapeLines(s, ShapeShapeKind(s), PointAt(0,0))
  else
    result := nil;
end;

function ShapeLines(s: Shape; kind: ShapeKind): LinesArray;
begin
  result := ShapeLines(s, kind, PointAt(0,0));
end;

function ShapeLines(s: Shape; kind: ShapeKind; const offset: Point2D): LinesArray;
var
  pts,pts1: Point2DArray;
  i: Longint;
begin
  pts := ShapePoints(s);
  SetLength(pts1, length(pts));
  
  if length(pts) < 2 then begin SetLength(result, 0); exit; end;
  if not ((kind = pkLineList) or (kind = pkLineStrip)) then exit;
  
  if kind = pkLineList then SetLength(result, Length(pts) div 2)
  else SetLength(result, Length(pts) - 1);
  
  for i := 0 to high(pts1) do pts1[i] := PointAdd(pts[i], offset);
  
  for i := 0 to High(result) do
  begin
    if kind = pkLineList then
      result[i] := LineFrom(pts1[i * 2], pts1[i * 2 + 1])
    else // strip
      result[i] := LineFrom(pts1[i], pts1[i + 1]);
  end;
end;

function ShapeTriangles(s: Shape): TriangleArray; overload;
begin
  if assigned(s) then
    result := ShapeTriangles(s, ShapeShapeKind(s))
  else
    result := nil;
end;

function ShapeTriangles(s: Shape; kind: ShapeKind): TriangleArray;
begin
  result := ShapeTriangles(s, kind, PointAt(0,0));
end;

function ShapeTriangles(s: Shape; kind: ShapeKind; const offset: Point2D): TriangleArray;
var
  pts, pts1: Point2DArray;
  i: Longint;
begin
  SetLength(result, 0);
  if not ((kind = pkTriangleStrip) or (kind = pkTriangleFan) or (kind = pkTriangleList) or (kind = pkTriangle)) then exit;
  pts := ShapePoints(s);
  if length(pts) < 3 then begin exit; end;
  
  if kind = pkTriangleList then SetLength(result, Length(pts) div 3)
  else if kind = pkTriangle then SetLength(result, 1)
  else SetLength(result, Length(pts) - 2);
  
  SetLength(pts1, Length(pts));
  for i := 0 to high(pts) do pts1[i] := PointAdd(pts[i], offset);
  
  for i := 0 to High(result) do
  begin
    case kind of
      pkTriangle:     result[i] := TriangleFrom(pts1[i],     pts1[i + 1],     pts1[i + 2]);
      pkTriangleList: result[i] := TriangleFrom(pts1[i * 3], pts1[i * 3 + 1], pts1[i * 3 + 2]);
      pkTriangleStrip:result[i] := TriangleFrom(pts1[i],     pts1[i + 1],     pts1[i + 2]);
      pkTriangleFan:  result[i] := TriangleFrom(pts1[0],     pts1[i + 1],     pts1[i + 2]);
    end;
  end;
end;

function ShapeRectangleIntersect(s: Shape; const rect: Rectangle): Boolean;
var
  kind: ShapeKind;
  i: Longint;
begin
  result := false;
  
  if not assigned(s) then exit;
  if not RectanglesIntersect(rect, ShapeAABB(s)) then exit;
  
  kind := ShapeShapeKind(s);
  
  case kind of
    pkPoint:      result := true;
    pkCircle:     result := CircleRectCollision(ShapeCircle(s), rect);
    pkLine:       result := RectLineCollision(rect, ShapeLine(s));
    pkTriangle:   result := TriangleRectangleIntersect(ShapeTriangle(s), rect);
    pkLineList, pkLineStrip:  
                  result := LinesRectIntersect(ShapeLines(s, kind), rect);
    pkTriangleStrip, pkTriangleFan, pkTriangleList:
                  result := TrianglesRectangleIntersect(ShapeTriangles(s, kind), rect);
  end;
  
  if not result then
  begin
    //Check sub shapes
    for i := 0 to High(s^.subShapes) do
    begin
      // if we have an intersection then return true
      if ShapeRectangleIntersect(s^.subShapes[i], rect) then
      begin
        result := true;
        exit;
      end;
    end;
  end;
end;

function ShapeVectorOutOfRect(s: shape; const bounds: Rectangle; const velocity: Vector  ): vector;
var
maxIdx :Longint;
begin
 result := VectorOverLinesFromLines(LinesFrom(bounds),LinesFrom(s), velocity, maxIdx);
end;

//=============================================================================

function PointPrototypeFrom(const pt: Point2D): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'PointPrototypeFrom(const pt: Point2D): ShapePrototype', '');
  {$ENDIF}
  
  New(result);
  
  SetLength(result^.points, 1);
  result^.points[0] := pt;
  result^.kind := pkPoint;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'PointPrototypeFrom(const pt: Point2D): ShapePrototype', '');
  {$ENDIF}
end;

function CirclePrototypeFrom(const pt: Point2D; r: Single): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'CirclePrototypeFrom(const pt: Point2D', '');
  {$ENDIF}
  
  New(result);
  
  SetLength(result^.points, 2);
  result^.points[0] := pt;
  result^.points[1] := PointAt(r, r);
  result^.kind := pkCircle;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'CirclePrototypeFrom(const pt: Point2D', '');
  {$ENDIF}
end;

// function EllipsePrototypeFrom(const pt: Point2D; w, h: Single): ShapePrototype;
// begin
//   New(result);
//   
//   SetLength(result^.points, 2);
//   result^.points[0] := pt;
//   result^.points[1] := PointAt(w, h);
//   result^.kind := pkEllipse;
// end;

function LinePrototypeFrom(const startPt, endPt: Point2D): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'LinePrototypeFrom(const startPt, endPt: Point2D): ShapePrototype', '');
  {$ENDIF}
  
  New(result);
  
  SetLength(result^.points, 2);
  result^.points[0] := startPt;
  result^.points[1] := endPt;
  result^.kind := pkLine;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'LinePrototypeFrom(const startPt, endPt: Point2D): ShapePrototype', '');
  {$ENDIF}
end;

function TrianglePrototypeFrom(const pt1, pt2, pt3: Point2D): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'TrianglePrototypeFrom(const pt1, pt2, pt3: Point2D): ShapePrototype', '');
  {$ENDIF}
  
  New(result);
  
  SetLength(result^.points, 3);
  result^.points[0] := pt1;
  result^.points[1] := pt2;
  result^.points[2] := pt3;
  result^.kind := pkTriangle;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'TrianglePrototypeFrom(const pt1, pt2, pt3: Point2D): ShapePrototype', '');
  {$ENDIF}
end;

function LineListPrototypeFrom(const points: Point2DArray): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'LineListPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
  
  result := PrototypeFrom(points, pkLineList);
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'LineListPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
end;

function LineStripPrototypeFrom(const points: Point2DArray): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'LineStripPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
  
  result := PrototypeFrom(points, pkLineStrip);
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'LineStripPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
end;

// function PolygonPrototypeFrom(const points: Point2DArray): ShapePrototype;
// begin
//   {$IFDEF TRACE}
//     TraceEnter('sgGeometry', 'PolygonPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
//   {$ENDIF}
//   
//   result := PrototypeFrom(points, pkPolygon);
//   
//   {$IFDEF TRACE}
//     TraceExit('sgGeometry', 'PolygonPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
//   {$ENDIF}
// end;

function TriangleStripPrototypeFrom(const points: Point2DArray): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'TriangleStripPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
  
  result := PrototypeFrom(points, pkTriangleStrip);
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'TriangleStripPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
end;

function TriangleFanPrototypeFrom(const points: Point2DArray): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'TriangleFanPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
  
  result := PrototypeFrom(points, pkTriangleFan);
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'TriangleFanPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
end;

function TriangleListPrototypeFrom(const points: Point2DArray): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'TriangleListPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
  
  result := PrototypeFrom(points, pkTriangleList);
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'TriangleListPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  {$ENDIF}
end;

function PrototypeFrom(const points: Point2DArray; kind:ShapeKind): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'PrototypeFrom(const points: Point2DArray', '');
  {$ENDIF}
  
  if Length(points) < MinimumPointsForKind(kind) then
  begin
    RaiseException('Insufficient points assigned to shape given its kind. Min is ' 
      + IntToStr(MinimumPointsForKind(kind))
      + ' was supplied ' 
      + IntToStr(Length(points)));
    exit;
  end;
  
  New(result);
  
  PrototypeSetKind(result, kind);
  PrototypeSetPoints(result, points);
  result^.shapeCount := 0;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'PrototypeFrom(const points: Point2DArray', '');
  {$ENDIF}
end;

procedure FreePrototype(var p: ShapePrototype);
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'FreePrototype(var p: ShapePrototype)', '');
  {$ENDIF}
  
  if not Assigned(p) then exit;
  if p^.shapeCount > 0 then begin RaiseWarning('Freeing prototype while it is still used by ' + IntToStr(p^.shapeCount) + ' shapes.'); end;
  
  SetLength(p^.points, 0);
  Dispose(p);
  CallFreeNotifier(p);
  p := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'FreePrototype(var p: ShapePrototype)', '');
  {$ENDIF}
end;

//=============================================================================

function MinimumPointsForKind(k: ShapeKind): Longint;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'MinimumPointsForKind(k: ShapeKind): Longint', '');
  {$ENDIF}
  
  case k of
    pkPoint: result := 1;
    pkCircle: result := 2;
    // pkEllipse: result := 2;
    pkLine: result := 2;
    pkTriangle: result := 3;
    pkLineList: result := 2;
    pkLineStrip: result := 2;
    // pkPolygon: result := 3;
    pkTriangleStrip: result := 3;
    pkTriangleFan: result := 3;
    pkTriangleList: result := 3;
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'MinimumPointsForKind(k: ShapeKind): Longint', '');
  {$ENDIF}
end;

function PrototypePointCount(p: ShapePrototype): Longint;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'PrototypePointCount(p: ShapePrototype): Longint', '');
  {$ENDIF}
  
  if not assigned(p) then result := 0
  else result := Length(p^.points);
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'PrototypePointCount(p: ShapePrototype): Longint', '');
  {$ENDIF}
end;

procedure PrototypeSetPoints(p: ShapePrototype; const points: Point2DArray);
var
  i: Longint;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'PrototypeSetPoints(p: ShapePrototype', '');
  {$ENDIF}
  
  if not assigned(p) then exit;
  
  if Length(points) < MinimumPointsForKind(p^.kind) then
  begin
    RaiseException('Insufficient points assigned to shape given its kind. Min is ' 
      + IntToStr(MinimumPointsForKind(p^.kind)) 
      + ' was supplied ' 
      + IntToStr(Length(points)));
    exit;
  end;
  
  SetLength(p^.points, Length(points));
  
  for i := 0 to High(points) do
  begin
    p^.points[i] := points[i];
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'PrototypeSetPoints(p: ShapePrototype', '');
  {$ENDIF}
end;

function PrototypePoints(p: ShapePrototype): Point2DArray;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'PrototypePoints(p: ShapePrototype): Point2DArray', '');
  {$ENDIF}
  
  if not assigned(p) then SetLength(result,0)
  else result := p^.points;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'PrototypePoints(p: ShapePrototype): Point2DArray', '');
  {$ENDIF}
end;

procedure PrototypeSetKind(p: ShapePrototype; kind: ShapeKind);
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'PrototypeSetKind(p: ShapePrototype', '');
  {$ENDIF}
  
  if not assigned(p) then
  begin
    RaiseException('No shape prototype supplied to set kind.');
    exit;
  end;
  
  p^.kind := kind;
  case kind of
    pkPoint: p^.drawWith := @DrawShapeAsPoint;
    pkCircle: p^.drawWith := @DrawShapeAsCircle;
    // pkEllipse: p^.drawWith := @DrawShapeAsEllipse;
    pkLine: p^.drawWith := @DrawShapeAsLine;
    pkTriangle: p^.drawWith := @DrawShapeAsTriangle;
    pkLineList: p^.drawWith := @DrawShapeAsLineList;
    pkLineStrip: p^.drawWith := @DrawShapeAsLineStrip;
    // pkPolygon: p^.drawWith := @DrawShapeAsPolygon;
    pkTriangleStrip: p^.drawWith := @DrawShapeAsTriangleStrip;
    pkTriangleFan: p^.drawWith := @DrawShapeAsTriangleFan;
    pkTriangleList: p^.drawWith := @DrawShapeAsTriangleList;
    else p^.drawWith := nil; 
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'PrototypeSetKind(p: ShapePrototype', '');
  {$ENDIF}
end;

function PrototypeKind(p: ShapePrototype): ShapeKind;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'PrototypeKind(p: ShapePrototype): ShapeKind', '');
  {$ENDIF}
  
  if not assigned(p) then result := ShapeKind(-1)
  else result := p^.kind;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'PrototypeKind(p: ShapePrototype): ShapeKind', '');
  {$ENDIF}
end;

//=============================================================================

function ShapeAtPoint(p: ShapePrototype; const pt: Point2D): Shape;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeAtPoint(p: ShapePrototype', '');
  {$ENDIF}
  
  New(result);
  result^.prototype := p;
  result^.pt := pt;
  result^.angle := 0.0;
  result^.scale := PointAt(1,1);
  result^.color := ColorWhite;
  SetLength(result^.subShapes, 0);
  
  if Assigned(p) then p^.shapeCount += 1;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeAtPoint(p: ShapePrototype', '');
  {$ENDIF}
end;

procedure FreeShape(var s: Shape);
var
  i: Longint;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'FreeShape(var s: Shape)', '');
  {$ENDIF}
  
  if not Assigned(s) then exit;
  if Assigned(s^.prototype) then s^.prototype^.shapeCount -= 1;
  
  for i := 0 to High(s^.subShapes) do
  begin
    FreeShape(s^.subShapes[i])
  end;
  SetLength(s^.subShapes, 0);
  
  Dispose(s);
  CallFreeNotifier(s);
  s := nil;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'FreeShape(var s: Shape)', '');
  {$ENDIF}
end;

procedure UpdateSubShapePoints(s: Shape; const parentM: Matrix2D);
var
  m: Matrix2D;
  i: Longint;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'UpdateSubShapePoints(s: Shape', '');
  {$ENDIF}
  
  if not Assigned(s) then begin exit; end;
  
  // Copy the points from the prototype
  s^.ptBuffer := Copy(s^.prototype^.points, 0, Length(s^.prototype^.points));
  
  // Scale, angle and translate based on this shape's position * parent's matrix
  //m := ScaleMatrix(s.scale) * RotationMatrix(s.angle) * TranslationMatrix(s.pt);
  m := ScaleRotateTranslateMatrix(s^.scale, s^.angle, s^.pt) * parentM;
  ApplyMatrix(m, s^.ptBuffer);
  
  for i := 0 to High(s^.subShapes) do
  begin
    UpdateSubShapePoints(s^.subShapes[i], m);
  end;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'UpdateSubShapePoints(s: Shape', '');
  {$ENDIF}
end;

procedure UpdateShapePoints(s: Shape);
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'UpdateShapePoints(s: Shape)', '');
  {$ENDIF}
  
  UpdateSubShapePoints(s, IdentityMatrix());
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'UpdateShapePoints(s: Shape)', '');
  {$ENDIF}
end;

function ShapePoints(s: Shape): Point2DArray;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapePoints(s: Shape): Point2DArray', '');
  {$ENDIF}
  
  if not Assigned(s) then begin SetLength(result, 0); exit; end;
  if not Assigned(s^.ptBuffer) then
  begin
    UpdateShapePoints(s);
  end;
  result := s^.ptBuffer;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapePoints(s: Shape): Point2DArray', '');
  {$ENDIF}
end;

function ShapePointCount(s: Shape): Longint;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapePointCount(s: Shape): Longint', '');
  {$ENDIF}
  
  if not Assigned(s) then begin result := 0; exit; end;
  result := PrototypePointCount(s^.prototype);
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapePointCount(s: Shape): Longint', '');
  {$ENDIF}
end;

procedure ShapeSetAngle(s: Shape; angle: Single);
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeSetAngle(s: Shape', '');
  {$ENDIF}
  
  if not Assigned(s) then begin exit; end;
  s^.angle := angle;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeSetAngle(s: Shape', '');
  {$ENDIF}
end;

function ShapeAngle(s: Shape): Single;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeAngle(s: Shape): Single', '');
  {$ENDIF}
  
  if not Assigned(s) then begin result := 0; exit; end;
  result := s^.angle;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeAngle(s: Shape): Single', '');
  {$ENDIF}
end;

procedure ShapeSetScale(s: Shape; const scale: Point2D);
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeSetScale(s: Shape', '');
  {$ENDIF}
  
  if not Assigned(s) then begin exit; end;
  s^.scale := scale;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeSetScale(s: Shape', '');
  {$ENDIF}
end;

function ShapeScale(s: Shape): Point2D;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeScale(s: Shape): Point2D', '');
  {$ENDIF}
  
  if not Assigned(s) then begin result := PointAt(0,0); exit; end;
  result := s^.scale;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeScale(s: Shape): Point2D', '');
  {$ENDIF}
end;

function ShapeShapePrototype(s: Shape): ShapePrototype;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeShapePrototype(s: Shape): ShapePrototype', '');
  {$ENDIF}
  
  if not Assigned(s) then begin result := nil; exit; end;
  result := s^.prototype;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeShapePrototype(s: Shape): ShapePrototype', '');
  {$ENDIF}
end;

procedure ShapeSetPrototype(s: Shape; p: ShapePrototype);
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeSetPrototype(s: Shape', '');
  {$ENDIF}
  
  if not Assigned(s) then exit;
  if Assigned(s^.prototype) then s^.prototype^.shapeCount -= 1;
  
  s^.prototype := p;
  
  if Assigned(s^.prototype) then s^.prototype^.shapeCount += 1;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeSetPrototype(s: Shape', '');
  {$ENDIF}
end;

procedure ShapeAddSubShape(parent, child: Shape);
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeAddSubShape(parent, child: Shape)', '');
  {$ENDIF}
  
  if not Assigned(parent) then exit;
  if not Assigned(child) then exit;
    
  SetLength(parent^.subShapes, Length(parent^.subShapes) + 1);
  parent^.subShapes[High(parent^.subShapes)] := child;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeAddSubShape(parent, child: Shape)', '');
  {$ENDIF}
end;

function ShapeColor(s: Shape): Color;
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeColor(s: Shape): Color', '');
  {$ENDIF}
  
  if not Assigned(s) then begin result := ColorBlack; exit; end;
  result := s^.color;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeColor(s: Shape): Color', '');
  {$ENDIF}
end;

procedure ShapeSetColor(s: Shape; c: Color);
begin
  {$IFDEF TRACE}
    TraceEnter('sgGeometry', 'ShapeSetColor(s: Shape', '');
  {$ENDIF}
  
  if not Assigned(s) then begin exit; end;
  
  s^.color := c;
  
  {$IFDEF TRACE}
    TraceExit('sgGeometry', 'ShapeSetColor(s: Shape', '');
  {$ENDIF}
end;

function ShapeShapeKind(s: Shape): ShapeKind;
begin
  if not assigned(s) then begin result := ShapeKind(-1); exit; end;
    
  result := PrototypeKind(ShapeShapePrototype(s));
end;

//=============================================================================

procedure DrawShapeAsPoint(dest: Bitmap; s:Shape; filled: Boolean; const offset: Point2D); overload;
var
  pts: Point2DArray;
begin
  pts := ShapePoints(s);
  if length(pts) = 0 then exit;
  
  DrawPixel(dest, s^.color, PointAdd(pts[0], offset));
end;

procedure DrawShapeAsCircle(dest: Bitmap; s:Shape; filled: Boolean; const offset: point2D); overload;
var
  // r: Single;
  // pts: Point2DArray;
  c: Circle;
begin
  c := ShapeCircle(s);
  
  //pts := ShapePoints(s);
  //if length(pts) < 2 then exit;
  //r := PointPointDistance(pts[0], pts[1]);
  
  DrawCircle(dest, s^.color, filled, c); //PointAdd(pts[0], offset), Round(r));
end;

// procedure DrawShapeAsEllipse(dest: Bitmap; s:Shape; filled: Boolean; const offset: Point2D); overload;
// var
//   pts: Point2DArray;
// begin
//   pts := ShapePoints(s);
//   if length(pts) < 2 then exit;
//   
//   DrawEllipse(dest, s^.color, filled, 
//     Round(pts[0].x+offset.X),
//     Round(pts[0].y+offset.Y), 
//     Round(pts[1].x) - Round(pts[0].x), 
//     Round(pts[1].y) - Round(pts[0].y));
// end;

procedure DrawShapeAsLine(dest: Bitmap; s:Shape; filled: Boolean; const offset: Point2D); overload;
var
  //pts: Point2DArray;
  ln: LineSegment;
begin
  ln := ShapeLine(s);
  // pts := ShapePoints(s);
  // if length(pts) < 2 then exit;
  
  DrawLine(dest, s^.color, ln); //PointAdd(pts[0], offset), PointAdd(pts[1], offset));
end;

procedure _DrawTriangles(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D; kind: ShapeKind);
var
  i: Longint;
  tri: TriangleArray;
begin
  tri := ShapeTriangles(s, kind);
  
  for i := 0 to High(tri) do
  begin
    if filled then FillTriangle(s^.color, tri[i])
    else DrawTriangle(s^.color, tri[i]);
  end;
end;

procedure DrawShapeAsTriangle(dest: Bitmap; s:Shape; filled: Boolean; const offset: Point2D); overload;
// var
//   //pts: Point2DArray;
//   tri: Triangle;
begin
  _DrawTriangles(dest, s, filled, offset, pkTriangle);
  // tri := ShapeTriangle(s);
  // 
  // // pts := ShapePoints(s);
  // // if length(pts) < 3 then exit;
  // 
  // if filled then
  //   FillTriangle(dest, s^.color, tri)
  //   //FillTriangle(dest, s^.color, pts[0].x+offset.X, pts[0].y+offset.Y, pts[1].x+offset.X, pts[1].y+offset.Y, pts[2].x+offset.X, pts[2].y+offset.Y)
  // else
  //   DrawTriangle(dest, s^.color, tri);
  //   //DrawTriangle(dest, s^.color, pts[0].x+offset.X, pts[0].y+offset.Y, pts[1].x+offset.X, pts[1].y+offset.Y, pts[2].x+offset.X, pts[2].y+offset.Y);
end;

procedure _DrawLines(dest: Bitmap; s: Shape; const offset: Point2D; kind: ShapeKind);
var
  lines: LinesArray;
  i: Longint;
begin
  lines := ShapeLines(s, kind, offset);
  for i := 0 to High(lines) do
  begin
    DrawLine(dest, s^.color, lines[i]);
  end;
end;

procedure DrawShapeAsLineList(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
begin
  _DrawLines(dest, s, offset, pkLineList);
end;
// var
//   i: Longint;
//   pts: Point2DArray;
// begin
//   pts := ShapePoints(s);
//   
//   for i := 0 to Length(pts) div 2 - 1 do
//   begin
//     DrawLine(dest, s^.color, PointAdd(pts[i * 2], offset), PointAdd(pts[i * 2 + 1], offset));
//   end;
// end;

procedure DrawShapeAsLineStrip(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
begin
  _DrawLines(dest, s, offset, pkLineStrip);
end;
// var
//   i: Longint;
//   pts: Point2DArray;
// begin
//   pts := ShapePoints(s);
//   
//   for i := 0 to Length(pts) - 2 do
//   begin
//     DrawLine(dest, s^.color, PointAdd(pts[i], offset), PointAdd(pts[i+ 1], offset));
//   end;
// end;

{procedure DrawShapeAsPolygon(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
var
  i, l: Longint;
  pts: Point2DArray;
begin
  pts := ShapePoints(s);
  if Length(pts) < 3 then exit;
  
  l := Length(pts);
  
  if filled then
  begin
    for i := 0 to Length(pts) - 3 do
    begin
      FillTriangle(dest, s^.color,
        pts[i].x,pts[i].y,
        pts[i + 1].x, pts[i + 1].y,
        pts[(i + 2) mod l].x, pts[(i + 2) mod l].y);
    end;
  end
  else
  begin
    for i := 0 to Length(pts) - 2 + 1 do
    begin
      DrawLine(dest, s^.color, pts[i], pts[(i+ 1) mod l]);
    end;
  end;
  
end;}

// procedure DrawTriangleFan(dest: Bitmap; s: Shape; const offset: Point2D);
// // var
// //   i: Longint;
// //   pts: Point2DArray;
// begin
//   DrawTriangleFan(dest, s, false, offset);
//   // pts := ShapePoints(s);
//   // 
//   // for i := 0 to Length(pts) - 3 do
//   // begin
//   //   DrawTriangle(dest, s^.color,
//   //     pts[0].x+offset.X,pts[0].y+offset.Y,
//   //     pts[i + 1].x+offset.X, pts[i + 1].y+offset.Y,
//   //     pts[i + 2].x+offset.X, pts[i + 2].y+offset.Y);
//   // end;
// end;
// 
// procedure FillTriangleFan(dest: Bitmap; s: Shape; const offset: Point2D);
// // var
// //   i: Longint;
// //   pts: Point2DArray;
// begin
//   DrawTriangleFan(dest, s, true, offset);
//   // pts := ShapePoints(s);
//   // 
//   // for i := 0 to Length(pts) - 3 do
//   // begin
//   //   FillTriangle(dest, s^.color,
//   //     pts[0].x+offset.X,pts[0].y+offset.Y,
//   //     pts[i + 1].x+offset.X, pts[i + 1].y+offset.Y,
//   //     pts[i + 2].x+offset.X, pts[i + 2].y+offset.Y);
//   // end;
// end;

procedure DrawShapeAsTriangleFan(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
begin
  //if filled then FillTriangleFan(dest, s, offset) else DrawTriangleFan(dest, s, offset);
  _DrawTriangles(dest, s, filled, offset, pkTriangleFan);
end;

// procedure DrawTriangleStrip(dest: Bitmap; s: Shape; const offset: Point2D);
// begin
//   DrawTriangleStrip(dest, s, false, offset);
// end;
// // var
// //   i: Longint;
// //   pts: Point2DArray;
// // begin
// //   pts := ShapePoints(s);
// //   
// //   for i := 0 to Length(pts) - 3 do
// //   begin
// //     DrawTriangle(dest, s^.color,
// //       pts[i].x+offset.X,pts[i].y+offset.Y,
// //       pts[i + 1].x+offset.X, pts[i + 1].y+offset.Y,
// //       pts[i + 2].x+offset.X, pts[i + 2].y+offset.Y);
// //   end;
// // end;
// 
// procedure FillTriangleStrip(dest: Bitmap; s: Shape; const offset: Point2D);
// begin
//   DrawTriangleStrip(dest, s, true, offset);
// end;
// 
// // var
// //   i: Longint;
// //   pts: Point2DArray;
// // begin
// //   pts := ShapePoints(s);
// //   
// //   for i := 0 to Length(pts) - 3 do
// //   begin
// //     FillTriangle(dest, s^.color,
// //       pts[i].x+offset.X,pts[i].y+offset.Y,
// //       pts[i + 1].x+offset.X, pts[i + 1].y+offset.Y,
// //       pts[i + 2].x+offset.X, pts[i + 2].y+offset.Y);
// //   end;
// // end;

procedure DrawShapeAsTriangleStrip(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
begin
  _DrawTriangles(dest, s, filled, offset, pkTriangleStrip);
  //if filled then FillTriangleStrip(dest, s, offset) else DrawTriangleStrip(dest, s, offset);
end;

// procedure DrawTriangleList(dest: Bitmap; s: Shape; const offset: Point2D);
// begin
//   DrawTriangleList(dest, s, false, offset);
// end;
// // var
// //   i: Longint;
// //   pts: Point2DArray;
// // begin
// //   pts := ShapePoints(s);
// //   
// //   for i := 0 to Length(pts) div 3 - 1 do
// //   begin
// //     DrawTriangle(dest, s^.color,
// //       pts[i * 3].x+offset.X, pts[i * 3].y+offset.Y,
// //       pts[i * 3 + 1].x+offset.X, pts[i * 3 + 1].y+offset.Y,
// //       pts[i * 3 + 2].x+offset.X, pts[i * 3 + 2].y+offset.Y);
// //   end;
// // end;
// 
// procedure FillTriangleList(dest: Bitmap; s: Shape; const offset: Point2D);
// begin
//   DrawTriangleList(dest, s, true, offset);
// end;
// // var
// //   i: Longint;
// //   pts: Point2DArray;
// // begin
// //   pts := ShapePoints(s);
// //   
// //   for i := 0 to Length(pts) div 3 - 1 do
// //   begin
// //     FillTriangle(dest, s^.color,
// //       pts[i * 3].x+offset.X, pts[i * 3].y+offset.Y,
// //       pts[i * 3 + 1].x+offset.X, pts[i * 3 + 1].y+offset.Y,
// //       pts[i * 3 + 2].x+offset.X, pts[i * 3 + 2].y+offset.Y);
// //   end;
// // end;

procedure DrawShapeAsTriangleList(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
begin
  _DrawTriangles(dest, s, filled, offset, pkTriangleList);
  //if filled then FillTriangleList(dest, s, offset) else DrawTriangleList(dest, s, offset);
end;


procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D); overload;
var
  i: Longint;
begin
  s^.prototype^.drawWith(dest, s, filled, offset);
  
  for i := 0 to High(s^.subShapes) do
  begin
    DrawShape(dest, s^.subShapes[i], filled, offset);
  end;
end;

procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean); overload;
begin
  DrawShape(dest, s, filled, PointAt(0,0));
end;

procedure DrawShape(dest: Bitmap; s: Shape); overload;
begin
  DrawShape(dest, s, false, PointAt(0,0));
end;

procedure FillShape(dest: Bitmap; s: Shape); overload;
begin
  DrawShape(dest, s, true, PointAt(0,0));
end;

procedure DrawShape(s: Shape; filled: Boolean); overload;
begin
  DrawShape(screen, s, filled, PointAt(-CameraX(), -CameraY()));
end;

procedure DrawShape(s: Shape); overload;
begin
  DrawShape(screen, s, false, PointAt(-CameraX(), -CameraY()));
end;

procedure FillShape(s: Shape); overload;
begin
  DrawShape(screen, s, true, PointAt(-CameraX, -CameraY));
end;

procedure DrawShapeOnScreen(s: Shape; filled: Boolean); overload;
begin
  DrawShape(screen, s, filled);
end;

procedure DrawShapeOnScreen(s: Shape); overload;
begin
  DrawShapeOnScreen(s, false);
end;

procedure FillShapeOnScreen(s: Shape); overload;
begin
  DrawShapeOnScreen(s, true);
end;

function SpriteShapeCollision(s: Sprite; shp: Shape): Boolean; overload;
begin
  result := false;
  if s = nil then exit;
  
  if not ShapeRectangleIntersect(shp, SpriteCollisionRectangle(s)) then 
    exit;
  
  //  Check pixel level details
  if SpriteCollisionKind(s) = AABBCollisions then 
    result := true
  else
    //TODO: add pixel level shape collisions
    result := true; //CellRectCollision(s^.collisionBitmap, SpriteCurrentCell(s), Round(s^.position.x), Round(s^.position.y), rect);    
end;


end.