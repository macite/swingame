//=============================================================================
// sgTypes.pas
//=============================================================================
//
// The Types unit contains the data types used by SwinGames for shapes,
// Sprites, Bitmaps, Sounds, etc.
//
//=============================================================================

/// @header sgTypes
unit sgTypes;

//=============================================================================
interface
//=============================================================================

  type
    /// A Point2D represents an location in Cartesian coordinates (x,y).
    /// The x value represents the distance from the left edge of the window or bitmap, increasing
    /// in value as you travel right. The y value represents the distance from the top 
    /// edge of the window or bitmap, and increases as you travel down toward the bottom.
    ///
    /// Point2D is a great way to keep track of the location of something in a 2D space like
    /// a Window or Bitmap.
    ///
    /// @struct Point2D
    /// @sameas Vector
    Point2D = packed record
      x, y: Single;
    end;

    /// Vectors represent a direction and distance, and can be visualised as an arrow from
    /// one point to another in 2 dimensional space. Internally, the Vector is stored as its
    /// x and y components (same as a `Point2D`).
    ///
    /// Vector is a great way to represent movement or forces. You could use a vector to track
    /// how much a character moves each update (as the Vector stores the direction and distance).
    /// Similarly, you could use a Vector to represent gravity or other forces. You can then 
    /// add a number of force vectors together to get a final force to be applied to a character.
    ///
    /// @struct Vector
    /// @field x: Single
    /// @field y: Single
    /// @sameas Point2D
    Vector = Point2D;
    
    /// Rectangles are simple rectangle shapes that exist at a point and have a set width 
    /// and height. This means that the rectangle always has edges that follow the sides of
    /// the Window or Bitmap (so they are aligned with the x and y axes). The rectangle's
    /// position is its top left corner - it then extends to the right and down from 
    /// this position.
    ///       
    /// @struct Rectangle
    Rectangle = packed record
      x, y: Single;
      width, height: Single;
    end;

    /// Quads (Quadrilaterals) are shapes with 4 sides, but unlike `Rectangle`, these shapes can have axis that
    /// do not line up with screen/bitmap axis.
    ///
    /// Points should be constructed with the top left as the first point, top right as the second, 
    /// bottom left as the third, and bottom right as the last point. Other orders may give unexpected
    /// outcomes.
    ///
    /// @struct Quad
    Quad = packed record
      points: array [0..3] of Point2D;
    end;

    /// Circles have a center point and a radius. This means that, unlike other shapes
    /// like the `Rectangle`, the circle extends out both left and right, and up and down
    /// from the point you position it at.
    ///
    /// @struct Circle
    Circle = packed record
      center: Point2D;
      radius: Single;
    end;

    /// A Line Segment is a line that extends from a start point
    /// to an end point.
    ///
    /// @struct LineSegment
    LineSegment = packed record
        startPoint: Point2D;
        endPoint: Point2D;
    end;
    
    /// A Triangle consists of three points -- one for each of the points on the triangle
    ///
    /// @struct Triangle
    Triangle = packed record 
      points : array [0..2] of Point2D;
    end;


    //TODO: Add touch support when iOS version is working again!
    //
    // /// @struct Finger
    // Finger = packed record
    //   id : Int64;
    //   position : Point2D;
    //   positionDelta : Point2D;
    //   lastPosition : Point2D;
    //   pressure : Single; //unknown unit guessing that it uses the size of the contact point on the screen. not much documentation from SDL.
    //   // lastPressure :Word;
    //   down : Boolean;
    // end;

    
    // /// @type FingerArray
    // /// @array_wrapper
    // /// @field data: array of Finger
    // FingerArray = Array of Finger;
      


    /// Each Resolution value has a format, as well as refresh rate, width and height.
    /// These can be used to determine the available screen resolutions.
    ///
    /// @struct Resolution
    Resolution = packed record
      format : Longword;
      refreshRate, width, height: Longint;
    end;
        
    // /// @struct AccelerometerMotion
    // AccelerometerMotion = packed record
    //   xAxis: Longint;
    //   yAxis: Longint;
    //   zAxis: Longint;
    // end;

    /// @ignore
    WndIR = record end;

    /// The Window type is used to refer to a window that you have opened
    /// in SwinGame. You create new Windows using `OpenWindow` and they can 
    /// be closed using `CloseWindow`.
    ///
    /// @class Window
    /// @pointer_wrapper
    /// @field pointer: pointer
    Window = ^WndIR;
    
    /// The `SoundEffect` type is used to refer to sound effects that can be
    /// played by the SwinGame audio code. Sound effects are loaded with
    /// `LoadSoundEffect`, played using `PlaySoundEffect`, and must be
    /// released using `FreeMusic`.
    ///
    /// SwinGame will mix the audio from multiple sound effects, making it
    /// possible to play multiple SoundEffects, or even to play the one
    /// SoundEffect multiple times.
    ///
    /// You can check if a SoundEffect is currently playing using
    /// `SoundEffectPlaying`.
    ///
    /// To stop a SoundEffect playing use `StopSoundEffect`. This will stop all
    /// instances of this one sound effect from playing.
    ///
    /// @note Use `Music` for background music for your games.
    ///
    /// @class SoundEffect
    /// @pointer_wrapper
    /// @field pointer: pointer
    SoundEffect = Pointer;
    
    
    /// The Music type is used to refer to music that can be
    /// played by the SwinGame audio code. Music files are loaded with
    /// `LoadMusic`, played using `PlayMusic`, and must be
    /// released using `FreeMusic`.
    ///
    /// Unlike `SoundEffect`s, there can only be one Music track
    /// playing at a time. Playing an new track will stop the currently
    /// playing music and switch to the new track.
    ///
    /// @class Music
    /// @pointer_wrapper
    /// @field pointer: pointer
    Music = Pointer;

    /// In SwinGame, Matrices can be used to combine together a number of
    /// operations that need to be performed on Vectors. You can translate, 
    /// rotate and scale, and combine these together into a single matrix
    /// that can then be applied to vectors and points.
    ///
    /// @struct Matrix2D
    Matrix2D = packed record
      elements : Array [0..2,0..2] of Single;
    end;

    /// The color type is used within the SwinGameAPI to store color values.
    /// The color values are represented as 32bit RGBA values where R stores the
    /// color's red component, G stores the green component, B stores the blue
    /// component and A stores an alpha value representing the opacity (transparency)
    ///  of the of the color.
    ///
    /// @type Color
    /// @data_wrapper
    /// @field data: Longword
    Color = Longword;
    
    /// An Animation Script stores a number of animations. Each animation is
    /// a list of frames linked together in the order they are to be performed.
    /// Each frame has the cell to draw, the duration it should be drawn for and
    /// the next frame in the animation. The template can then be used to create
    /// a number of animations.
    ///
    /// @class AnimationScript
    /// @pointer_wrapper
    /// @field pointer: pointer
    AnimationScript = Pointer;
        
    /// An Animation stores all of the details about animating a single image. It
    /// includes the current animation frame (which has the cell to be drawn, and
    /// the number of updates, the next frame, etc) and the number of times this has 
    /// been updated.
    ///
    /// When you update an animation it checks its `AnimationScript` to determine if it
    /// should move to the next frame. 
    ///
    /// When draw a bitmap with an animation, the animation is used to determine the 
    /// current cell to draw. This will change over time as the Animation is updated
    /// and it moves between the frames of the `AnimationScript`.
    ///
    /// @class Animation
    /// @pointer_wrapper
    /// @field pointer: pointer
    Animation = Pointer;
    
    /// @ignore
    BmpIR = record end;

    /// The bitmap type is a pointer to a BitmapData. The BitmapData record
    /// contains the data used by the SwinGame API to represent
    /// bitmaps. You can create new bitmaps in memory for drawing operatings
    /// using the `CreateBitmap` function. This can then be optimised for drawing
    /// to the screen using the `OptimiseBitmap` routine. Also see the `DrawBitmap`
    /// routines.
    ///
    /// @class Bitmap
    /// @pointer_wrapper
    /// @field pointer: pointer
    Bitmap = ^BmpIR;
    
    /// Determines the effect of the camera on a drawing operation.
    /// `DrawToScreen` means camera has no affect.
    /// `DrawToWorld` means camera has an affect.
    /// `DrawDefault` means camera has an affect only if drawn to the screen. 
    /// @enum DrawingDest
    DrawingDest = (
        DrawToScreen, // no camera effect
        DrawToWorld,  // camera effect
        DrawDefault   // camera effect on screen, but not on bitmaps
      );

    /// The drawing options struct contains the data that can 
    /// be used to provide different options to various drawing
    /// operations.
    ///
    /// @struct DrawingOptions
    DrawingOptions = packed record
      dest : Pointer;           // The bitmap or window used to draw on to
      scaleX : Single;          // Scale data
      scaleY : Single;          //
      angle : Single;           // Angle for rotations
      anchoroffsetX : Single;   // Centre point for rotations
      anchoroffsetY : Single;   //
      flipX : Boolean;          // Flip data
      flipY : Boolean;          //
      isPart : Boolean;         // Draw just a part?
      part : Rectangle;         // Part to draw
      camera: DrawingDest;      // Draw to world or screen coordinates (camera has effect?)
      lineWidth: Longint;       // Specify the width of line drawings.
    end;
        
    /// The CollisionSide enumeration is used to indicate the side a collision
    /// has occurred on.
    ///
    /// @enum CollisionSide
    CollisionSide = (
      Top,
      Bottom,
      Left,
      Right,
      TopLeft,
      TopRight,
      BottomLeft,
      BottomRight,
      None
    );
    
    /// Use this with the resource path functions to get the path to a
    /// given resource. Using these functions ensures that your resource
    /// paths are correct across different platforms
    ///
    /// @enum ResourceKind
    ResourceKind = (
      BundleResource,
      TimerResource,
      BitmapResource,
      FontResource,
      MusicResource,
      MapResource,
      SoundResource,
      AnimationResource,    //in load order,  Animation must be > sound
      PanelResource,        //                Panel must be > sound + bitmap
      OtherResource
    );
    
    /// @enum CollisionTestKind
    CollisionTestKind = (
        PixelCollisions,
        // ShapeCollision,
        AABBCollisions
      );
    
    /// This enumeration contains a list of all of the different kinds of
    /// events that a Sprite can raise. When the event is raised the assocated
    /// SpriteEventKind value passed to the event handler to indicate the
    /// kind of event that has occurred.
    ///
    /// @enum SpriteEventKind
    SpriteEventKind = ( 
      SpriteArrivedEvent,            // Sprite has arrived at the end of a move
      SpriteAnimationEndedEvent,     // The Sprite's animation has ended (not looped)
      SpriteTouchedEvent,            // The Sprite was touched
      SpriteClickedEvent             // The Sprite was touched
    );

    /// Sprites are used to represent Sprites drawn to the screen. Create a
    /// sprite using the CreateSprite function, and free it when complete with
    /// the FreeSprite function. The sprite contain a number of bitmaps used to
    /// store animations, or the like. Sprite drawing operations will draw the
    /// Sprite's current frame.
    ///
    /// @class Sprite
    /// @pointer_wrapper
    /// @field pointer: Pointer
    Sprite = Pointer;

    /// The SpriteEventHandler function pointer is used when you want to register
    /// to receive events from a Sprite.
    ///
    /// @type SpriteEventHandler
    SpriteEventHandler = procedure (s: Sprite; evt: SpriteEventKind); 

    /// SpriteFunctions are used with SpritePacks to provide a procedure to be
    /// called for each of the Sprites in the SpritePack.
    ///
    /// @type SpriteFunction
    SpriteFunction = procedure(s: Sprite);

    /// SpriteSingleFunctions are used with SpritePacks to provide a procedure to be
    /// called for each of the Sprites in the SpritePack. This version allows a 
    /// single value to be passed as a parameter along with the call.
    ///
    /// @type SpriteSingleFunction
    SpriteSingleFunction = procedure(s: Sprite; val: Single);
    
    /// A timer can be used to track how much time has elapsed since the timer
    /// was started. In games this can be used to ensure the game runs at the
    /// right speed regardless of the framerate of the users computer.
    /// 
    /// @class Timer
    /// @pointer_wrapper
    /// @field pointer: pointer
    Timer = Pointer;
        
    /// Fonts are used to render text to bitmaps and to the screen.
    /// Fonts must be loaded using the CreateFont routine. Also see the
    /// DrawText and DrawTextLines routines.
    ///
    /// @class Font
    /// @pointer_wrapper
    /// @field pointer: pointer
    Font = Pointer;

    /// Use font styles to set the style of a font. Setting the style is time
    /// consuming, so create alternative font variables for each different
    /// style you want to work with. Note that these values can be logical
    /// ORed together to combine styles, e.g. BoldFont or ItalicFont = both
    /// bold and italic.
    ///
    /// @enum FontStyle
    FontStyle = (
        NormalFont      = 0,
        BoldFont        = 1,
        ItalicFont      = 2,
        UnderlineFont   = 4
      );

    /// Use font alignment for certain drawing operations. With these
    /// operations you specify the area to draw in as well as the alignment
    /// within that area. See DrawTextLines.
    ///
    /// @enum FontAlignment
    FontAlignment = (
        AlignLeft   = 1,
        AlignCenter = 2,
        AlignRight  = 4
      );
    
    /// A mouse can have many different types of buttons. Most people know
    /// about the simple Left and Right buttons, but there is also a Middle
    /// button (sometimes part of a scoll wheel). Scroll wheel movement is also
    /// treated as mouse button "clicks" of either the wheel "up" or "down"
    /// buttons.
    ///
    /// @enum MouseButton
    MouseButton = (
      NoButton,
      LeftButton,
      MiddleButton,
      RightButton,
      WheelUpButton,
      WheelDownButton,
      MouseX1Button,
      MouseX2Button
    );

    /// @enum KeyCode
    KeyCode = (
      UnknownKey = 0,
      BackspaceKey = 8,
      TabKey = 9,
      ClearKey = 12,
      ReturnKey = 13,
      PauseKey = 19,
      EscapeKey = 27,
      SpaceKey = 32,
      ExclaimKey = 33,
      DoubleQuoteKey = 34,
      HashKey = 35,
      DollarKey = 36,
      AmpersandKey = 38,
      QuoteKey = 39,
      LeftParenKey = 40,
      RightParenKey = 41,
      AsteriskKey = 42,
      PlusKey = 43,
      CommaKey = 44,
      MinusKey = 45,
      PeriodKey = 46,
      SlashKey = 47,
      Key0 = 48,
      Key1 = 49,
      Key2 = 50,
      Key3 = 51,
      Key4 = 52,
      Key5 = 53,
      Key6 = 54,
      Key7 = 55,
      Key8 = 56,
      Key9 = 57,
      ColonKey = 58,
      SemiColonKey = 59,
      LessKey = 60,
      EqualsKey = 61,
      GreaterKey = 62,
      QuestionKey = 63,
      AtKey = 64,

      // Skip uppercase letters

      LeftBracketKey = 91,
      BackslashKey = 92,
      RightBracketKey = 93,
      CaretKey = 94,
      UnderscoreKey = 95,
      BackquoteKey = 96,
      AKey = 97,
      BKey = 98,
      CKey = 99,
      DKey = 100,
      EKey = 101,
      FKey = 102,
      GKey = 103,
      HKey = 104,
      IKey = 105,
      JKey = 106,
      KKey = 107,
      LKey = 108,
      MKey = 109,
      NKey = 110,
      OKey = 111,
      PKey = 112,
      QKey = 113,
      RKey = 114,
      SKey = 115,
      TKey = 116,
      UKey = 117,
      VKey = 118,
      WKey = 119,
      XKey = 120,
      YKey = 121,
      ZKey = 122,
      DeleteKey = 127,
      // End of ASCII mapped keysyms

      // Numeric keypad
      KeyPad0 = 256,
      KeyPad1 = 257,
      KeyPad2 = 258,
      KeyPad3 = 259,
      KeyPad4 = 260,
      KeyPad5 = 261,
      KeyPad6 = 262,
      KeyPad7 = 263,
      KeyPad8 = 264,
      KeyPad9 = 265,
      KeyPadPeriod = 266,
      KeyPadDivide = 267,
      KeyPadMultiply = 268,
      KeyPadMinus = 269,
      KeyPadPlus = 270,
      KeyPadEnter = 271,
      KeyPadEquals = 272,

      // Arrows + Home/End pad
      UpKey = 273,
      DownKey = 274,
      RightKey = 275,
      LeftKey = 276,
      InsertKey = 277,
      HomeKey = 278,
      EndKey = 279,
      PageUpKey = 280,
      PageDownKey = 281,

      // Function keys
      F1Key = 282,
      F2Key = 283,
      F3Key = 284,
      F4Key = 285,
      F5Key = 286,
      F6Key = 287,
      F7Key = 288,
      F8Key = 289,
      F9Key = 290,
      F10Key = 291,
      F11Key = 292,
      F12Key = 293,
      F13Key = 294,
      F14Key = 295,
      F15Key = 296,

      // Key state modifier keys
      NumLockKey = 300,
      CapsLockKey = 301,
      ScrollLockKey = 302,
      RightShiftKey = 303,
      LeftShiftKey = 304,
      RightCtrlKey = 305,
      LeftCtrlKey = 306,
      RightAltKey = 307,
      LeftAltKey = 308,
      RightMetaKey = 309,
      LeftMetaKey = 310,
      LeftSuperKey = 311, // Left "Windows" key
      RightSuperKey = 312, // Right "Windows" key
      ModeKey = 313, // "Alt Gr" key

      // Miscellaneous function keys
      HelpKey = 315,
      SysReqKey = 317,
      MenuKey = 319,
      PowerKey = 320, // Power Macintosh power key
      
      // Simplified Keys
      ShiftKey = 321,
      CtrlKey = 322,
      AltKey = 323,
      CommandKey = 324,
      SuperKey = 325,
      WindowsKey = 326,
      OptionKey = 327
    );
    
    /// The FreeNotifier is a function pointer used to notify user programs of
    /// swingame resources being freed. This should not be used by user programs.
    ///
    /// @type FreeNotifier
    FreeNotifier = procedure (p: Pointer); cdecl;
    
    /// GUIElementKind is an enum of all the GUI element types (such as buttons,
    /// labels and textboxes).
    /// 
    ///
    /// @enum GUIElementKind
    GUIElementKind = ( 
      gkLabel = 1, 
      gkButton = 2, 
      gkCheckBox = 4, 
      gkRadioGroup = 8, 
      gkTextBox = 16, 
      gkList = 32,
      gkAnyKind = 63 // = (1 or 2 or 4 or 8 or 16 or 32) 
      );

    /// The Event kind is an enum of all the events that could happen to a gui element
    ///
    /// @enum EventKind
    EventKind = (
        ekClicked,
        ekTextEntryEnded,
        ekSelectionMade
      );

    /// The file dialog select type is an enum of how a file dialog displays files/directories
    ///
    /// @enum FileDialogSelectType
    FileDialogSelectType = ( 
      fdFiles = 1, 
      fdDirectories = 2, 
      fdFilesAndDirectories = 3 // = (1 or 2)
      );
    
    /// In SwinGame a Panel is an input area that contains a number of GUI elements (like a 
    /// Window in the operating system). You can have a number of Panels open at once and 
    /// they are drawn with DrawInterface and UpdateInterface.
    ///
    /// @class Panel
    /// @pointer_wrapper
    /// @field pointer: Pointer
    Panel = Pointer;
    
    /// Region is the area within a panel and is used to represent the positions of the
    /// different kinds of elements you want to appear for the user to interact with.
    ///
    /// @class Region
    /// @pointer_wrapper
    /// @no_free_pointer_wrapper
    /// @field pointer: Pointer
    Region = Pointer;
    
    /// GUIEventCallback is a callback function for gui events in SwinGame. 
    /// This callback will be passed the region where the event occurred and
    /// the kind of event it was. You can then use this to trigger actions within
    /// your program. These callbacks are called as part of the process that
    /// runs within `UpdateInterface`. 
    ///
    /// @type GUIEventCallback
    GUIEventCallback = procedure (r: Region; kind: EventKind);
    
    /// A serial connection to an Arduino device. You can use this to send
    /// and receive data from the Sketch you have running within your
    /// Arduino device.
    ///
    /// @class ArduinoDevice
    /// @pointer_wrapper
    /// @field pointer : Pointer
    ArduinoDevice = Pointer;

    /// @enum ConnectionType
    ConnectionType = (
      TCP,
      UDP,
      UnknownConnection
    );

    // /// @ignore
    // HREQIR = record end;
    
    /// @ignore
    HRESPIR = record end;

    // /// @class HttpRequest
    // /// @pointer_wrapper
    // /// @field pointer : Pointer
    // HttpRequest = ^HREQIR;

    /// The HttpResponse contains the data that is the response to a http request.
    ///
    /// @class HttpResponse
    /// @pointer_wrapper
    /// @field pointer : Pointer
    HttpResponse = ^HRESPIR;

    /// @ignore
    CONIR = record end;
    /// @ignore
    MSGIR = record end;
    /// @ignore
    SVRRIR = record end;

    /// The Pointer to ConnectionData
    ///
    /// @class Connection
    /// @pointer_wrapper
    /// @field pointer : Pointer
    Connection  = ^CONIR;

    /// You can use the networking code to send
    /// Messages between programs over the network.
    ///
    /// @class Message
    /// @pointer_wrapper
    /// @field pointer : Pointer
    Message = ^MSGIR;
    
    /// @class ServerSocket
    /// @pointer_wrapper
    /// @field pointer : Pointer
    ServerSocket = ^SVRRIR;


    
//=============================================================================
implementation
//=============================================================================

uses sgShared;



//=============================================================================

initialization
begin
  InitialiseSwinGame();
end;

end.
