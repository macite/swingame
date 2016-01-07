//=============================================================================
// sgBackendTypes.pas
//=============================================================================
//
// This provides the type details for records pointed to by abstract types
// from sgTypes.
//

unit sgBackendTypes;

interface
uses sgTypes, sgDriverSDL2Types;

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
    REGION_PTR = 'REGI';
    PANEL_PTR = 'PANL';
    ARDUINO_PTR = 'ARDU';
    TIMER_PTR = 'TIMR';
    FONT_PTR = 'FONT';
    WINDOW_PTR = 'WIND';
    // HTTP_HEADER_PTR = 'HHDR';
    HTTP_REQUEST_PTR = 'HREQ';
    HTTP_RESPONSE_PTR = 'HRES';
    CONNECTION_PTR = 'CONP';
    MESSAGE_PTR = 'MSGP';
    SERVER_SOCKET_PTR = 'SVRS';
    NONE_PTR = 'NONE'; // done after clear


  type
    PointerIdentifier = array [0..3] of Char;

    /// @type ResolutionArray
    /// @array_wrapper
    /// @field data: array of Resolution
    ResolutionArray = Array of Resolution;

    /// @type LongintArray
    /// @array_wrapper
    /// @field data: array of Longint
    LongintArray = array of Longint;

    /// @type SingleArray
    /// @array_wrapper
    /// @field data: array of Single
    SingleArray = array of Single;

    /// @type StringArray
    /// @array_wrapper
    /// @field data: array of String
    StringArray = array of String;

    /// @type Point2DArray
    /// @array_wrapper
    /// @field data: array of Point2D
    Point2DArray = Array of Point2D;

    /// @type TriangleArray
    /// @array_wrapper
    /// @field data: array of Triangle
    TriangleArray = Array of Triangle;

    /// @type LinesArray
    /// @array_wrapper
    /// @field data: array of LineSegment
    LinesArray = Array of LineSegment;

    /// @type BitmapArray
    /// @array_wrapper
    /// @field data: array of Bitmap
    BitmapArray = array of Bitmap;

    /// The named index collection type is used to maintain a named collection of 
    /// index values that can then be used to lookup the location of the
    /// named value within a collection.
    ///
    /// @struct NamedIndexCollection
    /// @via_pointer
    NamedIndexCollection = packed record
      names: StringArray;   // The names of these ids
      ids: Pointer;         // A pointer to a TStringHash with this data
    end;

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

    ImageData = packed record
      surface            : sg_drawing_surface;  // The actual bitmap image
      clipStack          : Array of Rectangle;         // The clipping rectangle history for the bitmap
    end;

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
      filename, name     : String;              // Used for locating bitmaps during load/freeing
      image              : ImageData;
      
      //Used for bitmaps that are made up of cells
      cellW                : Longint;    // The width of a cell
      cellH                : Longint;    // The height of a cell
      cellCols             : Longint;    // The columns of cells in the bitmap
      cellRows             : Longint;    // The rows of cells in the bitmap
      cellCount            : Longint;    // The total number of cells in the bitmap
      
      nonTransparentPixels : Array of Array of Boolean;  // Pixel mask used for pixel level collisions
    end;

    /// An array of SpriteEventHandlers used internally by Sprites.
    ///
    /// @type SpriteEventHandlerArray
    /// @array_wrapper
    /// @field data: array of SpriteEventHandler
    SpriteEventHandlerArray = array of SpriteEventHandler;

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

      anchorPoint:            Point2D;
      positionAtAnchorPoint:  Boolean;
        
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

    /// GUIList is a list GUI Element which contains ItemLists
    ///
    /// @class GUIList
    /// @pointer_wrapper
    /// @no_free_pointer_wrapper
    /// @field pointer: pointer
    GUIList = ^GUIListData;
    
    /// GUILabel is a Label GUI Element which contains string font and font alignment
    ///
    /// @class GUILabel
    /// @pointer_wrapper
    /// @no_free_pointer_wrapper
    /// @field pointer: ^GUILabelData
    GUILabel = ^GUILabelData;
    
    /// GUICheckbox is a Checkbox GUI Element which contains a bool
    ///
    /// @class GUICheckbox
    /// @pointer_wrapper
    /// @no_free_pointer_wrapper
    /// @field pointer: ^GUICheckboxData
    GUICheckbox = ^GUICheckboxData;

    /// GUITextbox is a textbox gui component in swingame 
    /// it has a string font length limit region and font alignment
    ///
    /// @class GUITextbox
    /// @pointer_wrapper
    /// @no_free_pointer_wrapper
    /// @field pointer: ^RegionData
    GUITextbox = ^GUITextboxData;

    /// GUI radio group is a radio group gui component in swingame.
    ///
    ///
    /// @class GUIRadioGroup
    /// @pointer_wrapper
    /// @no_free_pointer_wrapper
    /// @field pointer : ^GUIRadioGroupData
    GUIRadioGroup = ^GUIRadioGroupData;

    /// Each list item has text and an image
    ///
    /// @struct GUIListItem
    ///
    /// @via_pointer
    GUIListItem = packed record
      text:     String;
      image:    Bitmap;
      cell:     Longint;
      parent:   GUIList;
    end;
    
    /// @struct GUIListData
    /// @via_pointer
    GUIListData = packed record
      verticalScroll: Boolean;
      //The areas for the up/left down/right scrolling buttons
      scrollUp:     Rectangle;
      scrollDown:   Rectangle;
      scrollArea:   Rectangle;
      columns:      Longint;
      rows:         Longint;
      rowHeight:    Single;
      colWidth:     Single;
      scrollSize:   Longint;
      placeholder:  Array of Rectangle;
      activeItem:   Longint;
      startingAt:   Longint;
      items:        Array of GUIListItem;
      scrollButton: Bitmap;
    end;

    /// @struct GUICheckboxData
    /// @via_pointer
    GUICheckboxData = packed record
      state:        Boolean;
    end;

      /// @struct GUILabelData
      /// @via_pointer
    GUILabelData = packed record
      contentString:  String;
    end;

    RegionPtr = ^RegionData;
    PanelPtr = ^PanelData;

    /// @struct RegionData
    /// @via_pointer
    RegionData = packed record
      id:             PointerIdentifier;
      stringID:       String;
      kind:           GUIElementKind;
      regionIdx:      Longint;
      elementIndex:   Longint;
      area:           Rectangle;
      active:         Boolean;
      parent:         PanelPtr;
      callbacks:      Array of GUIEventCallback;
      font:           Font;
      alignment:      FontAlignment;
    end;
  
    /// @struct GUIRadioGroupData
    /// @via_pointer
    GUIRadioGroupData = packed record
      groupID:      string;
      buttons:      Array of RegionPtr;
      activeButton: Longint;
    end;
    
    /// @struct GUITextboxData
    /// @via_pointer
    GUITextboxData = packed record
      contentString:  String;
      lengthLimit:    Longint;
      forRegion:      RegionPtr;
    end;

    ///@struct PanelData
    ///@via_pointer
    PanelData = packed record
      id:                   PointerIdentifier;
      name:                 String;
      filename:             String;
      panelID:              Longint;
      area:                 Rectangle;
      visible:              Boolean;
      active:               Boolean;
      draggable:            Boolean;

      // The panel's bitmaps
      panelBitmap:          Bitmap;
      panelBitmapInactive:  Bitmap;
      panelBitmapActive:    Bitmap;

      // The regions within the Panel
      regions:              Array of RegionData;
      regionIds:            NamedIndexCollection;

      // The extra details for the different kinds of controls
      labels:               Array of GUILabelData;
      checkBoxes:           Array of GUICheckboxData;
      radioGroups:          Array of GUIRadioGroupData;
      textBoxes:            Array of GUITextBoxData;
      lists:                Array of GUIListData;

      modal:                Boolean;                      // A panel that is modal blocks events from panels shown earlier.

      // Event callback mechanisms
      DrawAsVectors:        Boolean;
    end;

    ArduinoPtr = ^ArduinoData;

    ///@struct ArduinoData
    ///@via_pointer
    ArduinoData = record
      id: PointerIdentifier;
      name: String;
      ptr: Pointer;
      
      port: String;
      baud: LongInt;
      
      open: Boolean;
      hasError: Boolean;
      errorMessage: String;
    end;

    TimerPtr = ^TimerData;

    /// @struct TimerData
    /// @via_pointer
    TimerData = packed record
      id: PointerIdentifier;
      startTicks: Longword;
      pausedTicks: Longword;
      paused: Boolean;
      started: Boolean;
      
      name: String;         // the name of the timer registered with the _timers dictionary
    end;

    FontPtr = ^FontData;

    /// @struct FontData
    /// @via_pointer
    FontData = packed record
      id: PointerIdentifier;
      fptr : Pointer;
        //fptr: PTTF_Font;
        name: String;
    end;

    WindowPtr = ^WindowData;

    WindowData = packed record
      id:         PointerIdentifier;
      caption:    String;
      
      image:      ImageData;

      x, y:       Longint;

      open:       Boolean;
      fullscreen: Boolean;
      border:     Boolean;
      eventData:  sg_window_data;

      screenRect: Rectangle;
    
      tempString:       String;

      maxStringLen:     LongInt;
      textBitmap:       Bitmap;
      cursorBitmap:     Bitmap;
      font:             Font;
      foreColor:        Color;
      backgroundColor:  Color;
      area:             Rectangle; // area for input text
      readingString:    Boolean;
      textCancelled:    Boolean;
    end;

    UnknownDataPtr = ^UnknownData;
    UnknownData = packed record
      id : PointerIdentifier;
    end;

    ConnectionPtr = ^ConnectionData;
    MessagePtr = ^MessageData;
    ServerSocketPtr = ^ServerData;

    HttpResponsePtr = ^HttpResponseData;

    HttpHeaderData = record
      name : String;
      value: String;
    end;

    HttpRequestData = packed record
      id         : PointerIdentifier;
      // requestType: HttpMethod;
      url        : String;
      version    : String;
      headername : StringArray;
      headervalue: StringArray;
      body       : String;
    end;

    HttpResponseData = record
      id    : PointerIdentifier;
      data  : sg_http_response;
    end;

    MessageData = packed record
      id        : PointerIdentifier;
      data      : String;
      protocol  : ConnectionType;
      
      //TCP has a
      connection: ^ConnectionData;

      //UDP is from
      host      : String;
      port      : Word;
    end;  

    ConnectionData = packed record
      id              : PointerIdentifier;
      name            : String;
      socket          : sg_network_connection;
      ip              : LongWord;
      port            : LongInt;
      open            : Boolean;
      protocol        : ConnectionType;
      stringIP        : String;   //Allow for Reconnection
      messages        : array of MessageData;

      msgLen          : LongInt;  // This data is used to handle splitting of messages
      partMsgData     : String;   //   over multiple packets
    end;

    /// @struct ServerData
    /// @via_pointer
    ServerData = packed record
      id              : PointerIdentifier;
      name            : String;
      socket          : sg_network_connection; // socket used to accept connections
      port            : Word;
      newConnections  : LongInt; // the number of new connections -- reset on new scan for connections
      protocol        : ConnectionType;
      connections     : array of ConnectionPtr; // TCP connections
      messages        : array of MessageData; // UDP messages
    end;


  function PtrKind(p: Pointer): PointerIdentifier;

  function ToSoundEffectPtr(s: SoundEffect): SoundEffectPtr;
  function ToMusicPtr(m: Music): MusicPtr;
  function ToAnimationScriptPtr(a: AnimationScript): AnimationScriptPtr;
  function ToAnimationPtr(a: Animation): AnimationPtr;
  function ToBitmapPtr(b: Bitmap): BitmapPtr;
  function ToSpritePtr(s: Sprite): SpritePtr;
  function ToPanelPtr(p: Panel): PanelPtr;
  function ToRegionPtr(r: Region): RegionPtr;
  function ToArduinoPtr(a: ArduinoDevice): ArduinoPtr;
  function ToTimerPtr(t: Timer): TimerPtr;
  function ToFontPtr(f: Font): FontPtr;
  function ToWindowPtr(w: Window): WindowPtr;
  function ToConnectionPtr(c: Connection): ConnectionPtr;
  function ToServerSocketPtr(c: ServerSocket): ServerSocketPtr;
  function ToHttpResponsePtr(c: HttpResponse): HttpResponsePtr;
  function ToMessagePtr(c: Message): MessagePtr;

  function ToSurfacePtr(p: Pointer): psg_drawing_surface;

implementation
uses sgShared;

  function PtrKind(p: Pointer): PointerIdentifier;
  var
    ptr: UnknownDataPtr;
  begin
    ptr := UnknownDataPtr(p);
    if Assigned(ptr) then
      result := ptr^.id
    else
      result := NONE_PTR;
  end;

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

  function ToPanelPtr(p: Panel): PanelPtr;
  begin
    result := PanelPtr(p);
    if Assigned(result) and (result^.id <> PANEL_PTR) then
    begin
      RaiseWarning('Attempted to access a Panel that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToRegionPtr(r: Region): RegionPtr;
  begin
    result := RegionPtr(r);
    if Assigned(result) and (result^.id <> REGION_PTR) then
    begin
      RaiseWarning('Attempted to access a Region that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToArduinoPtr(a: ArduinoDevice): ArduinoPtr;
  begin
    result := ArduinoPtr(a);
    if Assigned(result) and (result^.id <> ARDUINO_PTR) then
    begin
      RaiseWarning('Attempted to access an Arduino that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToTimerPtr(t: Timer): TimerPtr;
  begin
    result := TimerPtr(t);
    if Assigned(result) and (result^.id <> TIMER_PTR) then
    begin
      RaiseWarning('Attempted to access a Timer that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToFontPtr(f: Font): FontPtr;
  begin
    result := FontPtr(f);
    if Assigned(result) and (result^.id <> FONT_PTR) then
    begin
      RaiseWarning('Attempted to access a Font that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToWindowPtr(w: Window): WindowPtr;
  begin
    result := WindowPtr(w);
    if Assigned(result) and (result^.id <> WINDOW_PTR) then
    begin
      RaiseWarning('Attempted to access a Window that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToConnectionPtr(c: Connection): ConnectionPtr;
  begin
    result := ConnectionPtr(c);
    if Assigned(result) and (result^.id <> CONNECTION_PTR) then
    begin
      RaiseWarning('Attempted to access a Connection that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToServerSocketPtr(c: ServerSocket): ServerSocketPtr;
  begin
    result := ServerSocketPtr(c);
    if Assigned(result) and (result^.id <> SERVER_SOCKET_PTR) then
    begin
      RaiseWarning('Attempted to access a Server Socket that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToMessagePtr(c: Message): MessagePtr;
  begin
    result := MessagePtr(c);
    if Assigned(result) and (result^.id <> MESSAGE_PTR) then
    begin
      RaiseWarning('Attempted to access a Message that appears to be an invalid pointer');
      result := nil;
    end;
  end;

  function ToHttpResponsePtr(c: HttpResponse): HttpResponsePtr;
  begin
    result := HttpResponsePtr(c);
    if Assigned(result) and (result^.id <> HTTP_RESPONSE_PTR) then
    begin
      RaiseWarning('Attempted to access a HTTP Response that appears to be an invalid pointer');
      result := nil;
    end;
  end;


  function ToSurfacePtr(p: Pointer): psg_drawing_surface;
  var
    id: PointerIdentifier;
    w: WindowPtr;
    b: BitmapPtr; 
  begin
    id := PtrKind(p);
    if id = WINDOW_PTR then 
    begin
      w := ToWindowPtr(p);
      result := @w^.image.surface;
    end
    else if id = BITMAP_PTR then
    begin
      b := ToBitmapPtr(p);
      result := @b^.image.surface;
    end
    else result := nil;
  end;
end.
