//=============================================================================
// sgAnimations.pas
//=============================================================================
//
// The Animation unit is responsible for defining and managing animation
// information for SwinGame.
//
// Change History:
//
// Version 3:
// - 2010-01-28: David  : Changed LoadAnimationScriptNamed to use an already loaded 
//                        bitmap if found
// - 2009-12-21: Andrew : Changed to include sound filename in animation loading.
// - 2009-12-18: Andrew : Added in code to verify animations have no loops
//                        with a 0 duration.
// - 2009-12-15: Andrew : Updated animation handling to use new NamedIndexCollection.
//                      : Fixed loading of animations with ranges like [,]
//                      : Added code to query animations, and more create code.
// - 2009-12-10: Andrew : Got basic Animation features working
// - 2009-12-08: Andrew : Changed name to AnimationScript
// - 2009-12-08: Andrew : Created
//=============================================================================



/// Animations in SwinGame can be used to move between cells in bitmaps and
/// sprites. Each Animation generates a number sequence that can then be used
/// when drawing bitmaps.
///
/// @module Animations
/// @static
unit sgAnimations;

//=============================================================================
interface
    uses sgTypes;
//=============================================================================
    
//----------------------------------------------------------------------------
// Creating an AnimationScript
//----------------------------------------------------------------------------
    
    /// Load animation details from a animation frames file.
    ///
    /// @lib
    /// @sn loadAnimationScriptFromFile:%s
    ///
    /// @class AnimationScript
    /// @constructor
    /// @csn initFromFile:%s
    function LoadAnimationScript(filename: String) : AnimationScript;
    
    /// Frees loaded animation frames data. Use this when you will no longer be 
    /// using the animation for any purpose, including within sprites.
    ///
    /// @lib
    ///
    /// @class AnimationScript
    /// @dispose
    procedure FreeAnimationScript(var scriptToFree: AnimationScript);
    
    
    
//----------------------------------------------------------------------------
// AnimationScript mapping routines
//----------------------------------------------------------------------------
    
    /// The index of the animation within the animation template that has the supplied name.
    ///
    /// @lib
    /// @sn animationScript:%s indexOfAnimation:%s
    ///
    /// @class AnimationScript
    /// @method IndexOfAnimation
    /// @csn indexOfAnimation:%s
    ///
    /// @doc_details
    function AnimationIndex(temp: AnimationScript; name: String): Longint;
    
    /// The name of the animation within the animation template at the specified index.
    ///
    /// @lib
    /// @sn animationScript:%s nameOfAnimation:%s
    ///
    /// @class AnimationScript
    /// @method NameOfAnimation
    /// @csn nameOfAnimation:%s
    ///
    /// @doc_details
    function AnimationName(temp: AnimationScript; idx: Longint): String;
    
    
    
//----------------------------------------------------------------------------
// AnimationScript mapping routines
//----------------------------------------------------------------------------
    
    /// Loads and returns a `AnimationScript`. The supplied ``filename`` is used to
    /// locate the `AnimationScript` to load. The supplied ``name`` indicates the 
    /// name to use to refer to this in SwinGame. The `AnimationScript` can then be
    /// retrieved by passing this ``name`` to the `AnimationScriptNamed` function. 
    ///
    /// @lib
    /// @sn loadAnimationScriptNamed:%s fromFile:%s
    ///
    /// @class AnimationScript
    /// @constructor
    /// @csn initWithName:%s fromFile:%s
    function LoadAnimationScriptNamed(name, filename: String): AnimationScript;
    
    /// Determines if SwinGame has animation frames loaded for the supplied ``name``.
    /// This checks against all loaded animation frames, those loaded without a name
    /// are assigned the filename as a default.
    ///
    /// @lib
    ///
    /// @doc_details
    function HasAnimationScript(name: String): Boolean;
    
    /// Returns the `AnimationScript` that has been loaded with the specified ``name``,
    /// see `LoadAnimationScriptNamed`.
    ///
    /// @lib
    function AnimationScriptNamed(name: String): AnimationScript;
    
    /// Releases the SwinGame resources associated with the animation template of the
    /// specified ``name``.
    ///
    /// @lib
    procedure ReleaseAnimationScript(name: String);
    
    /// Releases all of the animation templates that have been loaded.
    ///
    /// @lib
    procedure ReleaseAllAnimationScripts();
    
    
    /// Returns the name of the Animation Script.
    ///
    /// @lib
    ///
    /// @class AnimationScript
    /// @getter Name
    function AnimationScriptName(script: AnimationScript): String;
    
    
//----------------------------------------------------------------------------
// Creating an Animation
//----------------------------------------------------------------------------
    
    /// Creates an animation from a `AnimationScript`. This may play a sound effect
    /// if the animation is set to play a sound effect on its first frame.
    ///
    /// @lib CreateAnimationNamed
    /// @sn animationNamed:%s from:%s
    ///
    /// @class Animation
    /// @constructor
    /// @csn initAsName:%s from:%s
    function CreateAnimation(identifier: String;    script: AnimationScript): Animation; overload;
    
    /// Creates an animation from a `AnimationScript`. If ``withSound`` is ``true``, this may
    /// play a sound effect if the animation is set to play a sound effect on its first frame.
    ///
    /// @lib CreateAnimationNamedWithSound
    /// @sn animationNamed:%s from:%s withSound:%s
    ///
    /// @class Animation
    /// @constructor
    /// @csn initAsName:%s from:%s withSound:%s
    ///
    /// @doc_details
    function CreateAnimation(identifier: String;    script: AnimationScript; withSound: Boolean): Animation; overload;
    
    /// Creates an animation from an `AnimationScript`. If ``withSound`` is ``true``, this may
    /// play a sound effect if the animation is set to play a sound effect on its first frame.
    ///
    /// @lib
    /// @sn animationAtIndex:%s from:%s withSound:%s
    ///
    /// @class Animation
    /// @constructor
    /// @csn initAtIndex:%s from:%s withSound:%s
    ///
    /// @doc_details
    function CreateAnimation(identifier: Longint;    script: AnimationScript; withSound: Boolean): Animation; overload;
    
    /// Creates an animation from an `AnimationScript`. This may play a sound effect
    /// if the animation is set to play a sound effect on its first frame.
    ///
    /// @lib CreateAnimationWithSound
    /// @sn animationAtIndex:%s from:%s
    ///
    /// @class Animation
    /// @constructor
    /// @csn initAtIndex:%s from:%s
    ///
    /// @doc_details
    function CreateAnimation(identifier: Longint;    script: AnimationScript): Animation; overload;
    
    /// Disposes of the resources used in the animation.
    ///
    /// @lib
    ///
    /// @class Animation
    /// @dispose
    procedure FreeAnimation(var ani: Animation);
    
    
    
//----------------------------------------------------------------------------
// Drawing Animations
//----------------------------------------------------------------------------
    
    /// Assign a new starting animation to the passed in animation from the `AnimationScript`.
    /// This may play a sound if the first frame of the animation is linked to a sound effect.
    ///
    /// @lib AssignAnimationNamed
    /// @sn assignAnimationNamed:%s to:%s from:%s
    ///
    /// @class Animation
    /// @overload AssignAnimation AssignAnimationNamed
    /// @csn assignAnimationNamed:%s from:%s
    procedure AssignAnimation(anim: Animation; name: String; script: AnimationScript); overload;
    
    /// Assign a new starting animation to the passed in animation from the `AnimationScript`.
    /// This may play a sound if the first frame of the animation is linked to a sound effect, and withSound is true.
    ///
    /// @lib AssignAnimationNamedWithSound
    /// @sn assignAnimationNamed:%s to:%s from:%s withSound:%s
    ///
    /// @class Animation
    /// @overload AssignAnimation AssignAnimationNamedWithSound
    /// @csn assignAnimationNamed:%s from:%s withSound:%s
    ///
    /// @doc_details
    procedure AssignAnimation(anim: Animation; name: String; script: AnimationScript; withSound: Boolean); overload;
    
    /// Assign a new starting animation to the passed in animation from the `AnimationScript`.
    /// This may play a sound if the first frame of the animation is linked to a sound effect.
    ///
    /// @lib AssignAnimation
    /// @sn assignAnimation:%s to:%s from:%s
    ///
    /// @class Animation
    /// @method AssignAnimation
    /// @csn assignAnimation:%s from:%s
    ///
    /// @doc_details
    procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript); overload;
    
    /// Assign a new starting animation to the passed in animation from the `AnimationScript`.
    /// This may play a sound if the first frame of the animation is linked to a sound effect, and 
    /// ``withSound`` is ``true``.
    ///
    /// @lib AssignAnimationWithSound
    /// @sn assignAnimation:%s to:%s from:%s withSound:%s
    ///
    /// @class Animation
    /// @overload AssignAnimation AssignAnimationWithSound
    /// @csn assignAnimation:%s from:%s withSound:%s
    ///
    /// @doc_details
    procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript; withSound: Boolean); overload;
    
    
    
//----------------------------------------------------------------------------
// Drawing Animations
//----------------------------------------------------------------------------
    
    /// Uses the `Animation` information to draw a `Bitmap` at the specified
    /// ``x``,``y`` location.
    ///
    /// @lib
    /// @sn drawAnimation:%s bitmap:%s x:%s y:%s
    ///
    /// @class Animation
    /// @method DrawBitmap
    /// @csn drawBitmap:%s x:%s y:%s
    procedure DrawAnimation(ani: Animation; bmp: Bitmap; x,y: Longint); overload;
    
    /// Uses the animation information to draw a bitmap at the specified
    /// point.
    ///
    /// @lib DrawAnimationAtPoint
    /// @sn drawAnimation:%s bitmap:%s pt:%s
    ///
    /// @class Animation
    /// @overload DrawBitmap DrawBitmapAtPt
    /// @csn drawBitmap:%s pt:%s
    procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
    
    /// Uses the animation information to draw a bitmap at the specified
    /// x,y location on a destination bitmap.
    ///
    /// @lib DrawAnimationOntoDest
    /// @sn drawOnto:%s animation:%s bitmap:%s x:%s y:%s
    ///
    /// @class Animation
    /// @overload DrawBitmap DrawBitmapOnto
    /// @self 2
    /// @csn drawOnto:%s bitmap:%s x:%s y:%s
    ///
    /// @doc_details
    procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; x,y: Longint); overload;
    
    /// Uses the animation information to draw a bitmap at the specified
    /// point on a destination bitmap.
    ///
    /// @lib DrawAnimationOntoDestAtPt
    /// @sn drawOnto:%s animation:%s bitmap:%s pt:%s
    ///
    /// @class Animation
    /// @overload DrawBitmap DrawBitmapAtPtOnto
    /// @self 2
    /// @csn drawOnto:%s using:%s pt:%s
    ///
    /// @doc_details
    procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
    
    /// Uses the animation information to draw a bitmap to the screen at the specified
    /// x,y location.
    ///
    /// @lib DrawAnimationOnScreen
    /// @sn drawAnimation:%s bitmap:%s onScreenAtX:%s y:%s
    ///
    /// @class Animation
    /// @method DrawBitmapOnScreen
    /// @csn drawBitmap:%s onScreenAtX:%s y:%s
    ///
    /// @doc_details
    procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; x,y: Longint); overload;
    
    /// Uses the animation information to draw a bitmap to the screen at the specified
    /// point.
    ///
    /// @lib DrawAnimationOnScreenAtPt
    /// @sn drawAnimation:%s bitmap:%s onScreenAtPt:%s
    ///
    /// @class Animation
    /// @overload DrawBitmapOnScreen DrawBitmapAtPtOnScreen
    /// @csn drawBitmap:%s onScreenAtPt:%s
    ///
    /// @doc_details
    procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
    
    
    
//----------------------------------------------------------------------------
// Updating Animations
//----------------------------------------------------------------------------
    
    /// Updates the animation, updating the time spent and possibly moving to a new
    /// frame in the animation. This may play a sound effect if the new frame
    /// triggers a sound.
    /// 
    /// @lib
    /// @sn updateAnimation:%s
    ///
    /// @class Animation
    /// @method Update
    /// @csn update
    procedure UpdateAnimation(anim: Animation); overload;
    
    /// Updates the animation a certain percentage and possibly moving to a new
    /// frame in the animation. This may play a sound effect if the new frame
    /// triggers a sound.
    /// 
    /// @lib UpdateAnimationPct
    /// @sn updateAnimation:%s pct:%s
    ///
    /// @class Animation
    /// @overload Update UpdatePct
    /// @csn updatePct:%s
    ///
    /// @doc_details
    procedure UpdateAnimation(anim: Animation; pct: Single); overload;
    
    /// Updates the animation a certain percentage and possibly moving to a new
    /// frame in the animation. This may play a sound effect if the new frame
    /// triggers a sound and withSound is true.
    /// 
    /// @lib UpdateAnimationPctAndSound
    /// @sn updateAnimation:%s pct:%s withSound:%s
    /// 
    /// @class Animation
    /// @overload Update UpdatePctAndSound
    /// @csn updatePct:%s withSound:%s
    ///
    /// @doc_details
    procedure UpdateAnimation(anim: Animation; pct: Single; withSound: Boolean); overload;
    
    /// Restarts the animation. This may play a sound effect if the first frame
    /// triggers a sound.
    /// 
    /// @lib RestartAnimation
    /// 
    /// @class Animation
    /// @method Restart
    procedure RestartAnimation(anim: Animation); overload;
    
    /// Restarts the animation. This may play a sound effect if the first frame
    /// triggers a sound and withSound is true.
    /// 
    /// @lib ResetAnimationWithSound
    /// @sn resetAnimation:%s withSound:%s
    /// 
    /// @class Animation
    /// @overload Reset ResetWithSound
    /// @csn resetWithSound:%s
    ///
    /// @doc_details
    procedure RestartAnimation(anim: Animation; withSound: Boolean); overload;
    
    
    
//----------------------------------------------------------------------------
// Query Animation
//----------------------------------------------------------------------------
    
    /// Indicates if an animation has ended. Animations with loops will never end.
    /// 
    /// @lib
    ///
    /// @class Animation
    /// @getter Ended
    function AnimationEnded(anim: Animation): Boolean;
    
    /// Returns the current cell (the part of the image or sprite) of this animation.
    /// This can be used to animate an image or sprite.
    ///
    /// @lib
    ///
    /// @class Animation
    /// @getter CurrentCell
    ///
    /// @doc_details
    function AnimationCurrentCell(anim: Animation): Longint;
    
    /// Returns true if the animation entered a new frame on its last update.
    /// This can be used to trigger actions on frames within an animation.
    ///
    /// @lib
    /// 
    /// @class Animation
    /// @getter EnteredFrame
    ///
    /// @doc_details
    function AnimationEnteredFrame(anim: Animation): Boolean;
    
    /// Returns the amount of time spent in the current frame. When this exceeds
    /// the frame's duration the animation moves to the next frame.
    /// 
    /// @lib
    /// 
    /// @class Animation
    /// @getter FrameTime
    ///
    /// @doc_details
    function AnimationFrameTime(anim: Animation): Single;
    
    /// Returns the vector assigned to the current frame in the animation.
    ///
    /// @lib
    ///
    /// @class Animation
    /// @getter Vector
    ///
    /// @doc_details
    function AnimationCurrentVector(anim: Animation): Vector;
    
//=============================================================================
implementation
    uses
        SysUtils, StrUtils, Classes, 
        stringhash, sgSharedUtils, sgNamedIndexCollection,
        sgShared, sgResources, sgTrace, sgAudio, sgImages, sgGeometry;
//=============================================================================

var
    _Animations: TStringHash;

function DoLoadAnimationScript(name, filename: String) : AnimationScript;
type
    RowData = record
        id,cell,dur,next: Longint;
        snd: SoundEffect;
        mvmt: Vector;
    end;
    IdData = record
        name: String;
        startId: Longint;
    end;
var
    rows: Array of RowData;
    ids: Array of IdData;
    input: Text; //the bundle file
    line, id, data, path: String;
    lineNo, maxId: Longint;
    
    procedure AddRow(myRow: RowData);
    var
        j: Longint;
    begin
        // Check if beyond current range
        if myRow.id > maxId then
        begin
            SetLength(rows, myRow.id + 1);
            
            // Mark new rows as "empty"
            for j := maxId + 1 to High(rows) do
            begin
                rows[j].id := -1;
                rows[j].snd := nil;
                rows[j].cell := -1;
                rows[j].next := -1;
                rows[j].mvmt := VectorTo(0,0);
            end;
            
            maxId := myRow.id;
        end;
        
        if rows[myRow.id].id <> -1 then
        begin
            RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A frame with id ' + IntToStr(myRow.id) + ' already exists.');
            exit;
        end
        else
        begin
            // Success add the row.
            rows[myRow.id] := myRow;
        end;
    end;

    procedure AddID(myId: IdData);
    var
        j: Longint;
    begin
        // Check if name is already in ids
        for j := 0 to High(ids) do
        begin
            if ids[j].name = myId.name then
            begin
                RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. The id ' + myId.name + ' already exists.');
                exit;
            end;
        end;
        
        SetLength(ids, Length(ids) + 1);
        ids[High(ids)] := myId;
    end;
    
    procedure ProcessFrame();
    var
        myRow: RowData;
    begin
        if CountDelimiter(data, ',') <> 3 then
        begin
            RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A frame must have 4 values separated as id,cell,dur,next');
            exit;
        end;
        myRow.id    := MyStrToInt(ExtractDelimited(1,data,[',']), false);
        myRow.cell  := MyStrToInt(ExtractDelimited(2,data,[',']), false);
        myRow.dur   := MyStrToInt(ExtractDelimited(3,data,[',']), false);
        myRow.next  := MyStrToInt(ExtractDelimited(4,data,[',']), true);
        myRow.snd   := nil;
        myRow.mvmt  := VectorTo(0,0);
        
        AddRow(myRow);
    end;
    
    procedure ProcessMultiFrame();
    var
        id_range, cell_range: Array of Longint;
        dur, next, j: Longint;
        myRow: RowData;
    begin
        if CountDelimiterWithRanges(data, ',') <> 3 then
        begin
            RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A multi-frame must have 4 values separated as id-range,cell-range,dur,next');
            exit;
        end;
        
        id_range := ProcessRange(ExtractDelimitedWithRanges(1, data));
        cell_range := ProcessRange(ExtractDelimitedWithRanges(2, data));
        
        if Length(id_range) <> Length(cell_range) then
        begin
            RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. The range of cells and ids is not the same length.');
            exit;
        end;
        
        dur := MyStrToInt(ExtractDelimitedWithRanges(3,data), false);
        next := MyStrToInt(ExtractDelimitedWithRanges(4,data), true);
        
        for j := Low(id_range) to High(id_range) do
        begin
            myRow.id        := id_range[j];
            myRow.cell      := cell_range[j];
            myRow.dur       := dur;
            myRow.snd       := nil;
            myRow.mvmt      := VectorTo(0,0);
            if j <> High(id_range) then
                myRow.next := id_range[j + 1]
            else
                myRow.next := next;
            AddRow(myRow);
        end;
    end;
    
    procedure ProcessId();
    var
        myIdData: IdData;
    begin
        if CountDelimiterWithRanges(data, ',') <> 1 then
        begin
            RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A id must have 2 values separated as name,start-id');
            exit;
        end;
        
        myIdData.name := ExtractDelimited(1,data,[',']);
        myIdData.startId := MyStrToInt(ExtractDelimited(2,data,[',']), false);
        
        AddID(myIdData);
    end;
    
    procedure ProcessSound();
    var
        id: Longint;
        sndId, sndFile: String;
    begin
        if CountDelimiter(data, ',') <> 2 then
        begin
            RaiseWarning('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A sound must have three parts frame #,sound name,sound file.');
            exit;
        end;

        id := MyStrToInt(ExtractDelimited(1,data,[',']), true);
        
        sndId := ExtractDelimited(2,data,[',']);
        sndFile := ExtractDelimited(3,data,[',']);
        
        if not HasSoundEffect(sndId) then
        begin
            if LoadSoundEffectNamed(sndId, sndFile) = nil then
            begin
                RaiseWarning('At line ' + IntToStr(lineNo) + ' in animation ' + filename + ': Cannot find ' + sndId + ' sound file ' + sndFile);
                exit;
            end;
        end;
        
        if WithinRange(Length(rows), id) then
            rows[id].snd := SoundEffectNamed(sndId)
        else
            RaiseWarning('At line ' + IntToStr(lineNo) + ' in animation ' + filename + ': No frame with id ' + IntToStr(id) + ' for sound file ' + sndFile)
    end;
    
    procedure ProcessVector();
    var
      id_range: Array of Longint;
      id, i: Longint;
      xVal, yVal: String;
      x, y: Single;
      v: Vector;
    begin
        if CountDelimiter(data, ',') <> 2 then
        begin
            RaiseWarning('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. A vector must have three parts frame #s, x value, y value.');
            exit;
        end;
        
        id_range := ProcessRange(ExtractDelimitedWithRanges(1, data));
        xVal := ExtractDelimitedWithRanges(2,data);
        yVal := ExtractDelimitedWithRanges(3,data);
        
        if not TryStrToFloat(xVal, x) then
        begin
            RaiseWarning('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. X value must be a number.');
            exit;
        end;
        
        if not TryStrToFloat(yVal, y) then
        begin
            RaiseWarning('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Y value must be a number.');
            exit;
        end;
        
        v := VectorTo(x, y);
        
        // WriteLn('Vector ', PointToString(v) );
        // 
        for i := Low(id_range) to High(id_range) do
        begin
            id := id_range[i];
            
            if WithinRange(Length(rows), id) then
            begin
                rows[id].mvmt := v;
            end;
        end;
    end;
    
    procedure ProcessLine();
    begin
        // Split line into id and data
        id := ExtractDelimited(1, line, [':']);
        data := ExtractDelimited(2, line, [':']);
        
        // Verify that id is a single char
        if Length(id) <> 1 then
        begin
            RaiseWarning('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with frame #: ' + id + '. This should be a single character.');
            exit;
        end;
        
        // Process based on id
        case LowerCase(id)[1] of // in all cases the data variable is read
            'f': ProcessMultiFrame(); //test... or ProcessFrame();
            'm': ProcessMultiFrame();
            'i': ProcessId();
            's': ProcessSound();
            'v': ProcessVector();
            else
            begin
                RaiseWarning('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with id: ' + id + '. This should be one of f,m,i, s, or v.');
                exit;
            end;
        end;
    end;
    
    procedure BuildFrameLists();
    var
        frames: Array of AnimationFrame;
        j, nextIdx, addedIdx: Longint;
    begin
        SetLength(frames, Length(rows));
        
        for j := 0 to High(frames) do
        begin
            // Allocte space for frames
            New(frames[j]);
        end;
        
        // Link the frames together
        for j := 0 to High(frames) do
        begin
            frames[j]^.index        := j;
            frames[j]^.cellIndex    := rows[j].cell;
            frames[j]^.sound        := rows[j].snd;
            frames[j]^.duration     := rows[j].dur;
            frames[j]^.movement     := rows[j].mvmt;
            //WriteLn(j, PointToString(frames[j]^.movement));
            //WriteLn('Frame ', HexStr(frames[j]), ' Vector ', PointToString(frames[j]^.movement));
            
            // Get the next id and then 
            nextIdx := rows[j].next;
            
            if nextIdx = -1 then
            begin
                //The end of a list of frames = no next
                frames[j]^.next := nil;
            end
            else if (nextIdx < 0) or (nextIdx > High(frames)) then
            begin
                FreeAnimationScript(result);
                result := nil;
                RaiseWarning('Error in animation ' + filename + '. Error with frame: ' + IntToStr(j) + '. Next is outside of available frames.');
                exit;
            end
            else
                frames[j]^.next := frames[nextIdx];
                
            //WriteLn(j, ' = ', frames[j]^.cellIndex, ' -> ', nextIdx);
        end;
        
        //We have the data ready, now lets create the linked lists...
        New(result);
        
        result^.name        := name;        // name taken from parameter of DoLoadAnimationScript
        result^.filename    := filename;    // filename also taken from parameter
        result^.frames      := frames;      // The frames of this animation.
        
        SetLength(result^.animations, Length(ids));
        SetLength(result^.animObjs, 0);
        result^.nextAnimIdx := 0;
        InitNamedIndexCollection(result^.animationIds);     //Setup the name <-> id mappings
        
        for j := 0 to High(ids) do                          //Add in the animation starting indexes
        begin
            addedIdx := AddName(result^.animationIds, ids[j].name);     //Allocate the index
            result^.animations[addedIdx] := ids[j].startId;             //Store the linked index
            //WriteLn('load ids: ', addedIdx, ' - startid ', ids[j].startId)
        end;
    end;
    
    procedure MakeFalse(var visited: Array of Boolean);
    var
        i: Longint;
    begin
        for i := 0 to High(visited) do
        begin
            visited[i] := false;
        end;
    end;
    
    function SumLoop(start: AnimationFrame): Single;
    var
        current: AnimationFrame;
    begin
        result := start^.duration;
        current := start^.next;
        
        while (current <> start) and (assigned(current)) do
        begin
            result := result + current^.duration;
            current := current^.next;
        end;
    end;
    
    // Animations with loops must have a duration > 0 for at least one frame
    procedure CheckAnimationLoops();
    var
        i: Longint;
        done: Boolean;
        visited: Array of Boolean;
        current: AnimationFrame;
    begin
        done := true;
        
        // check for all positive
        for i := 0 to High(result^.frames) do
        begin
            if result^.frames[i]^.duration = 0 then
            begin
                done := false;
                break;
            end;
        end;
        
        if done then exit;
        
        SetLength(visited, Length(result^.frames));
        
        // Check through each animation for a loop
        for i := 0 to High(result^.animations) do
        begin
            MakeFalse(visited);
            
            current := result^.frames[result^.animations[i]];
            
            // Check for a loop
            while current <> nil do
            begin
                if visited[current^.index] then
                begin
                    if SumLoop(current) = 0 then
                    begin
                        FreeAnimationScript(result);
                        RaiseException('Error in animation ' + filename + '. Animation contains a loop with duration 0 starting at cell ' + IntToStr(current^.index));
                        exit;
                    end;
                    break;
                end
                else
                    current := current^.next;
            end;
        end;
    end;
    
    procedure VerifyVersion();
    begin
        if EOF(input) then exit;
        line := '';
        
        while (Length(line) = 0) or (MidStr(line,1,2) = '//') do
        begin
            ReadLn(input, line);
            line := Trim(line);
        end;
        
        //Verify that the line has the right version
        if line <> 'SwinGame Animation #v1' then 
            RaiseException('Error in animation ' + filename + '. Animation files must start with "SwinGame Animation #v1"');
    end;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'LoadAnimationScript');
    {$ENDIF}
    
    path := FilenameToResource(name, AnimationResource);
    if not FileExists(path) then
    begin
        result := nil;
        exit;
    end;
    
    lineNo := 0;
    maxId := -1;
    SetLength(rows, 0);
    result := nil;
    
    Assign(input, path);
    Reset(input);
    
    VerifyVersion();
    
    try
        while not EOF(input) do
        begin
            lineNo := lineNo + 1;
        
            ReadLn(input, line);
            line := Trim(line);
            if Length(line) = 0 then continue;  //skip empty lines
            if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
        
            ProcessLine();
        end;
    
        BuildFrameLists();
        CheckAnimationLoops();
    finally
        Close(input);
    end;
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'LoadAnimationScript');
    {$ENDIF}    
end;

function LoadAnimationScript(filename: String) : AnimationScript;
begin
    result := LoadAnimationScriptNamed(filename, filename);
end;

function AnimationScriptName(script: AnimationScript): String;
begin
    result := '';
    if Assigned(script) then result := script^.name;
end;

procedure FreeAnimationScript(var scriptToFree: AnimationScript);
begin
    if Assigned(scriptToFree) then
        ReleaseAnimationScript(scriptToFree^.name);
    scriptToFree := nil;
end;

function LoadAnimationScriptNamed(name, filename: String): AnimationScript;
var
    obj: tResourceContainer;
    frm: AnimationScript;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'MapAnimationFrame', name + ' = ' + filename);
    {$ENDIF}
    
    // WriteLn('Loading Animation Script ', name, ' for ', filename);
    
    if _Animations.containsKey(name) then
    begin
        // WriteLn('  Using existing... ', name, ' for ', filename);
        result := AnimationScriptNamed(name);
        exit;
    end;
    
    frm := DoLoadAnimationScript(filename, name);
    if not assigned(frm) then
    begin
        result := nil;
        exit;
    end;
    
    obj := tResourceContainer.Create(frm);
    
    if not _Animations.setValue(name, obj) then
    begin
        RaiseException('** Leaking: Caused by loading AnimationScript resource twice, ' + name);
        result := nil;
        exit;
    end;
    result := frm;
    
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'MapAnimationFrame', HexStr(result));
    {$ENDIF}
end;

function HasAnimationScript(name: String): Boolean;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'HasAnimationScript', name);
    {$ENDIF}
    
    result := _Animations.containsKey(name);
    
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'HasAnimationScript', BoolToStr(result, true));
    {$ENDIF}
end;

function AnimationScriptNamed(name: String): AnimationScript;
var
    tmp : TObject;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'AnimationScriptNamed', name);
    {$ENDIF}
    
    tmp := _Animations.values[name];
    if assigned(tmp) then result := AnimationScript(tResourceContainer(tmp).Resource)
    else result := nil;
    
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'AnimationScriptNamed', HexStr(result));
    {$ENDIF}
end;

procedure DoFreeAnimationScript(var frm: AnimationScript);
var
    i: Longint;
begin
    // WriteLn('Freeing Animation Script ', HexStr(frm), ' = ', frm^.name);
    if not assigned(frm) then exit;

    FreeNamedIndexCollection(frm^.animationIds);
    
    // WriteLn(frm^.nextAnimIdx);
    // Must use downto as animations are removed from the array in FreeAnimation!
    for i := frm^.nextAnimIdx - 1 downto 0 do
    begin
        FreeAnimation(frm^.animObjs[i]);
    end;
    
    for i := 0 to High(frm^.frames) do
    begin
        Dispose(frm^.frames[i]);
        frm^.frames[i] := nil;
    end;
    Dispose(frm);
    frm := nil;
end;

procedure ReleaseAnimationScript(name: String);
var
    frm: AnimationScript;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'ReleaseAnimationScript', 'frm = ' + name);
    {$ENDIF}
    
    frm := AnimationScriptNamed(name);
    
    if (assigned(frm)) then
    begin
        _Animations.remove(name).Free();
        DoFreeAnimationScript(frm);
    end;
    
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'ReleaseAnimationScript');
    {$ENDIF}
end;

procedure ReleaseAllAnimationScripts();
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'ReleaseAllAnimationScripts', '');
    {$ENDIF}
    
    ReleaseAll(_Animations, @ReleaseAnimationScript);
    
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'ReleaseAllAnimationScripts');
    {$ENDIF}
end;

function StartFrame(id: Longint; temp: AnimationScript) : AnimationFrame;
begin
    result := nil;
    if temp = nil then exit;
    
    if (id < 0) or (id > High(temp^.animations)) then exit;
    
    result := temp^.frames[id];
    
end;

//----------------------------------------------------------------------------

function StartFrame(name: String; temp: AnimationScript) : AnimationFrame;
begin
    if assigned(temp) then
        result := StartFrame(IndexOf(temp^.animationIds, name), temp)
    else
        result := nil;
end;

procedure _AddAnimation(script: AnimationScript; ani: Animation);
begin
    if Length(script^.animObjs) <= script^.nextAnimIdx then
      SetLength(script^.animObjs, script^.nextAnimIdx + 1);
    
    script^.animObjs[script^.nextAnimIdx] := ani;
    ani^.script := script;
    script^.nextAnimIdx += 1;
end;

procedure _RemoveAnimation(script: AnimationScript; ani: Animation);
var
    i: Integer;
begin
    for i := Low(script^.animObjs) to script^.nextAnimIdx - 1 do
    begin
        if script^.animObjs[i] = ani then
        begin
            script^.nextAnimIdx -= 1;                                        // Move back one (will point to high first time...)
            script^.animObjs[i] := script^.animObjs[script^.nextAnimIdx];    // Copy back old last place
            script^.animObjs[script^.nextAnimIdx] := nil;                    // Just to make sure...
            exit;
        end;
    end;
    RaiseWarning('Could not remove animation! ' + HexStr(ani) + AnimationScriptName(script));
end;

procedure FreeAnimation(var ani: Animation);
var
    toFree: Animation;
begin
    // WriteLn('Freeing Anim ', HexStr(ani));
    if assigned(ani) then
    begin
        toFree := ani;
        _RemoveAnimation(ani^.script, ani);
        // WriteLn('Now Disposing Anim ', HexStr(ani), '=', HexStr(toFree));
        Dispose(toFree); //ani may have been overridden by last call...
        ani := nil;
    end;
end;

function CreateAnimation(identifier: Longint; script: AnimationScript; withSound: Boolean): Animation; overload;
begin
    result := nil;
    if script = nil then exit;
    if (identifier < 0) or (identifier > High(script^.animations)) then
      begin
      RaiseWarning('Unable to create animation number ' + IntToStr(identifier) + ' from script ' + script^.name);
      exit;
    end;
    
    new(result);
    _AddAnimation(script, result);
    AssignAnimation(result, identifier, script, withSound);
    // WriteLn('Created ', HexStr(result));
end;

function CreateAnimation(identifier: Longint;    script: AnimationScript): Animation; overload;
begin
    result := CreateAnimation(identifier, script, True);
end;

function CreateAnimation(identifier: String; script: AnimationScript; withSound: Boolean): Animation; overload;
var
    idx: Integer;
begin
    result := nil;
    if script = nil then exit;
        
    idx := IndexOf(script^.animationIds, identifier);
	if (idx < 0) or (idx > High(script^.animations)) then
    begin
		RaiseWarning('Unable to create animation "' + identifier + '" from script ' + script^.name);
		exit;
	end;

    result := CreateAnimation(idx, script, withSound);
end;

function CreateAnimation(identifier: String;    script: AnimationScript): Animation; overload;
begin
    result := CreateAnimation(identifier, script, True);
end;

procedure AssignAnimation(anim: Animation; name: String; script: AnimationScript); overload;
begin
    AssignAnimation(anim, name, script, true);
end;

procedure AssignAnimation(anim: Animation; name: String; script: AnimationScript; withSound: Boolean); overload;
begin
    AssignAnimation(anim, AnimationIndex(script, name), script, withSound);
end;

procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript); overload;
begin
    AssignAnimation(anim, idx, script, true);
end;

procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript; withSound: Boolean); overload;
begin
    if (not assigned(anim)) or (not assigned(script)) then exit;
    if (idx < 0) or (idx > High(script^.animations)) then 
    begin 
        RaiseWarning('Assigning an animation frame that is not within range 0-' + IntToStr(High(script^.animations)) + '.'); 
        exit; 
    end;
    
    // Animation is being assigned to another script
    if anim^.script <> script then
    begin
        _RemoveAnimation(anim^.script, anim);   // remove from old script
        _AddAnimation(script, anim);            // add to new script
    end;
    anim^.firstFrame        := script^.frames[script^.animations[idx]];
    RestartAnimation(anim, withSound);
end;

procedure UpdateAnimation(anim: Animation; pct: Single; withSound: Boolean); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'UpdateAnimation', '');
        try
    {$ENDIF}
    
    if AnimationEnded(anim) then exit;
        
    anim^.frameTime     := anim^.frameTime + pct;
    anim^.enteredFrame  := false;
    
    if anim^.frameTime >= anim^.currentFrame^.duration then
    begin
        anim^.frameTime := anim^.frameTime - anim^.currentFrame^.duration; //reduce the time
        anim^.lastFrame := anim^.currentFrame; //store last frame
        anim^.currentFrame := anim^.currentFrame^.next; //get the next frame
        
        //if assigned(anim^.currentFrame) then
        //WriteLn('Frame ', HexStr(anim^.currentFrame), ' Vector ', PointToString(anim^.currentFrame^.movement));
        
        if assigned(anim^.currentFrame) and assigned(anim^.currentFrame^.sound) and withSound then
        begin
            PlaySoundEffect(anim^.currentFrame^.sound);
        end;
    end;
    
    {$IFDEF TRACE}
        finally
            TraceExit('sgAnimations', 'UpdateAnimation', '');
        end;
    {$ENDIF}
end;

procedure UpdateAnimation(anim: Animation; pct: Single); overload;
begin
    UpdateAnimation(anim, pct, True);
end;

procedure UpdateAnimation(anim: Animation); overload;
begin
    UpdateAnimation(anim, 1, True);
end;

function AnimationEnded(anim: Animation): Boolean;
begin
    if not Assigned(anim) then
        result := true
    else 
        result := not Assigned(anim^.currentFrame);
end;

procedure RestartAnimation(anim: Animation); overload;
begin
    RestartAnimation(anim, true);
end;

procedure RestartAnimation(anim: Animation; withSound: Boolean); overload;
begin
    if not assigned(anim) then exit;
    
    anim^.currentFrame  := anim^.firstFrame;
    anim^.lastFrame     := anim^.firstFrame;
    anim^.frameTime     := 0;
    anim^.enteredFrame  := true;
    
    if assigned(anim^.currentFrame) and assigned(anim^.currentFrame^.sound) and withSound then
        PlaySoundEffect(anim^.currentFrame^.sound);
end;


function AnimationCurrentCell(anim: Animation): Longint;
begin
    if not assigned(anim) then
        result := 0 //no animation - return the first frame
    else if not AnimationEnded(anim) then
        result := anim^.currentFrame^.cellIndex
    else if not assigned(anim^.lastFrame) then
        result := -1
    else  //Use the last frame drawn.
        result := anim^.lastFrame^.cellIndex;
end;

function AnimationCurrentVector(anim: Animation): Vector;
begin
    if not assigned(anim) then 
        result := VectorTo(0,0)
    else if not AnimationEnded(anim) then
    begin
        result := anim^.currentFrame^.movement;
        // WriteLn(PointToString(result));
    end
    else
        result := VectorTo(0,0)
end;

function AnimationEnteredFrame(anim: Animation): Boolean;
begin
    if not Assigned(anim) then
        result := false
    else 
        result := anim^.enteredFrame;
end;

function AnimationFrameTime(anim: Animation): Single;
begin
    if not Assigned(anim) then
        result := -1
    else 
        result := anim^.frameTime;
end;


procedure DrawAnimation(ani: Animation; bmp: Bitmap; x, y: Longint); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'DrawAnimation', ''); try
    {$ENDIF}
    
    DrawCell(bmp, AnimationCurrentCell(ani), x, y);
    
    {$IFDEF TRACE}
        finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
    {$ENDIF}
end;

procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'DrawAnimation', ''); try
    {$ENDIF}

    DrawCell(bmp, AnimationCurrentCell(ani), pt);

    {$IFDEF TRACE}
        finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
    {$ENDIF}
end;

procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; x,y: Longint); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'DrawAnimation', ''); try
    {$ENDIF}

    DrawCell(dest, bmp, AnimationCurrentCell(ani), x, y);

    {$IFDEF TRACE}
        finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
    {$ENDIF}
end;

procedure DrawAnimation(dest: Bitmap; ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'DrawAnimation', ''); try
    {$ENDIF}

    DrawCell(dest, bmp, AnimationCurrentCell(ani), pt);

    {$IFDEF TRACE}
        finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
    {$ENDIF}
end;

procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; x,y: Longint); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'DrawAnimationOnScreen', ''); try
    {$ENDIF}
    
    DrawCellOnScreen(bmp, AnimationCurrentCell(ani), x, y);
    
    {$IFDEF TRACE}
        finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
    {$ENDIF}
end;

procedure DrawAnimationOnScreen(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'DrawAnimationOnScreen', ''); try
    {$ENDIF}
    
    DrawCellOnScreen(bmp, AnimationCurrentCell(ani), pt);
    
    {$IFDEF TRACE}
        finally TraceExit('sgAnimations', 'DrawAnimation', ''); end;
    {$ENDIF}
end;


function AnimationIndex(temp: AnimationScript; name: String): Longint;
begin
    if not assigned(temp) then result := -1
    else result := IndexOf(temp^.animationIds, name);
end;

function AnimationName(temp: AnimationScript; idx: Longint): String;
begin
    if not assigned(temp) then result := ''
    else result := NameAt(temp^.animationIds, idx);
end;

//=============================================================================
initialization
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'Initialise', '');
    {$ENDIF}
    
    InitialiseSwinGame();
    
    _Animations := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'Initialise');
    {$ENDIF}
end;

finalization
begin
  ReleaseAllAnimationScripts();
  FreeAndNil(_Animations);
end;

//=============================================================================
end.
