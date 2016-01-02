//=============================================================================
// sgAnimations.pas
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
    function LoadAnimationScript(const filename: String) : AnimationScript;
    
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
    function AnimationIndex(temp: AnimationScript; const name: String): Longint;
    
    /// The name of the animation within the animation template at the specified index.
    ///
    /// @lib AnimationScriptAnimationName
    /// @sn animationScript:%s nameOfAnimation:%s
    ///
    /// @class AnimationScript
    /// @method NameOfAnimation
    /// @csn nameOfAnimation:%s
    ///
    /// @doc_details
    function AnimationName(temp: AnimationScript; idx: Longint): String;
    
    /// The name of the animation currently being played.
    ///
    /// @lib 
    /// @sn nameOfAnimation:%s
    ///
    /// @class Animation
    /// @getter Name
    ///
    /// @doc_details
    function AnimationName(temp: Animation): String;
    
    
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
    function LoadAnimationScriptNamed(const name, filename: String): AnimationScript;
    
    /// Determines if SwinGame has animation frames loaded for the supplied ``name``.
    /// This checks against all loaded animation frames, those loaded without a name
    /// are assigned the filename as a default.
    ///
    /// @lib
    ///
    /// @doc_details
    function HasAnimationScript(const name: String): Boolean;
    
    /// Returns the `AnimationScript` that has been loaded with the specified ``name``,
    /// see `LoadAnimationScriptNamed`.
    ///
    /// @lib
    function AnimationScriptNamed(const name: String): AnimationScript;
    
    /// Releases the SwinGame resources associated with the animation template of the
    /// specified ``name``.
    ///
    /// @lib
    procedure ReleaseAnimationScript(const name: String);
    
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

    /// Returns the number of Animations within an Animation Script.
    ///
    /// @lib
    ///
    /// @class AnimationScript
    /// @getter AnimationCount
    function AnimationCount(script: animationScript): Longint;
    
    
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
    function CreateAnimation(const identifier: String; script: AnimationScript): Animation; overload;
    
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
    function CreateAnimation(const identifier: String; script: AnimationScript; withSound: Boolean): Animation; overload;
    
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
    function CreateAnimation(identifier: Longint; script: AnimationScript; withSound: Boolean): Animation; overload;
    
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
    function CreateAnimation(identifier: Longint; script: AnimationScript): Animation; overload;
    
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
    procedure AssignAnimation(anim: Animation; const name: String; script: AnimationScript); overload;
    
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
    procedure AssignAnimation(anim: Animation; const name: String; script: AnimationScript; withSound: Boolean); overload;
    
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
    procedure DrawAnimation(ani: Animation; bmp: Bitmap; x, y: Single); overload;
        
    /// Uses the animation information to draw a bitmap at the specified
    /// x,y location given the passed in options.
    ///
    /// @lib DrawAnimationWithOptions
    /// @sn drawAnimation:%s bitmap:%s atX:%s y:%s opts:%s
    ///
    /// @class Animation
    /// @overload DrawBitmap DrawBitmapWithOptions
    /// @csn drawBitmap:%s atX:%s y:%s opts:%s
    ///
    /// @doc_details
    procedure DrawAnimation(ani: Animation; bmp: Bitmap; x, y: Single; const opts : DrawingOptions); overload;
    
    /// Uses the animation information to draw a bitmap at the specified
    /// point.
    ///
    /// @lib DrawAnimationAtPoint
    /// @sn drawAnimation:%s bitmap:%s pt:%s
    ///
    /// @class Animation
    /// @overload DrawBitmap DrawBitmapAtPt
    /// @csn drawBitmap:%s pt:%s
    ///
    /// @doc_details
    procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;

    /// Uses the animation information to draw a bitmap at the specified
    /// point given the passed in options.
    ///
    /// @lib DrawAnimationAtPointWithOptions
    /// @sn drawAnimation:%s bitmap:%s pt:%s opts:%s
    ///
    /// @class Animation
    /// @overload DrawBitmap DrawBitmapPtWithOptions
    /// @csn drawBitmap:%s at:%s opts:%s
    ///
    /// @doc_details
    procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D; const opts : DrawingOptions); overload;    
    
    
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
    /// the frames duration the animation moves to the next frame.
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
        sgShared, sgResources, sgTrace, sgAudio, sgImages, sgGeometry, sgDrawingOptions, sgBackendTypes;
//=============================================================================

var
    _Animations: TStringHash;

function DoLoadAnimationScript(const name, filename: String) : AnimationScript;
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
    a: AnimationScriptPtr;
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
        New(a);
        
        a^.id          := ANIMATION_SCRIPT_PTR;
        a^.name        := name;        // name taken from parameter of DoLoadAnimationScript
        a^.filename    := filename;    // filename also taken from parameter
        a^.frames      := frames;      // The frames of this animation.
        
        SetLength(a^.animations, Length(ids));
        SetLength(a^.animObjs, 0);
        a^.nextAnimIdx := 0;
        InitNamedIndexCollection(a^.animationIds);     //Setup the name <-> id mappings
        
        for j := 0 to High(ids) do                          //Add in the animation starting indexes
        begin
            addedIdx := AddName(a^.animationIds, ids[j].name);     //Allocate the index
            a^.animations[addedIdx] := ids[j].startId;             //Store the linked index
            //WriteLn('load ids: ', addedIdx, ' - startid ', ids[j].startId)
        end;

        result := a;    
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
        for i := 0 to High(a^.frames) do
        begin
            if a^.frames[i]^.duration = 0 then
            begin
                done := false;
                break;
            end;
        end;
        
        if done then exit;
        
        SetLength(visited, Length(a^.frames));
        
        // Check through each animation for a loop
        for i := 0 to High(a^.animations) do
        begin
            MakeFalse(visited);
            
            current := a^.frames[a^.animations[i]];
            
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
    
    function VerifyVersion(): Boolean;
    begin
        result := false;
        if EOF(input) then exit;
        line := '';
        
        while (Length(line) = 0) or (MidStr(line,1,2) = '//') do
        begin
            ReadLn(input, line);
            line := Trim(line);
        end;
        
        //Verify that the line has the right version
        if line <> 'SwinGame Animation #v1' then 
        begin
            RaiseWarning('Error in animation ' + filename + '. Animation files must start with "SwinGame Animation #v1"');
            exit;
        end;
        result := true;
    end;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'LoadAnimationScript');
    {$ENDIF}
    
    path := FilenameToResource(name, AnimationResource);
    if not FileExists(path) then
    begin
        RaiseWarning('Unable to locate ' + name + ' animation file at ' + filename);
        result := nil;
        exit;
    end;
    
    lineNo := 0;
    maxId := -1;
    SetLength(rows, 0);
    result := nil;
    
    Assign(input, path);
    Reset(input);
    
    try
        if not VerifyVersion() then
        begin
            RaiseWarning('Error loading animation script: ' + path);
            exit
        end;

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

function LoadAnimationScript(const filename: String) : AnimationScript;
begin
    result := LoadAnimationScriptNamed(filename, filename);
end;

function AnimationScriptName(script: AnimationScript): String;
var
    a: AnimationScriptPtr;
begin
    result := '';
    a := ToAnimationScriptPtr(script);
    if Assigned(a) then result := a^.name;
end;

procedure FreeAnimationScript(var scriptToFree: AnimationScript);
var
    a: AnimationScriptPtr;
begin
    a := ToAnimationScriptPtr(scriptToFree);

    if Assigned(a) then
        ReleaseAnimationScript(a^.name);
    scriptToFree := nil;
end;

function LoadAnimationScriptNamed(const name, filename: String): AnimationScript;
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

function HasAnimationScript(const name: String): Boolean;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'HasAnimationScript', name);
    {$ENDIF}
    
    result := _Animations.containsKey(name);
    
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'HasAnimationScript', BoolToStr(result, true));
    {$ENDIF}
end;

function AnimationScriptNamed(const name: String): AnimationScript;
var
    tmp : TObject;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'AnimationScriptNamed', name);
    {$ENDIF}
    
    tmp := _Animations.values[name];
    if assigned(tmp) then result := AnimationScript(tResourceContainer(tmp).Resource)
    else
    begin
        result := nil;
        RaiseWarning('Unable to locate AnimationScript named ' + name);
    end; 
    
    {$IFDEF TRACE}
        TraceExit('sgAnimations', 'AnimationScriptNamed', HexStr(result));
    {$ENDIF}
end;

procedure DoFreeAnimationScript(var frm: AnimationScript);
var
    i: Longint;
    a: AnimationScriptPtr;
begin
    a := ToAnimationScriptPtr(frm);
    if not assigned(a) then exit;

    FreeNamedIndexCollection(a^.animationIds);
    
    // WriteLn(a^.nextAnimIdx);
    // Must use downto as animations are removed from the array in FreeAnimation!
    for i := a^.nextAnimIdx - 1 downto 0 do
    begin
        FreeAnimation(a^.animObjs[i]);
    end;
    
    for i := 0 to High(a^.frames) do
    begin
        Dispose(a^.frames[i]);
        a^.frames[i] := nil;
    end;
    a^.id := NONE_PTR;
    Dispose(a);
    frm := nil;
end;

procedure ReleaseAnimationScript(const name: String);
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

function AnimationCount(script: animationScript): Longint;
var
    a: AnimationScriptPtr;
begin
    a := ToAnimationScriptPtr(script);
    if Assigned(a) then
        result := Length(a^.animations)
    else
        result := 0;
end;

function _AnimationEnded(anim: AnimationPtr): Boolean;
begin
    if not Assigned(anim) then
        result := true
    else 
        result := not Assigned(anim^.currentFrame);
end;

function StartFrame(id: Longint; temp: AnimationScript) : AnimationFrame;
var
    a: AnimationScriptPtr;
begin
    a := ToAnimationScriptPtr(temp);
    result := nil;
    if a = nil then exit;
    
    if (id < 0) or (id > High(a^.animations)) then exit;
    
    result := a^.frames[id];
    
end;

//----------------------------------------------------------------------------

function StartFrame(const name: String; temp: AnimationScript) : AnimationFrame;
var
    a: AnimationScriptPtr;
begin
    a := ToAnimationScriptPtr(temp);
    if Assigned(a) then
        result := StartFrame(IndexOf(a^.animationIds, name), temp)
    else
        result := nil;
end;

procedure _AddAnimation(script: AnimationScriptPtr; ani: AnimationPtr);
begin
    if Length(script^.animObjs) <= script^.nextAnimIdx then
      SetLength(script^.animObjs, script^.nextAnimIdx + 1);
    
    script^.animObjs[script^.nextAnimIdx] := ani;
    ani^.script := script;
    script^.nextAnimIdx += 1;
end;

procedure _RemoveAnimation(script: AnimationScriptPtr; ani: AnimationPtr);
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
    toFree: AnimationPtr;
    a: AnimationPtr;
begin
    a := ToAnimationPtr(ani);
    // WriteLn('Freeing Anim ', HexStr(ani));
    if assigned(a) then
    begin
        toFree := a;
        _RemoveAnimation(a^.script, a);
        toFree^.id := NONE_PTR;
        // WriteLn('Now Disposing Anim ', HexStr(ani), '=', HexStr(toFree));
        Dispose(toFree); //ani may have been overridden by last call...
        ani := nil;
    end;
end;

procedure _AssignAnimation(anim: AnimationPtr; idx: Longint; script: AnimationScriptPtr; withSound: Boolean); forward;

function _CreateAnimation(identifier: Longint; script: AnimationScriptPtr; withSound: Boolean): Animation; overload;
var
    aptr: AnimationPtr;
begin
    result := nil;

    if script = nil then exit;
    if (identifier < 0) or (identifier > High(script^.animations)) then
    begin
      RaiseWarning('Unable to create animation number ' + IntToStr(identifier) + ' from script ' + script^.name);
      exit;
    end;
    
    new(aptr);
    aptr^.id := ANIMATION_PTR;
    _AddAnimation(script, aptr);
    result := aptr;

    _AssignAnimation(aptr, identifier, script, withSound);
    // WriteLn('Created ', HexStr(result));
end;

function _CreateAnimation(const identifier: String; script: AnimationScriptPtr; withSound: Boolean): Animation; overload;
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

    result := _CreateAnimation(idx, script, withSound);
end;

function CreateAnimation(identifier: Longint; script: AnimationScript; withSound: Boolean): Animation; overload;
begin
    result := _CreateAnimation(identifier, ToAnimationScriptPtr(script), withSound);
end;

function CreateAnimation(identifier: Longint; script: AnimationScript): Animation; overload;
begin
    result := CreateAnimation(identifier, script, True);
end;

function CreateAnimation(const identifier: String; script: AnimationScript; withSound: Boolean): Animation; overload;
begin
    result := _CreateAnimation(identifier, ToAnimationScriptPtr(script), withSound);
end;

function CreateAnimation(const identifier: String;    script: AnimationScript): Animation; overload;
begin
    result := CreateAnimation(identifier, script, True);
end;

procedure AssignAnimation(anim: Animation; const name: String; script: AnimationScript); overload;
begin
    AssignAnimation(anim, name, script, true);
end;

procedure AssignAnimation(anim: Animation; const name: String; script: AnimationScript; withSound: Boolean); overload;
begin
    AssignAnimation(anim, AnimationIndex(script, name), script, withSound);
end;

procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript); overload;
begin
    AssignAnimation(anim, idx, script, true);
end;

procedure AssignAnimation(anim: Animation; idx: Longint; script: AnimationScript; withSound: Boolean); overload;
begin
    _AssignAnimation(ToAnimationPtr(anim), idx, ToAnimationScriptPtr(script), withSound);
end;

procedure _AssignAnimation(anim: AnimationPtr; idx: Longint; script: AnimationScriptPtr; withSound: Boolean);
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
    anim^.animationName     := AnimationName(script, idx); 
    RestartAnimation(anim, withSound);
end;

procedure _UpdateAnimation(anim: AnimationPtr; pct: Single; withSound: Boolean); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'UpdateAnimation', '');
        try
    {$ENDIF}
    
    if _AnimationEnded(anim) then exit;
        
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

procedure UpdateAnimation(anim: Animation; pct: Single; withSound: Boolean); overload;
begin
    _UpdateAnimation(ToAnimationPtr(anim), pct, withSound);
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
    result := _AnimationEnded(ToAnimationPtr(anim));
end;

procedure _RestartAnimation(anim: AnimationPtr; withSound: Boolean); overload;
begin
    if not assigned(anim) then exit;
    
    anim^.currentFrame  := anim^.firstFrame;
    anim^.lastFrame     := anim^.firstFrame;
    anim^.frameTime     := 0;
    anim^.enteredFrame  := true;
    
    if assigned(anim^.currentFrame) and assigned(anim^.currentFrame^.sound) and withSound then
        PlaySoundEffect(anim^.currentFrame^.sound);
end;

procedure RestartAnimation(anim: Animation); overload;
begin
    RestartAnimation(ToAnimationPtr(anim), true);
end;

procedure RestartAnimation(anim: Animation; withSound: Boolean); overload;
begin
    _RestartAnimation(ToAnimationPtr(anim), withSound);
end;

function _AnimationCurrentCell(anim: AnimationPtr): Longint;
begin
    if not assigned(anim) then
        result := 0 //no animation - return the first frame
    else if not _AnimationEnded(anim) then
        result := anim^.currentFrame^.cellIndex
    else if not assigned(anim^.lastFrame) then
        result := -1
    else  //Use the last frame drawn.
        result := anim^.lastFrame^.cellIndex;
end;

function AnimationCurrentCell(anim: Animation): Longint;
begin
    result := _AnimationCurrentCell(ToAnimationPtr(anim));
end;

function _AnimationCurrentVector(anim: AnimationPtr): Vector;
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

function AnimationCurrentVector(anim: Animation): Vector;
begin
    result := _AnimationCurrentVector(ToAnimationPtr(anim));
end;

function _AnimationEnteredFrame(anim: AnimationPtr): Boolean;
begin
    if not Assigned(anim) then
        result := false
    else 
        result := anim^.enteredFrame;
end;

function AnimationEnteredFrame(anim: Animation): Boolean;
begin
    result := _AnimationEnteredFrame(ToAnimationPtr(anim));
end;

function AnimationFrameTime(anim: Animation): Single;
var
    a: AnimationPtr;
begin
    a := ToAnimationPtr(anim);
    if not Assigned(a) then
        result := -1
    else 
        result := a^.frameTime;
end;


procedure DrawAnimation(ani: Animation; bmp: Bitmap; x, y: Single; const opts: DrawingOptions); overload;
begin
    {$IFDEF TRACE}
        TraceEnter('sgAnimations', 'DrawAnimation', AnimationName(ani)); 
        try
    {$ENDIF}
    
    DrawCell(bmp, AnimationCurrentCell(ani), x, y, opts);
    
    {$IFDEF TRACE}
        finally 
            TraceExit('sgAnimations', 'DrawAnimation', AnimationName(ani)); 
        end;
    {$ENDIF}
end;

procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D; const opts: DrawingOptions); overload;
begin
    DrawAnimation(ani, bmp, pt.x, pt.y, OptionDefaults());
end;

procedure DrawAnimation(ani: Animation; bmp: Bitmap; x, y: Single); overload;
begin
    DrawAnimation(ani, bmp, x, y, OptionDefaults());
end;


procedure DrawAnimation(ani: Animation; bmp: Bitmap; const pt: Point2D); overload;
begin
    DrawAnimation(ani, bmp, pt.x, pt.y, OptionDefaults());
end;


function AnimationIndex(temp: AnimationScript; const name: String): Longint;
var
    a: AnimationScriptPtr;
begin
    a := ToAnimationScriptPtr(temp);
    if not assigned(a) then result := -1
    else result := IndexOf(a^.animationIds, name);
end;

function AnimationName(temp: AnimationScript; idx: Longint): String;
var
    a: AnimationScriptPtr;
begin
    a := ToAnimationScriptPtr(temp);
    if not assigned(a) then result := ''
    else result := NameAt(a^.animationIds, idx);
end;

function AnimationName(temp: Animation): String;
var
    a: AnimationPtr;
begin
    a := ToAnimationPtr(temp);

    if not assigned(a) then result := ''
    else result := a^.animationName;
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
