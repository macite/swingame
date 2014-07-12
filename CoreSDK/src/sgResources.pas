//=============================================================================
// sgResources.pas
//=============================================================================
// Change History:
//
// Version 3:
// - 2009-12-21: Andrew : Moved bundle loading into the bundles directory.
//                      : Added the ability to load bundles within bundles.
// - 2009-12-18: Andrew : Removed links to old mappy tile code... need new map code
// - 2009-12-10: Andrew : Switched to DrawCell for start up animation
//                      : Added reading of cell information on bitmap loading
//                      : Switched to use Animation
// - 2009-12-07: Andrew : Moved out loading of image, font, and tilemap resources
// - 2009-11-10: Andrew : Changed sn to csn tags
// - 2009-11-06: Andrew : Moved out loading of audio resources... others to follow
// - 2009-10-16: Andrew : Moved free notifier, and ensured free notifier called after dispose
// - 2009-09-11: Andrew : Fixed to load resources without needing path
// - 2009-07-29: Andrew : Renamed Get... functions and check for opened audio
// - 2009-07-28: Andrew : Added ShowLogos splash screen
// - 2009-07-17: Andrew : Small fixes for return types.
// - 2009-07-14: Andrew : Added resource loading and freeing procedures.
//                      : Added FreeNotifier
// - 2009-07-08: Andrew : Fixed iterator use in release all
// - 2009-07-05: Clinton: Fixed delphi-support for ExtractDelimited, formatting
// - 2009-07-03: Andrew : Fixed header comments
// - 2009-06-23: Andrew : Created
//
//=============================================================================



/// Supports the loading and releasing of game resources and bundles, including 
/// application path settings. Current resource types include images, sounds, 
/// music and animation files to name a few. Resource files must be saved in
/// specific locations of the ``Resources`` folder for your game.
///
/// @module Resources
unit sgResources;

//=============================================================================
interface
    uses sgTypes;
//=============================================================================

//----------------------------------------------------------------------------
// Bundle handling routines
//----------------------------------------------------------------------------
    
    /// Load a resource bundle showing load progress.
    ///
    /// @lib
    /// @sn loadResourceBundleNamed:%s showingProgress:%s
    procedure LoadResourceBundle(name: String; showProgress: Boolean); overload;
    
    /// Load a resource bundle showing load progress.
    /// 
    /// @lib LoadResourceBundle(name, True)
    /// @uname LoadResourceBundleWithProgress
    procedure LoadResourceBundle(name: String); overload;
    
    /// Load a resource bundle mapping it to a given name, showing progress.
    /// 
    /// @lib
    /// @sn mapResourceBundle:%s filename:%s showProgress:%s
    procedure LoadResourceBundleNamed(name, filename: String; showProgress: Boolean);
    
    /// Release the resource bundle with the given name.
    /// 
    /// @lib
    procedure ReleaseResourceBundle(name: String);
    
    /// Returns ``true`` if the resource bundle is loaded.
    /// 
    /// @lib
    function HasResourceBundle(name: String): Boolean;
    
    
    
//----------------------------------------------------------------------------
// Release all resources procedure
//----------------------------------------------------------------------------
    
    /// Release all of the resources loaded by SwinGame.
    /// 
    /// @lib
    procedure ReleaseAllResources();
    
    
    
//----------------------------------------------------------------------------
// Resource Path and Application Path
//----------------------------------------------------------------------------
    
    /// Returns the path to a resource based on a base path and a the resource kind.
    ///
    /// @lib
    /// @sn pathToResourceBase:%s filename:%s resourceKind:%s
    function PathToResourceWithBase(path, filename: String; kind: ResourceKind): String; overload; // forward;
    
    /// Returns the path to a resource based on a base path and a the resource kind.
    ///
    /// @lib PathToOtherResourceWithBase
    /// @sn pathToResourceBase:%s filename:%s
    function PathToResourceWithBase(path, filename: String): String; overload; // forward;
    
    /// Returns the path to the filename that exists within the game's resources folder
    /// in the indicated sub directory. For example, to get the "level1.txt" file from
    /// the Resources/levels folder you call this passing in "level1.txt" as the filename
    /// and "levels" as the subdir.
    ///
    /// @lib PathToResourseInSubdir
    /// @sn pathToResource:%s inSubdir:%s
    function PathToResource(filename, subdir: String): String; overload;
    
    /// Returns the path to the filename that exists within the game's resources folder
    /// in the indicated sub directory of the directory for the given resource kind .
    /// For example, to get the "background.png" file from "level1" folder in the images folder
    /// (i.e. Resources/images/level1/background.png) you call this passing in ``background.png`` as the filename
    /// ``ImageResource`` as the kind and ``level1`` as the subdir.
    ///
    /// @lib PathToResourseKindInSubdir
    /// @sn pathToResource:%s kind:%s inSubdir:%s
    function PathToResource(filename: String; kind: ResourceKind; subdir: String): String; overload;
    
    /// Returns the path to a resource given its filename, kind, and any subPaths. For example: to load
    /// the image ``bullet01.png`` from the ``bullets`` subdirectory you pass in ``bullet01.png`` as the filename,
    /// ``ImageResource`` as the kind, and ``bullets`` as the subPaths. This will then return the full path
    /// to the resource according to the platform in question. 
    ///
    /// For example: ``.../Resources/images/bullets/bullet01.png``
    /// 
    /// @lib PathToResourceWithSubPaths
    /// @sn pathToResourceFilename:%s kind:%s subPaths:%s
    function PathToResource(filename: String; kind: ResourceKind; const subPaths: StringArray): String; overload;
    
    /// Returns the path to the filename for a given file resource.
    /// 
    /// @lib PathToResource
    /// @sn pathToResourceFilename:%s kind:%s
    function PathToResource(filename: String; kind: ResourceKind): String; overload;
    
    /// Returns the path to the filename within the game's resources folder.
    /// 
    /// @lib PathToOtherResource
    /// @sn pathToResourceFilename:%s
    function PathToResource(filename: String): String; overload;
    
    /// Returns the path to the file with the passed in name for a given resource
    /// kind. This checks if the path exists, throwing an exception if the file
    /// does not exist in the expected locations.
    ///
    /// @lib
    /// @sn filenameFor:%s ofKind:%s
    function FilenameToResource(name: String; kind: ResourceKind): String;
    
    /// Sets the path to the executable. This path is used for locating game
    /// resources.
    /// 
    /// @lib SetAppPathWithExe
    /// @sn setAppPath:%s withExe:%s
    procedure SetAppPath(path: String; withExe: Boolean); overload;
    
    /// Sets the path to the executable. This path is used for locating game
    /// resources.
    /// 
    /// @lib SetAppPath
    procedure SetAppPath(path: String); overload;
    
    /// Returns the application path set within SwinGame. This is the path
    /// used to determine the location of the game's resources.
    ///
    /// @lib
    function AppPath(): String;
        
//----------------------------------------------------------------------------
// Notifier of resource freeing
//----------------------------------------------------------------------------
    
    /// Using this procedure you can register a callback that is executed
    /// each time a resource is freed. This is called by different versions of
    /// SwinGame to keep track of resources and should not be called by user code.
    ///
    /// @lib
    procedure RegisterFreeNotifier(fn: FreeNotifier);
    
    
//=============================================================================
implementation
//=============================================================================

    uses SysUtils, StrUtils, Classes, // system
             stringhash, sgSharedUtils,          // libsrc
             {$ifdef WINDOWS} Windows, 
			 {$else}
				{$ifndef DARWIN}
				unix, BaseUnix,
				{$endif}
			 {$endif}
             sgText, sgAudio, sgGraphics, sgInput, sgCharacters, sgShared, sgTimers, sgUtils,
             sgSprites, sgTrace, sgImages, sgAnimations, sgUserInterface, sgMaps, sgNetworking,
             sgArduino; // Swingame

//----------------------------------------------------------------------------
// Global variables for resource management.
//----------------------------------------------------------------------------

    var
        _Bundles: TStringHash;
        // The full path location of the current executable (or script). This is
        // particuarly useful when determining the path to resources (images, maps,
        // sounds, music etc).

        _AppPathSet: Boolean;
        
    
    procedure RegisterFreeNotifier(fn: FreeNotifier);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgResources', 'sgResources.RegisterFreeNotifier', 'fn: ' + HexStr(fn));
        {$ENDIF}
        _FreeNotifier := fn;
        {$IFDEF TRACE}
            TraceExit('sgResources', 'sgResources.RegisterFreeNotifier');
        {$ENDIF}

    end;
    
//----------------------------------------------------------------------------
// Private types
//----------------------------------------------------------------------------
    
    type 
        //
        // Used in loading bundles
        //
        TResourceIdentifier = record
            name, path: String;
            data: Array of Longint;
            kind: ResourceKind;
        end;
        
        //
        // Used to store bundle details
        //
        TResourceBundle = class(tObject)
        public
            identifiers: array of TResourceIdentifier;
            constructor Create();
            procedure add(res: tResourceIdentifier);
            procedure LoadResources(showProgress: Boolean; kind: ResourceKind); overload;
            procedure LoadResources(showProgress: Boolean); overload;
            procedure ReleaseResources();
        end;

//----------------------------------------------------------------------------
// Private type functions/procedures
//----------------------------------------------------------------------------

    constructor TResourceBundle.create();
    begin
        inherited create;
        SetLength(identifiers, 0);
    end;

    procedure TResourceBundle.add(res: tResourceIdentifier);
    begin
        SetLength(identifiers, Length(identifiers) + 1);
        identifiers[High(identifiers)] := res;
    end;

    procedure TResourceBundle.LoadResources(showProgress: Boolean; kind: ResourceKind); //overload;
    var
        current: tResourceIdentifier;
        i: Longint;
        
        procedure rbLoadFont();
        begin
            if Length(current.data) <> 1 then
            begin
                RaiseException('Font must have a size assigned. ' + current.name);
                exit;
            end;
            LoadFontNamed(current.name, current.path, current.data[0])
        end;
        
        procedure rbLoadBitmap();
        var
            bmp: sgTypes.Bitmap;
        begin
            bmp := LoadBitmapNamed(current.name, current.path);
            
            if Length(current.data) > 0 then
            begin
                if Length(current.data) <> 5 then
                begin
                        RaiseException('Invalid number of values for bitmap ' + current.name + ' expected 5 values for width, height, cellCols, cellRows, cellCount');
                        exit;
                end;
                
                BitmapSetCellDetails(bmp, current.data[0], current.data[1], current.data[2], current.data[3], current.data[4]);
            end;
        end;
        
    begin
        for i := Low(identifiers) to High(identifiers) do
        begin
            current := identifiers[i];
            
            if current.kind = kind then
            begin
                case kind of
                    BundleResource:         LoadResourceBundleNamed(current.name, current.path, false);
                    TimerResource:          CreateTimer(current.name);
                    BitmapResource:         rbLoadBitmap();
                    FontResource:           rbLoadFont();
                    SoundResource:          LoadSoundEffectNamed(current.name, current.path);
                    MusicResource:          LoadMusicNamed(current.name, current.path);
                    MapResource:            LoadMapNamed(current.name, current.path);
                    AnimationResource:      LoadAnimationScriptNamed(current.name, current.path);
                    PanelResource:          LoadPanelNamed(current.name, current.path);
                    CharacterResource:      LoadCharacterNamed(current.name, current.path);
                    else
                        RaiseException('Unknown resource kind in LoadResources' + IntToStr(Longint(kind)));
                end;
            end;
        end;
    end;

    procedure TResourceBundle.LoadResources(showProgress: Boolean); //overload;
    var
        kind: ResourceKind;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgResources', 'TResourceBundle.LoadResources');
        {$ENDIF}
        
        for kind := Low(ResourceKind) to High(ResourceKind) do
        begin
            {$IFDEF TRACE}
                Trace('sgResources', 'Info', 'TResourceBundle.LoadResources', 'Calling for ' + IntToStr(Longint(kind)));
            {$ENDIF}
            LoadResources(showProgress, kind);          
        end;
        {$IFDEF TRACE}
            TraceExit('sgResources', 'TResourceBundle.LoadResources');
        {$ENDIF}
    end;

    procedure TResourceBundle.ReleaseResources();
    var
        current: tResourceIdentifier;
        i: Longint;
    begin
    
        for i := Low(identifiers) to High(identifiers) do
        begin
            current := identifiers[i];

            case current.kind of
                BundleResource:         ReleaseResourceBundle(current.name);
                BitmapResource:         ReleaseBitmap(current.name);
                TimerResource:          ReleaseTimer(current.name);
                FontResource:               ReleaseFont(current.name);
                SoundResource:          ReleaseSoundEffect(current.name);
                MusicResource:          ReleaseMusic(current.name);
                PanelResource:          ReleasePanel(current.name);
                MapResource:                ReleaseMap(current.name);
                AnimationResource:  ReleaseAnimationScript(current.name);
                CharacterResource:  ReleaseCharacter(current.name);
            end;
        end;
        SetLength(identifiers, 0);
    end;

    //----------------------------------------------------------------------------
    
    procedure ReleaseAllResources();
    begin
        ReleaseAllTimers();
        ReleaseAllFonts();
        ReleaseAllMusic();
        ReleaseAllPanels();
        ReleaseAllMaps();
        ReleaseAllSprites();
        ReleaseAllCharacters();
        ReleaseAllConnections();
        ReleaseAllAnimationScripts();
        ReleaseAllSoundEffects();
        ReleaseAllBitmaps();
        ReleaseAllArduinoDevices();
        _Bundles.deleteAll();
    end;
    
    //----------------------------------------------------------------------------
    
    function StringToResourceKind(kind: String): ResourceKind;
    begin
        kind := Uppercase(Trim(kind));
        if kind = 'BUNDLE' then result := BundleResource
        else if kind = 'BITMAP'         then result := BitmapResource
        else if kind = 'TIMER'          then result := TimerResource
        else if kind = 'SOUND'          then result := SoundResource
        else if kind = 'MUSIC'          then result := MusicResource
        else if kind = 'FONT'               then result := FontResource
        else if kind = 'MAP'                then result := MapResource
        else if kind = 'ANIM'               then result := AnimationResource
        else if kind = 'PANEL'          then result := PanelResource
        else if kind = 'CHARACTER'  then result := CharacterResource
        else result := OtherResource;
    end;
    
    function FilenameToResource(name: String; kind: ResourceKind): String;
    begin
        result := name;
        
        if not FileExists(result) then
        begin
            result := PathToResource(name, kind);
            
            if not FileExists(result) then
            begin
                RaiseWarning('Unable to locate resource at ' + result);
                result := '';
                exit;
            end;
        end;
    end;
    
    //----------------------------------------------------------------------------
    
    procedure LoadResourceBundle(name: String; showProgress: Boolean); overload;
    begin
        LoadResourceBundleNamed(name, name, showProgress);
    end;
    
    // Called to read in each line of the resource bundle.
    // ptr is a pointer to a tResourceBundle to load details into
    procedure ProcessBundleLine(const line: LineData; ptr: Pointer);
    var
        delim: TSysCharSet;
        i: Longint;
        current: tResourceIdentifier;
    begin
        delim := [ ',' ]; //comma delimited
        
        current.kind := StringToResourceKind(ExtractDelimited(1, line.data, delim));
        current.name := ExtractDelimited(2, line.data, delim);
        
        if Length(current.name) = 0 then 
        begin
            RaiseException('No name for resource.');
            exit;
        end;
        
        current.path := ExtractDelimited(3, line.data, delim);
        
        if (Length(current.path) = 0) and not (current.kind = TimerResource) then 
        begin
            RaiseException('No path supplied for resource ' + current.name);
            exit;
        end;
        
        if CountDelimiter(line.data, ',') > 2 then
        begin
            SetLength(current.data, CountDelimiter(line.data, ',') - 2);
            
            for i := 4 to CountDelimiter(line.data, ',') + 1 do //Start reading from the 4th position (after the 3rd ,)
            begin
                if not TryStrToInt(ExtractDelimited(i, line.data, delim), current.data[i - 4]) then
                begin
                    RaiseException('Invalid data expected a whole number at position ' + IntToStr(i + 1));
                    exit;
                end;
            end;
        end
        else
        begin
            SetLength(current.data, 0);
        end;
            
        //WriteLn('Bundle: ', current.name, ' - ', current.path, ' - ', current.size);
        tResourceBundle(ptr).add(current);
    end;
        
    procedure LoadResourceBundleNamed(name, filename: String; showProgress: Boolean);
    var
        bndl: tResourceBundle;
    begin
        {$IFDEF TRACE}
            TraceEnter('sgResources', 'LoadResourceBundle');
        {$ENDIF}
        
        if _Bundles.containsKey(name) then
        begin
            RaiseWarning('Warning loaded Resource Bundle resource twice, ' + name);
            exit;
        end;
        
        bndl := tResourceBundle.Create();
        ProcessLinesInFile(filename, BundleResource, @ProcessBundleLine, bndl);
        bndl.LoadResources(showProgress);
    
        if not _Bundles.setValue(name, bndl) then //store bundle
        begin
            bndl.ReleaseResources();
            bndl.Free();
            RaiseException('Error loaded Bundle twice, ' + name);
            exit;
        end;
        
        {$IFDEF TRACE}
            TraceExit('sgResources', 'LoadResourceBundle');
        {$ENDIF}
    end;
    
    procedure LoadResourceBundle(name: String); overload;
    begin
        LoadResourceBundle(name, True);
    end;
    
    function HasResourceBundle(name: String): Boolean;
    begin
        result := _Bundles.containsKey(name);
    end;
    
    procedure ReleaseResourceBundle(name: String);
    var
        bndl: tResourceBundle;
    begin
        if HasResourceBundle(name) then
        begin
            bndl := tResourceBundle(_Bundles.remove(name));
            bndl.ReleaseResources();
            bndl.Free();
        end;
    end;
    
    //----------------------------------------------------------------------------
    
    
    
    
    function PathToResourceWithBase(path, filename: String; kind: ResourceKind): String; overload;
    begin
        case kind of
        {$ifdef UNIX}
            BundleResource:         result := PathToResourceWithBase(path, 'bundles/' + filename);
            FontResource:               result := PathToResourceWithBase(path, 'fonts/' + filename);
            SoundResource:          result := PathToResourceWithBase(path, 'sounds/' + filename);
            BitmapResource:         result := PathToResourceWithBase(path, 'images/' + filename);
            MapResource:                result := PathToResourceWithBase(path, 'maps/' + filename);
            AnimationResource:  result := PathToResourceWithBase(path, 'animations/' + filename);
            PanelResource:          result := PathToResourceWithBase(path, 'panels/' + filename);
            CharacterResource:  result := PathToResourceWithBase(path, 'characters/' + filename);
        {$else}
            BundleResource:         result := PathToResourceWithBase(path, 'bundles\' + filename);
            FontResource:               result := PathToResourceWithBase(path, 'fonts\' + filename);
            SoundResource:          result := PathToResourceWithBase(path, 'sounds\' + filename);
            BitmapResource:         result := PathToResourceWithBase(path, 'images\' + filename);
            MapResource:                result := PathToResourceWithBase(path, 'maps\' + filename);
            AnimationResource:  result := PathToResourceWithBase(path, 'animations\' + filename);
            PanelResource:          result := PathToResourceWithBase(path, 'panels\' + filename);
            CharacterResource:  result := PathToResourceWithBase(path, 'characters\' + filename);
        {$endif}
            
            else result := PathToResourceWithBase(path, filename);
        end;
    end;
    
    var _mac_relative_path: String = '/../Resources/';
    
    procedure _SetMacRelativePath();
    begin
        if DirectoryExists(applicationPath + '/Resources') then
        begin
            _mac_relative_path := '/Resources/';
        end
        else //DirectoryExists(applicationPath + '../Resources') then
        begin
            _mac_relative_path := '/../Resources/';
        end;
        
        // WriteLn('Set _mac_relative_path to ', _mac_relative_path, ' = ', applicationPath + _mac_relative_path);
    end;
    
    function PathToResourceWithBase(path, filename: String): String; overload;
    begin
        {$ifdef UNIX}
            {$ifdef DARWIN}
                result := path + _mac_relative_path;
            {$else}
                result := path + '/Resources/';
            {$endif}
        {$else}
        //Windows
            result := path + '\Resources\';
        {$endif}
        result := result + filename;
    end;
    
    function PathToResource(filename: String): String; overload;
    begin
        result := PathToResourceWithBase(applicationPath, filename);
    end;
    
    function PathToResource(filename: String; kind: ResourceKind): String; overload;
    begin
        result := PathToResourceWithBase(applicationPath, filename, kind);
    end;
    
    function PathToResource(filename, subdir: String): String; overload;
    var
        paths: array [0..0] of String;
    begin
        paths[0] := subdir;
        result := PathToResource(filename, OtherResource, paths);
    end;
    
    function PathToResource(filename: String; kind: ResourceKind; subdir: String): String; overload;
    var
        paths: array [0..0] of String;
    begin
        paths[0] := subdir;
        result := PathToResource(filename, kind, paths);
    end;
    
    procedure _GuessAppPath(); forward;

    function PathToResource(filename: String; kind: ResourceKind; const subPaths: StringArray): String; overload;
    var
        temp: String;
        i: Longint;
    begin
        if not _AppPathSet then _GuessAppPath();

        if Length(subPaths) > 0 then
        begin
            temp := '';
            
            for i := 0 to High(subPaths) do
            begin
                {$ifdef UNIX}
                    temp := temp + subPaths[i] + '/';
                {$else} // Windows
                    temp := temp + subPaths[i] + '\';
                {$endif}
            end;
            
            filename := temp + filename;
        end;
        
        result := PathToResource(filename, kind)
    end;
    
    procedure SetAppPath(path: String); overload;
    begin
        SetAppPath(path, True);
    end;
    
    procedure SetAppPath(path: String; withExe: Boolean);
    begin
        {$IFDEF TRACE}
            TraceEnter('sgResources', 'SetAppPath(' + path + ', ' + BoolToStr(withExe, true) + ')');
        {$ENDIF}
        
        if withExe then applicationPath := ExtractFileDir(path)
        else applicationPath := path;
        
        {$IFDEF DARWIN}
        _SetMacRelativePath();
        {$ENDIF}

        {$IFDEF IOS}
            applicationPath := applicationPath + '/MyResources/Resources';
        {$ENDIF}
        
        {$IFDEF TRACE}
            TraceExit('sgResources', 'SetAppPath', applicationPath);
        {$ENDIF}
    end;
    
    function AppPath(): String;
    begin
        result := applicationPath;
    end;
    
    
//----------------------------------------------------------------------------
    
    {$ifdef DARWIN}
    //
    // Find the executable path in MacOS
    //
    function CFBundleGetMainBundle(): Pointer; cdecl; external 'CoreFoundation'; {$EXTERNALSYM CFBundleGetMainBundle}
    function CFBundleCopyBundleURL(bundle: Pointer): Pointer; cdecl; external 'CoreFoundation'; {$EXTERNALSYM CFBundleCopyBundleURL}
    function CFURLCopyFileSystemPath(bundle: Pointer; pathKind: LongInt): Pointer; cdecl; external 'CoreFoundation'; {$EXTERNALSYM CFURLCopyFileSystemPath}
    procedure CFStringGetCString(theString: Pointer; buffer:PChar; bufSize, encode: LongInt); cdecl; external 'CoreFoundation'; {$EXTERNALSYM CFStringGetCString}
    procedure CFRelease(ptr: Pointer); cdecl; external 'CoreFoundation'; {$EXTERNALSYM CFRelease}
    
    procedure _GuessAppPath();
    var
        path: PChar;
        cwd: String;
        mainBundle: Pointer = nil;
        mainBundleURL: Pointer = nil;
        cfStringRef: Pointer = nil;
    begin
        cwd := GetCurrentDir();
        
        if DirectoryExists(cwd + '/Resources') then
            SetAppPath(cwd + '/Resources', False)
        else if DirectoryExists(cwd + '../Resources') then
            SetAppPath(cwd + '../Resources', False)
        else if DirectoryExists(cwd + '../../Resources') then 
            SetAppPath(cwd + '../../Resources', False)
        else
        begin
            mainBundle := CFBundleGetMainBundle();
            if not assigned(mainBundle) then
            begin
                SetAppPath(cwd, False);
                exit;
            end;
            
            mainBundleURL := CFBundleCopyBundleURL(mainBundle);
            if not assigned(mainBundleURL) then
            begin
                SetAppPath(cwd, False);
                exit;
            end;
            
            cfStringRef := CFURLCopyFileSystemPath(mainBundleURL, 0);
            if not assigned(cfStringRef) then
            begin
                SetAppPath(cwd, False);
                exit;
            end;
            
            path := StrAlloc(4096);
            
            CFStringGetCString(cfStringRef, path, 4096, 1536); // 1536 = 0x0600
            
            // Check for C# mono mac path location
            if Pos('Resources/bin', path) = Length(path) - Length('Resources/bin') + 1 then
                cwd := String(path) + '/..'
            else
                cwd := String(path) + '/Contents/MacOS';

            CFRelease(mainBundleURL);
            CFRelease(cfStringRef);
            StrDispose(path);
            
            SetAppPath(cwd, False);
        end;
    end;
    {$else}
        {$ifdef UNIX}
            procedure _GuessAppPath();
            var
                len: cint;
                path: PChar;
            begin
                path := StrAlloc(4096);

                // Read symbolic link /proc/self/exe                
                len := fpReadLink('/proc/self/exe', path, 4096);

                if len = -1 then
                begin
                    SetAppPath(GetCurrentDir(), False);
                    exit;
                end;
                
                path[len] := #0;
                
                SetAppPath(path, True);
				StrDispose(path);
            end;
        {$else}
            //
            // Get Executable path in Windows
            //
            procedure _GuessAppPath();
            var
                path: PChar;
            begin
                path := StrAlloc(4096);
                if GetModuleFileName(0, path, 4096) <> 0 then
                    SetAppPath(path, true)
                else
                    SetAppPath(GetCurrentDir(), False);
                StrDispose(path);
            end;
        {$endif}
    {$endif}


//=============================================================================

    initialization
    begin
        {$IFDEF TRACE}
            TraceEnter('sgResources', 'initialization');
        {$ENDIF}
        
        InitialiseSwinGame();
        
        _Bundles := TStringHash.Create(False, 1024);
        
        try
            
            if ParamCount() >= 0 then SetAppPath(ParamStr(0), True)
            else _GuessAppPath();
            
            _AppPathSet := true;
            
        except
        end;
        
        {$IFDEF TRACE}
            TraceExit('sgResources', 'initialization');
        {$ENDIF}
	    end;
    
    finalization
    begin
      _Bundles.deleteAll();
      FreeAndNil(_Bundles);
    end;
    
end.
