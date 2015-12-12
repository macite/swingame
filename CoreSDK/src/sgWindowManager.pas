//=============================================================================
// sgWindowManager.pas
//=============================================================================
//
// Code to keep track of the open windows.
//
//=============================================================================



/// The WindowManager is responsible for keeping track of all of the Windows
/// you open in SwinGame. 
/// 
/// @module WindowManager
/// @static
unit sgWindowManager;

//=============================================================================
interface
  uses sgTypes;

  /// Opens the window so that it can be drawn onto and used to respond to user
  /// actions. The window itself is only drawn when you call `RefreshScreen`. 
  ///
  /// The first window opened will be the primary window, closing this window
  /// will cause SwinGame to indicate the user wants to quit the program.
  ///
  /// Unless otherwise specified using `DrawingOptions`, all drawing operations 
  /// will draw onto the current window. This starts as the first window opened
  /// but can be changed with `SelectWindow`.
  ///
  /// @param caption The caption for the window
  /// @param width The width of the window
  /// @param height The height of the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  ///
  /// @lib
  /// @uname OpenWindow
  /// @sn openWindow:%s width:%s height:%s
  function OpenWindow(const caption: String; width, height: Longint): Window; overload;

  /// Close a window that you have opened.
  ///
  /// @lib CloseWindowNamed
  procedure CloseWindow(const name: String); overload;

  /// Close a window.
  ///
  /// @lib
  procedure CloseWindow(wind: Window); overload;

  /// Is there a window a window with the specified name.
  ///
  /// @lib
  function HasWindow(const name: String): Boolean;

  /// Get the window with the speficied name.
  ///
  /// @lib
  function WindowNamed(const name: String): Window;

  /// Save screenshot to specific directory.
  /// 
  /// @lib
  /// @sn window:%s saveScreenshotTo:%s
  ///
  /// @class Window
  /// @method SaveScreenshot
  /// @csn saveScreenshotTo:%s
  procedure SaveScreenshot(src: Window; const filepath: String);

//=============================================================================
implementation
uses  SysUtils, StringHash,
      sgNamedIndexCollection, sgDriverGraphics, sgDriverImages, sgShared, sgBackendTypes, sgInputBackend,
      sgResources, sgDriverSDL2Types;

// _Windows keeps track of all of the open Windows -- accessed by title
var _Windows: TStringHash;


function OpenWindow(const caption: String; width: Longint; height: Longint): Window;
var
  realCaption: String;
  idx: Longint;
  wind: WindowPtr;
  obj: tResourceContainer;
begin
  realCaption := caption;
  idx := 0;
  
  while _Windows.containsKey(realCaption) do
  begin
    realCaption := caption + ': ' + IntToStr(idx);
    idx := idx + 1;
  end;

  wind := sgDriverGraphics.OpenWindow(realCaption, width, height);
  result := Window(wind);

  obj := tResourceContainer.Create(result);

  if not _Windows.setValue(realCaption, obj) then
  begin
    CloseWindow(result);
    result := nil;
    RaiseException('Error opening window  ' + realCaption);
    exit;
  end;

  if not Assigned(_PrimaryWindow) then
  begin
    _PrimaryWindow := wind;
    _CurrentWindow := wind;
  end;
end;

procedure CloseWindow(wind: Window); overload;
var
  wp: WindowPtr;
begin
  wp := ToWindowPtr(wind);
  if Assigned(wp) then
  begin

    if wp = _CurrentWindow then
    begin
      _CurrentWindow := _PrimaryWindow;
    end;

    if wp = _PrimaryWindow then
    begin
      sgInputBackend._quit := true;
      _PrimaryWindow := nil;

      if wp = _CurrentWindow then // in case they ignore the quit!
        _CurrentWindow := nil;
    end;

    sgDriverGraphics.CloseWindow(wp);
  end;
end;

procedure CloseWindow(const name: String); overload;
begin
  CloseWindow(WindowNamed(name));
end;

procedure CloseAllWindows();
begin
  ReleaseAll(_Windows, @CloseWindow);
end;


function HasWindow(const name: String): Boolean;
begin
  result := _Windows.containsKey(name);
end;

function WindowNamed(const name: String): Window;
var
  tmp : TObject;
begin
  tmp := _Windows.values[name];

  if Assigned(tmp) then
    result := Window(tResourceContainer(tmp).Resource)
  else
    result := nil; 
end;

procedure SaveScreenshot(src: Window; const filepath: String);
var
  surface: psg_drawing_surface;
begin
  surface := ToSurfacePtr(src);
  if not assigned(surface) then exit;
  
  sgDriverImages.SaveSurface(surface, filepath);
end;


//=============================================================================

initialization
begin
  InitialiseSwinGame();
  
  _Windows := TStringHash.Create(False, 10);
end;

finalization
begin
  CloseAllWindows();
  FreeAndNil(_Windows);
end;

//=============================================================================

end.

//=============================================================================
