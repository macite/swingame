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

  ///
  /// @lib
  ///
  procedure SetCurrentWindow(wnd: Window);

  ///
  /// @lib SetCurrentWindowNamed
  ///
  procedure SetCurrentWindow(name: String);


  ///
  /// @lib
  ///
  function WindowCount(): Longint;

  ///
  /// @lib
  ///
  function WindowAtIndex(idx: Longint): Window;


//=============================================================================
implementation
uses  SysUtils,
      sgNamedIndexCollection, sgDriverGraphics, sgDriverImages, sgShared, sgBackendTypes, sgInputBackend,
      sgResources, sgDriverSDL2Types;

// _Windows keeps track of all of the open Windows -- accessed by title
var 
  _WindowNames: NamedIndexCollection;
  _Windows: array of WindowPtr;


function OpenWindow(const caption: String; width: Longint; height: Longint): Window;
var
  realCaption: String;
  idx: Longint;
  wind: WindowPtr;
begin
  realCaption := caption;
  idx := 0;
  
  while HasName(_WindowNames, realCaption) do
  begin
    realCaption := caption + ': ' + IntToStr(idx);
    idx := idx + 1;
  end;

  wind := sgDriverGraphics.OpenWindow(realCaption, width, height);
  result := Window(wind);

  AddName(_WindowNames, realCaption);

  SetLength(_Windows, Length(_Windows) + 1);
  _Windows[High(_Windows)] := wind;

  if not Assigned(_PrimaryWindow) then
  begin
    _PrimaryWindow := wind;
    _CurrentWindow := wind;
  end;
end;

procedure CloseWindow(wind: Window); overload;
var
  wp: WindowPtr;
  i, idx: Longint;
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

    idx := RemoveName(_WindowNames, wp^.caption);
    if idx >= 0 then
    begin
      for i := idx to High(_Windows) - 1 do
      begin
        _Windows[i] := _Windows[i + 1];
      end;
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
  while Length(_windows) > 0 do
  begin
    CloseWindow(_windows[0]);
  end;
end;


function HasWindow(const name: String): Boolean;
begin
  result := HasName(_WindowNames, name);
end;

function WindowNamed(const name: String): Window;
var
  idx: Longint;
begin
  idx := IndexOf(_WindowNames, name);
  result := WindowAtIndex(idx);
end;

procedure SaveScreenshot(src: Window; const filepath: String);
var
  surface: psg_drawing_surface;
begin
  surface := ToSurfacePtr(src);
  if not assigned(surface) then exit;
  
  sgDriverImages.SaveSurface(surface, filepath);
end;

procedure SetCurrentWindow(wnd: Window);
var
  w: WindowPtr;
begin
  w := ToWindowPtr(wnd);
  if Assigned(w) then _CurrentWindow := w;
end;

procedure SetCurrentWindow(name: String);
begin
  SetCurrentWindow(WindowNamed(name));
end;

function WindowCount(): Longint;
begin
  result := Length(_Windows);
end;


function WindowAtIndex(idx: Longint): Window;
begin
  if (idx >= 0) and (idx <= High(_Windows)) then
    result := Window(_Windows[idx])
  else
    result := nil; 
end;



//=============================================================================

initialization
begin
  InitialiseSwinGame();
  
  InitNamedIndexCollection(_WindowNames);
  SetLength(_Windows, 0);
end;

finalization
begin
  SetLength(_Windows, 0);
  FreeNamedIndexCollection(_WindowNames);
end;

//=============================================================================

end.

//=============================================================================
