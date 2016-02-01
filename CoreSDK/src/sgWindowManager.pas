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
  ///
  /// @class Window
  /// @dispose
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
  procedure SetCurrentWindow(const name: String);


  ///
  /// @lib
  ///
  function WindowCount(): Longint;

  ///
  /// @lib
  ///
  function WindowAtIndex(idx: Longint): Window;

  /// Checks to see if the window has been asked to close. You need to handle
  /// this if you want the game to end when the window is closed. This value
  /// is updated by the `ProcessEvents` routine.
  ///
  /// @returns: True if the window has been requested to close.
  ///
  /// @lib WindowCloseRequested
  function WindowCloseRequested(wind: Window): Boolean;

  /// Checks to see if the primary window has been asked to close. You need to handle
  /// this if you want the game to end when the window is closed. This value
  /// is updated by the `ProcessEvents` routine.
  ///
  /// @returns: True if the window has been requested to close.
  ///
  /// @lib PrimaryWindowCloseRequested
  function WindowCloseRequested(): Boolean;

  /// Returns the window that the user has focused on.
  ///
  /// @lib
  function WindowWithFocus(): Window;

  /// Move the window to a new Position on the screen.
  ///
  /// @lib
  procedure MoveWindow(wind: Window; x, y: Longint);

  /// Move the window to a new Position on the screen.
  ///
  /// @lib MoveWindowNamed
  procedure MoveWindow(const name: String; x, y: Longint);

  /// Returns the Position of the window on the desktop.
  ///
  /// @lib
  function WindowPosition(wind: Window): Point2D;

  /// Returns the Position of the window on the desktop.
  ///
  /// @lib WindowPositionNamed
  function WindowPosition(const name: String): Point2D;

  /// Return the x Position of the window -- the distance from the
  /// left side of the primary desktop.
  ///
  /// @lib
  function WindowX(wind: Window): Longint;

  /// Return the x Position of the window -- the distance from the
  /// left side of the primary desktop.
  ///
  /// @lib WindowXNamed
  function WindowX(const name: String): Longint;

  /// Return the y Position of the window -- the distance from the
  /// top side of the primary desktop.
  ///
  /// @lib
  function WindowY(wind: Window): Longint;

  /// Return the y Position of the window -- the distance from the
  /// top side of the primary desktop.
  ///
  /// @lib WindowYNamed
  function WindowY(const name: String): Longint;

  /// Returns the width of a window.
  ///
  /// @lib
  function WindowWidth(wind: Window): Longint;

  /// Returns the width of a window.
  ///
  /// @lib WindowWidthNamed
  function WindowWidth(const name: String): Longint;

  /// Returns the height of a window.
  ///
  /// @lib
  function WindowHeight(wind: Window): Longint;

  /// Returns the height of a window.
  ///
  /// @lib WindowHeightNamed
  function WindowHeight(const name: String): Longint;

  /// Changes the size of the screen.
  ///
  /// @param width, height: The new width and height of the screen
  ///
  /// Side Effects:
  /// - The screen changes to the specified size
  ///
  /// @lib
  /// @sn changeScreenSizeToWidth:%s height:%s
  procedure ChangeScreenSize(width, height: Longint);

  /// Changes the size of the window.
  ///
  /// @param width, height: The new width and height of the window
  ///
  /// @lib
  /// @sn changeWindowSize:%s toWidth:%s height:%s
  procedure ChangeWindowSize(wind: Window; width, height: Longint);

  /// Changes the size of the window.
  ///
  /// @param width, height: The new width and height of the window
  ///
  /// @lib changeWindowSizeNamed
  /// @sn changeWindowSizeOfWindowNamed:%s toWidth:%s height:%s
  procedure ChangeWindowSize(const name: String; width, height: Longint);

  /// Switches the application to full screen or back from full screen to
  /// windowed.
  ///
  /// Side Effects:
  /// - The window switched between fullscreen and windowed
  ///
  /// @lib
  procedure ToggleFullScreen();
  
  /// Toggle the Window border mode. This enables you to toggle from a bordered
  /// window to a borderless window.
  ///
  /// @lib
  procedure ToggleWindowBorder();
  
  /// Returns the width of the screen currently displayed.
  ///
  /// @returns: The screen's width
  ///
  /// @lib
  ///
  /// @class Graphics
  /// @static
  /// @getter ScreenWidth
  function ScreenWidth(): Longint;

  /// Returns the height of the screen currently displayed.
  ///
  /// @returns: The screen's height
  ///
  /// @lib
  ///
  /// @class Graphics
  /// @static
  /// @getter ScreenHeight
  function ScreenHeight(): Longint;

//=============================================================================
implementation
uses  SysUtils,
      sgNamedIndexCollection, sgDriverGraphics, sgDriverImages, sgShared, sgBackendTypes, sgInputBackend,
      sgResources, sgDriverSDL2Types, sgDriverInput, sgGeometry;

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
        SetLength(_Windows, Length(_Windows) - 1);
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

  function WindowCloseRequested(wind: Window): Boolean;
  var
    wp: WindowPtr;
  begin
    wp := ToWindowPtr(wind);
    if Assigned(wp) then
    begin
      result := (wp^.eventData.close_requested <> 0) or HasQuit();
    end
    else result := false;
  end;

  function WindowCloseRequested(): Boolean;
  begin
    result := WindowCloseRequested(Window(_PrimaryWindow));  
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

  procedure SetCurrentWindow(const name: String);
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

  function WindowWithFocus(): Window;
  var
    i: Integer;
    wp: WindowPtr;
  begin
    for i := 0 to High(_Windows) do
    begin
      wp := _Windows[i];
      if wp^.eventData.has_focus <> 0 then
      begin
        result := Window(wp);
        exit;
      end; 
    end;

    result := Window(_Windows[0]);
  end;

  procedure MoveWindow(wind: Window; x, y: Longint);
  var
    wp: WindowPtr;
    p: Point2D;
  begin
    wp := ToWindowPtr(wind);
    if Assigned(wp) then
    begin
      sgDriverInput.MoveWindow(wp, x, y);

      // Read in the new Position...
      p := sgDriverInput.WindowPosition(wp);
      wp^.x := Round(p.x);
      wp^.y := Round(p.y);
    end
  end;

  function WindowPosition(wind: Window): Point2D;
  var
    wp: WindowPtr;
  begin
    wp := ToWindowPtr(wind);
    if Assigned(wp) then
    begin
      result := sgDriverInput.WindowPosition(wp);
    end
    else
      result := PointAt(0,0);
  end;

  function WindowPosition(const name: String): Point2D;
  begin
    result := WindowPosition(WindowNamed(name));
  end;

  function WindowX(wind: Window): Longint;
  begin
    result := Round(WindowPosition(wind).x);
  end;

  function WindowX(const name: String): Longint;
  begin
    result := Round(WindowPosition(name).x);
  end;

  function WindowY(wind: Window): Longint;
  begin
    result := Round(WindowPosition(wind).y);
  end;

  function WindowY(const name: String): Longint;
  begin
    result := Round(WindowPosition(name).y);
  end;

  procedure MoveWindow(const name: String; x, y: Longint);
  begin
    MoveWindow(WindowNamed(name), x, y);
  end;

  procedure ToggleFullScreen();
  begin
    if Assigned(_CurrentWindow) then
      sgDriverGraphics.SetVideoModeFullScreen(_CurrentWindow);
  end;
  
  procedure ToggleWindowBorder();
  begin
    sgDriverGraphics.SetVideoModeNoFrame(_CurrentWindow);
  end;

  procedure ChangeWindowSize(wind: Window; width, height: Longint);
  var
    wp: WindowPtr;
  begin
    wp := ToWindowPtr(wind);

    if (not Assigned(wp)) or (width < 1) or (height < 1) then
    begin
      exit; 
    end;

    if (width = wp^.image.surface.width) and (height = wp^.image.surface.height) then exit;

    sgDriverGraphics.ResizeWindow(wp, width, height);
  end;

  procedure ChangeWindowSize(const name: String; width, height: Longint);
  begin
    ChangeWindowSize(WindowNamed(name), width, height);
  end;

  procedure ChangeScreenSize(width, height: Longint);
  begin
    if not Assigned(_CurrentWindow) then exit;

    ChangeWindowSize(Window(_CurrentWindow), width, height);
  end;

  function WindowWidth(wind: Window): Longint;
  var
    w: WindowPtr;
  begin
    w := ToWindowPtr(wind);
    if not Assigned(w) then result := 0
    else result := w^.image.surface.width;
  end;

  function WindowWidth(const name: String): Longint;
  begin
    result := WindowWidth(WindowNamed(name));
  end;

  function WindowHeight(wind: Window): Longint;
  var
    w: WindowPtr;
  begin
    w := ToWindowPtr(wind);
    if not Assigned(w) then result := 0
    else result := w^.image.surface.height;
  end;

  function WindowHeight(const name: String): Longint;
  begin
    result := WindowHeight(WindowNamed(name));
  end;

  function ScreenWidth(): Longint;
  begin
    result := WindowWidth(Window(_CurrentWindow));
  end;

  function ScreenHeight(): Longint;
  begin
    result := WindowHeight(Window(_CurrentWindow));
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
