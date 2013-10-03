program FileDialogTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  Math, SysUtils,
  sgUtils,
  sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, sgImages, sgInput;

procedure InitInterface();
begin
  DrawGUIAsVectors(true);
  GUISetForegroundColor(ColorWhite);
end;

procedure Main();
var
  lst: GUIList;
begin
  OpenAudio();
  OpenGraphicsWindow('File Dialog Test', 800, 600);
  
  InitInterface();
  ShowOpenDialog(fdFiles);
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    FillRectangle(ColorRed, 0,0, 50, 50);
    FillRectangle(ColorGreen, 50,0, 50, 50);
    FillRectangle(ColorBlue, 100, 0, 50, 50);
    
    DrawInterface();
    UpdateInterface();
    
    DrawFramerate(0,0);
    RefreshScreen();
    
    if KeyTyped(vk_s) then TakeScreenShot('FileDialogTest');
  until WindowCloseRequested() or DialogComplete() or DialogCancelled();
  
  WriteLn(DialogPath());
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
