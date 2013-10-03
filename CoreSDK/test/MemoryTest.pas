program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgAudio, sgText, sgGraphics, sgResources, sgInput;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('Memory Test', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen();
    
    ReleaseAllResources();
    LoadResourceBundle('splash.txt');
    
    DrawFramerate(0,0);
    DrawText('Close to end', ColorWhite, 100, 100);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
