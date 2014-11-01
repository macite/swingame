program HelloWorld;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  SwinGame, sgShared;

procedure TestLineDraw();
var
  p1, p2: Point2D;
begin
  
  p1.x := -6000;
  p1.y := 2000;
  p2.x := 1000;
  p2.y := 20000;
  
  DrawLineOnScreen(ColorRed, p1, p2);
  
end;

procedure Main();
var
  img: Bitmap;
  i: Integer;
begin
  OpenAudio();  
  
  // LoadResourceBundle('splash.txt');
  // LoadResourceBundle('bundle.txt');
  // LoadSoundEffectNamed('blah', 'blah');
  // LoadMusicNamed('blah', 'blah');
  // LoadBitmapNamed('blah', 'blah');
  // LoadAnimationScriptNamed('blah', 'blah');
  // LoadPanel('blah');
  // LoadAnimationScript('test.txt');
  // FillRectangle(ColorRed, 0, 0, 10, 10);
  // LoadDefaultColors();
  // LoadBitmap('cape.jpg');
  
  OpenGraphicsWindow('Hello World', 640, 480);
  LoadDefaultColors();
  ShowSwinGameSplashScreen();
  
  TestLineDraw();
  
  img := CreateBitmap(800, 600);
  
  //Test releasing of resources - not assigned
  // WriteLn('BMP: ', HexStr(BitmapNamed('fred')));
  // ReleaseBitmap('fred');
  // WriteLn('FNT: ', HexStr(FontNamed('fred')));
  // ReleaseFont('fred');
  // WriteLn('SND: ', HexStr(SoundEffectNamed('fred')));
  // ReleaseSoundEffect('fred');
  // WriteLn('MUS: ', HexStr(MusicNamed('fred')));
  // ReleaseMusic('fred');
  // WriteLn('MAP: ', HexStr(TileMapNamed('fred')));
  // ReleaseTileMap('fred');
  // ReleaseResourceBundle('fred');
  
  // StartTimer(TimerNamed('TestTimer'));
  
  for i := 0 to 2 do img := CreateBitmap(800,600);
  
  repeat // The game loop...
    ProcessEvents();
    
    //DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
    ClearScreen(ColorBlack);
    FillRectangle(ColorWhite, 10, 10, 780, 580);
    
    ClearSurface(img, RandomColor());
    DrawBitmap(img, 50, 50);
    
    for i := 0 to 9 do DrawText('Hello World', ColorBlack, 'arial', i + 10, 10 + i * 10, 10 + i *  10);
    
    DrawFramerate(0,0);
    
    RefreshScreen();
    // CyclePool();
  until WindowCloseRequested();
  
  WriteLn('Time: ', TimerTicks(TimerNamed('TestTimer')));
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
