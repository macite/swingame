program GameMain;
uses SwinGame, sgMaps; 

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  LoadDefaultColors();
  ShowSwinGameSplashScreen();

  WriteLn('Testing LoadBitmap'); 
  LoadBitmapNamed('testing','testing.bmp'); 
  WriteLn('Testing LoadSoundEffect'); 
  LoadSoundEffectNamed('testing2','testing.wav'); 
  WriteLn('Testing LoadMusic'); 
  LoadMusicNamed('testing3','testing.wav'); 
  WriteLn('Testing LoadFont'); 
  LoadFontNamed('testingfont', 'testing.ttf',12); 
  WriteLn('Testing LoadPanel'); 
  LoadPanelNamed('testing panel', 'testing panel'); 

  try 
    WriteLn('Testing LoadMap'); 
    LoadMapNamed('testing map', 'testmap'); 
  except
  end; 

  try 
    WriteLn('Testing LoadBundle'); 
    LoadResourceBundleNamed('bundle testing','bundlefile', true); 
  except
  end;

  try 
    WriteLn('Testing LoadCharacter'); 
    LoadCharacterNamed('testing character' , 'testing.character'); 
  except 
  end; 
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen(ColorWhite);
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.
