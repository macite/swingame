program HowToControlMusic;
uses
  SwinGame, sgTypes;
  
procedure Main();  
begin
  OpenAudio();

  OpenGraphicsWindow('How To Control Music', 320, 240);
  LoadDefaultColors();
  LoadMusic('game.ogg');
  LoadMusic('diving-turtle.mp3');
  LoadMusic('fast.mp3');
  LoadMusic('gentle-thoughts-1.mp3');
  LoadMusic('morning-workout.mp3');
  LoadMusic('saber.ogg');

  SetMusicVolume(1);

  repeat // The game loop...
    ProcessEvents();      

    ClearScreen(ColorWhite);

    if KeyDown(RightCtrlKey) OR KeyDown(LeftCtrlKey) then
    begin
      if KeyTyped(Key1) then FadeMusicIn('game.ogg', -1, 10000);
      if KeyTyped(Key2) then FadeMusicIn('fast.mp3', -1, 10000);
      if KeyTyped(Key3) then FadeMusicIn('gentle-thoughts-1.mp3', -1, 10000);
      if KeyTyped(Key4) then FadeMusicIn('morning-workout.mp3', -1, 10000);
      if KeyTyped(Key5) then FadeMusicIn('saber.ogg', -1, 10000);
      if KeyTyped(Key6) then FadeMusicIn('diving-turtle.mp3', -1, 10000);      
    end
    else if KeyTyped(RightAltKey) OR KeyTyped(LeftAltKey) then
    begin
      FadeMusicOut(10000);
    end
    else
    begin
      if KeyTyped(Key1) then PlayMusic('game.ogg');
      if KeyTyped(Key2) then PlayMusic('fast.mp3');
      if KeyTyped(Key3) then PlayMusic('gentle-thoughts-1.mp3');
      if KeyTyped(Key4) then PlayMusic('morning-workout.mp3');
      if KeyTyped(Key5) then PlayMusic('saber.ogg');      
      if KeyTyped(Key6) then PlayMusic('diving-turtle.mp3');
      if KeyTyped(PKey) then PauseMusic();
      if KeyTyped(RKey) then ResumeMusic();
      if KeyTyped(KeyPadPlus) and (MusicVolume() < 1)  then SetMusicVolume(MusicVolume() + 0.1);
      if KeyTyped(KeyPadMinus) and (MusicVolume() > 0) then SetMusicVolume(MusicVolume() - 0.1);      
      if KeyTyped(SKey) then if MusicPlaying() then StopMusic();
    end;
    DrawText('Control Music (Escape or q to quit)', ColorRed, 'Arial', 18, 15, 15);
    DrawText('1-6 to play different music', ColorBlue, 'Arial', 14, 20, 50);
    DrawText('CTRL + (1-6) to Fade Music In', ColorBlue, 'Arial', 14, 20, 75);
    DrawText('Alt + (1-6) to Fade Music Out', ColorBlue, 'Arial', 14, 20, 100);
    DrawText('p to pause music', ColorBlue, 'Arial', 14, 20, 125);
    DrawText('r to resume music', ColorBlue, 'Arial', 14, 20, 150);
    DrawText('+ or - increase volume by 10%', ColorBlue, 'Arial', 14, 20, 175);
    DrawText('s to stop playing music', ColorBlue, 'Arial', 14, 20, 200);

    RefreshScreen(60);    
  until WindowCloseRequested() OR KeyTyped(EscapeKey) OR KeyTyped(QKey);

  CloseAudio();
  ReleaseAllResources();
end;
begin
  Main();
end.