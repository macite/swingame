program HowToControlSound;
uses
  SwinGame, sgTypes;  
procedure Main();
var
  sndEffect : SoundEffect;	
begin
  OpenAudio();
  
  OpenGraphicsWindow('How To Control Sound', 320, 240);
  LoadDefaultColors();  

  sndEffect := LoadSoundEffect('chipmunk.ogg');
  LoadSoundEffect('bells.ogg');
  LoadSoundEffect('camera.ogg');      
  LoadSoundEffect('comedy_boing.ogg'); 
  LoadSoundEffect('dinosaur.ogg');
  LoadSoundEffect('dog_bark.ogg');      
  
  repeat // The game loop...
    ProcessEvents();
    
    if KeyDown(vk_RCTRL) OR KeyDown(vk_LCTRL) then
    begin
      if KeyTyped(vk_1) then sndEffect := SoundEffectNamed('chipmunk.ogg');
      if KeyTyped(vk_2) then sndEffect := SoundEffectNamed('bells.ogg');
      if KeyTyped(vk_3) then sndEffect := SoundEffectNamed('camera.ogg');
      if KeyTyped(vk_4) then sndEffect := SoundEffectNamed('comedy_boing.ogg');
      if KeyTyped(vk_5) then sndEffect := SoundEffectNamed('dinosaur.ogg');
      if KeyTyped(vk_6) then sndEffect := SoundEffectNamed('dog_bark.ogg');
    end
    else
    begin
      if KeyTyped(vk_1) then PlaySoundEffect(sndEffect);
      if KeyTyped(vk_2) then PlaySoundEffect(sndEffect, 0.5);
      if KeyTyped(vk_3) then PlaySoundEffect(sndEffect, 3, 0.25);
      if KeyTyped(vk_4) then PlaySoundEffect(sndEffect, -1, 0.1);
      if KeyTyped(vk_5) then if SoundEffectPlaying(sndEffect) then StopSoundEffect(sndEffect);
    end;
    
    ClearScreen(ColorWhite);
    DrawText('Control Sound (Escape or q to quit)', ColorRed, 'Arial', 18, 15, 15);
    DrawText('1: Play Sound At Full Volume', ColorBlue, 'Arial', 14, 20, 50);
    DrawText('2: Play Sound At 50% Volume', ColorBlue, 'Arial', 14, 20, 80);
    DrawText('3: Play Sound At 25% Volume 3 Times', ColorBlue, 'Arial', 14, 20, 110);
    DrawText('4: Play Sound Continuously at 10%', ColorBlue, 'Arial', 14, 20, 140);
    DrawText('5: Stop Playing Current Sound', ColorBlue, 'Arial', 14, 20, 170);
    DrawText('CTRL + (1-6) load different sound effects', ColorBlue, 'Arial', 14, 20, 200);		
    
    RefreshScreen(60);
  until WindowCloseRequested() OR KeyTyped(vk_ESCAPE) OR KeyTyped(VK_Q);

  CloseAudio();
  ReleaseAllResources();
end;
begin
  Main();
end.