program HowToUseListBox;
uses 
 SwinGame, sgTypes, sgUserInterface;
 
procedure Main();
var
  p1,p2 : Panel;  
  lst    : GUIList;  
begin
  OpenAudio();
  OpenGraphicsWindow('How To Use ListBox ', 320, 240);
  LoadDefaultColors();
  
  GUISetBackgroundColor(ColorWhite); 
  GUISetForegroundColor(ColorGreen);  
  
  LoadResourceBundle('MainMenu.txt');
  
  //LoadMusic
  LoadMusic('game.ogg');
  LoadMusic('diving-turtle.mp3');
  LoadMusic('fast.mp3');
  LoadMusic('gentle-thoughts-1.mp3');
  LoadMusic('morning-workout.mp3');
  LoadMusic('saber.ogg');
  LoadMusic('chipmunk.ogg');
  LoadMusic('bells.ogg');
  LoadMusic('camera.ogg');      
  LoadMusic('comedy_boing.ogg'); 
  LoadMusic('dinosaur.ogg');
  LoadMusic('dog_bark.ogg');
  
  
  p1 := LoadPanel('lstBoxPanel.txt');  
  ShowPanel(p1);
  lst := ListFromRegion(RegionWithID(p1, 'List1'));
  
  ListAddItem(lst, 'Game');
  ListAddItem(lst, 'Diving Turtle');
  ListAddItem(lst, 'Fast');
  ListAddItem(lst, 'Gentle Thoughts');
  ListAddItem(lst, 'Morning Workout');
  ListAddItem(lst, 'Saber');
  ListAddItem(lst, 'Chipmunk');
  ListAddItem(lst, 'Bells');
  ListAddItem(lst, 'Camera');
  ListAddItem(lst, 'Comedy Boing');
  ListAddItem(lst, 'Dinosaur');
  ListAddItem(lst, 'Dog Bark');
  
  p2 := LoadPanel('lstBoxPanel2.txt');  
  ShowPanel(p2);
  PanelSetDraggable(p1, false);  
  PanelSetDraggable(p2, false);  
    
  repeat // The game loop...
    ProcessEvents();
    
    if RegionClickedId() = 'Button1' then
    begin
      if ListActiveItemIndex(lst) = 0 then PlayMusic('game.ogg');
      if ListActiveItemIndex(lst) = 1 then PlayMusic('diving-turtle.mp3');      
      if ListActiveItemIndex(lst) = 2 then PlayMusic('fast.mp3');            
      if ListActiveItemIndex(lst) = 3 then PlayMusic('gentle-thoughts-1.mp3');    
      if ListActiveItemIndex(lst) = 4 then PlayMusic('morning-workout.mp3');      
      if ListActiveItemIndex(lst) = 5 then PlayMusic('saber.ogg');
      if ListActiveItemIndex(lst) = 6 then PlayMusic('chipmunk.ogg');      
      if ListActiveItemIndex(lst) = 7 then PlayMusic('bells.ogg');
      if ListActiveItemIndex(lst) = 8 then PlayMusic('camera.ogg');
      if ListActiveItemIndex(lst) = 9 then PlayMusic('comedy_boing.ogg');
      if ListActiveItemIndex(lst) = 10 then PlayMusic('dinosaur.ogg');
      if ListActiveItemIndex(lst) = 11 then PlayMusic('dog_bark.ogg');
    end;
    if RegionClickedId() = 'Button2' then  if MusicPlaying() then PauseMusic();      
    if RegionClickedId() = 'Button3' then ResumeMusic();    
    if RegionClickedId() = 'Button4' then if MusicPlaying() then StopMusic();    
    
    DrawInterface();      
    UpdateInterface();
    
    RefreshScreen();        
  until WindowCloseRequested() or (RegionClickedId() = 'Button5');
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
    Main();
end.