program HowToPlayMusic;
uses
    SwinGame;    

procedure Main();
begin
    OpenAudio();

    OpenGraphicsWindow('How To Play Music', 320, 240);

    LoadDefaultColors();

    ClearScreen(ColorWhite);
    LoadMusic('diving-turtle.mp3');		
    PlayMusic('diving-turtle.mp3'); 
    DrawText('How To Play Music!!!', ColorRed, 40, 120);  

  RefreshScreen();
  
  Delay(5000);  
  CloseAudio();
  ReleaseAllResources();  
end;

begin
    Main();
end.
