program TestText;
uses
  SwinGame;

procedure Main();
begin
  OpenGraphicsWindow('Test Text', 640, 480);

  LoadFontNamed('arial', 'arial.ttf', 32);
  //LoadFontNamed(FontNameFor('arial.ttf', 10), 'arial.ttf', 10);
  
  repeat // The game loop...
    ProcessEvents();
    ClearScreen(ColorWhite);

    DrawFramerate(0,0);    
    DrawText('Hello World', ColorBlack, 'arial', 70, 70);
    //DrawText('Hello World', ColorBlack, FontNamed('arial'), 70, 70);

    RefreshScreen(60);
  until WindowCloseRequested();  
end;

begin
  Main();
end.
