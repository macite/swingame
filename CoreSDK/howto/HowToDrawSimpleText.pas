program HowToDrawSimpleText;
uses 
    SwinGame, sgTypes;

procedure Main();
begin    
    OpenGraphicsWindow('Drawing Text', 800, 600);
    
    ClearScreen();

    DrawText('You Win!!!', ColorWhite, 300, 200);

    RefreshScreen();
    
    Delay(5000);

    ReleaseAllResources();    
end;

begin
    Main();
end.