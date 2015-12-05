program HowToUseGameTimer;
uses 
    SwinGame, sgTypes, sysUtils;

procedure Main();
var
    gameTime: Timer;
    ticks: Integer;
    toDraw: String;
begin    
    OpenGraphicsWindow('Game Timer', 150, 150);
    gameTime := CreateTimer();
    StartTimer(gameTime);

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawText('[P]ause', ColorBlack, 0, 0);
        DrawText('[R]esume', ColorBlack, 0, 10);
        DrawText('[S]top', ColorBlack, 0, 20);
        DrawText('[B]egin', ColorBlack, 0, 30);
        
        if KeyTyped(PKey) then PauseTimer(gameTime);
        if KeyTyped(RKey) then ResumeTimer(gameTime);
        if KeyTyped(SKey) then StopTimer(gameTime);
        if KeyTyped(BKey) then StartTimer(gameTime);

        ticks := TimerTicks(gameTime);
        Str(ticks, toDraw);
        DrawText(toDraw, ColorRed, 20, 70);
        RefreshScreen();
    until WindowCloseRequested();

    ReleaseAllResources();
end;

begin
    Main();
end.