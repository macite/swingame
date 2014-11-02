program HowToDrawAnAnimation;
uses 
    SwinGame, sgTypes;

procedure Main();
var
    explosion: Animation;
begin
    OpenAudio();
    OpenGraphicsWindow('Draw Animation', 200, 200);
    
    LoadResourceBundle('explosion_bundle.txt');

    explosion := CreateAnimation('explosion_loop', AnimationScriptNamed('explosionScrpt'));
    
    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawAnimation(explosion, BitmapNamed('explosionBmp'), 64, 64);
        UpdateAnimation(explosion);
        RefreshScreen(60);
    until WindowCloseRequested();

    Delay(800);

    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.