program HowToCreateAnAnimation;
uses 
    SwinGame, sgTypes;

procedure Main();
var
    explosion: Sprite;
begin
    OpenAudio();
    OpenGraphicsWindow('Create Animation', 200, 200);

    LoadResourceBundle('explosion_bundle.txt');

    explosion := CreateSprite(BitmapNamed('explosionBmp'), AnimationScriptNamed('explosionScrpt'));

    SpriteStartAnimation(explosion, 'explosion_loop');

    SpriteSetX(explosion, 64);
    SpriteSetY(explosion, 64);

    repeat
        ClearScreen(ColorWhite);
        DrawSprite(explosion);
        RefreshScreen(60);

        UpdateSprite(explosion);

        ProcessEvents();
    until WindowCloseRequested();

    Delay(800);
    FreeSprite(explosion);
    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.