program HowToCreateMultipleAnimationsFromOneScript;
uses
    SwinGame, sgTypes;

procedure Main();
var
    explosion: Sprite;
begin
    OpenAudio();
    OpenGraphicsWindow('Create Multiple Animations', 200, 200);

    LoadResourceBundle('explosion_bundle.txt');

    explosion := CreateSprite(BitmapNamed('explosionBmp'), AnimationScriptNamed('explosionScrpt'));

    SpriteSetX(explosion, 64);
    SpriteSetY(explosion, 64);

    repeat
        ClearScreen(ColorWhite);
        DrawText('[E]xplosion', ColorBlack, 0, 0);
        DrawText('[I]mplosion', ColorBlack, 0, 10);
        DrawText('[L]oop', ColorBlack, 0, 20);
        DrawSprite(explosion);
        RefreshScreen(60);

        UpdateSprite(explosion);

        ProcessEvents();
        if KeyTyped(EKey) then SpriteStartAnimation(explosion, 'explosion');
        if KeyTyped(IKey) then SpriteStartAnimation(explosion, 'implosion');
        if KeyTyped(LKey) then SpriteStartAnimation(explosion, 'explosion_loop');

    until WindowCloseRequested();
    FreeSprite(explosion);
    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.