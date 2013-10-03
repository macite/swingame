program HowToUseAnimationWithMultipleSprites;
uses
    SwinGame, sgTypes;

procedure Main();
var
    myFrog, myLizard: Sprite;
begin
    OpenAudio();
    OpenGraphicsWindow('Dance', 800, 600);

    LoadResourceBundle('dance_bundle.txt');
    myFrog := CreateSprite(BitmapNamed('FrogBmp'), AnimationScriptNamed('WalkingScript'));
    SpriteStartAnimation(myFrog, 'Dance');

    SpriteSetX(myFrog, 496);
    SpriteSetY(myFrog, 250);

    myLizard := CreateSprite(BitmapNamed('LizardBmp'), AnimationScriptNamed('WalkingScript'));
    SpriteStartAnimation(myLizard, 'Dance');

    SpriteSetX(myLizard, 238);
    SpriteSetY(myLizard, 272);


    repeat
        ClearScreen(ColorWhite);
        DrawSprite(myFrog);
        DrawSprite(myLizard);

        RefreshScreen(60);
        UpdateSprite(myFrog);
        UpdateSprite(myLizard);

        ProcessEvents();
    until WindowCloseRequested();
    FreeSprite(myLizard);
    FreeSprite(myFrog);
    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.