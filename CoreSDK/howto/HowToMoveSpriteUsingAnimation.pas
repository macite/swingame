program HowToMoveSpriteUsingAnimation;
uses
    SwinGame, sgTypes;

procedure DoWalking(sprt: Sprite; key: KeyCode;const animationName: String; dx, dy: Single);
begin
    if KeyDown(key) then
    begin
        SpriteStartAnimation(sprt, animationName);
        SpriteSetDX(sprt, dx);
        SpriteSetDY(sprt, dy);      
    end;
end;

procedure Main();
var
    myFrog: Sprite;
begin
    OpenAudio();
    OpenGraphicsWindow('Moving Sprite Using Animation', 800, 600);

    LoadResourceBundle('dance_bundle.txt');

    myFrog := CreateSprite(BitmapNamed('FrogBmp'), AnimationScriptNamed('WalkingScript'));
    SpriteStartAnimation(myFrog, 'StandFront');

    SpriteSetX(myFrog, 382);
    SpriteSetY(myFrog, 274);

    repeat
        ClearScreen(ColorWhite);
        DrawSprite(myFrog);
        RefreshScreen(60);
        UpdateSprite(myFrog);

        ProcessEvents();
        if SpriteAnimationHasEnded(myFrog) then
        begin
            SpriteSetDX(myFrog, 0);
            SpriteSetDY(myFrog, 0);
            
            DoWalking(myFrog, UPKey, 'WalkBack', 0, -0.25);
            DoWalking(myFrog, DOWNKey, 'WalkFront', 0, +0.25);
            DoWalking(myFrog, LEFTKey, 'WalkLeft',  -0.25, 0);
            DoWalking(myFrog, RIGHTKey, 'WalkRight',  +0.25, 0);

            DoWalking(myFrog, WKey, 'MoonWalkBack', 0, -0.25);
            DoWalking(myFrog, SKey, 'MoonWalkFront', 0, +0.25);
            DoWalking(myFrog, AKey, 'MoonWalkLeft', -0.25, 0);
            DoWalking(myFrog, DKey, 'MoonWalkRight', +0.25, 0);
        end;
    until WindowCloseRequested();
    FreeSprite(myFrog);
    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.