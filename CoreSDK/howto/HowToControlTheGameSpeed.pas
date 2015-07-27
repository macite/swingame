program HowToControlTheGameSpeed;
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
    gameTime: Timer;
    ticks, lastUpdate: Integer;
    fraction: single;
begin
    OpenAudio();
    OpenGraphicsWindow('Controlling Gamespeed', 800, 600);

    LoadResourceBundle('dance_bundle.txt');
    
    myFrog := CreateSprite(BitmapNamed('FrogBmp'), AnimationScriptNamed('WalkingScript'));
    SpriteStartAnimation(myFrog, 'StandFront');

    SpriteSetX(myFrog, 382);
    SpriteSetY(myFrog, 274);
    
    gameTime := CreateTimer();
    StartTimer(gameTime);
    fraction := 0.0;
    lastUpdate := 0;

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawSprite(myFrog);
        RefreshScreen();
        UpdateSprite(myFrog, fraction);
        
        ticks := TimerTicks(gameTime);
        fraction := (ticks - lastUpdate) / 10;
        lastUpdate := ticks;

        if SpriteAnimationHasEnded(myFrog) then
        begin
            SpriteSetDX(myFrog, 0);
            SpriteSetDY(myFrog, 0);
            
            DoWalking(myFrog, VK_UP, 'WalkBack', 0, -0.25);
            DoWalking(myFrog, VK_DOWN,  'WalkFront', 0, +0.25);
            DoWalking(myFrog, VK_LEFT,  'WalkLeft',  -0.25, 0);
            DoWalking(myFrog, VK_RIGHT, 'WalkRight',  +0.25, 0);

            DoWalking(myFrog, VK_W,      'MoonWalkBack',  0, -0.25);
            DoWalking(myFrog, VK_S,  'MoonWalkFront', 0, +0.25);
            DoWalking(myFrog, VK_A,  'MoonWalkLeft',  -0.25, 0);
            DoWalking(myFrog, VK_D, 'MoonWalkRight',  +0.25, 0);
        end;
    until WindowCloseRequested();
    FreeSprite(myFrog);
    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.