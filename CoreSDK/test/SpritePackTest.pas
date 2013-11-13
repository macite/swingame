program SpritePackTest;
uses SwinGame, sgTypes;

procedure HandleSpriteEvents(s: Sprite; kind: SpriteEventKind);
begin
	case kind of
		SpriteArrivedEvent: SpriteMoveTo(s, RandomScreenPoint(), 5);
	end;
end;

procedure CreateNewBall();
var
    ball: Sprite;
begin
     ball := CreateSprite(BitmapNamed('ball'));
    SpriteSetPosition(ball, RandomScreenPoint());
    SpriteMoveTo(ball, RandomScreenPoint(), 5);
end;

procedure Main();
begin
	OpenGraphicsWindow('SpritePackTest');
	LoadDefaultColors();

	LoadBitmapNamed('ball', 'ball_small.png');

	CallOnSpriteEvent(@HandleSpriteEvents);

	CreateSpritePack('Other');

	CreateNewBall();

    repeat
    	ProcessEvents();

    	if KeyTyped(vk_Space) then CreateNewBall();
    	if KeyTyped(vk_1) then SelectSpritePack('Default');
    	if KeyTyped(vk_2) then SelectSpritePack('Other');
    	if KeyTyped(vk_3) then SelectSpritePack('Non Existant');

    	ClearScreen(ColorWhite);
    	DrawText('Pack: ' + CurrentSpritePack(), ColorBlack, 0, 0);
    	DrawFramerate(0, 20);

	    DrawAllSprites();
	    UpdateAllSprites();

	    RefreshScreen();    	
    until WindowCloseRequested();
    
    ReleaseAllResources();
end;

begin
	Main();
end.
