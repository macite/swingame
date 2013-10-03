program HowToCreateAnAnimation;
uses sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations;

procedure Main();
var
	walking: Sprite;
	fish: Bitmap;
begin
    OpenAudio();
	OpenGraphicsWindow('Walking Frog', 800, 600);

	fish := LoadBitmapNamed('walking', 'frog.png');
    BitmapSetCellDetails(fish, 32, 48, 4, 4, 16);
    LoadAnimationScriptNamed('WalkingScrpt', 'kermit.txt');
	
	walking := CreateSprite(fish, AnimationScriptNamed('WalkingScrpt'));
	SpriteStartAnimation(walking, 'WalkFront');
	
	SpriteSetX(walking, 400);
    SpriteSetY(walking, 300);
	
	repeat
		ClearScreen(ColorWhite);
		DrawSprite(walking);
		RefreshScreen(60);
		
		UpdateSprite(walking);
		
		ProcessEvents();
	until WindowCloseRequested();
	
	Delay(800);
end;

begin
	Main();
end.