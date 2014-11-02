program SpaceInvaders;
uses 
	sgGraphics, sgSprites, sgPhysics, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations, sgResources, sgText;
{	
const MAX_BULLETS = 10;			const ALIEN_BULLET_Y_DIFF = 24;
const DEMERIT_POINTS = 10;		const SHOOT_PERCENT = 0.05;
}
const PLAYER_LIFE = 3;			const PLAYER_X_POS = 300;
const PLAYER_Y_POS = 410;		const PLAYER_SPEED = 2;
const SPRITE_WIDTH = 40;		const BULLET_X_OFFSET = 12;
const BULLET_SPEED = 4;			const PLAYER_BULLET_Y_DIFF = 1;
const COLS = 10;				const ROWS = 5;
const ALIEN_DISTANCE = 40;		const SCREEN_EDGE = 5;
const ALIEN_SPEED = 1;
	
type
	MovementDirection = ( MoveLeft, MoveRight );
	
	Bullet = record
		bulletSprt: Sprite;
		inUse: Boolean;
	end;
		
	Player = record
		fighter: Sprite;
		playerBlt: Bullet;
		life: Integer;
	end;
	
	Alien = record
		alive: Boolean;
		alienSprt: Sprite;
	end;
		
	Fleet = record
		aliensArray: Array [0..COLS-1] of Array [0..ROWS-1] of Alien;
	//	bulletsArray: Array [0..MAX_BULLETS-1] of Bullet;
	end;
	
	SpaceInvadersData = record
		playerData: Player;
		fleetData: Fleet;
	//	score: Integer;
	end;

procedure SetupBullet(var b: Bullet; isPlayers: Boolean);
begin
	b.inUse := false;
	
	if isPlayers then
	begin
		b.bulletSprt := CreateSprite(BitmapNamed('PlayerBullet'));
		SpriteSetDy(b.bulletSprt, -BULLET_SPEED);
	end;
//	else
//	begin
//		b.bulletSprt := CreateSprite(BitmapNamed('AlienBullet'));
//		SpriteSetDy(b.bulletSprt, BULLET_SPEED);
//	end;
end;
	
procedure InitPlayer(var p: Player);
begin
	p.fighter := CreateSprite(BitmapNamed('Player'));
	SpriteSetX(p.fighter, PLAYER_X_POS);
	SpriteSetY(p.fighter, PLAYER_Y_POS);
	SetupBullet(p.playerBlt, true);
	
	p.life := PLAYER_LIFE;
end;

procedure PlaceAlien(var a: Alien; x, y: Integer);
begin
	SpriteSetX(a.alienSprt, x);
	SpriteSetY(a.alienSprt, y);
	SpriteStartAnimation(a.alienSprt, 'move');
end;

procedure MoveAlien(var a: Alien; dx: Single);
begin
	SpriteSetDX(a.alienSprt, dx);
end;

procedure SetupFleet(var f: Fleet);
var
	i, xPos, yPos, col, row: Integer;
begin
	xPos := ALIEN_DISTANCE;
	yPos := ALIEN_DISTANCE;
	
	for col := 0 to COLS-1 do
	begin
		for row := 0 to ROWS - 1 do
		begin
			f.aliensArray[col, row].alienSprt := CreateSprite(BitmapNamed('Invader1'), AnimationScriptNamed('InvaderAnim'));
			PlaceAlien(f.aliensArray[col, row], xPos, yPos);
			MoveAlien(f.aliensArray[col, row], ALIEN_SPEED);
			f.aliensArray[col, row].alive := true;
			yPos += ALIEN_DISTANCE;
		end;
		yPos := ALIEN_DISTANCE;
		xPos += ALIEN_DISTANCE;
	end;
	
//	for i := 0 to MAX_BULLETS-1 do
//	begin
//		SetupBullet(f.bulletsArray[i], false);
//	end;
end;
	
procedure InitGame(var data: SpaceInvadersData);
begin
	//data.score := 0;
	InitPlayer(data.playerData);
	SetupFleet(data.fleetData);
end;

procedure FireBullet(var b: Bullet; s: Sprite; isPlayer: Boolean);
begin
	b.inUse := true;
	SpriteSetX(b.bulletSprt, SpriteX(s) + BULLET_X_OFFSET);
	
	if isPlayer then SpriteSetY(b.bulletSprt, SpriteY(s) - PLAYER_BULLET_Y_DIFF);
//	else SpriteSetY(b.bulletSprt, SpriteY(s) + ALIEN_BULLET_Y_DIFF);
end;

procedure HandleInput(var p: Player);
begin
	if KeyDown(VK_RIGHT) then
	begin
		if SpriteX(p.fighter) < ScreenWidth() - SPRITE_WIDTH then 
			SpriteSetDx(p.fighter, PLAYER_SPEED);
	end
	else if KeyDown(VK_LEFT) then
	begin
		if SpriteX(p.fighter) > 0 then
			SpriteSetDx(p.fighter, -PLAYER_SPEED);
	end;

	if KeyTyped(VK_SPACE) then
	begin
		FireBullet(p.playerBlt, p.fighter, true);
	end;
end;

procedure UpdatePlayerAndBullet(var p: Player);
begin
	UpdateSprite(p.fighter);
	if p.playerBlt.inUse then
	begin
		UpdateSprite(p.playerBlt.bulletSprt);
	end;
	
	// Reset movement, so keys have to be held down
	SpriteSetDx(p.fighter, 0);
end;

procedure DrawPlayerAndBullet(var p: Player);
begin
	DrawSprite(p.fighter);
	if p.playerBlt.inUse then
	begin
		DrawSprite(p.playerBlt.bulletSprt);
	end;
end;

procedure NewAlienDirection(var f: Fleet; direction: MovementDirection);
var
	col, row: Integer;
begin
	for col := 0 to COLS - 1 do
	begin
		for row := 0 to ROWS - 1 do
		begin
			if direction = MoveRight then
				MoveAlien(f.aliensArray[col, row], ALIEN_SPEED)
			else if direction = MoveLeft then
				MoveAlien(f.aliensArray[col, row], -ALIEN_SPEED);
		end;
	end;
end;

procedure CheckAlienDirection(var f: Fleet);
var 
	col, row: Integer;
	foundLeftMost, foundRightMost: Boolean;
begin
	foundLeftMost := false;
	foundRightMost := false;
	
	for col := 0 to COLS - 1 do
	begin
		for row := 0 to ROWS - 1 do
		begin
			//check from left -> right
			if f.aliensArray[col, row].alive then
			begin
				foundLeftMost := True;
				if SpriteX(f.aliensArray[col, row].alienSprt) < SCREEN_EDGE then
				begin
					NewAlienDirection(f, MoveRight);
					exit;
				end;
			end;
			
			//check from right -> left
			if f.aliensArray[COLS - (col + 1), row].alive then
			begin
				foundRightMost := True;
				if SpriteX(f.aliensArray[COLS - (col + 1), row].alienSprt) > ScreenWidth() - (SPRITE_WIDTH + SCREEN_EDGE) then
				begin
					NewAlienDirection(f, MoveLeft);
					exit;
				end;
			end;
			
			if foundRightMost and foundLeftMost then exit;
		end;
	end;
end;

procedure DrawAndUpdateFleet(var f: Fleet);
var
	col, row: Integer;
begin
	for col := 0 to COLS - 1 do
	begin
		for row := 0 to ROWS - 1 do
		begin
			if f.aliensArray[col, row].alive then
			begin
				DrawSprite(f.aliensArray[col, row].alienSprt);
				UpdateSprite(f.aliensArray[col, row].alienSprt);
			end;
		end;
	end;	
end;
{
procedure HandleCollision(var data: SpaceInvadersData);
var
	i, col, row: Integer;
begin
	//destroy the alien
	for col := 0 to COLS - 1 do
	begin
		for row := 0 to ROWS - 1 do
		begin
			if data.fleetData.aliensArray[col, row].alive and
			data.playerData.playerBlt.inUse and 
			SpriteCollision(data.fleetData.aliensArray[col, row].alienSprt, data.playerData.playerBlt.bulletSprt) then
			begin
				data.fleetData.aliensArray[col, row].alive := false;
				data.playerData.playerBlt.inUse := false;
				data.score += 1;
			end;
		end;
	end;
	
	//destroy the player
	for i := Low(data.fleetData.bulletsArray) to High(data.fleetData.bulletsArray) do
	begin
		if SpriteCollision(data.fleetData.bulletsArray[i].bulletSprt, data.playerData.fighter) and
		data.fleetData.bulletsArray[i].inUse then
		begin
			SpriteSetX(data.playerData.fighter, PLAYER_X_POS);
			data.playerData.life -= 1;
			data.fleetData.bulletsArray[i].inUse := false;
			data.score -= DEMERIT_POINTS;
		end;
	end;
end;

procedure AlienShoot(var data: SpaceInvadersData);
var
	col, row, i: Integer;
begin
	for i := Low(data.fleetData.bulletsArray) to High(data.fleetData.bulletsArray) do
	begin
		if not data.fleetData.bulletsArray[i].inUse then 
		begin
			break;
		end;	//found a free spot f!
	end;
  
	if (i > High(data.fleetData.bulletsArray)) or data.fleetData.bulletsArray[i].inUse then
	begin
		exit; //none found...
	end;
  
	//choose a column to shoot
	col := Rnd(COLS);

	for row := High(data.fleetData.aliensArray[col]) downto Low(data.fleetData.aliensArray[col]) do
	begin
		if data.fleetData.aliensArray[col][row].alive then break; //we have a shooter...
	end;
  
	if (row >= Low(data.fleetData.aliensArray[col])) and data.fleetData.aliensArray[col][row].alive then //we found one...
	begin
		FireBullet(data.fleetData.bulletsArray[i], data.fleetData.aliensArray[col][row].alienSprt, false);
    end;
end;

procedure UpdateAlienBullet(var data: SpaceInvadersData);
var
	i: Integer;
begin
	for i := Low(data.fleetData.bulletsArray) to High(data.fleetData.bulletsArray) do
	begin
		if data.fleetData.bulletsArray[i].inUse then
		begin
			DrawSprite(data.fleetData.bulletsArray[i].bulletSprt);
			UpdateSprite(data.fleetData.bulletsArray[i].bulletSprt);
		end;
		
		if SpriteOffscreen(data.fleetData.bulletsArray[i].bulletSprt) then
			data.fleetData.bulletsArray[i].inUse := false;
	end;
end;

function AliensAreDestroyed(f: Fleet): Boolean;
var
	col, row: Integer;
begin
	for col := 0 to COLS - 1 do
	begin
		for row := 0 to ROWS - 1 do
		begin
			if f.aliensArray[col, row].alive then
			begin
				result := false;
				exit;
			end;
		end;
	end;
	
	result := true;
end;
}
procedure UpdateGame(var data: SpaceInvadersData);
begin
	UpdatePlayerAndBullet(data.playerData);
	DrawAndUpdateFleet(data.fleetData);
	CheckAlienDirection(data.fleetData);
//	UpdateAlienBullet(data);
	
//	if Rnd() < SHOOT_PERCENT then
//		AlienShoot(data);
	
	RefreshScreen(60);
end;

procedure DrawGame(var data: SpaceInvadersData);
begin
	ClearScreen();
	DrawBitmap(BitmapNamed('Background'), 0, 0);
	DrawFramerate(10,8);
	DrawPlayerAndBullet(data.playerData);	
end;
	
procedure Main();
var
	gameData: SpaceInvadersData;
begin
	OpenAudio();
    OpenGraphicsWindow('Space Invaders', 640, 480);
	LoadResourceBundle('SpaceInvaders.txt');

	InitGame(gameData);
	repeat
		DrawGame(gameData);
		UpdateGame(gameData);
		ProcessEvents();
		HandleInput(gameData.playerData);
//		HandleCollision(gameData);
	until WindowCloseRequested() // or (gameData.playerData.life = 0) or AliensAreDestroyed(gameData.fleetData);
{	
	if WindowCloseRequested() then WriteLn('YOU GAVE UP!!! Seriously?')
	else if (gameData.playerData.life = 0) then WriteLn('Your best just was not good enough to save the earth...')
	else WriteLn('CONGRATULATIONS!!! You have just saved the earth...');
	
	WriteLn('Your Score: ', gameData.score);
}
end;

begin
	Main();
end.