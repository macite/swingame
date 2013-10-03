program PongCentipede;
uses
	SwinGame, sgTypes, SysUtils, StrUtils;

const
	PADDLE_HEIGHT_RATIO = 5;
	PADDLE_WIDTH_RATIO	= 35;
	BALL_SPEED					= 10;
	MOVE_SPEED					= 5;
	BALL_PREFIX					= 'BallData:';
	DEL						= ',';

type
	Ball = record
		circ : Circle;
		v 	 : Vector;
		active : Boolean;
	end;
	
	//Players = record
	
	Player = record
		pos : Integer;
		paddle : Rectangle;
		con	: Connection;
		receivedUpdate : Boolean;
	end;
	

	Players = Array of Player;
	Balls = Array [0..7] of Ball;

//----------------------------------------------------------------------------
// GUI
//----------------------------------------------------------------------------
	
procedure InitPanel(var aMenuPanel : Panel);
begin
  GUISetForegroundColor(ColorBlack);
  GUISetBackgroundColor(ColorWhite);
  ShowPanel(aMenuPanel);
  ActivatePanel(aMenuPanel);
  DrawGUIAsVectors(True);
end;

procedure SetLabelText();
begin
  LabelSetText('MyIPLbl', 'My IP:');
  LabelSetText('MyIPVal', MyIP());
  LabelSetText('MyPorttLbl', 'Port: ');
  LabelSetText('HostStatusLbl', 'Status: ');

  LabelSetText('ServerLbl', 'Server:');
  LabelSetText('ServerPorttLbl', 'Port: ');
  LabelSetText('ClientStatusLbl', 'Status: ');
  LabelSetText('ServerVal', '127.0.0.1');
end;

//----------------------------------------------------------------------------
// Establishing Connection
//----------------------------------------------------------------------------

function CreateHost() : LongInt;
var
  lPort : Integer;
begin
	result := -1;
  if TryStrToInt(TextBoxText('MyPortVal'), lPort) then
  begin
    result := CreateUDPHost(lPort);
    LabelSetText('HostStatusVal', 'Listening...');
    LabelSetText('HostLbl', 'Disconnect');
  end;
end;

function ConnectToHost() : Connection;
var
  lPort : Integer;
begin
	result := nil;
  if TryStrToInt(TextBoxText('ServerPortVal'), lPort) then
  begin
		result := CreateUDPConnection(TextBoxText('ServerVal'), lPort, 0); 
		SendUDPMessage('AcceptMe:', result);
    if Assigned(result) then
    begin
      LabelSetText('ClientStatusVal', 'Connected');
      LabelSetText('ConnLbl', 'Disconnect');
    end;
  end;
end;

function HandleGUIInput( var aConnected, aIsHost : Boolean) : Connection;
begin
  //Server Connect and Disconnect
	result := nil;
  if (RegionClickedID() = 'HostBtn') then
	begin
		aConnected 			:= CreateHost() <> -1;
		aIsHost 				:= aConnected;
	end
  //Client Connect and Disconnect
  else if (RegionClickedID() = 'ConnectBtn') then
	begin
    result 			:= ConnectToHost();
		if not Assigned(result) then exit;
		aConnected 	:= True;
		aIsHost 		:= False;
	end;
end;

function SetupConnection(var aHostConnection : Connection) : Boolean;
var
	lHostPanel, lClientPanel : Panel;
	lConnectionIsSet : Boolean = False;
begin
  LoadResourceBundle('MainMenu.txt');
	lHostPanel := LoadPanel('hostpanel.txt');
	lClientPanel := LoadPanel('clientpanel.txt');
	InitPanel(lHostPanel);
	InitPanel(lClientPanel);
	SetLabelText();
	
	repeat
		ProcessEvents();
		ClearScreen();
		aHostConnection := HandleGUIInput(lConnectionIsSet, result);
		DrawInterface();
		UpdateInterface();
		RefreshScreen(60);
	until WindowCloseRequested() or lConnectionIsSet;
	HidePanel(lHostPanel);
	HidePanel(lClientPanel);
end;

//----------------------------------------------------------------------------
// Init Game Code
//----------------------------------------------------------------------------

procedure InitPlayer(var aPlayer : Player; aSide : char; aConnection : Connection);
begin	
	aPlayer.paddle.height := ScreenHeight div PADDLE_HEIGHT_RATIO;
	aPlayer.paddle.width  := ScreenWidth  div PADDLE_WIDTH_RATIO;
	aPlayer.paddle.y 	   	:= Round((ScreenHeight div 2) - (aPlayer.paddle.height div 2)); 
	aPlayer.con		 				:= aConnection;
	
	case aSide of
		'l' : aPlayer.paddle.x := Round(aPlayer.paddle.width * 3); 
		'r' : aPlayer.paddle.x := Round(ScreenWidth - aPlayer.paddle.width * 2); 
		'm'	: aPlayer.paddle.x := Round((ScreenWidth div 2) - (aPlayer.paddle.width div 2)); 
	end;
end;

procedure InitBalls(var aBalls : Balls);
var
	i : Integer;
begin
	for i:= Low(aBalls) to High(aBalls) do
	begin
		aBalls[i].circ.center.x := Round(ScreenWidth div 2);
		aBalls[i].circ.center.y := Round(ScreenHeight div 2);
		aBalls[i].circ.radius	 := (ScreenWidth  div PADDLE_WIDTH_RATIO) div 2;
		aBalls[i].v.x := BALL_SPEED;
		aBalls[i].v.y := BALL_SPEED;
	end;
end;

procedure InitHost(var aPlayers : Players; var aBalls : Balls);
begin
	SetLength(aPlayers, 1);
	InitPlayer(aPlayers[0], 'l', nil);
	InitBalls(aBalls);
end;

function BallToString(const aBall : Ball) : String;
begin
	result := IntToStr(Round(aBall.circ.center.x)) + DEL + 
						IntToStr(Round(aBall.circ.center.y));
end;

procedure AddPlayer(const aConnection : Connection; var aPlayers : Players; out aPlayerCount : Integer);
var
	i : Integer;
begin
	SetLength(aPlayers, Length(aPlayers) + 1);
	InitPlayer(aPlayers[High(aPlayers)], 'r', aConnection);
	aPlayerCount := Length(aPlayers);
	for i := 1 to High(aPlayers) do
		aPlayers[i].receivedUpdate := False;
end;

//----------------------------------------------------------------------------
// Ball Update
//----------------------------------------------------------------------------

procedure CollideCircleLine(var aBall : Ball; const line: LineSegment);
var
	npx, npy, dotPod: Single;
	toLine: Vector;
	intersect: Point2D;
begin
	//TODO: fix collision pt.... cast back along velocity...
	intersect := ClosestPointOnLine(CenterPoint(aBall.circ), line);
	
	toLine := UnitVector(VectorFromPoints(CenterPoint(aBall.circ), intersect));
	// Project velocity across to-line
	dotPod := - DotProduct(toLine, aBall.v);
	
	npx := dotPod * toLine.x;
	npy := dotPod * toLine.y;
	
	aBall.v.x := aBall.v.x + 2 * npx;
	aBall.v.y := aBall.v.y + 2 * npy;
end;

procedure CollideCircleRectangle(var aBall : Ball; const rect: Rectangle; bounds: Boolean); overload;
var
	hitIdx: Longint;
	lines: LinesArray;
	outVec, mvmt: Vector;
begin
	mvmt := aBall.v;
	hitIdx := -1;
	
	// Get the line hit...
	lines := LinesFrom(rect);
	outVec := VectorOverLinesFromCircle(aBall.circ, lines, mvmt, hitIdx);
		
	// back out of rectangle
	aBall.circ.center.x += outVec.x; //aBall.circ.center.x + (pct * mvmt.x);
   aBall.circ.center.y += outVec.y;// aBall.circ.center.y + (pct * mvmt.y);
//	MoveSprite(aBall, outVec, 1.0);
	
	// bounce...
	CollideCircleLine(aBall, lines[hitIdx]);
end;

procedure KeepBallOnScreen(var aBall : Ball; aGameWidth, aGameHeight : Integer);
begin
	if ((aBall.circ.center.x - aBall.circ.radius) <= 0) then
	begin
		aBall.v.x := -aBall.v.x;
		aBall.circ.center.x := aBall.circ.radius;
	end else if ((aBall.circ.center.x + aBall.circ.radius) >= aGameWidth) then
	begin
		aBall.v.x := -aBall.v.x;
		aBall.circ.center.x := aGameWidth - aBall.circ.radius;
	end;
	if ((aBall.circ.center.y - aBall.circ.radius) <= 0) then
	begin
	 	aBall.v.y := -aBall.v.y;
		aBall.circ.center.y := aBall.circ.radius;
	end else if ((aBall.circ.center.y + aBall.circ.radius) >= aGameHeight) then
	begin
		aBall.v.y := -aBall.v.y;
		aBall.circ.center.y := aGameHeight - aBall.circ.radius;
	end;
end;

procedure CollideCircleCircle(var aBallA : Ball; const aBallB: Ball);
var
  hitLine: LineSegment;
  outVec, mvmt, normal, colVec: Vector;
  mvmtMag, prop: Single;
  spriteCenter, hitPt: Point2D;
begin
  //TODO: what if height > width!!
  spriteCenter := CenterPoint(aBallA.circ);
  mvmt := aBallA.v;
  
  outVec := VectorOutOfCircleFromCircle(aBallA.circ, aBallB.circ, mvmt);
  // Back out of circle
	aBallA.circ.center.x += outVec.x; //aBall.circ.center.x + (pct * mvmt.x);
  aBallA.circ.center.y += outVec.y;// aBall.circ.center.y + (pct * mvmt.y);
  
  // Normal of the collision...
  colVec := UnitVector(VectorFromPoints(aBallB.circ.center, spriteCenter));
  normal := VectorNormal(colVec);
  
  hitPt := AddVectors(aBallB.circ.center, VectorMultiply(colVec, Single(aBallB.circ.radius + 1.42)));
  hitLine := LineFromVector(AddVectors(hitPt, VectorMultiply(normal, 100)), VectorMultiply(normal, -200));
    
  CollideCircleLine(aBallA, hitLine);

  // do part velocity
//  mvmtMag := VectorMagnitude(mvmt);
//  prop := VectorMagnitude(outVec) / mvmtMag; //proportion of move "undone" by back out
//  if prop > 0 then MoveSprite(s, prop); //TODO: Allow proportion of move to be passed in (overload)... then do velocity based on prop * pct
end;

procedure UpdateBall(var aBall : Ball; var aBalls: Balls; const aPlayers : Players; const aGameWidth, aGameHeight, aIdx : Integer);
var
	i, j : Integer;
begin
	KeepBallOnScreen(aBall, aGameWidth, aGameHeight);

	for i := Low(aBalls) to Length(aPlayers) - 1 do
	begin
		if (i = aIdx) then continue;
		if (CircleCircleCollision(aBall.circ, aBalls[i].circ)) then
			CollideCircleCircle(aBall, aBalls[i]);
	end;

	for i := Low(aPlayers) to High(aPlayers) do
	begin
		if CircleRectCollision(aBall.circ, aPlayers[i].paddle) then
			CollideCircleRectangle(aBall, aPlayers[i].paddle, FALSE);
	end;
	aBall.circ.center.x += aBall.v.x;
	aBall.circ.center.y += aBall.v.y;
end;


//----------------------------------------------------------------------------
// Draw
//----------------------------------------------------------------------------

function ColorMap(aIndex : Integer) : Color;
begin
	result := 0;
	case aIndex of
		0: result := ColorWhite;
		1: result := ColorRed;
		2: result := ColorGreen;
		3: result := ColorBlue;
		4: result := ColorYellow;
		5: result := ColorTurquoise;
		6: result := ColorMagenta;
	end;
end;


procedure Draw(const aPlayer1 : Player; const aBalls : Balls; const aPlayerCount, aMyIdx: Integer);
var
	i : Integer;
	lCenterX, lCenterY : Single;
begin
	lCenterX := aPlayer1.paddle.x + (aPlayer1.paddle.width div 2);
	lCenterY := aPlayer1.paddle.y + (aPlayer1.paddle.height div 2);
	FillRectangle(ColorMap(aMyIdx - 1), aPlayer1.paddle);
	for i := Low(aBalls) to aPlayerCount - 1 do
	begin
		FillCircle(ColorMap(i), aBalls[i].circ);
		DrawLine(ColorMap(i), lCenterX, lCenterY, aBalls[i].circ.center.x, aBalls[i].circ.center.y);
	end;
end;

//----------------------------------------------------------------------------
// Ball Update
//----------------------------------------------------------------------------

procedure MovePlayer(var aPlayer : Player; const aIsHost : Boolean);
const
	PLAYER_PREFIX = 'PlayerData:';
var
	lMousePosY : Integer;
	lRectCenter : Integer;
begin
	if not MouseDown(LeftButton) then exit;
	lMousePosY := Round(MouseY());
	lRectCenter := Round(aPlayer.paddle.y + (aPlayer.paddle.height div 2));
	if (lMousePosY < lRectCenter) and (aPlayer.paddle.y > 0) then
	begin
		if (lRectCenter - lMousePosY) < MOVE_SPEED then
			aPlayer.paddle.y := lMousePosY - (aPlayer.paddle.height div 2)
		else
			aPlayer.paddle.y -= MOVE_SPEED;
		if not aIsHost then SendUDPMessage(PLAYER_PREFIX + IntToStr(Round(aPlayer.paddle.y)), aPlayer.con)
	end else if (lMousePosY > lRectCenter) and ((aPlayer.paddle.y + aPlayer.paddle.height) < ScreenHeight) then
	begin
		if (lMousePosY - lRectCenter) < MOVE_SPEED then
			aPlayer.paddle.y := lMousePosY - (aPlayer.paddle.height div 2)
		else
			aPlayer.paddle.y += MOVE_SPEED;
		if not aIsHost then SendUDPMessage(PLAYER_PREFIX + IntToStr(Round(aPlayer.paddle.y)), aPlayer.con);
	end;
end;

procedure SetPlayerPosition(var aPlayer : Player; const aMyIdx, aPlayerCount, aGameWidth : Integer);
begin	
	if (aMyIDx = aPlayerCount) then
		aPlayer.paddle.x :=(aGameWidth * aPlayerCount) - Round(aPlayer.paddle.width * 3)
	else
		aPlayer.paddle.x :=(aGameWidth * aMyIdx) - Round(aPlayer.paddle.width div 2) - (aGameWidth div 2);
end;

procedure ObtainGameData(var aPlayer : Player; var aBalls : Balls; var aMyIdx : Integer; var aGameWidth, aGameHeight, aPlayerCount : Integer; var aConnected : Boolean);
var
	lGameSet 		: Boolean = False;
	lBallSet	 	: Boolean = False;
	lMessageID, lData : String;	
	i						: Integer;
	lMessageReceived : Boolean = False;
const
	NUM_OF_PACKET_CHECKS = 20;
begin
		for i := 0 to NUM_OF_PACKET_CHECKS do
		begin
			if UDPMessageReceived() then
				lMessageReceived := True
			else
				break;
		end;
		if (lMessageReceived) then
		begin
			aConnected := True;
			lMessageID := ReadLastMessage(aPlayer.con);
			ClearMessageQueue(aPlayer.con);
			if (ExtractDelimited(1, lMessageID, [':']) = 'GameData') then
			begin
				lData 			:= ExtractDelimited(2, lMessageID, 		 [':']);
				aGameWidth 	:= StrToInt(ExtractDelimited(1, lData, [',']));
				aGameHeight := StrToInt(ExtractDelimited(2, lData, [',']));
				aMyIdx 			:= StrToInt(ExtractDelimited(3, lData, [',']));
				//aPlayer.paddle.x := StrToInt(ExtractDelimited(4, lData, [',']));
				SetCameraX(aGameWidth * (aMyIDx -1));
				aPlayerCount := StrToInt(ExtractDelimited(4, lData, [',']));
				SetPlayerPosition(aPlayer, aMyIdx, aPlayerCount, aGameWidth);
				for i := Low(aBalls) to High(aBalls) do
				begin
					aBalls[i].circ.center.x 	:= StrToInt(ExtractDelimited(5 + i * 2, lData, [',']));
					aBalls[i].circ.center.y 	:= StrToInt(ExtractDelimited(6 + i * 2, lData, [',']));
				end;
			end;
	end;
end;

procedure UpdateClientPosition(var aPlayers : Players);
var
	i : Integer;
	lMsg : String;
begin
	for i := 1 to High(aPlayers) do
	begin
		if MessageCount(aPlayers[i].con) > 0 then
		begin
			lMsg := ReadMessage(aPlayers[i].con);
			WriteLn(lMsg);
			if (ExtractDelimited(1, lMsg, [':']) = 'PlayerData') then
				aPlayers[i].paddle.y 	:= StrToInt(ExtractDelimited(2, lMsg, [':']));
			if (ExtractDelimited(1, lMsg, [':']) = 'Received') then
			begin
				WriteLn('Received');
				aPlayers[i].receivedUpdate := True;
			end;
		end;
	end;
end;

procedure BroadcastGameData(var aPlayers : Players; aGameWidth, aGameHeight : Integer; const aBalls : Balls);
var
	i : Integer;
	lMsg : String;
	lGameData, lBallData : String;
const
	GAME_PREFIX = 'GameData:';
	NEW_SIDE			= 'RIGHT';
begin
	lGameData := GAME_PREFIX + IntToStr(aGameWidth) + DEL + IntToStr(aGameHeight) + DEL;
	lBallData := '';
	for i := Low(aBalls) to High(aBalls) do
		lBallData += DEL + BallToString(aBalls[i]);

	for i := 1 to High(aPlayers) do
	begin
		lMsg := lGameData + IntToStr(i + 1);
		lMsg := lMsg + DEL + IntToStr(Length(aPlayers)) + lBallData;
		SetPlayerPosition(aPlayers[i], i+1, Length(aPlayers), aGameWidth);
		SendUDPMessage(lMsg , aPlayers[i].con);
	end;
end;

procedure HandleClient(var aPlayer : Player; var aBalls : Balls; const aPlayerCount, aMyIDX : Integer);
var
	lPlayerMessage : String;
begin
		MovePlayer(aPlayer, False);
		Draw(aPlayer, aBalls, aPlayerCount, aMyIDX);
end;

procedure HandleHost(var aPlayers : Players; var aBalls : Balls; const aGameWidth, aGameHeight, aPlayerCount, aMyIDX : Integer);
var
	i : Integer;
begin
	for i := Low(Balls) to Length(aPlayers) - 1 do
		UpdateBall(aBalls[i], aBalls, aPlayers, aGameWidth * Length(aPlayers), aGameHeight, i);
	MovePlayer(aPlayers[0], True);
	BroadcastGameData(aPlayers, aGameWidth, aGameHeight, aBalls);
	Draw(aPlayers[0], aBalls, aPlayerCount, aMyIDX);
end;

procedure Main();
var
	lBalls : Balls;
	lPlayers : Players;
	lHostConnection : Connection;
	lIsHost			: Boolean = True;
	lConnected		: Boolean = False;
	lPlayer	: Player;
	lMyIDX, lGameWidth, lGameHeight, lPlayerCount : Integer;
begin
	OpenGraphicsWindow('Pong Centipede', 960, 640);
	LoadDefaultColors();
	lPlayerCount := 1;
	lIsHost := SetupConnection(lHostConnection);
	
	if (lIsHost) then
	begin
		InitHost(lPlayers, lBalls);
		lGameWidth := ScreenWidth;
		lGameHeight:= ScreenHeight;
		lMyIDX := 1;
	end else begin
		InitPlayer(lPlayer, 'r', lHostConnection);
		InitBalls(lBalls);
	end;	
	
	repeat
		if lIsHost then
		begin
			UDPMessageReceived();
			if (ConnectionQueueSize() > 0) then
				AddPlayer(FetchConnection(), lPlayers, lPlayerCount);

			UpdateClientPosition(lPlayers);
		end else begin
			ObtainGameData(lPlayer, lBalls, lMyIdx, lGameWidth, lGameHeight, lPlayerCount, lConnected);
		end;		

		ProcessEvents();
		ClearScreen();
		
		if not lIsHost and not lConnected then
		begin			
			SendUDPMessage('AcceptMe:', lHostConnection);
			continue;
		end;
		
		if (lIsHost) then HandleHost(lPlayers, lBalls, lGameWidth, lGameHeight, lPlayerCount, lMyIDX)
		else begin HandleClient(lPlayer, lBalls, lPlayerCount, lMyIDX); end;
		
		DrawFrameRate(0,0);
		RefreshScreen(60);
	until WindowCloseRequested();
end;

begin
	Main();
end.