program WhacAMole;
uses 
SwinGame, sgTypes, sgUserInterface, SysUtils;

const 
  // numOfMole = 30;
  // Level = 10;
  NUM_HOLES = 9;
  TIME_PER_ROUND = 120;

  MOLE_WIDTH = 245;
  MOLE_HEIGHT = 163;

type
  GameState = (InMenu, InGame);
  HoleState = (Empty, Mole, MoleUp, MoleDown, Wack, Bam);
  
  HoleData = record    
    moleSprite: Sprite;
    kapow:      Sprite;
    state:      HoleState;
  end;

  HoleTimeData = record
    showAt:     Integer;
    hole:       ^HoleData; 
  end;
  
  GameData = record
    state:          GameState;
    timeComplete:   Integer;
    score:          Integer;
    molesRemaining: Integer;
    level:          Integer;
    lives:          Integer;    
    hole:           array [0..NUM_HOLES - 1] of HoleData;

    holePlan:       array of HoleTimeData;    // hole plan lays out the order the holes will go in
    planIdx:        Integer;
    roundTime:      Integer;                  // number of milliseconds in the round
    timeFactor:     Single;
    bonus:          Integer;

    lastUpdateTime: Integer;  // time of last update

    // Resources
    scoreFont:      Font;
    background:     Bitmap;
    fullBar:        Bitmap;
    emptyBar:       Bitmap;
    lifeBitmap:     Bitmap;
    gTimer:         Timer;
  end;


procedure SetupLevel(var data: GameData);
const
  START_MOLES = 9;
  MIN_TIME    = 5000;
  START_TIME  = 15200;
var
  molePct: Single;
  i, j: Integer;
  avgTimeBetween: Integer;
  holeFree: array [0 .. NUM_HOLES - 1] of Integer;
  offset: Integer;
  timeAccum: Integer;
begin
  StopTimer(data.gTimer);

  data.bonus := 0;
  data.lastUpdateTime := 0;
  data.planIdx := 0;

  if data.lives < 0 then
  begin
    data.level := 1;
    data.lives := 3;
  end;

  if data.level = 1 then data.score := 0; // reset score at level 1

  data.molesRemaining := START_MOLES + (data.level mod 10); // one more every level
  data.roundTime  := START_TIME; // - (2500 * data.level div 10);
  //if data.roundTime < MIN_TIME then data.roundTime := MIN_TIME;

  data.timeFactor := 1 + (data.level div 10);

  // generate hole plan
  molePct := 2.0 - (1.0 * (((data.level - 1) mod 10) / 10)); // drops to 100% every 10 levels
  SetLength(data.holePlan, Round(molePct * data.molesRemaining));
  WriteLn('Len ', Length(data.holePlan), ' and ', Length(holeFree));

  avgTimeBetween := data.roundTime div Length(data.holePlan);
  timeAccum := 0;

  for i := 0 to High(holeFree) do 
  begin
    holeFree[i] := -1;
    WriteLn('i ', i);
  end;

  for i := 0 to High(data.holePlan) do
  begin
    data.holePlan[i].showAt := i * avgTimeBetween;
    offset := Rnd(NUM_HOLES);

    for j := 0 to High(holeFree) do
    begin
      if holeFree[(j + offset) mod NUM_HOLES] < data.holePlan[i].showAt then break;
      if j = High(holeFree) then WriteLn('CANT FIND');
    end;

    WriteLn('At ', data.holePlan[i].showAt, ' use ', i, ' = ', (j + offset) mod NUM_HOLES);
    data.holePlan[i].hole := @data.hole[(j + offset) mod NUM_HOLES];
    holeFree[(j + offset) mod NUM_HOLES] := Round(data.holePlan[i].showAt + (500 * 12));
  end;

  // reset holes
  for i := 0 to NUM_HOLES - 1  do
  begin
    data.hole[i].state := Empty;
  end;

  StartTimer(data.gTimer);
end;

procedure EndLevel(var data: GameData);
begin
  if data.molesRemaining > 0 then
  begin
     data.lives -= 1;
  end
  else 
    data.level += 1;

  data.state := InMenu;
end;



procedure DisplayScore(score: Integer; fnt: Font);
var
  scoreTxt: String;
const
  TXT_X = 277;
  TXT_Y = 152;
begin
  scoreTxt := IntToStr(score);
  //DrawText(scoreTxt, ColorBlack, fnt, TXT_X - TextWidth(fnt, scoreTxt), TXT_Y);
end;

procedure DisplayMolesRemaining(moleRemain: Integer; fnt: Font);
var
  moleTxt: String;
const
  MOLE_TXT_X = 448;
  MOLE_TXT_Y = 152;
begin
  moleTxt := IntToStr(moleRemain);
  //DrawText(moleTxt, ColorBlack, fnt, MOLE_TXT_X - TextWidth(fnt, moleTxt), MOLE_TXT_Y);
end;

procedure DisplayCurrentLevel(level: Integer; fnt: Font);
var
    lvlTxt: String;
const
    LEVEL_TXT_X = 720;
    LEVEL_TXT_Y = 152;
begin
  lvlTxt := IntToStr(level);
  //DrawText(lvlTxt, ColorBlack, fnt, LEVEL_TXT_X - TextWidth(fnt, lvlTxt), LEVEL_TXT_Y);
end;

procedure DrawLife(const gData : GameData);
var  
  i: Integer;
const
  H_GAP   = 38;
  LIFE_X  = 73;
  LIFE_Y  = 89;  
begin  
  for i := 0 to gData.lives - 1 do
  begin
    DrawBitmap(gData.lifeBitmap, LIFE_X + H_GAP * i, LIFE_Y);    
  end;  
end;

procedure DrawBackGroundImage();
begin  
  DrawBitmap ('bgImage', 0, 0);  
end;

procedure DrawProgressBar(var gData : GameData);
var
  rect: Rectangle;
const
  BAR_HEIGHT = 279;
  BAR_WIDTH = 34;
  BAR_X = 32;
  BAR_Y = 439;
begin
  rect := RectangleFrom(0, 0, BAR_WIDTH, round(TimerTicks(gData.gTimer) / gData.roundTime * BAR_HEIGHT));

  DrawBitmap(gData.fullBar, BAR_X, BAR_Y);
  DrawBitmap(gData.emptyBar, BAR_X, BAR_Y, OptionPartBmp(rect));
end;

procedure InitMolesSprite(var gData : GameData);
var  
  i, y, xM, vGap, hGap : Integer;  
begin
  xM := 260;  
  hGap := 0;
  vGap := 0;  
  y := 252;
    
  for i := 0 to NUM_HOLES - 1  do
  begin
    gData.hole[i].moleSprite := CreateSprite(BitmapNamed('WhacAMole'), AnimationScriptNamed('WhacAMole_temp'));    
    gData.hole[i].kapow := CreateSprite(BitmapNamed('Kapow'), AnimationScriptNamed('Kapow_temp'));    
    // if i = 3 then
    // begin
    //   hGap := 0;
    //   vGap := 170;      
    // end;
    // if i = 6 then 
    // begin
    //   hGap := 0;
    //   vGap := 333;
    // end;

    hGap := (i mod 3) * 250;
    vGap := (i div 3) * 167;

    SpriteSetX(gData.hole[i].moleSprite, xM+hGap);
    SpriteSetY(gData.hole[i].moleSprite, y+vGap);    

    SpriteSetX(gData.hole[i].kapow, xM+hGap);    
    SpriteSetY(gData.hole[i].kapow, y+vGap);    

    gData.hole[i].state := Empty;    
    // hGap += 250;    
  end; 
end;

procedure DoHoleWhacked(var gData: GameData; var hole: HoleData);
begin  
  if gData.molesRemaining > 0 then
    gData.molesRemaining -= 1;

  // Update bonus and score
  gData.bonus += 1;
  gData.Score += gData.bonus;

  // Update hole state and animations
  hole.state := Bam;
  SpriteStartAnimation(hole.moleSprite, 'Hide');
  SpriteStartAnimation(hole.kapow, 'Kapow');
end;

procedure DoMoleAction(var hole: HoleData);
begin
  // If the animation has ended...
  if SpriteAnimationHasEnded(hole.moleSprite) then
  begin
    // Start going down...
    hole.state := MoleDown;
    SpriteStartAnimation(hole.moleSprite, 'MoleDown');
  end;
end;

procedure ShowNextMole(var data: GameData); 
var
  hole: ^HoleData;
begin
  if data.planIdx >= Length(data.holePlan) then exit;

  if TimerTicks(data.gTimer) >= data.holePlan[data.planIdx].showAt then
  begin
    hole := data.holePlan[data.planIdx].hole;
    if not (hole^.state = Empty) then WriteLn('Hole not empty!');

    hole^.state := MoleUp;
    SpriteStartAnimation(hole^.moleSprite, 'MoleUp', true);

    data.planIdx += 1;
  end;
end;

procedure PlayGame(var gData : GameData);
var
  i : Integer;
  pct: Single;
  timePassed: Integer;
begin
    timePassed := TimerTicks(gData.gTimer) - gData.lastUpdateTime;
    gData.lastUpdateTime := TimerTicks(gData.gTimer);

    pct := timePassed / (500 * gData.timeFactor);

    ShowNextMole(gData);

    for i := 0 to NUM_HOLES - 1 do
    begin
      UpdateSprite(gData.hole[i].moleSprite, pct);
      UpdateSprite(gData.hole[i].kapow, pct);
      
      case gData.hole[i].state of
        MoleUp:   if SpriteAnimationHasEnded(gData.hole[i].moleSprite) then gData.hole[i].state := Mole;
        MoleDown: if SpriteAnimationHasEnded(gData.hole[i].moleSprite) then gData.hole[i].state := Empty;
        Mole:     DoMoleAction(gData.hole[i]);
        Wack:     DoHoleWhacked(gData, gData.hole[i]);
        Bam:
        begin
          if SpriteAnimationHasEnded(gData.hole[i].kapow) then gData.hole[i].state := Empty;          
        end;
      end;      
    end;

    if TimerTicks(gData.gTimer) >= gData.roundTime then
    begin
      EndLevel(gData);
      gData.state := InMenu;
      SetupLevel(gData);
    end    

    
end;

procedure DrawMoles(const gData: GameData);
var
  i: Integer;
begin
  for i := 0 to NUM_HOLES - 1 do
  begin      
    if gData.hole[i].state = Bam then 
      DrawSprite(gData.hole[i].kapow)
    else
      DrawSprite(gData.hole[i].moleSprite);    
  end;
end;

procedure DrawGame(var gData : GameData);
begin
  DrawBitmap(gData.background, 0, 0);
  DrawLife(gData);
  DrawProgressBar(gData);
  DisplayScore(gData.Score, gData.scoreFont);
  DisplayMolesRemaining(gData.molesRemaining, gData.scoreFont);
  DisplayCurrentLevel(gData.Level, gData.scoreFont);
  DrawMoles(gData);
  DrawFramerate(0,0);
  RefreshScreen();
end;

procedure HandleInput(var gData : GameData);
var
  mousePos : Point2D;
  i : Integer;
begin

  if MouseClicked(LeftButton) then
  begin
    mousePos := MousePosition();
    
    for i := 0 to NUM_HOLES - 1 do
    begin
      if SpriteOnScreenAt(gData.hole[i].moleSprite, mousePos) then
      begin
        if not (gData.hole[i].state = Bam) then 
        begin
          gData.hole[i].state := Wack;
          exit; // skip resetting bonus
        end;

        break;  // exit loop, resetting bonus as if miss
      end;
    end;

    // Missed holes...
    gData.bonus := 0;
  end;
end;

procedure InitGame(var gData : GameData);  
begin
  gData.state         := InMenu;
  gData.score         := 0;
  gData.lives         := 3;
  gData.level         := 1;
  gData.timeComplete  := 0;
  
  InitMolesSprite(gData);  
end;

procedure LoadResources(var data: GameData);
begin
  LoadResourceBundle('WhacAMoleBundle.txt');  

  InitGame(data);

  data.background := BitmapNamed('bgImage');
  data.emptyBar   := BitmapNamed('progressEmpty');
  data.fullBar    := BitmapNamed('progressFull');
  data.lifeBitmap := BitmapNamed('life');
  data.scoreFont  := FontNamed('scoreFont');
  data.gTimer     := CreateTimer();

  // LoadSoundEffect('comedy_boing.ogg');
end;

procedure Main(); 
var
  gData : GameData;  
  i:  Integer;
begin
  OpenAudio();
  OpenGraphicsWindow('Whack a Mole', 1024, 768);
  LoadDefaultColors();
  LoadResources(gData);
  
  PlayMusic('WhacMusic');

  SetupLevel(gData);

  repeat // The game loop...
    ProcessEvents();    
    HandleInput(gData);

    PlayGame(gData);    

    DrawGame(gData);
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
  Main();
end.