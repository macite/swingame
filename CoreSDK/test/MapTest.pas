program MapTest;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, sgMaps, sgCamera,
  sgInput, sgAudio, sgShared, SysUtils,
  StrUtils, sgResources, sgGraphics, sgText,sgNamedIndexCollection, sgCharacters, sgAnimations, sgSprites, sgGeometry;
//Main procedure
const
	speed = 5;

procedure UpdateCamera();
begin

  if KeyDown(VK_LEFT) then
    MoveCameraBy(-2,0)
  else if KeyDown(VK_RIGHT) then
    MoveCameraBy(2,0)
  else if KeyDown(VK_UP) then
    MoveCameraBy(0,-2)
  else if KeyDown(VK_DOWN) then
    MoveCameraBy(0,2)
end;
//updates all event driven actions.
procedure UpdateActions(map:Map);
begin
if not KeyDown(VK_LSHIFT) then
  UpdateCamera();


  
  if MouseClicked(LeftButton) then
    UpdateSelect(map);
end;
//updates camera position based on user input

procedure Main();
var
  myMap: Map;
	c: Character;
  p2d : Point2DArray;
  i,j,k : Longint;
  
begin
  OpenAudio();
  OpenGraphicsWindow('Maps Tests', 640, 480);
  
	SetLength(p2d, 5);
	
	p2d[0].x := 0;
	p2d[0].y := -speed;	
	p2d[1].x := 0;
	p2d[1].y := speed;	
	p2d[2].x := -speed;
	p2d[3].y := 0;	
	p2d[3].x := speed;
	p2d[3].y := 0;
	p2d[4].x := 0;
	p2d[4].y := 0;
	
  c := LoadCharacter('CharasDude.txt');
  writeln('Map Loading');
  //myMap := NewMap();
  myMap := LoadMap('test1.txt');
  writeln('Map Loaded');
  //SetTileValue(myMap,TileAt(myMap,2,2), 'test', 0.8);
  //SaveMap(myMap, 'test3.txt');
  //writeln('have not changed dimension');
  //writeln('have  changed dimension');
  repeat // The game loop...
    ProcessEvents();


      if KeyDown(vk_Up) then c^.CharSprite^.velocity:= p2d[0]
      else if KeyDown(vk_Down) then c^.CharSprite^.velocity:= p2d[1]
      else if KeyDown(vk_Left) then c^.CharSprite^.velocity:= p2d[2]
      else if KeyDown(vk_Right) then c^.CharSprite^.velocity:= p2d[3]
      else c^.CharSprite^.velocity:= p2d[4];
      if KeyTyped(vk_Q) then MapSetDimension(myMap, 5,5,1,50,50,False);
      if keyTyped(vk_e) Then SetTileKind(TileAt(myMap,0,0),0);
      if keyTyped(vk_w) Then SetTileKind(TileAt(myMap,0,0),2);
      if KeyTyped(vk_1) then ToggleLayerVisibility(c, 1);
      if keyTyped(vk_s) Then SaveMap(myMap, 'test3.txt');
      if keyTyped(vk_l) Then myMap := LoadMap('test3.txt');
      if keyTyped(vk_k) Then myMap := LoadMap('test1.txt');
      if keyTyped(vk_r) Then RemoveValue(myMap, 'test');
      if keyTyped(vk_z) Then FreeMap(myMap);
      if keyTyped(vk_p) then
      begin
       for k := 0 to 1000 do
       begin
          myMap := LoadMap('test1.txt');
        FreeMap(myMap);
        //ReleaseAllResources();
        myMap:=nil;
       end;
      end;

    UpdateSpriteAnimation(c^.CharSprite);
    //if KeyDown(vk_m) then
      MoveSprite(c^.CharSprite);

    CenterCameraOn(c, VectorTo(0,0));
        UpdateActions(myMap);
    ClearScreen(Colorwhite);
    DrawMap(myMap);
   // writeln('have not drawn');
    DrawMapGrid(myMap);
    //writeln('have  drawn');

    DrawMapDebug(myMap);
    DrawCharacter(c);
	//	DrawCharacterWithStationary(c, 0, 1);
    
    if SpriteHasCollidedWithTile(myMap, 2, c^.CharSprite, i, j) then
    begin
      //HighLightTile(@myMap^.Tiles[j, i], myMap);
      //WriteLn('Character Velocity: ', PointToString(c^.CharSprite^.velocity));
      MoveOut(myMap,c^.CharSprite,i, j);
    end;
    
    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  ReleaseAllResources();
  CloseAudio();
end;
begin
  Main();
end.
