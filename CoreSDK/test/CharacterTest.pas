program CharacterTest;

uses
  sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, sgCharacters, sgNamedIndexCollection, sgSprites, sgInput;
    
const
  speed = 5;
      
procedure Main();
var
  c, isometric: Character;
  i: integer;
  p2d : Array of Point2D;
  
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
   
  c:= LoadCharacter('CharasDude.txt'); 
  
  isometric:= LoadCharacter('IsometricDude.txt'); 
  
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
   
   
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    DrawCharacterWithStationary(c, 0, 1);
    DrawCharacterWithStationary(isometric, 0, 1);
//    DrawCharacter(c);

    UpdateSpriteAnimation(c^.CharSprite);
    MoveSprite(c^.CharSprite);
    UpdateSpriteAnimation(isometric^.CharSprite);
    MoveSprite(isometric^.CharSprite);
    
    {if KeyDown(vk_Up) then c^.CharSprite^.velocity:= p2d[0]
        if KeyDown(vk_Down) then c^.CharSprite^.velocity:= p2d[1]
        if KeyDown(vk_Left) then c^.CharSprite^.velocity:= p2d[2]
        if KeyDown(vk_Right) then c^.CharSprite^.velocity:= p2d[3]
        else c^.CharSprite^.velocity:= p2d[4];}

    if KeyDown(vk_Down) then
      c^.CharSprite^.position.y +=  10;     
    if KeyDown(vk_Right) then
      c^.CharSprite^.position.x +=  10;        
  
    if KeyDown(vk_Q) then isometric^.CharSprite^.velocity:= VectorTo(-speed,-speed)
    else if KeyDown(vk_W) then isometric^.CharSprite^.velocity:= VectorTo(speed, -speed)
    else if KeyDown(vk_A) then isometric^.CharSprite^.velocity:= VectorTo(-speed, speed)
    else if KeyDown(vk_S) then isometric^.CharSprite^.velocity:= VectorTo(speed, speed)
    else isometric^.CharSprite^.velocity:= p2d[4];
    
    if KeyTyped(vk_1) then ToggleLayerVisibility(c, 1);
    if KeyTyped(vk_2) then ToggleLayerVisibility(isometric, 1);
        
    DrawFramerate(0,0);
    RefreshScreen(60);
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
