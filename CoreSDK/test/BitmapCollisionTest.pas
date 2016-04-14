program BitmapCollisionTest;
uses sgTypes, SwinGame;

type
  MySprite = record
    x, y: Single;
    hasAnimation: Boolean;
    anim: Animation;
    bmp: Bitmap;
  end;

procedure LoadResources();
begin
  LoadBitmapNamed('bubble', 'bubble.png');
  LoadResourceBundle('dance_bundle.txt');
end;

function CreateMySprite(x, y: Single; useFrog: Boolean): MySprite;
begin
  result.x := x;
  result.y := y;
  result.hasAnimation := useFrog;
  if useFrog then
  begin
    result.bmp := BitmapNamed('FrogBmp');
    result.anim := CreateAnimation('LoopFrontWalk', AnimationScriptNamed('WalkingScript'));
  end
  else
    result.bmp := BitmapNamed('bubble');
end;

function MySpritesCollide(const s1, s2: MySprite): Boolean;
var
  c1, c2: Integer;
begin
  if s1.hasAnimation or s2.hasAnimation then
  begin
    if s1.hasAnimation then c1 := AnimationCurrentCell(s1.anim)
    else c1 := 0;
    if s2.hasAnimation then c2 := AnimationCurrentCell(s2.anim)
    else c2 := 0;

    result := CellCollision(s1.bmp, c1, s1.x, s1.y, s2.bmp, c2, s2.x, s2.y);
  end
  else
    result := BitmapCollision(s1.bmp, s1.x, s1.y, s2.bmp, s2.x, s2.y);
end;

procedure DrawMySprite(const sprt: MySprite);
begin
  if sprt.hasAnimation then
    DrawAnimation(sprt.anim, sprt.bmp, sprt.x, sprt.y)
  else
    DrawBitmap(sprt.bmp, sprt.x, sprt.y);
end;

procedure UpdateMySprite(var sprt: MySprite);
begin
  if sprt.hasAnimation then
  begin
    UpdateAnimation(sprt.anim);
  end;
end;

procedure Highlight(const sprt: MySprite);
begin
  DrawRectangle(ColorRed, sprt.x, sprt.y, BitmapCellWidth(sprt.bmp), BitmapCellHeight(sprt.bmp));
end;

procedure TestCollisions(const sprites: array of MySprite);
var
  i, j: Integer;
begin
  for i := 0 to High(sprites) - 1 do
  begin
    for j := i + 1 to High(sprites) do
    begin
      if MySpritesCollide(sprites[i], sprites[j]) then
      begin
        Highlight(sprites[i]);
        Highlight(sprites[j]);
      end;
    end;
  end;
end;

procedure Main();
var
  sprites: array [0..2] of MySprite;
  i: Integer;
begin
  OpenWindow('Bitmap Collision Test', 600, 600);
  LoadResources();

  sprites[0] := CreateMySprite(100, 100, false);
  sprites[1] := CreateMySprite(300, 100, true);
  sprites[2] := CreateMySprite(500, 100, false);

  repeat
    ProcessEvents();
    if KeyDown(LeftKey) then sprites[2].x -= 3;
    if KeyDown(RightKey) then sprites[2].x += 3;

    ClearScreen(ColorWhite);

    for i := 0 to High(sprites) do
    begin
      UpdateMySprite(sprites[i]);
      DrawMySprite(sprites[i]);
    end;

    TestCollisions(sprites);

    RefreshScreen();
  until WindowCloseRequested();
end;

begin
  Main();
end.
