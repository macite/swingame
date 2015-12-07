program SpriteRotationTest;
uses SwinGame, sgTypes;

function SpriteLocationMatrix(s: Sprite): Matrix2D;
var
	scale, newX, newY, w, h: Single;
	anchorMatrix: Matrix2D;
begin
	scale := SpriteScale(s);
	w := SpriteLayerWidth(s, 0);
	h := SpriteLayerHeight(s, 0);
	
	anchorMatrix := TranslationMatrix(SpriteAnchorPoint(s));

	result := IdentityMatrix();
	result := MatrixMultiply(MatrixInverse(anchorMatrix), result);
	result := MatrixMultiply(RotationMatrix(SpriteRotation(s)), result);
	result := MatrixMultiply(anchorMatrix, result);

	newX := SpriteX(s) - (w * scale / 2.0) + (w / 2.0);
	newY := SpriteY(s) - (h * scale / 2.0) + (h / 2.0);
	result := MatrixMultiply(TranslationMatrix(newX / scale, newY / scale), result);

	result := MatrixMultiply(ScaleMatrix(scale), result);	
end;

procedure Main();
var
	sprt, s2: Sprite;
	tri, initTri: Triangle;
	triB, initTriB: Triangle;
	r: Rectangle;
begin
	OpenGraphicsWindow('Sprite Rotation', 600, 600);
	sprt := CreateSprite(BitmapNamed('rocket_sprt.png'));
	SpriteSetMoveFromAnchorPoint(sprt, true);
	SpriteSetX(sprt, 300);
	SpriteSetY(sprt, 300);

	s2 := CreateSprite(BitmapNamed('rocket_sprt.png'));
	SpriteSetMoveFromAnchorPoint(s2, true);
	SpriteSetX(s2, 100);
	SpriteSetY(s2, 100);

	r := RectangleFrom(400, 100, 100, 50);

	initTri := TriangleFrom(0, 0, BitmapWidth(BitmapNamed('rocket_sprt.png')), BitmapHeight(BitmapNamed('rocket_sprt.png')), 0, BitmapHeight(BitmapNamed('rocket_sprt.png')));
	initTriB := TriangleFrom(BitmapWidth(BitmapNamed('rocket_sprt.png')), 0, BitmapWidth(BitmapNamed('rocket_sprt.png')), BitmapHeight(BitmapNamed('rocket_sprt.png')), 0, 0);

	repeat
		ProcessEvents();

		ClearScreen(ColorWhite);
		DrawFramerate(0, 0);

		if KeyDown(LeftKey) then
		begin
			SpriteSetRotation(sprt, SpriteRotation(sprt) - 5);
		end;

		if KeyDown(RightKey) then
		begin
			SpriteSetRotation(sprt, SpriteRotation(sprt) + 5);
		end;

		if KeyDown(ShiftKey) then
		begin
			if KeyDown(UpKey) then
			begin
				SpriteSetScale(sprt, SpriteScale(sprt) + 0.1);
			end;

			if KeyDown(DownKey) then
			begin
				SpriteSetScale(sprt, SpriteScale(sprt) - 0.1);
			end;
		end
		else
		begin
			if KeyDown(UpKey) then
			begin
				SpriteSetDY(sprt, SpriteDY(sprt) - 0.1);
			end;

			if KeyDown(DownKey) then
			begin
				SpriteSetDY(sprt, SpriteDY(sprt) + 0.1);
			end;
		end;

		if KeyTyped(Key0) then SpriteSetRotation(sprt, -9045);
		if KeyTyped(Key9) then SpriteSetRotation(sprt, 9045);

		tri := initTri;
		triB := initTriB;
		ApplyMatrix(SpriteLocationMatrix(sprt), tri);
		ApplyMatrix(SpriteLocationMatrix(sprt), triB);
		FillTriangle(ColorGreen, tri);
		FillTriangle(ColorGreen, triB);

		tri := initTri;
		triB := initTriB;
		ApplyMatrix(SpriteLocationMatrix(s2), tri);
		ApplyMatrix(SpriteLocationMatrix(s2), triB);
		FillTriangle(ColorBlue, tri);
		FillTriangle(ColorBlue, triB);

		if SpriteRectCollision(sprt, r) then
			FillRectangle(ColorPurple, r)
		else
			DrawRectangle(ColorPurple, r);

		DrawSprite(sprt);
		DrawSprite(s2);
		UpdateSprite(sprt);

		if SpriteCollision(sprt, s2) then
		begin
			DrawCircle(ColorRed, SpriteCollisionCircle(s2));
		end;

		DrawCircle(ColorGreen, SpriteCollisionCircle(sprt));

		DrawLine(ColorGreen, LineFromVector(CenterPoint(sprt), MatrixMultiply(RotationMatrix(SpriteRotation(sprt)), VectorMultiply(SpriteVelocity(sprt), 10.0))));

		RefreshScreen();
	until WindowCloseRequested();

end;

begin
	Main();
end.
