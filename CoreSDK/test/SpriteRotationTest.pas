program SpriteRotationTest;
uses SwinGame, sgTypes;

procedure Main();
var
	sprt: Sprite;
begin
	OpenGraphicsWindow('Sprite Rotation', 600, 600);
	sprt := CreateSprite(BitmapNamed('rocket_sprt.png'));

	SpriteSetMoveFromAnchorPoint(sprt, true);

	SpriteSetX(sprt, 300);
	SpriteSetY(sprt, 300);

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


		DrawSprite(sprt);
		UpdateSprite(sprt);

		DrawCircle(ColorGreen, SpriteCollisionCircle(sprt));

		DrawLine(ColorGreen, LineFromVector(CenterPoint(sprt), MatrixMultiply(RotationMatrix(SpriteRotation(sprt)), VectorMultiply(SpriteVelocity(sprt), 10.0))));

		WriteLn(SpriteRotation(sprt):4:2);

		RefreshScreen();
	until WindowCloseRequested();

end;

begin
	Main();
end.
