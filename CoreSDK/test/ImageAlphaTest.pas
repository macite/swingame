program ImageAlphaTest;
uses SwinGame;

procedure Main();
var
	img1, img2 : Bitmap;
begin
	OpenGraphicsWindow('Image Alpha Test', 800, 600);
	LoadDefaultColors();

	img1 := LoadBitmap('Obs1.png');
	img2 := LoadBitmap('ball_small.png');

	repeat
		ProcessEvents();
		ClearScreen(ColorWhite);

		DrawBitmap(img1, 0, 0);
		DrawBitmap(img2, 0, BitmapHeight(img1));

		if BitmapPointCollision(img1, 0, 0, MousePosition()) then
		begin
			FillRectangle(ColorRed, 0, 0, 10, 10);
		end;

		if BitmapPointCollision(img2, 0, BitmapHeight(img1), MousePosition()) then
		begin
			FillRectangle(ColorRed, 0, 10, 10, 10);
		end;

		RefreshScreen(60);
	until WindowCloseRequested();
end;

begin
	Main();
end.